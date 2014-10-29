{-# LANGUAGE BangPatterns        #-}
{-# LANGUAGE FlexibleInstances   #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TupleSections       #-}

-- Module      : Gen.Render
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Gen.Render where

import           Control.Applicative
import           Control.Error
import           Control.Lens        ((^.))
import           Control.Monad
import           Data.Aeson
import           Data.Char
import qualified Data.Foldable       as Fold
import           Data.HashMap.Strict (HashMap)
import qualified Data.HashMap.Strict as Map
import           Data.List           (intersperse)
import           Data.Monoid
import           Data.Text           (Text)
import qualified Data.Text           as Text
import qualified Data.Text.Lazy.IO   as LText
import           Gen.Names
import qualified Data.Vector         as Vector
import           Gen.AST       hiding (HashMap)
import           Gen.Log
import           Gen.ToJSON    ()
import           System.Directory
import           System.FilePath     hiding (normalise)
import           Text.EDE            (Template)
import qualified Text.EDE            as EDE
import           Text.EDE.Filters

class ToPath a where
    toPath :: a -> FilePath

instance ToPath FilePath where
    toPath = id

instance ToPath Library where
    toPath (Library l) = Text.unpack l

instance ToPath Abbrev where
    toPath (Abbrev a) = "gen/Network/AWS" </> Text.unpack a <.> "hs"

instance ToPath NS where
    toPath (NS xs) = "gen" </> Text.unpack (Text.intercalate "/" xs) <.> "hs"

data Templates = Templates
    { tmplCabal   :: Template
    , tmplRoot    :: Template
    , tmplService :: ServiceType -> (Template, Template)
    }

getTemplates :: Script Templates
getTemplates = do
    dir  <- scriptIO getCurrentDirectory

    let load f = loadTemplate (dir </> "tmpl" </> f <.> "ede")

    ctor <- Templates <$> load "cabal" <*> load "root"

    !xml <- (,) <$> load "types-xml"   <*> load "operation-xml"
    !js  <- (,) <$> load "types-json"  <*> load "operation-json"
    !qry <- (,) <$> load "types-query" <*> load "operation-query"

    return $! ctor $ \t ->
        case t of
            RestXml  -> xml
            RestJson -> js
            RestS3   -> xml
            Json     -> js
            Query    -> qry

loadTemplate :: FilePath -> Script Template
loadTemplate f =
    scriptIO (say "Parse Template" f *> EDE.eitherParseFile f)
        >>= hoistEither

getAssets :: FilePath -> Script [FilePath]
getAssets f = map (combine f) . filter dots <$> scriptIO (getDirectoryContents f)
  where
    dots "."  = False
    dots ".." = False
    dots _    = True

render :: FilePath -> FilePath -> [Service] -> Script ()
render dir assets ss = do
    as <- getAssets assets
    ts <- getTemplates
    mapM_ (go as ts) ss
  where
    write lbl f t e = render' lbl dir f t (env e)

    go !as !Templates{..} !s@Service{..} = do
        say "Create Directory" src
        scriptIO (createDirectoryIfMissing True src)

        write "Render Types" (rel (_svcNs ^. nsTypes)) t s

        forM_ _svcOperations $ \x ->
            write "Render Operation" (rel (_opNamespace x)) o x

        write "Render Root"  (rel (_svcNs ^. nsRoot)) tmplRoot s
        write "Render Cabal" (rel (lib <.> "cabal"))  tmplCabal s

        forM_ as $ \x ->
            let f = rel (takeFileName x)
             in say "Copying Asset" f >> scriptIO (copyFile x f)
      where
        (t, o) = tmplService _svcType

        src = rel ("src" :: FilePath)

        rel :: ToPath a => a -> FilePath
        rel = combine dir . combine lib . toPath

        lib = toPath _svcLibrary

render' :: ToJSON a => Text -> FilePath -> FilePath -> Template -> a -> Script ()
render' lbl d f t e = do
    scriptIO (say lbl file)
    hs <- hoistEither $ EDE.eitherRenderWith filters t (env e)
    scriptIO $ do
        say
        createDirectoryIfMissing True dir
        LText.writeFile file hs
  where
    file = d </> f
    dir  = dropFileName file

env :: ToJSON a => a -> Object
env x =
    case toJSON x of
        Object o -> o
        e        -> error ("Failed to extract JSON Object from: " ++ show e)

filters :: HashMap Text Fun
filters = EDE.defaultFilters
    <> Map.fromList fs
    <> Map.fromList gs
  where
    fs = funN "pad"     pad         [4, 8]
      ++ funN "indent"  indent      [4, 6, 8, 10]
      ++ funN "wrap"    (wrap "")   [66, 76, 80]
      ++ funN "above"   (wrap "| ") [66, 76]
      ++ funN "below"   (wrap "^ ") [66, 76]
      where
        funN k g = Fold.foldl' (f k g) []

        f k g xs n = (k <> Text.pack (show n), Fun TText TText (g n)) : xs

    wrap p n t =
        case normalise n t of
            []     -> ""
            (x:xs) -> Text.intercalate "\n" . map ("-- " <>) $ p <> x : xs

    gs = [ ("dropLower",    Fun TText TText (Text.dropWhile (not . isUpper)))
         , ("notEmptyText", Fun TText TBool (not . Text.null . Text.strip))
         , ("lens",         Fun TText TText lensPrefix)
         , ("prism",        Fun TText TText prismPrefix)
         , ("field",        Fun TText TText fieldPrefix)
         , ("haddock",      Fun TText TText haddock)
         , ("prismTypes",   Fun TList TText prismTypes)
         , ("fieldParams",  Fun TList TText (Text.intercalate " " . fieldParams))
         , ("tupleParams",  Fun TList TText (tuple . fieldParams))
         ]
      where
        haddock t = case Text.splitOn "." t of
            [] -> ""
            xs -> Text.intercalate "\n"
                . intersperse "."
                . map ((<> ".") . Text.strip)
                $ init xs

        prismTypes v = tuple $
            case Vector.toList v of
                [x] -> maybeToList (obj "wrapped" x)
                xs  -> mapMaybe (obj "type") xs
          where

            obj k (Object o) = Map.lookup k o >>= text
            obj _ _          = Nothing

            text (String t) = Just t
            text _          = Nothing

        fieldParams v =
            map (mappend "p" . Text.pack . show) [1..Vector.length v]

        tuple [x] = x
        tuple xs  = "(" <> Text.intercalate ", " xs <> ")"

say' :: Text -> String -> Script ()
say' lbl = scriptIO . say lbl
