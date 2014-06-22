{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Generator.Templates
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Generator.Templates where

import           Control.Applicative
import           Control.Error
import           Generator.AST
import           System.FilePath            hiding (normalise)
import           Text.EDE                   (Template)
import qualified Text.EDE                   as EDE

data Templates = Templates
    { tmplCabal   :: Template
    , tmplMake    :: Template
    , tmplVersion :: Template
    , tmplCurrent :: Template
    , tmplService :: ServiceType -> (Template, Template)
    }

templates :: Script Templates
templates = do
    ctor <- Templates
        <$> load "cabal"
        <*> load "makefile"
        <*> load "version"
        <*> load "current"

    !xml <- (,) <$> load "types-rest-xml"  <*> load "operation-rest-xml"
    !rjs <- (,) <$> load "types-rest-json" <*> load "operation-rest-json"
    !s3  <- (,) <$> load "types-s3"        <*> load "operation-s3"
    !js  <- (,) <$> load "types-json"      <*> load "operation-json"
    !qry <- (,) <$> load "types-query"     <*> load "operation-query"

    return $! ctor $ \t ->
        case t of
            RestXML  -> xml
            RestJSON -> rjs
            RestS3   -> s3
            JSON     -> js
            Query    -> qry

load :: FilePath -> Script Template
load p = scriptIO (EDE.eitherParseFile ("tmpl" </> p <.> "ede")) >>= hoistEither

-- render :: FilePath -> [Service] -> Templates -> Script ()
-- render dir ss Templates{..} = do
--     forM_ ss $ \s@Service{..} -> do
--         let (types, oper) = tmplService s2Type

--         forM_ s2Operations $ \o@Operation{..} ->
--             write (path o2Namespace) oper o

--         write (path s2TypesNamespace) types s
--         write (path s2VersionNamespace) tmplVersion s

--     forM_ (current ss) $ \s ->
--         write (path (s2Abbrev s)) tmplCurrent s

--     write "amazonka.cabal" tmplCabal (Cabal ss)
--     write "Makefile" tmplMake (Cabal ss)
--   where
--     write f t e =
--         let path = dir </> f
--          in fmapLT show (syncIO $ putStrLn path) *> render' path t (env e)

-- render' :: FilePath -> Template -> Object -> Script ()
-- render' p t o = do
--     hs <- hoistEither $ EDE.eitherRenderWith filters t o
--     scriptIO $ createDirectoryIfMissing True (dropFileName p)
--         >> LText.writeFile p hs

-- filters = defaultFilters <> Map.fromList fs
--   where
--     fs = funN "pad"    pad         [4, 8]
--       ++ funN "indent" indent      [4, 6, 8]
--       ++ funN "wrap"   (wrap "")   [66, 76, 80]
--       ++ funN "above"  (wrap "| ") [66, 76]
--       ++ funN "below"  (wrap "^ ") [66, 76]

--     wrap p n t =
--         case normalise n t of
--             []       -> ""
--             (x : xs) -> Text.intercalate "\n" . map ("-- " <>) $ p <> x : xs

--     funN k g = Fold.foldl' (f k g) []

--     f k g xs n = (k <> Text.pack (show n), Fun TText TText (g n)) : xs

-- base :: FilePath
-- base = "lib"

-- class ToPath a where
--     path :: a -> FilePath

-- instance ToPath Abbrev where
--     path (Abbrev a) = base </> "Network/AWS" </> Text.unpack a <.> "hs"

-- instance ToPath NS where
--     path (NS xs) = base </> (Text.unpack $ Text.intercalate "/" xs) <.> "hs"

-- instance ToPath Operation where
--     path = path . o2Namespace
