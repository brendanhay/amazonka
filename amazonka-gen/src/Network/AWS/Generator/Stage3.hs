{-# LANGUAGE BangPatterns      #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TupleSections     #-}

-- Module      : Network.AWS.Generator.Stage3
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Generator.Stage3 where

import           Control.Applicative
import           Control.Error
import           Control.Monad
import           Data.Aeson
import qualified Data.Foldable                as Fold
import           Data.HashMap.Strict          (HashMap)
import qualified Data.HashMap.Strict          as Map
import           Data.List
import           Data.Maybe
import           Data.Ord
import           Data.Semigroup
import           Data.String
import           Data.String.CaseConversion
import qualified Data.Text                    as Text
import qualified Data.Text.IO                 as Text
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding      as LText
import qualified Data.Text.Lazy.IO            as LText
import           Data.Text.Util
import           Network.AWS.Generator.Stage2
import           Network.AWS.Generator.Types
import           System.Directory
import           System.FilePath              hiding (normalise)
import           Text.EDE                     (Resolver, Template)
import qualified Text.EDE                     as EDE
import           Text.EDE.Filters

data Templates = Templates
    { tmplCabal   :: Template
    , tmplVersion :: Template
    , tmplCurrent :: Template
    , tmplService :: ServiceType -> (Template, Template)
    }

templates :: Script Templates
templates = do
    ctor  <- Templates
        <$> load "tmpl/cabal.ede"
        <*> load "tmpl/version.ede"
        <*> load "tmpl/current.ede"

    !rxml <- (,)
        <$> load "tmpl/types-rest-xml.ede"
        <*> load "tmpl/operation-rest-xml.ede"

    !rjs  <- (,)
        <$> load "tmpl/types-rest-json.ede"
        <*> load "tmpl/operation-rest-json.ede"

    !s3   <- (,)
        <$> load "tmpl/types-s3.ede"
        <*> load "tmpl/operation-s3.ede"

    !js   <- (,)
        <$> load "tmpl/types-json.ede"
        <*> load "tmpl/operation-json.ede"

    !qry  <- (,)
        <$> load "tmpl/types-query.ede"
        <*> load "tmpl/operation-query.ede"

    return $! ctor $ \t ->
        case t of
            RestXML  -> rxml
            RestJSON -> rjs
            RestS3   -> s3
            JSON     -> js
            Query    -> qry
  where
    load p = scriptIO (EDE.eitherParseFile p) >>= hoistEither

render :: FilePath -> [Service] -> Templates -> Script ()
render dir ss Templates{..} = do
    forM_ ss $ \s@Service{..} -> do
        let (svc, oper) = tmplService s2Type

        forM_ s2Operations $ \o@Operation{..} -> do
            let opath = dir </> path o2Namespace
            msg opath *> render' opath oper (env o)

        let tpath = dir </> path s2TypesNamespace
            vpath = dir </> path s2VersionNamespace

        msg tpath *> render' tpath svc (env s)
        msg vpath *> render' vpath tmplVersion (env s)

    forM_ (current ss) $ \s -> do
        let cpath = dir </> path (s2Abbrev s)
        msg cpath *> render' cpath tmplCurrent (env s)

    let cbl = dir </> "amazonka.cabal"
    msg cbl *> render' cbl tmplCabal (env (Cabal ss))
  where
    msg = fmapLT show . syncIO . putStrLn

render' :: FilePath -> Template -> Object -> Script ()
render' p t o = do
    hs <- hoistEither $ EDE.eitherRenderWith filters t o
    scriptIO $ createDirectoryIfMissing True (dropFileName p)
        >> LText.writeFile p hs

filters = defaultFilters <> Map.fromList fs
  where
    fs = funN "pad"    pad         [4, 8]
      ++ funN "indent" indent      [4, 6, 8]
      ++ funN "wrap"   (wrap "")   [66, 76, 80]
      ++ funN "above"  (wrap "| ") [66, 76]
      ++ funN "below"  (wrap "^ ") [66, 76]

    wrap p n t =
        case normalise n t of
            []       -> ""
            (x : xs) -> Text.intercalate "\n" . map ("-- " <>) $ p <> x : xs

    funN k g = Fold.foldl' (f k g) []

    f k g xs n = (k <> Text.pack (show n), Fun TText TText (g n)) : xs

base :: FilePath
base = "lib"

class ToPath a where
    path :: a -> FilePath

instance ToPath Abbrev where
    path (Abbrev a) = "Network/AWS" </> Text.unpack a <.> "hs"

instance ToPath NS where
    path (NS xs) = base </> (Text.unpack $ Text.intercalate "/" xs) <.> "hs"

instance ToPath Operation where
    path = path . o2Namespace
