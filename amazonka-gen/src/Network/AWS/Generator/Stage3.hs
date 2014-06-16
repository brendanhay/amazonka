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
import           Data.String
import           Data.String.CaseConversion
import qualified Data.Text                    as Text
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding      as LText
import qualified Data.Text.Lazy.IO            as LText
import           Network.AWS.Generator.Stage2
import           Network.AWS.Generator.Types
import           System.Directory
import           System.FilePath
import           Text.EDE                     (Template)
import qualified Text.EDE                     as EDE

data Templates = Templates
    { tmplCabal   :: Template
    , tmplService :: Type -> (Template, Template)
    }

templates :: Script Templates
templates = do
    !cbl  <- load "tmpl/cabal.ede"

    !rxml <- (,)
        <$> load "tmpl/service-rest-xml.ede"
        <*> load "tmpl/operation-rest-xml.ede"

    !rjs  <- (,)
        <$> load "tmpl/service-rest-json.ede"
        <*> load "tmpl/operation-rest-json.ede"

    !s3   <- (,)
        <$> load "tmpl/service-s3.ede"
        <*> load "tmpl/operation-s3.ede"

    !js   <- (,)
        <$> load "tmpl/service-json.ede"
        <*> load "tmpl/operation-json.ede"

    !qry  <- (,)
        <$> load "tmpl/service-query.ede"
        <*> load "tmpl/operation-query.ede"

    return $! Templates cbl $ \t ->
        case t of
            RestXML  -> rxml
            RestJSON -> rjs
            RestS3   -> s3
            JSON     -> js
            Query    -> qry
  where
    load p = scriptIO (EDE.eitherParseFile p) >>= hoistEither

render' :: FilePath -> Template -> Object -> Script ()
render' p t o = do
    hs <- hoistEither $ EDE.eitherRender t o
    scriptIO $ createDirectoryIfMissing True (dropFileName p)
        >> LText.writeFile p hs

render :: FilePath -> [Service] -> Templates -> Script ()
render dir ss Templates{..} = do
    forM_ ss $ \s@Service{..} -> do
        let (svc, oper) = tmplService s2Type

        render' (dir </> path s2Namespace) svc (env s)

        forM_ s2Operations $ \o@Operation{..} ->
            render' (dir </> path o2Namespace) oper (env o)

    render' (dir </> "amazonka.cabal") tmplCabal $ env (Cabal ss)

base :: FilePath
base = "lib"

class ToPath a where
    path :: a -> FilePath

instance ToPath NS where
    path (NS xs) = base </> (Text.unpack $ Text.intercalate "/" xs) <.> "hs"

instance ToPath Service where
    path = path . s2Namespace

instance ToPath Operation where
    path = path . o2Namespace
