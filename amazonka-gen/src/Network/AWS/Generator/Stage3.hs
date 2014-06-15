{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
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

import           Control.Monad
import           Data.String
import           Data.String.CaseConversion
import qualified Data.Text                    as Text
import           Data.Text.Lazy               (Text)
import qualified Data.Text.Lazy               as LText
import           Data.Text.Lazy.Builder
import qualified Data.Text.Lazy.Encoding      as LText
import           Network.AWS.Generator.Stage2
import           Network.AWS.Generator.Types
import           System.FilePath
import           Text.Shakespeare             (RenderUrl)
import           Text.Shakespeare.Text

render :: Service -> [(FilePath, Text)]
render s@Service{..} = service : map operation s2Operations
  where
    service = (path s,) . layout s2Namespace $
        case s2Type of
            RestXML  -> $(textFile "tmpl/service-rest-xml")
            RestJSON -> $(textFile "tmpl/service-rest-json")
            RestS3   -> $(textFile "tmpl/service-s3")
            JSON     -> $(textFile "tmpl/service-json")
            Query    -> $(textFile "tmpl/service-query")

    operation o@Operation{..} = (path o,) . layout o2Namespace $
        case s2Type of
            RestXML  -> $(textFile "tmpl/operation-rest-xml")
            RestJSON -> $(textFile "tmpl/operation-rest-json")
            RestS3   -> $(textFile "tmpl/operation-s3")
            JSON     -> $(textFile "tmpl/operation-json")
            Query    -> $(textFile "tmpl/operation-query")

layout :: NS -> (RenderUrl url -> Builder) -> Text
layout namespace content = toLazyText $
    $(textFileReload "tmpl/_include/layout") renderURL

renderURL :: RenderUrl url
renderURL _ _ = ""

class ToPath a where
    path :: a -> FilePath

instance ToPath NS where
    path = (<.> "hs") . Text.unpack . Text.intercalate "/" . unNS

instance ToPath Service where
    path = path . s2Namespace

instance ToPath Operation where
    path = path . o2Namespace
