{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}

-- Module      : Network.AWS.Internal.XML
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal.XML
    (
    -- * XML Helpers
      withNS
    , withNS'
    , withRootNS
    , withRootNS'
    , xpTextContent
    ) where

import           Data.ByteString               (ByteString)
import           Data.Text                     (Text)
import qualified Data.Text.Encoding            as Text
import           Text.XML.Expat.Pickle.Generic

withNS :: ByteString -> XMLGeneric a
withNS ns = withNS' ns $ xmlOptions ns

withNS' :: ByteString -> XMLOptions -> XMLGeneric a
withNS' ns opts = pu { root = (mkNName ns . nnLocalPart) `fmap` root pu }
  where
    pu = genericXMLPickler opts

withRootNS :: ByteString -> ByteString -> XMLGeneric a
withRootNS ns name = withRootNS' ns name $ xmlOptions ns

withRootNS' :: ByteString -> ByteString -> XMLOptions -> XMLGeneric a
withRootNS' ns name opts = (genericXMLPickler opts)
   { root = Just $ mkNName ns name
   }

xmlOptions :: ByteString -> XMLOptions
xmlOptions ns = (namespacedXMLOptions ns)
    { xmlListElement = mkNName ns "member"
    }

xpTextContent :: XMLPU [Node] Text
xpTextContent = (Text.decodeUtf8, Text.encodeUtf8) `xpWrap` xpContent xpText
