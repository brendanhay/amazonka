{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE Rank2Types        #-}
{-# LANGUAGE RecordWildCards   #-}

-- Module      : Network.AWS.Internal
-- Copyright   : (c) 2013 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

module Network.AWS.Internal
    (
    -- * Internal Modules
      module Network.AWS.Internal.Instances
    , module Network.AWS.Internal.Signing
    , module Network.AWS.Internal.String
    , module Network.AWS.Internal.Types

    -- * Convenience
    , module GHC.Generics
    , module Network.HTTP.QueryString.Pickle
    , module Text.XML.Expat.Pickle.Generic

    -- * XML Helpers
    , withNS
    , withNS'
    , withRootNS
    , withRootNS'
    , namespaced
    ) where

import           Data.ByteString                 (ByteString)
import qualified Data.ByteString.Char8           as BS
import           Data.Char
import           GHC.Generics
import           Network.AWS.Internal.Instances
import           Network.AWS.Internal.Signing
import           Network.AWS.Internal.String
import           Network.AWS.Internal.Types
import           Network.HTTP.QueryString.Pickle
import           Text.XML.Expat.Pickle.Generic

withNS :: ByteString -> XMLGeneric a
withNS ns = withNS' ns (namespaced ns)

withNS' :: ByteString -> XMLOptions -> XMLGeneric a
withNS' ns opts = pu { root = (mkNName ns . nnLocalPart) `fmap` (root pu) }
  where
    pu = genericXMLPickler opts

withRootNS :: ByteString -> ByteString -> XMLGeneric a
withRootNS ns name = withRootNS' ns name (namespaced ns)

withRootNS' :: ByteString -> ByteString -> XMLOptions -> XMLGeneric a
withRootNS' ns name opts = (genericXMLPickler opts)
   { root = Just $ mkNName ns name
   }

namespaced :: ByteString -> XMLOptions
namespaced ns =
    let XMLOptions{..} = defaultXMLOptions
    in defaultXMLOptions
           { xmlCtorModifier  = mkNName ns . BS.pack
           , xmlFieldModifier = mkNName ns . BS.pack . dropWhile isLower
           }
