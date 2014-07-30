{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.V2009_04_15.DomainMetadata
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns information about the domain, including when the domain was
-- created, the number of items and attributes in the domain, and the size of
-- the attribute names and values.
module Network.AWS.SimpleDB.V2009_04_15.DomainMetadata where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.SimpleDB.V2009_04_15.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DomainMetadata = DomainMetadata
    { _dmrDomainName :: Text
      -- ^ The name of the domain for which to display the metadata of.
    } deriving (Generic)

instance ToQuery DomainMetadata where
    toQuery = genericToQuery def

instance AWSRequest DomainMetadata where
    type Sv DomainMetadata = SimpleDB
    type Rs DomainMetadata = DomainMetadataResponse

    request = post "DomainMetadata"
    response _ = xmlResponse

data DomainMetadataResponse = DomainMetadataResponse
    { _dmsAttributeNameCount :: Maybe Integer
      -- ^ The number of unique attribute names in the domain.
    , _dmsAttributeValueCount :: Maybe Integer
      -- ^ The number of all attribute name/value pairs in the domain.
    , _dmsItemCount :: Maybe Integer
      -- ^ The number of all items in the domain.
    , _dmsTimestamp :: Maybe Integer
      -- ^ The data and time when metadata was calculated, in Epoch (UNIX)
      -- seconds.
    , _dmsItemNamesSizeBytes :: Maybe Integer
      -- ^ The total size of all item names in the domain, in bytes.
    , _dmsAttributeValuesSizeBytes :: Maybe Integer
      -- ^ The total size of all attribute values in the domain, in bytes.
    , _dmsAttributeNamesSizeBytes :: Maybe Integer
      -- ^ The total size of all unique attribute names in the domain, in
      -- bytes.
    } deriving (Generic)

instance FromXML DomainMetadataResponse where
    fromXMLOptions = xmlOptions
