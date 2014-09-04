{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
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
module Network.AWS.SimpleDB.V2009_04_15.DomainMetadata
    (
    -- * Request
      DomainMetadata
    -- ** Request constructor
    , mkDomainMetadataRequest
    -- ** Request lenses
    , dmrDomainName

    -- * Response
    , DomainMetadataResponse
    -- ** Response lenses
    , dmsItemCount
    , dmsItemNamesSizeBytes
    , dmsAttributeNameCount
    , dmsAttributeNamesSizeBytes
    , dmsAttributeValueCount
    , dmsAttributeValuesSizeBytes
    , dmsTimestamp
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DomainMetadata' request.
mkDomainMetadataRequest :: Text -- ^ 'dmrDomainName'
                        -> DomainMetadata
mkDomainMetadataRequest p1 = DomainMetadata
    { _dmrDomainName = p1
    }
{-# INLINE mkDomainMetadataRequest #-}

newtype DomainMetadata = DomainMetadata
    { _dmrDomainName :: Text
      -- ^ The name of the domain for which to display the metadata of.
    } deriving (Show, Generic)

-- | The name of the domain for which to display the metadata of.
dmrDomainName :: Lens' DomainMetadata (Text)
dmrDomainName = lens _dmrDomainName (\s a -> s { _dmrDomainName = a })
{-# INLINE dmrDomainName #-}

instance ToQuery DomainMetadata where
    toQuery = genericQuery def

data DomainMetadataResponse = DomainMetadataResponse
    { _dmsItemCount :: Maybe Integer
      -- ^ The number of all items in the domain.
    , _dmsItemNamesSizeBytes :: Maybe Integer
      -- ^ The total size of all item names in the domain, in bytes.
    , _dmsAttributeNameCount :: Maybe Integer
      -- ^ The number of unique attribute names in the domain.
    , _dmsAttributeNamesSizeBytes :: Maybe Integer
      -- ^ The total size of all unique attribute names in the domain, in
      -- bytes.
    , _dmsAttributeValueCount :: Maybe Integer
      -- ^ The number of all attribute name/value pairs in the domain.
    , _dmsAttributeValuesSizeBytes :: Maybe Integer
      -- ^ The total size of all attribute values in the domain, in bytes.
    , _dmsTimestamp :: Maybe Integer
      -- ^ The data and time when metadata was calculated, in Epoch (UNIX)
      -- seconds.
    } deriving (Show, Generic)

-- | The number of all items in the domain.
dmsItemCount :: Lens' DomainMetadataResponse (Maybe Integer)
dmsItemCount = lens _dmsItemCount (\s a -> s { _dmsItemCount = a })
{-# INLINE dmsItemCount #-}

-- | The total size of all item names in the domain, in bytes.
dmsItemNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmsItemNamesSizeBytes = lens _dmsItemNamesSizeBytes (\s a -> s { _dmsItemNamesSizeBytes = a })
{-# INLINE dmsItemNamesSizeBytes #-}

-- | The number of unique attribute names in the domain.
dmsAttributeNameCount :: Lens' DomainMetadataResponse (Maybe Integer)
dmsAttributeNameCount = lens _dmsAttributeNameCount (\s a -> s { _dmsAttributeNameCount = a })
{-# INLINE dmsAttributeNameCount #-}

-- | The total size of all unique attribute names in the domain, in bytes.
dmsAttributeNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmsAttributeNamesSizeBytes = lens _dmsAttributeNamesSizeBytes (\s a -> s { _dmsAttributeNamesSizeBytes = a })
{-# INLINE dmsAttributeNamesSizeBytes #-}

-- | The number of all attribute name/value pairs in the domain.
dmsAttributeValueCount :: Lens' DomainMetadataResponse (Maybe Integer)
dmsAttributeValueCount = lens _dmsAttributeValueCount (\s a -> s { _dmsAttributeValueCount = a })
{-# INLINE dmsAttributeValueCount #-}

-- | The total size of all attribute values in the domain, in bytes.
dmsAttributeValuesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmsAttributeValuesSizeBytes = lens _dmsAttributeValuesSizeBytes (\s a -> s { _dmsAttributeValuesSizeBytes = a })
{-# INLINE dmsAttributeValuesSizeBytes #-}

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
dmsTimestamp :: Lens' DomainMetadataResponse (Maybe Integer)
dmsTimestamp = lens _dmsTimestamp (\s a -> s { _dmsTimestamp = a })
{-# INLINE dmsTimestamp #-}

instance FromXML DomainMetadataResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DomainMetadata where
    type Sv DomainMetadata = SimpleDB
    type Rs DomainMetadata = DomainMetadataResponse

    request = post "DomainMetadata"
    response _ = xmlResponse
