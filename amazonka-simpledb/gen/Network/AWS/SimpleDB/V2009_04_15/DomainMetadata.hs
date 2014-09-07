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
    , mkDomainMetadata
    -- ** Request lenses
    , dmDomainName

    -- * Response
    , DomainMetadataResponse
    -- ** Response lenses
    , dmrsItemCount
    , dmrsItemNamesSizeBytes
    , dmrsAttributeNameCount
    , dmrsAttributeNamesSizeBytes
    , dmrsAttributeValueCount
    , dmrsAttributeValuesSizeBytes
    , dmrsTimestamp
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.V2009_04_15.Types
import Network.AWS.Prelude

newtype DomainMetadata = DomainMetadata
    { _dmDomainName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DomainMetadata' request.
mkDomainMetadata :: Text -- ^ 'dmDomainName'
                 -> DomainMetadata
mkDomainMetadata p1 = DomainMetadata
    { _dmDomainName = p1
    }

-- | The name of the domain for which to display the metadata of.
dmDomainName :: Lens' DomainMetadata Text
dmDomainName = lens _dmDomainName (\s a -> s { _dmDomainName = a })

instance ToQuery DomainMetadata where
    toQuery = genericQuery def

data DomainMetadataResponse = DomainMetadataResponse
    { _dmrsItemCount :: Maybe Integer
    , _dmrsItemNamesSizeBytes :: Maybe Integer
    , _dmrsAttributeNameCount :: Maybe Integer
    , _dmrsAttributeNamesSizeBytes :: Maybe Integer
    , _dmrsAttributeValueCount :: Maybe Integer
    , _dmrsAttributeValuesSizeBytes :: Maybe Integer
    , _dmrsTimestamp :: Maybe Integer
    } deriving (Show, Generic)

-- | The number of all items in the domain.
dmrsItemCount :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsItemCount = lens _dmrsItemCount (\s a -> s { _dmrsItemCount = a })

-- | The total size of all item names in the domain, in bytes.
dmrsItemNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsItemNamesSizeBytes =
    lens _dmrsItemNamesSizeBytes (\s a -> s { _dmrsItemNamesSizeBytes = a })

-- | The number of unique attribute names in the domain.
dmrsAttributeNameCount :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsAttributeNameCount =
    lens _dmrsAttributeNameCount (\s a -> s { _dmrsAttributeNameCount = a })

-- | The total size of all unique attribute names in the domain, in bytes.
dmrsAttributeNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsAttributeNamesSizeBytes =
    lens _dmrsAttributeNamesSizeBytes
         (\s a -> s { _dmrsAttributeNamesSizeBytes = a })

-- | The number of all attribute name/value pairs in the domain.
dmrsAttributeValueCount :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsAttributeValueCount =
    lens _dmrsAttributeValueCount
         (\s a -> s { _dmrsAttributeValueCount = a })

-- | The total size of all attribute values in the domain, in bytes.
dmrsAttributeValuesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsAttributeValuesSizeBytes =
    lens _dmrsAttributeValuesSizeBytes
         (\s a -> s { _dmrsAttributeValuesSizeBytes = a })

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
dmrsTimestamp :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsTimestamp = lens _dmrsTimestamp (\s a -> s { _dmrsTimestamp = a })

instance FromXML DomainMetadataResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DomainMetadata where
    type Sv DomainMetadata = SimpleDB
    type Rs DomainMetadata = DomainMetadataResponse

    request = post "DomainMetadata"
    response _ = xmlResponse
