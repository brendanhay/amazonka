{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SimpleDB.DomainMetadata
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
module Network.AWS.SimpleDB.DomainMetadata
    (
    -- * Request
      DomainMetadata
    -- ** Request constructor
    , mkDomainMetadata
    -- ** Request lenses
    , dmDomainName

    -- * Response
    , DomainMetadataResponse
    -- ** Response constructor
    , mkDomainMetadataResponse
    -- ** Response lenses
    , dmrItemCount
    , dmrItemNamesSizeBytes
    , dmrAttributeNameCount
    , dmrAttributeNamesSizeBytes
    , dmrAttributeValueCount
    , dmrAttributeValuesSizeBytes
    , dmrTimestamp
    ) where

import Network.AWS.Request.Query
import Network.AWS.SimpleDB.Types
import Network.AWS.Prelude

newtype DomainMetadata = DomainMetadata
    { _dmDomainName :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DomainMetadata' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @DomainName ::@ @Text@
--
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
    { _dmrItemCount :: Maybe Integer
    , _dmrItemNamesSizeBytes :: Maybe Integer
    , _dmrAttributeNameCount :: Maybe Integer
    , _dmrAttributeNamesSizeBytes :: Maybe Integer
    , _dmrAttributeValueCount :: Maybe Integer
    , _dmrAttributeValuesSizeBytes :: Maybe Integer
    , _dmrTimestamp :: Maybe Integer
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DomainMetadataResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @ItemCount ::@ @Maybe Integer@
--
-- * @ItemNamesSizeBytes ::@ @Maybe Integer@
--
-- * @AttributeNameCount ::@ @Maybe Integer@
--
-- * @AttributeNamesSizeBytes ::@ @Maybe Integer@
--
-- * @AttributeValueCount ::@ @Maybe Integer@
--
-- * @AttributeValuesSizeBytes ::@ @Maybe Integer@
--
-- * @Timestamp ::@ @Maybe Integer@
--
mkDomainMetadataResponse :: DomainMetadataResponse
mkDomainMetadataResponse = DomainMetadataResponse
    { _dmrItemCount = Nothing
    , _dmrItemNamesSizeBytes = Nothing
    , _dmrAttributeNameCount = Nothing
    , _dmrAttributeNamesSizeBytes = Nothing
    , _dmrAttributeValueCount = Nothing
    , _dmrAttributeValuesSizeBytes = Nothing
    , _dmrTimestamp = Nothing
    }

-- | The number of all items in the domain.
dmrItemCount :: Lens' DomainMetadataResponse (Maybe Integer)
dmrItemCount = lens _dmrItemCount (\s a -> s { _dmrItemCount = a })

-- | The total size of all item names in the domain, in bytes.
dmrItemNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrItemNamesSizeBytes =
    lens _dmrItemNamesSizeBytes (\s a -> s { _dmrItemNamesSizeBytes = a })

-- | The number of unique attribute names in the domain.
dmrAttributeNameCount :: Lens' DomainMetadataResponse (Maybe Integer)
dmrAttributeNameCount =
    lens _dmrAttributeNameCount (\s a -> s { _dmrAttributeNameCount = a })

-- | The total size of all unique attribute names in the domain, in bytes.
dmrAttributeNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrAttributeNamesSizeBytes =
    lens _dmrAttributeNamesSizeBytes
         (\s a -> s { _dmrAttributeNamesSizeBytes = a })

-- | The number of all attribute name/value pairs in the domain.
dmrAttributeValueCount :: Lens' DomainMetadataResponse (Maybe Integer)
dmrAttributeValueCount =
    lens _dmrAttributeValueCount (\s a -> s { _dmrAttributeValueCount = a })

-- | The total size of all attribute values in the domain, in bytes.
dmrAttributeValuesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrAttributeValuesSizeBytes =
    lens _dmrAttributeValuesSizeBytes
         (\s a -> s { _dmrAttributeValuesSizeBytes = a })

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
dmrTimestamp :: Lens' DomainMetadataResponse (Maybe Integer)
dmrTimestamp = lens _dmrTimestamp (\s a -> s { _dmrTimestamp = a })

instance FromXML DomainMetadataResponse where
    fromXMLOptions = xmlOptions

instance AWSRequest DomainMetadata where
    type Sv DomainMetadata = SimpleDB
    type Rs DomainMetadata = DomainMetadataResponse

    request = post "DomainMetadata"
    response _ = xmlResponse
