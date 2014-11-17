{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.SDB.DomainMetadata
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
--
-- <http://docs.aws.amazon.com/AmazonSimpleDB/latest/DeveloperGuide/SDB_API_DomainMetadata.html>
module Network.AWS.SDB.DomainMetadata
    (
    -- * Request
      DomainMetadata
    -- ** Request constructor
    , domainMetadata
    -- ** Request lenses
    , dmDomainName

    -- * Response
    , DomainMetadataResponse
    -- ** Response constructor
    , domainMetadataResponse
    -- ** Response lenses
    , dmrAttributeNameCount
    , dmrAttributeNamesSizeBytes
    , dmrAttributeValueCount
    , dmrAttributeValuesSizeBytes
    , dmrItemCount
    , dmrItemNamesSizeBytes
    , dmrTimestamp
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.SDB.Types
import qualified GHC.Exts

newtype DomainMetadata = DomainMetadata
    { _dmDomainName :: Text
    } deriving (Eq, Ord, Show, Generic, Monoid, IsString)

-- | 'DomainMetadata' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmDomainName' @::@ 'Text'
--
domainMetadata :: Text -- ^ 'dmDomainName'
               -> DomainMetadata
domainMetadata p1 = DomainMetadata
    { _dmDomainName = p1
    }

-- | The name of the domain for which to display the metadata of.
dmDomainName :: Lens' DomainMetadata Text
dmDomainName = lens _dmDomainName (\s a -> s { _dmDomainName = a })

data DomainMetadataResponse = DomainMetadataResponse
    { _dmrAttributeNameCount       :: Maybe Int
    , _dmrAttributeNamesSizeBytes  :: Maybe Integer
    , _dmrAttributeValueCount      :: Maybe Int
    , _dmrAttributeValuesSizeBytes :: Maybe Integer
    , _dmrItemCount                :: Maybe Int
    , _dmrItemNamesSizeBytes       :: Maybe Integer
    , _dmrTimestamp                :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DomainMetadataResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmrAttributeNameCount' @::@ 'Maybe' 'Int'
--
-- * 'dmrAttributeNamesSizeBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dmrAttributeValueCount' @::@ 'Maybe' 'Int'
--
-- * 'dmrAttributeValuesSizeBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dmrItemCount' @::@ 'Maybe' 'Int'
--
-- * 'dmrItemNamesSizeBytes' @::@ 'Maybe' 'Integer'
--
-- * 'dmrTimestamp' @::@ 'Maybe' 'Int'
--
domainMetadataResponse :: DomainMetadataResponse
domainMetadataResponse = DomainMetadataResponse
    { _dmrItemCount                = Nothing
    , _dmrItemNamesSizeBytes       = Nothing
    , _dmrAttributeNameCount       = Nothing
    , _dmrAttributeNamesSizeBytes  = Nothing
    , _dmrAttributeValueCount      = Nothing
    , _dmrAttributeValuesSizeBytes = Nothing
    , _dmrTimestamp                = Nothing
    }

-- | The number of unique attribute names in the domain.
dmrAttributeNameCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrAttributeNameCount =
    lens _dmrAttributeNameCount (\s a -> s { _dmrAttributeNameCount = a })

-- | The total size of all unique attribute names in the domain, in bytes.
dmrAttributeNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrAttributeNamesSizeBytes =
    lens _dmrAttributeNamesSizeBytes
        (\s a -> s { _dmrAttributeNamesSizeBytes = a })

-- | The number of all attribute name/value pairs in the domain.
dmrAttributeValueCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrAttributeValueCount =
    lens _dmrAttributeValueCount (\s a -> s { _dmrAttributeValueCount = a })

-- | The total size of all attribute values in the domain, in bytes.
dmrAttributeValuesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrAttributeValuesSizeBytes =
    lens _dmrAttributeValuesSizeBytes
        (\s a -> s { _dmrAttributeValuesSizeBytes = a })

-- | The number of all items in the domain.
dmrItemCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrItemCount = lens _dmrItemCount (\s a -> s { _dmrItemCount = a })

-- | The total size of all item names in the domain, in bytes.
dmrItemNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrItemNamesSizeBytes =
    lens _dmrItemNamesSizeBytes (\s a -> s { _dmrItemNamesSizeBytes = a })

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
dmrTimestamp :: Lens' DomainMetadataResponse (Maybe Int)
dmrTimestamp = lens _dmrTimestamp (\s a -> s { _dmrTimestamp = a })

instance ToPath DomainMetadata where
    toPath = const "/"

instance ToQuery DomainMetadata

instance ToHeaders DomainMetadata

instance AWSRequest DomainMetadata where
    type Sv DomainMetadata = SDB
    type Rs DomainMetadata = DomainMetadataResponse

    request  = post "DomainMetadata"
    response = xmlResponse

instance FromXML DomainMetadataResponse where
    parseXML c = DomainMetadataResponse
        <$> c .: "AttributeNameCount"
        <*> c .: "AttributeNamesSizeBytes"
        <*> c .: "AttributeValueCount"
        <*> c .: "AttributeValuesSizeBytes"
        <*> c .: "ItemCount"
        <*> c .: "ItemNamesSizeBytes"
        <*> c .: "Timestamp"
