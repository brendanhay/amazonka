{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.SDB.DomainMetadata
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Returns information about the domain, including when the domain was
-- created, the number of items and attributes in the domain, and the size
-- of the attribute names and values.
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
    , dmrItemNamesSizeBytes
    , dmrAttributeNameCount
    , dmrAttributeValuesSizeBytes
    , dmrAttributeValueCount
    , dmrAttributeNamesSizeBytes
    , dmrTimestamp
    , dmrItemCount
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types

-- | /See:/ 'domainMetadata' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmDomainName'
newtype DomainMetadata = DomainMetadata'{_dmDomainName :: Text} deriving (Eq, Read, Show)

-- | 'DomainMetadata' smart constructor.
domainMetadata :: Text -> DomainMetadata
domainMetadata pDomainName = DomainMetadata'{_dmDomainName = pDomainName};

-- | The name of the domain for which to display the metadata of.
dmDomainName :: Lens' DomainMetadata Text
dmDomainName = lens _dmDomainName (\ s a -> s{_dmDomainName = a});

instance AWSPager A where
        page rq rs
          | stop True = Nothing
          | otherwise = Just

instance AWSRequest DomainMetadata where
        type Sv DomainMetadata = SDB
        type Rs DomainMetadata = DomainMetadataResponse
        request = post
        response
          = receiveXMLWrapper "DomainMetadataResult"
              (\ s h x ->
                 DomainMetadataResponse' <$>
                   (x .@? "ItemNamesSizeBytes") <*>
                     (x .@? "AttributeNameCount")
                     <*> (x .@? "AttributeValuesSizeBytes")
                     <*> (x .@? "AttributeValueCount")
                     <*> (x .@? "AttributeNamesSizeBytes")
                     <*> (x .@? "Timestamp")
                     <*> (x .@? "ItemCount"))

instance ToHeaders DomainMetadata where
        toHeaders = const mempty

instance ToPath DomainMetadata where
        toPath = const "/"

instance ToQuery DomainMetadata where
        toQuery DomainMetadata'{..}
          = mconcat
              ["Action" =: ("DomainMetadata" :: ByteString),
               "Version" =: ("2009-04-15" :: ByteString),
               "DomainName" =: _dmDomainName]

-- | /See:/ 'domainMetadataResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmrItemNamesSizeBytes'
--
-- * 'dmrAttributeNameCount'
--
-- * 'dmrAttributeValuesSizeBytes'
--
-- * 'dmrAttributeValueCount'
--
-- * 'dmrAttributeNamesSizeBytes'
--
-- * 'dmrTimestamp'
--
-- * 'dmrItemCount'
data DomainMetadataResponse = DomainMetadataResponse'{_dmrItemNamesSizeBytes :: Maybe Integer, _dmrAttributeNameCount :: Maybe Int, _dmrAttributeValuesSizeBytes :: Maybe Integer, _dmrAttributeValueCount :: Maybe Int, _dmrAttributeNamesSizeBytes :: Maybe Integer, _dmrTimestamp :: Maybe Int, _dmrItemCount :: Maybe Int} deriving (Eq, Read, Show)

-- | 'DomainMetadataResponse' smart constructor.
domainMetadataResponse :: DomainMetadataResponse
domainMetadataResponse = DomainMetadataResponse'{_dmrItemNamesSizeBytes = Nothing, _dmrAttributeNameCount = Nothing, _dmrAttributeValuesSizeBytes = Nothing, _dmrAttributeValueCount = Nothing, _dmrAttributeNamesSizeBytes = Nothing, _dmrTimestamp = Nothing, _dmrItemCount = Nothing};

-- | The total size of all item names in the domain, in bytes.
dmrItemNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrItemNamesSizeBytes = lens _dmrItemNamesSizeBytes (\ s a -> s{_dmrItemNamesSizeBytes = a});

-- | The number of unique attribute names in the domain.
dmrAttributeNameCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrAttributeNameCount = lens _dmrAttributeNameCount (\ s a -> s{_dmrAttributeNameCount = a});

-- | The total size of all attribute values in the domain, in bytes.
dmrAttributeValuesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrAttributeValuesSizeBytes = lens _dmrAttributeValuesSizeBytes (\ s a -> s{_dmrAttributeValuesSizeBytes = a});

-- | The number of all attribute name\/value pairs in the domain.
dmrAttributeValueCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrAttributeValueCount = lens _dmrAttributeValueCount (\ s a -> s{_dmrAttributeValueCount = a});

-- | The total size of all unique attribute names in the domain, in bytes.
dmrAttributeNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrAttributeNamesSizeBytes = lens _dmrAttributeNamesSizeBytes (\ s a -> s{_dmrAttributeNamesSizeBytes = a});

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
dmrTimestamp :: Lens' DomainMetadataResponse (Maybe Int)
dmrTimestamp = lens _dmrTimestamp (\ s a -> s{_dmrTimestamp = a});

-- | The number of all items in the domain.
dmrItemCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrItemCount = lens _dmrItemCount (\ s a -> s{_dmrItemCount = a});
