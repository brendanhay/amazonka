{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DomainMetadata
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the domain, including when the domain was
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
    , dmrsItemNamesSizeBytes
    , dmrsAttributeNameCount
    , dmrsAttributeValuesSizeBytes
    , dmrsAttributeValueCount
    , dmrsAttributeNamesSizeBytes
    , dmrsTimestamp
    , dmrsItemCount
    , dmrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.SDB.Types

-- | /See:/ 'domainMetadata' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmDomainName'
newtype DomainMetadata = DomainMetadata'
    { _dmDomainName :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DomainMetadata' smart constructor.
domainMetadata :: Text -> DomainMetadata
domainMetadata pDomainName_ =
    DomainMetadata'
    { _dmDomainName = pDomainName_
    }

-- | The name of the domain for which to display the metadata of.
dmDomainName :: Lens' DomainMetadata Text
dmDomainName = lens _dmDomainName (\ s a -> s{_dmDomainName = a});

instance AWSRequest DomainMetadata where
        type Sv DomainMetadata = SDB
        type Rs DomainMetadata = DomainMetadataResponse
        request = postQuery
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
                     <*> (x .@? "ItemCount")
                     <*> (pure (fromEnum s)))

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
-- * 'dmrsItemNamesSizeBytes'
--
-- * 'dmrsAttributeNameCount'
--
-- * 'dmrsAttributeValuesSizeBytes'
--
-- * 'dmrsAttributeValueCount'
--
-- * 'dmrsAttributeNamesSizeBytes'
--
-- * 'dmrsTimestamp'
--
-- * 'dmrsItemCount'
--
-- * 'dmrsStatus'
data DomainMetadataResponse = DomainMetadataResponse'
    { _dmrsItemNamesSizeBytes       :: !(Maybe Integer)
    , _dmrsAttributeNameCount       :: !(Maybe Int)
    , _dmrsAttributeValuesSizeBytes :: !(Maybe Integer)
    , _dmrsAttributeValueCount      :: !(Maybe Int)
    , _dmrsAttributeNamesSizeBytes  :: !(Maybe Integer)
    , _dmrsTimestamp                :: !(Maybe Int)
    , _dmrsItemCount                :: !(Maybe Int)
    , _dmrsStatus                   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DomainMetadataResponse' smart constructor.
domainMetadataResponse :: Int -> DomainMetadataResponse
domainMetadataResponse pStatus_ =
    DomainMetadataResponse'
    { _dmrsItemNamesSizeBytes = Nothing
    , _dmrsAttributeNameCount = Nothing
    , _dmrsAttributeValuesSizeBytes = Nothing
    , _dmrsAttributeValueCount = Nothing
    , _dmrsAttributeNamesSizeBytes = Nothing
    , _dmrsTimestamp = Nothing
    , _dmrsItemCount = Nothing
    , _dmrsStatus = pStatus_
    }

-- | The total size of all item names in the domain, in bytes.
dmrsItemNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsItemNamesSizeBytes = lens _dmrsItemNamesSizeBytes (\ s a -> s{_dmrsItemNamesSizeBytes = a});

-- | The number of unique attribute names in the domain.
dmrsAttributeNameCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrsAttributeNameCount = lens _dmrsAttributeNameCount (\ s a -> s{_dmrsAttributeNameCount = a});

-- | The total size of all attribute values in the domain, in bytes.
dmrsAttributeValuesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsAttributeValuesSizeBytes = lens _dmrsAttributeValuesSizeBytes (\ s a -> s{_dmrsAttributeValuesSizeBytes = a});

-- | The number of all attribute name\/value pairs in the domain.
dmrsAttributeValueCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrsAttributeValueCount = lens _dmrsAttributeValueCount (\ s a -> s{_dmrsAttributeValueCount = a});

-- | The total size of all unique attribute names in the domain, in bytes.
dmrsAttributeNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsAttributeNamesSizeBytes = lens _dmrsAttributeNamesSizeBytes (\ s a -> s{_dmrsAttributeNamesSizeBytes = a});

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
dmrsTimestamp :: Lens' DomainMetadataResponse (Maybe Int)
dmrsTimestamp = lens _dmrsTimestamp (\ s a -> s{_dmrsTimestamp = a});

-- | The number of all items in the domain.
dmrsItemCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrsItemCount = lens _dmrsItemCount (\ s a -> s{_dmrsItemCount = a});

-- | FIXME: Undocumented member.
dmrsStatus :: Lens' DomainMetadataResponse Int
dmrsStatus = lens _dmrsStatus (\ s a -> s{_dmrsStatus = a});
