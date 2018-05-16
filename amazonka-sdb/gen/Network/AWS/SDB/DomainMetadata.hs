{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SDB.DomainMetadata
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the domain, including when the domain was created, the number of items and attributes in the domain, and the size of the attribute names and values.
--
--
module Network.AWS.SDB.DomainMetadata
    (
    -- * Creating a Request
      domainMetadata
    , DomainMetadata
    -- * Request Lenses
    , dmDomainName

    -- * Destructuring the Response
    , domainMetadataResponse
    , DomainMetadataResponse
    -- * Response Lenses
    , dmrsItemNamesSizeBytes
    , dmrsAttributeValuesSizeBytes
    , dmrsAttributeNameCount
    , dmrsAttributeNamesSizeBytes
    , dmrsAttributeValueCount
    , dmrsItemCount
    , dmrsTimestamp
    , dmrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.SDB.Types
import Network.AWS.SDB.Types.Product

-- | /See:/ 'domainMetadata' smart constructor.
newtype DomainMetadata = DomainMetadata'
  { _dmDomainName :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainMetadata' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmDomainName' - The name of the domain for which to display the metadata of.
domainMetadata
    :: Text -- ^ 'dmDomainName'
    -> DomainMetadata
domainMetadata pDomainName_ = DomainMetadata' {_dmDomainName = pDomainName_}


-- | The name of the domain for which to display the metadata of.
dmDomainName :: Lens' DomainMetadata Text
dmDomainName = lens _dmDomainName (\ s a -> s{_dmDomainName = a})

instance AWSRequest DomainMetadata where
        type Rs DomainMetadata = DomainMetadataResponse
        request = postQuery sdb
        response
          = receiveXMLWrapper "DomainMetadataResult"
              (\ s h x ->
                 DomainMetadataResponse' <$>
                   (x .@? "ItemNamesSizeBytes") <*>
                     (x .@? "AttributeValuesSizeBytes")
                     <*> (x .@? "AttributeNameCount")
                     <*> (x .@? "AttributeNamesSizeBytes")
                     <*> (x .@? "AttributeValueCount")
                     <*> (x .@? "ItemCount")
                     <*> (x .@? "Timestamp")
                     <*> (pure (fromEnum s)))

instance Hashable DomainMetadata where

instance NFData DomainMetadata where

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
data DomainMetadataResponse = DomainMetadataResponse'
  { _dmrsItemNamesSizeBytes       :: !(Maybe Integer)
  , _dmrsAttributeValuesSizeBytes :: !(Maybe Integer)
  , _dmrsAttributeNameCount       :: !(Maybe Int)
  , _dmrsAttributeNamesSizeBytes  :: !(Maybe Integer)
  , _dmrsAttributeValueCount      :: !(Maybe Int)
  , _dmrsItemCount                :: !(Maybe Int)
  , _dmrsTimestamp                :: !(Maybe Int)
  , _dmrsResponseStatus           :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DomainMetadataResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dmrsItemNamesSizeBytes' - The total size of all item names in the domain, in bytes.
--
-- * 'dmrsAttributeValuesSizeBytes' - The total size of all attribute values in the domain, in bytes.
--
-- * 'dmrsAttributeNameCount' - The number of unique attribute names in the domain.
--
-- * 'dmrsAttributeNamesSizeBytes' - The total size of all unique attribute names in the domain, in bytes.
--
-- * 'dmrsAttributeValueCount' - The number of all attribute name/value pairs in the domain.
--
-- * 'dmrsItemCount' - The number of all items in the domain.
--
-- * 'dmrsTimestamp' - The data and time when metadata was calculated, in Epoch (UNIX) seconds.
--
-- * 'dmrsResponseStatus' - -- | The response status code.
domainMetadataResponse
    :: Int -- ^ 'dmrsResponseStatus'
    -> DomainMetadataResponse
domainMetadataResponse pResponseStatus_ =
  DomainMetadataResponse'
    { _dmrsItemNamesSizeBytes = Nothing
    , _dmrsAttributeValuesSizeBytes = Nothing
    , _dmrsAttributeNameCount = Nothing
    , _dmrsAttributeNamesSizeBytes = Nothing
    , _dmrsAttributeValueCount = Nothing
    , _dmrsItemCount = Nothing
    , _dmrsTimestamp = Nothing
    , _dmrsResponseStatus = pResponseStatus_
    }


-- | The total size of all item names in the domain, in bytes.
dmrsItemNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsItemNamesSizeBytes = lens _dmrsItemNamesSizeBytes (\ s a -> s{_dmrsItemNamesSizeBytes = a})

-- | The total size of all attribute values in the domain, in bytes.
dmrsAttributeValuesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsAttributeValuesSizeBytes = lens _dmrsAttributeValuesSizeBytes (\ s a -> s{_dmrsAttributeValuesSizeBytes = a})

-- | The number of unique attribute names in the domain.
dmrsAttributeNameCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrsAttributeNameCount = lens _dmrsAttributeNameCount (\ s a -> s{_dmrsAttributeNameCount = a})

-- | The total size of all unique attribute names in the domain, in bytes.
dmrsAttributeNamesSizeBytes :: Lens' DomainMetadataResponse (Maybe Integer)
dmrsAttributeNamesSizeBytes = lens _dmrsAttributeNamesSizeBytes (\ s a -> s{_dmrsAttributeNamesSizeBytes = a})

-- | The number of all attribute name/value pairs in the domain.
dmrsAttributeValueCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrsAttributeValueCount = lens _dmrsAttributeValueCount (\ s a -> s{_dmrsAttributeValueCount = a})

-- | The number of all items in the domain.
dmrsItemCount :: Lens' DomainMetadataResponse (Maybe Int)
dmrsItemCount = lens _dmrsItemCount (\ s a -> s{_dmrsItemCount = a})

-- | The data and time when metadata was calculated, in Epoch (UNIX) seconds.
dmrsTimestamp :: Lens' DomainMetadataResponse (Maybe Int)
dmrsTimestamp = lens _dmrsTimestamp (\ s a -> s{_dmrsTimestamp = a})

-- | -- | The response status code.
dmrsResponseStatus :: Lens' DomainMetadataResponse Int
dmrsResponseStatus = lens _dmrsResponseStatus (\ s a -> s{_dmrsResponseStatus = a})

instance NFData DomainMetadataResponse where
