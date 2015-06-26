{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.StorageGateway.DescribeWorkingStorage
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

-- | This operation returns information about the working storage of a
-- gateway. This operation is supported only for the gateway-stored volume
-- architecture. This operation is deprecated in cached-volumes API version
-- (20120630). Use DescribeUploadBuffer instead.
--
-- Working storage is also referred to as upload buffer. You can also use
-- the DescribeUploadBuffer operation to add upload buffer to a
-- stored-volume gateway.
--
-- The response includes disk IDs that are configured as working storage,
-- and it includes the amount of working storage allocated and used.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DescribeWorkingStorage.html>
module Network.AWS.StorageGateway.DescribeWorkingStorage
    (
    -- * Request
      DescribeWorkingStorage
    -- ** Request constructor
    , describeWorkingStorage
    -- ** Request lenses
    , dwsGatewayARN

    -- * Response
    , DescribeWorkingStorageResponse
    -- ** Response constructor
    , describeWorkingStorageResponse
    -- ** Response lenses
    , dwsrGatewayARN
    , dwsrDiskIds
    , dwsrWorkingStorageAllocatedInBytes
    , dwsrWorkingStorageUsedInBytes
    , dwsrStatusCode
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the of the gateway.
--
-- /See:/ 'describeWorkingStorage' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwsGatewayARN'
newtype DescribeWorkingStorage = DescribeWorkingStorage'{_dwsGatewayARN :: Text} deriving (Eq, Read, Show)

-- | 'DescribeWorkingStorage' smart constructor.
describeWorkingStorage :: Text -> DescribeWorkingStorage
describeWorkingStorage pGatewayARN = DescribeWorkingStorage'{_dwsGatewayARN = pGatewayARN};

-- | FIXME: Undocumented member.
dwsGatewayARN :: Lens' DescribeWorkingStorage Text
dwsGatewayARN = lens _dwsGatewayARN (\ s a -> s{_dwsGatewayARN = a});

instance AWSRequest DescribeWorkingStorage where
        type Sv DescribeWorkingStorage = StorageGateway
        type Rs DescribeWorkingStorage =
             DescribeWorkingStorageResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkingStorageResponse' <$>
                   (x .?> "GatewayARN") <*> (x .?> "DiskIds" .!@ mempty)
                     <*> (x .?> "WorkingStorageAllocatedInBytes")
                     <*> (x .?> "WorkingStorageUsedInBytes")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeWorkingStorage where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DescribeWorkingStorage" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DescribeWorkingStorage where
        toJSON DescribeWorkingStorage'{..}
          = object ["GatewayARN" .= _dwsGatewayARN]

instance ToPath DescribeWorkingStorage where
        toPath = const "/"

instance ToQuery DescribeWorkingStorage where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'describeWorkingStorageResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dwsrGatewayARN'
--
-- * 'dwsrDiskIds'
--
-- * 'dwsrWorkingStorageAllocatedInBytes'
--
-- * 'dwsrWorkingStorageUsedInBytes'
--
-- * 'dwsrStatusCode'
data DescribeWorkingStorageResponse = DescribeWorkingStorageResponse'{_dwsrGatewayARN :: Maybe Text, _dwsrDiskIds :: Maybe [Text], _dwsrWorkingStorageAllocatedInBytes :: Maybe Integer, _dwsrWorkingStorageUsedInBytes :: Maybe Integer, _dwsrStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeWorkingStorageResponse' smart constructor.
describeWorkingStorageResponse :: Int -> DescribeWorkingStorageResponse
describeWorkingStorageResponse pStatusCode = DescribeWorkingStorageResponse'{_dwsrGatewayARN = Nothing, _dwsrDiskIds = Nothing, _dwsrWorkingStorageAllocatedInBytes = Nothing, _dwsrWorkingStorageUsedInBytes = Nothing, _dwsrStatusCode = pStatusCode};

-- | FIXME: Undocumented member.
dwsrGatewayARN :: Lens' DescribeWorkingStorageResponse (Maybe Text)
dwsrGatewayARN = lens _dwsrGatewayARN (\ s a -> s{_dwsrGatewayARN = a});

-- | An array of the gateway\'s local disk IDs that are configured as working
-- storage. Each local disk ID is specified as a string (minimum length of
-- 1 and maximum length of 300). If no local disks are configured as
-- working storage, then the DiskIds array is empty.
dwsrDiskIds :: Lens' DescribeWorkingStorageResponse [Text]
dwsrDiskIds = lens _dwsrDiskIds (\ s a -> s{_dwsrDiskIds = a}) . _Default;

-- | The total working storage in bytes allocated for the gateway. If no
-- working storage is configured for the gateway, this field returns 0.
dwsrWorkingStorageAllocatedInBytes :: Lens' DescribeWorkingStorageResponse (Maybe Integer)
dwsrWorkingStorageAllocatedInBytes = lens _dwsrWorkingStorageAllocatedInBytes (\ s a -> s{_dwsrWorkingStorageAllocatedInBytes = a});

-- | The total working storage in bytes in use by the gateway. If no working
-- storage is configured for the gateway, this field returns 0.
dwsrWorkingStorageUsedInBytes :: Lens' DescribeWorkingStorageResponse (Maybe Integer)
dwsrWorkingStorageUsedInBytes = lens _dwsrWorkingStorageUsedInBytes (\ s a -> s{_dwsrWorkingStorageUsedInBytes = a});

-- | FIXME: Undocumented member.
dwsrStatusCode :: Lens' DescribeWorkingStorageResponse Int
dwsrStatusCode = lens _dwsrStatusCode (\ s a -> s{_dwsrStatusCode = a});
