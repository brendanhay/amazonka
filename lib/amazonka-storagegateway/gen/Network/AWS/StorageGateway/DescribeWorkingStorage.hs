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
-- Module      : Network.AWS.StorageGateway.DescribeWorkingStorage
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the working storage of a gateway. This operation is only supported in the stored volumes gateway type. This operation is deprecated in cached volumes API version (20120630). Use DescribeUploadBuffer instead.
--
--
-- The response includes disk IDs that are configured as working storage, and it includes the amount of working storage allocated and used.
--
module Network.AWS.StorageGateway.DescribeWorkingStorage
    (
    -- * Creating a Request
      describeWorkingStorage
    , DescribeWorkingStorage
    -- * Request Lenses
    , dwsGatewayARN

    -- * Destructuring the Response
    , describeWorkingStorageResponse
    , DescribeWorkingStorageResponse
    -- * Response Lenses
    , dwsrsGatewayARN
    , dwsrsDiskIds
    , dwsrsWorkingStorageAllocatedInBytes
    , dwsrsWorkingStorageUsedInBytes
    , dwsrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing the of the gateway.
--
--
--
-- /See:/ 'describeWorkingStorage' smart constructor.
newtype DescribeWorkingStorage = DescribeWorkingStorage'
  { _dwsGatewayARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkingStorage' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwsGatewayARN' - Undocumented member.
describeWorkingStorage
    :: Text -- ^ 'dwsGatewayARN'
    -> DescribeWorkingStorage
describeWorkingStorage pGatewayARN_ =
  DescribeWorkingStorage' {_dwsGatewayARN = pGatewayARN_}


-- | Undocumented member.
dwsGatewayARN :: Lens' DescribeWorkingStorage Text
dwsGatewayARN = lens _dwsGatewayARN (\ s a -> s{_dwsGatewayARN = a})

instance AWSRequest DescribeWorkingStorage where
        type Rs DescribeWorkingStorage =
             DescribeWorkingStorageResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DescribeWorkingStorageResponse' <$>
                   (x .?> "GatewayARN") <*> (x .?> "DiskIds" .!@ mempty)
                     <*> (x .?> "WorkingStorageAllocatedInBytes")
                     <*> (x .?> "WorkingStorageUsedInBytes")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeWorkingStorage where

instance NFData DescribeWorkingStorage where

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
          = object
              (catMaybes [Just ("GatewayARN" .= _dwsGatewayARN)])

instance ToPath DescribeWorkingStorage where
        toPath = const "/"

instance ToQuery DescribeWorkingStorage where
        toQuery = const mempty

-- | A JSON object containing the following fields:
--
--
--
-- /See:/ 'describeWorkingStorageResponse' smart constructor.
data DescribeWorkingStorageResponse = DescribeWorkingStorageResponse'
  { _dwsrsGatewayARN                     :: !(Maybe Text)
  , _dwsrsDiskIds                        :: !(Maybe [Text])
  , _dwsrsWorkingStorageAllocatedInBytes :: !(Maybe Integer)
  , _dwsrsWorkingStorageUsedInBytes      :: !(Maybe Integer)
  , _dwsrsResponseStatus                 :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeWorkingStorageResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dwsrsGatewayARN' - Undocumented member.
--
-- * 'dwsrsDiskIds' - An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
--
-- * 'dwsrsWorkingStorageAllocatedInBytes' - The total working storage in bytes allocated for the gateway. If no working storage is configured for the gateway, this field returns 0.
--
-- * 'dwsrsWorkingStorageUsedInBytes' - The total working storage in bytes in use by the gateway. If no working storage is configured for the gateway, this field returns 0.
--
-- * 'dwsrsResponseStatus' - -- | The response status code.
describeWorkingStorageResponse
    :: Int -- ^ 'dwsrsResponseStatus'
    -> DescribeWorkingStorageResponse
describeWorkingStorageResponse pResponseStatus_ =
  DescribeWorkingStorageResponse'
    { _dwsrsGatewayARN = Nothing
    , _dwsrsDiskIds = Nothing
    , _dwsrsWorkingStorageAllocatedInBytes = Nothing
    , _dwsrsWorkingStorageUsedInBytes = Nothing
    , _dwsrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
dwsrsGatewayARN :: Lens' DescribeWorkingStorageResponse (Maybe Text)
dwsrsGatewayARN = lens _dwsrsGatewayARN (\ s a -> s{_dwsrsGatewayARN = a})

-- | An array of the gateway's local disk IDs that are configured as working storage. Each local disk ID is specified as a string (minimum length of 1 and maximum length of 300). If no local disks are configured as working storage, then the DiskIds array is empty.
dwsrsDiskIds :: Lens' DescribeWorkingStorageResponse [Text]
dwsrsDiskIds = lens _dwsrsDiskIds (\ s a -> s{_dwsrsDiskIds = a}) . _Default . _Coerce

-- | The total working storage in bytes allocated for the gateway. If no working storage is configured for the gateway, this field returns 0.
dwsrsWorkingStorageAllocatedInBytes :: Lens' DescribeWorkingStorageResponse (Maybe Integer)
dwsrsWorkingStorageAllocatedInBytes = lens _dwsrsWorkingStorageAllocatedInBytes (\ s a -> s{_dwsrsWorkingStorageAllocatedInBytes = a})

-- | The total working storage in bytes in use by the gateway. If no working storage is configured for the gateway, this field returns 0.
dwsrsWorkingStorageUsedInBytes :: Lens' DescribeWorkingStorageResponse (Maybe Integer)
dwsrsWorkingStorageUsedInBytes = lens _dwsrsWorkingStorageUsedInBytes (\ s a -> s{_dwsrsWorkingStorageUsedInBytes = a})

-- | -- | The response status code.
dwsrsResponseStatus :: Lens' DescribeWorkingStorageResponse Int
dwsrsResponseStatus = lens _dwsrsResponseStatus (\ s a -> s{_dwsrsResponseStatus = a})

instance NFData DescribeWorkingStorageResponse where
