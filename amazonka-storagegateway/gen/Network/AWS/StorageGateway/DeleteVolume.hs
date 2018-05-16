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
-- Module      : Network.AWS.StorageGateway.DeleteVolume
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified storage volume that you previously created using the 'CreateCachediSCSIVolume' or 'CreateStorediSCSIVolume' API. This operation is only supported in the cached volume and stored volume types. For stored volume gateways, the local disk that was configured as the storage volume is not deleted. You can reuse the local disk to create another storage volume.
--
--
-- Before you delete a volume, make sure there are no iSCSI connections to the volume you are deleting. You should also make sure there is no snapshot in progress. You can use the Amazon Elastic Compute Cloud (Amazon EC2) API to query snapshots on the volume you are deleting and check the snapshot status. For more information, go to <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
--
-- In the request, you must provide the Amazon Resource Name (ARN) of the storage volume you want to delete.
--
module Network.AWS.StorageGateway.DeleteVolume
    (
    -- * Creating a Request
      deleteVolume
    , DeleteVolume
    -- * Request Lenses
    , dvVolumeARN

    -- * Destructuring the Response
    , deleteVolumeResponse
    , DeleteVolumeResponse
    -- * Response Lenses
    , dvrsVolumeARN
    , dvrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | A JSON object containing the 'DeleteVolumeInput$VolumeARN' to delete.
--
--
--
-- /See:/ 'deleteVolume' smart constructor.
newtype DeleteVolume = DeleteVolume'
  { _dvVolumeARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvVolumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
deleteVolume
    :: Text -- ^ 'dvVolumeARN'
    -> DeleteVolume
deleteVolume pVolumeARN_ = DeleteVolume' {_dvVolumeARN = pVolumeARN_}


-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
dvVolumeARN :: Lens' DeleteVolume Text
dvVolumeARN = lens _dvVolumeARN (\ s a -> s{_dvVolumeARN = a})

instance AWSRequest DeleteVolume where
        type Rs DeleteVolume = DeleteVolumeResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DeleteVolumeResponse' <$>
                   (x .?> "VolumeARN") <*> (pure (fromEnum s)))

instance Hashable DeleteVolume where

instance NFData DeleteVolume where

instance ToHeaders DeleteVolume where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DeleteVolume" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteVolume where
        toJSON DeleteVolume'{..}
          = object
              (catMaybes [Just ("VolumeARN" .= _dvVolumeARN)])

instance ToPath DeleteVolume where
        toPath = const "/"

instance ToQuery DeleteVolume where
        toQuery = const mempty

-- | A JSON object containing the of the storage volume that was deleted
--
--
--
-- /See:/ 'deleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  { _dvrsVolumeARN      :: !(Maybe Text)
  , _dvrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteVolumeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvrsVolumeARN' - The Amazon Resource Name (ARN) of the storage volume that was deleted. It is the same ARN you provided in the request.
--
-- * 'dvrsResponseStatus' - -- | The response status code.
deleteVolumeResponse
    :: Int -- ^ 'dvrsResponseStatus'
    -> DeleteVolumeResponse
deleteVolumeResponse pResponseStatus_ =
  DeleteVolumeResponse'
    {_dvrsVolumeARN = Nothing, _dvrsResponseStatus = pResponseStatus_}


-- | The Amazon Resource Name (ARN) of the storage volume that was deleted. It is the same ARN you provided in the request.
dvrsVolumeARN :: Lens' DeleteVolumeResponse (Maybe Text)
dvrsVolumeARN = lens _dvrsVolumeARN (\ s a -> s{_dvrsVolumeARN = a})

-- | -- | The response status code.
dvrsResponseStatus :: Lens' DeleteVolumeResponse Int
dvrsResponseStatus = lens _dvrsResponseStatus (\ s a -> s{_dvrsResponseStatus = a})

instance NFData DeleteVolumeResponse where
