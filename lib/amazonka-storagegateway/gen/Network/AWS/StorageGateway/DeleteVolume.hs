{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DeleteVolume
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified storage volume that you previously created using the 'CreateCachediSCSIVolume' or 'CreateStorediSCSIVolume' API. This operation is only supported in the cached volume and stored volume types. For stored volume gateways, the local disk that was configured as the storage volume is not deleted. You can reuse the local disk to create another storage volume.
--
--
-- Before you delete a volume, make sure there are no iSCSI connections to the volume you are deleting. You should also make sure there is no snapshot in progress. You can use the Amazon Elastic Compute Cloud (Amazon EC2) API to query snapshots on the volume you are deleting and check the snapshot status. For more information, go to <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots> in the /Amazon Elastic Compute Cloud API Reference/ .
--
-- In the request, you must provide the Amazon Resource Name (ARN) of the storage volume you want to delete.
module Network.AWS.StorageGateway.DeleteVolume
  ( -- * Creating a Request
    deleteVolume,
    DeleteVolume,

    -- * Request Lenses
    delVolumeARN,

    -- * Destructuring the Response
    deleteVolumeResponse,
    DeleteVolumeResponse,

    -- * Response Lenses
    dvvrsVolumeARN,
    dvvrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the 'DeleteVolumeInput$VolumeARN' to delete.
--
--
--
-- /See:/ 'deleteVolume' smart constructor.
newtype DeleteVolume = DeleteVolume' {_delVolumeARN :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteVolume' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'delVolumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
deleteVolume ::
  -- | 'delVolumeARN'
  Text ->
  DeleteVolume
deleteVolume pVolumeARN_ =
  DeleteVolume' {_delVolumeARN = pVolumeARN_}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
delVolumeARN :: Lens' DeleteVolume Text
delVolumeARN = lens _delVolumeARN (\s a -> s {_delVolumeARN = a})

instance AWSRequest DeleteVolume where
  type Rs DeleteVolume = DeleteVolumeResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          DeleteVolumeResponse'
            <$> (x .?> "VolumeARN") <*> (pure (fromEnum s))
      )

instance Hashable DeleteVolume

instance NFData DeleteVolume

instance ToHeaders DeleteVolume where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.DeleteVolume" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON DeleteVolume where
  toJSON DeleteVolume' {..} =
    object (catMaybes [Just ("VolumeARN" .= _delVolumeARN)])

instance ToPath DeleteVolume where
  toPath = const "/"

instance ToQuery DeleteVolume where
  toQuery = const mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the storage volume that was deleted.
--
--
--
-- /See:/ 'deleteVolumeResponse' smart constructor.
data DeleteVolumeResponse = DeleteVolumeResponse'
  { _dvvrsVolumeARN ::
      !(Maybe Text),
    _dvvrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteVolumeResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dvvrsVolumeARN' - The Amazon Resource Name (ARN) of the storage volume that was deleted. It is the same ARN you provided in the request.
--
-- * 'dvvrsResponseStatus' - -- | The response status code.
deleteVolumeResponse ::
  -- | 'dvvrsResponseStatus'
  Int ->
  DeleteVolumeResponse
deleteVolumeResponse pResponseStatus_ =
  DeleteVolumeResponse'
    { _dvvrsVolumeARN = Nothing,
      _dvvrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the storage volume that was deleted. It is the same ARN you provided in the request.
dvvrsVolumeARN :: Lens' DeleteVolumeResponse (Maybe Text)
dvvrsVolumeARN = lens _dvvrsVolumeARN (\s a -> s {_dvvrsVolumeARN = a})

-- | -- | The response status code.
dvvrsResponseStatus :: Lens' DeleteVolumeResponse Int
dvvrsResponseStatus = lens _dvvrsResponseStatus (\s a -> s {_dvvrsResponseStatus = a})

instance NFData DeleteVolumeResponse
