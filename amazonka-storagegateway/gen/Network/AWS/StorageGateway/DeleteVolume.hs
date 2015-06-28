{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.DeleteVolume
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

-- | This operation delete the specified gateway volume that you previously
-- created using the CreateStorediSCSIVolume API. For gateway-stored
-- volumes, the local disk that was configured as the storage volume is not
-- deleted. You can reuse the local disk to create another storage volume.
--
-- Before you delete a gateway volume, make sure there are no iSCSI
-- connections to the volume you are deleting. You should also make sure
-- there is no snapshot in progress. You can use the Amazon Elastic Compute
-- Cloud (Amazon EC2) API to query snapshots on the volume you are deleting
-- and check the snapshot status. For more information, go to
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeSnapshots.html DescribeSnapshots>
-- in the /Amazon Elastic Compute Cloud API Reference/.
--
-- In the request, you must provide the Amazon Resource Name (ARN) of the
-- storage volume you want to delete.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteVolume.html>
module Network.AWS.StorageGateway.DeleteVolume
    (
    -- * Request
      DeleteVolume
    -- ** Request constructor
    , deleteVolume
    -- ** Request lenses
    , dvVolumeARN

    -- * Response
    , DeleteVolumeResponse
    -- ** Response constructor
    , deleteVolumeResponse
    -- ** Response lenses
    , dvrVolumeARN
    , dvrStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing the DeleteVolumeInput$VolumeARN to delete.
--
-- /See:/ 'deleteVolume' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvVolumeARN'
newtype DeleteVolume = DeleteVolume'
    { _dvVolumeARN :: Text
    } deriving (Eq,Read,Show)

-- | 'DeleteVolume' smart constructor.
deleteVolume :: Text -> DeleteVolume
deleteVolume pVolumeARN =
    DeleteVolume'
    { _dvVolumeARN = pVolumeARN
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
dvVolumeARN :: Lens' DeleteVolume Text
dvVolumeARN = lens _dvVolumeARN (\ s a -> s{_dvVolumeARN = a});

instance AWSRequest DeleteVolume where
        type Sv DeleteVolume = StorageGateway
        type Rs DeleteVolume = DeleteVolumeResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteVolumeResponse' <$>
                   (x .?> "VolumeARN") <*> (pure s))

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
          = object ["VolumeARN" .= _dvVolumeARN]

instance ToPath DeleteVolume where
        toPath = const "/"

instance ToQuery DeleteVolume where
        toQuery = const mempty

-- | A JSON object containing the of the storage volume that was deleted
--
-- /See:/ 'deleteVolumeResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dvrVolumeARN'
--
-- * 'dvrStatus'
data DeleteVolumeResponse = DeleteVolumeResponse'
    { _dvrVolumeARN :: !(Maybe Text)
    , _dvrStatus    :: !Status
    } deriving (Eq,Show)

-- | 'DeleteVolumeResponse' smart constructor.
deleteVolumeResponse :: Status -> DeleteVolumeResponse
deleteVolumeResponse pStatus =
    DeleteVolumeResponse'
    { _dvrVolumeARN = Nothing
    , _dvrStatus = pStatus
    }

-- | The Amazon Resource Name (ARN) of the storage volume that was deleted.
-- It is the same ARN you provided in the request.
dvrVolumeARN :: Lens' DeleteVolumeResponse (Maybe Text)
dvrVolumeARN = lens _dvrVolumeARN (\ s a -> s{_dvrVolumeARN = a});

-- | FIXME: Undocumented member.
dvrStatus :: Lens' DeleteVolumeResponse Status
dvrStatus = lens _dvrStatus (\ s a -> s{_dvrStatus = a});
