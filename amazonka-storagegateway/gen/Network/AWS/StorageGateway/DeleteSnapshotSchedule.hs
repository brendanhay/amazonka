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
-- Module      : Network.AWS.StorageGateway.DeleteSnapshotSchedule
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation deletes a snapshot of a volume.
--
-- You can take snapshots of your gateway volumes on a scheduled or ad-hoc
-- basis. This API enables you to delete a snapshot schedule for a volume.
-- For more information, see
-- <http://docs.aws.amazon.com/storagegateway/latest/userguide/WorkingWithSnapshots.html Working with Snapshots>.
-- In the @DeleteSnapshotSchedule@ request, you identify the volume by
-- providing its Amazon Resource Name (ARN).
--
-- To list or delete a snapshot, you must use the Amazon EC2 API. in
-- /Amazon Elastic Compute Cloud API Reference/.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteSnapshotSchedule.html AWS API Reference> for DeleteSnapshotSchedule.
module Network.AWS.StorageGateway.DeleteSnapshotSchedule
    (
    -- * Creating a Request
      DeleteSnapshotSchedule
    , deleteSnapshotSchedule
    -- * Request Lenses
    , dVolumeARN

    -- * Destructuring the Response
    , DeleteSnapshotScheduleResponse
    , deleteSnapshotScheduleResponse
    -- * Response Lenses
    , dsssrsVolumeARN
    , dsssrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types
import           Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'deleteSnapshotSchedule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dVolumeARN'
newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule'
    { _dVolumeARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSnapshotSchedule' smart constructor.
deleteSnapshotSchedule :: Text -> DeleteSnapshotSchedule
deleteSnapshotSchedule pVolumeARN_ =
    DeleteSnapshotSchedule'
    { _dVolumeARN = pVolumeARN_
    }

-- | Undocumented member.
dVolumeARN :: Lens' DeleteSnapshotSchedule Text
dVolumeARN = lens _dVolumeARN (\ s a -> s{_dVolumeARN = a});

instance AWSRequest DeleteSnapshotSchedule where
        type Sv DeleteSnapshotSchedule = StorageGateway
        type Rs DeleteSnapshotSchedule =
             DeleteSnapshotScheduleResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSnapshotScheduleResponse' <$>
                   (x .?> "VolumeARN") <*> (pure (fromEnum s)))

instance ToHeaders DeleteSnapshotSchedule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.DeleteSnapshotSchedule" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON DeleteSnapshotSchedule where
        toJSON DeleteSnapshotSchedule'{..}
          = object ["VolumeARN" .= _dVolumeARN]

instance ToPath DeleteSnapshotSchedule where
        toPath = const "/"

instance ToQuery DeleteSnapshotSchedule where
        toQuery = const mempty

-- | /See:/ 'deleteSnapshotScheduleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dsssrsVolumeARN'
--
-- * 'dsssrsStatus'
data DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
    { _dsssrsVolumeARN :: !(Maybe Text)
    , _dsssrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DeleteSnapshotScheduleResponse' smart constructor.
deleteSnapshotScheduleResponse :: Int -> DeleteSnapshotScheduleResponse
deleteSnapshotScheduleResponse pStatus_ =
    DeleteSnapshotScheduleResponse'
    { _dsssrsVolumeARN = Nothing
    , _dsssrsStatus = pStatus_
    }

-- | Undocumented member.
dsssrsVolumeARN :: Lens' DeleteSnapshotScheduleResponse (Maybe Text)
dsssrsVolumeARN = lens _dsssrsVolumeARN (\ s a -> s{_dsssrsVolumeARN = a});

-- | Undocumented member.
dsssrsStatus :: Lens' DeleteSnapshotScheduleResponse Int
dsssrsStatus = lens _dsssrsStatus (\ s a -> s{_dsssrsStatus = a});
