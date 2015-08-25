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
-- In the 'DeleteSnapshotSchedule' request, you identify the volume by
-- providing its Amazon Resource Name (ARN).
--
-- To list or delete a snapshot, you must use the Amazon EC2 API. in
-- /Amazon Elastic Compute Cloud API Reference/.
--
-- /See:/ <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_DeleteSnapshotSchedule.html AWS API Reference> for DeleteSnapshotSchedule.
module Network.AWS.StorageGateway.DeleteSnapshotSchedule
    (
    -- * Creating a Request
      deleteSnapshotSchedule
    , DeleteSnapshotSchedule
    -- * Request Lenses
    , dVolumeARN

    -- * Destructuring the Response
    , deleteSnapshotScheduleResponse
    , DeleteSnapshotScheduleResponse
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
newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule'
    { _dVolumeARN :: Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteSnapshotSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVolumeARN'
deleteSnapshotSchedule
    :: Text -- ^ 'dVolumeARN'
    -> DeleteSnapshotSchedule
deleteSnapshotSchedule pVolumeARN_ =
    DeleteSnapshotSchedule'
    { _dVolumeARN = pVolumeARN_
    }

-- | Undocumented member.
dVolumeARN :: Lens' DeleteSnapshotSchedule Text
dVolumeARN = lens _dVolumeARN (\ s a -> s{_dVolumeARN = a});

instance AWSRequest DeleteSnapshotSchedule where
        type Rs DeleteSnapshotSchedule =
             DeleteSnapshotScheduleResponse
        request = postJSON storageGateway
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
          = object
              (catMaybes [Just ("VolumeARN" .= _dVolumeARN)])

instance ToPath DeleteSnapshotSchedule where
        toPath = const "/"

instance ToQuery DeleteSnapshotSchedule where
        toQuery = const mempty

-- | /See:/ 'deleteSnapshotScheduleResponse' smart constructor.
data DeleteSnapshotScheduleResponse = DeleteSnapshotScheduleResponse'
    { _dsssrsVolumeARN :: !(Maybe Text)
    , _dsssrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'DeleteSnapshotScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsssrsVolumeARN'
--
-- * 'dsssrsStatus'
deleteSnapshotScheduleResponse
    :: Int -- ^ 'dsssrsStatus'
    -> DeleteSnapshotScheduleResponse
deleteSnapshotScheduleResponse pStatus_ =
    DeleteSnapshotScheduleResponse'
    { _dsssrsVolumeARN = Nothing
    , _dsssrsStatus = pStatus_
    }

-- | Undocumented member.
dsssrsVolumeARN :: Lens' DeleteSnapshotScheduleResponse (Maybe Text)
dsssrsVolumeARN = lens _dsssrsVolumeARN (\ s a -> s{_dsssrsVolumeARN = a});

-- | The response status code.
dsssrsStatus :: Lens' DeleteSnapshotScheduleResponse Int
dsssrsStatus = lens _dsssrsStatus (\ s a -> s{_dsssrsStatus = a});
