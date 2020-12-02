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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a snapshot of a volume.
--
--
-- You can take snapshots of your gateway volumes on a scheduled or ad hoc basis. This API action enables you to delete a snapshot schedule for a volume. For more information, see <http://docs.aws.amazon.com/storagegateway/latest/userguide/WorkingWithSnapshots.html Working with Snapshots> . In the @DeleteSnapshotSchedule@ request, you identify the volume by providing its Amazon Resource Name (ARN). This operation is only supported in stored and cached volume gateway types.
--
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
    , dsssrsResponseStatus
    ) where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types
import Network.AWS.StorageGateway.Types.Product

-- | /See:/ 'deleteSnapshotSchedule' smart constructor.
newtype DeleteSnapshotSchedule = DeleteSnapshotSchedule'
  { _dVolumeARN :: Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSnapshotSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dVolumeARN' - Undocumented member.
deleteSnapshotSchedule
    :: Text -- ^ 'dVolumeARN'
    -> DeleteSnapshotSchedule
deleteSnapshotSchedule pVolumeARN_ =
  DeleteSnapshotSchedule' {_dVolumeARN = pVolumeARN_}


-- | Undocumented member.
dVolumeARN :: Lens' DeleteSnapshotSchedule Text
dVolumeARN = lens _dVolumeARN (\ s a -> s{_dVolumeARN = a})

instance AWSRequest DeleteSnapshotSchedule where
        type Rs DeleteSnapshotSchedule =
             DeleteSnapshotScheduleResponse
        request = postJSON storageGateway
        response
          = receiveJSON
              (\ s h x ->
                 DeleteSnapshotScheduleResponse' <$>
                   (x .?> "VolumeARN") <*> (pure (fromEnum s)))

instance Hashable DeleteSnapshotSchedule where

instance NFData DeleteSnapshotSchedule where

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
  { _dsssrsVolumeARN      :: !(Maybe Text)
  , _dsssrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DeleteSnapshotScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dsssrsVolumeARN' - Undocumented member.
--
-- * 'dsssrsResponseStatus' - -- | The response status code.
deleteSnapshotScheduleResponse
    :: Int -- ^ 'dsssrsResponseStatus'
    -> DeleteSnapshotScheduleResponse
deleteSnapshotScheduleResponse pResponseStatus_ =
  DeleteSnapshotScheduleResponse'
    {_dsssrsVolumeARN = Nothing, _dsssrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
dsssrsVolumeARN :: Lens' DeleteSnapshotScheduleResponse (Maybe Text)
dsssrsVolumeARN = lens _dsssrsVolumeARN (\ s a -> s{_dsssrsVolumeARN = a})

-- | -- | The response status code.
dsssrsResponseStatus :: Lens' DeleteSnapshotScheduleResponse Int
dsssrsResponseStatus = lens _dsssrsResponseStatus (\ s a -> s{_dsssrsResponseStatus = a})

instance NFData DeleteSnapshotScheduleResponse where
