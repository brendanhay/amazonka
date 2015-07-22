{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.UpdateSnapshotSchedule
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- This operation updates a snapshot schedule configured for a gateway
-- volume.
--
-- The default snapshot schedule for volume is once every 24 hours,
-- starting at the creation time of the volume. You can use this API to
-- change the snapshot schedule configured for the volume.
--
-- In the request you must identify the gateway volume whose snapshot
-- schedule you want to update, and the schedule information, including
-- when you want the snapshot to begin on a day and the frequency (in
-- hours) of snapshots.
--
-- <http://docs.aws.amazon.com/storagegateway/latest/APIReference/API_UpdateSnapshotSchedule.html>
module Network.AWS.StorageGateway.UpdateSnapshotSchedule
    (
    -- * Request
      UpdateSnapshotSchedule
    -- ** Request constructor
    , updateSnapshotSchedule
    -- ** Request lenses
    , ussrqDescription
    , ussrqVolumeARN
    , ussrqStartAt
    , ussrqRecurrenceInHours

    -- * Response
    , UpdateSnapshotScheduleResponse
    -- ** Response constructor
    , updateSnapshotScheduleResponse
    -- ** Response lenses
    , ussrsVolumeARN
    , ussrsStatus
    ) where

import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response
import           Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   UpdateSnapshotScheduleInput$Description
-- -   UpdateSnapshotScheduleInput$RecurrenceInHours
-- -   UpdateSnapshotScheduleInput$StartAt
-- -   UpdateSnapshotScheduleInput$VolumeARN
--
-- /See:/ 'updateSnapshotSchedule' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ussrqDescription'
--
-- * 'ussrqVolumeARN'
--
-- * 'ussrqStartAt'
--
-- * 'ussrqRecurrenceInHours'
data UpdateSnapshotSchedule = UpdateSnapshotSchedule'
    { _ussrqDescription       :: !(Maybe Text)
    , _ussrqVolumeARN         :: !Text
    , _ussrqStartAt           :: !Nat
    , _ussrqRecurrenceInHours :: !Nat
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateSnapshotSchedule' smart constructor.
updateSnapshotSchedule :: Text -> Natural -> Natural -> UpdateSnapshotSchedule
updateSnapshotSchedule pVolumeARN pStartAt pRecurrenceInHours =
    UpdateSnapshotSchedule'
    { _ussrqDescription = Nothing
    , _ussrqVolumeARN = pVolumeARN
    , _ussrqStartAt = _Nat # pStartAt
    , _ussrqRecurrenceInHours = _Nat # pRecurrenceInHours
    }

-- | Optional description of the snapshot that overwrites the existing
-- description.
ussrqDescription :: Lens' UpdateSnapshotSchedule (Maybe Text)
ussrqDescription = lens _ussrqDescription (\ s a -> s{_ussrqDescription = a});

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
ussrqVolumeARN :: Lens' UpdateSnapshotSchedule Text
ussrqVolumeARN = lens _ussrqVolumeARN (\ s a -> s{_ussrqVolumeARN = a});

-- | The hour of the day at which the snapshot schedule begins represented as
-- /hh/, where /hh/ is the hour (0 to 23). The hour of the day is in the
-- time zone of the gateway.
ussrqStartAt :: Lens' UpdateSnapshotSchedule Natural
ussrqStartAt = lens _ussrqStartAt (\ s a -> s{_ussrqStartAt = a}) . _Nat;

-- | Frequency of snapshots. Specify the number of hours between snapshots.
ussrqRecurrenceInHours :: Lens' UpdateSnapshotSchedule Natural
ussrqRecurrenceInHours = lens _ussrqRecurrenceInHours (\ s a -> s{_ussrqRecurrenceInHours = a}) . _Nat;

instance AWSRequest UpdateSnapshotSchedule where
        type Sv UpdateSnapshotSchedule = StorageGateway
        type Rs UpdateSnapshotSchedule =
             UpdateSnapshotScheduleResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSnapshotScheduleResponse' <$>
                   (x .?> "VolumeARN") <*> (pure (fromEnum s)))

instance ToHeaders UpdateSnapshotSchedule where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("StorageGateway_20130630.UpdateSnapshotSchedule" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON UpdateSnapshotSchedule where
        toJSON UpdateSnapshotSchedule'{..}
          = object
              ["Description" .= _ussrqDescription,
               "VolumeARN" .= _ussrqVolumeARN,
               "StartAt" .= _ussrqStartAt,
               "RecurrenceInHours" .= _ussrqRecurrenceInHours]

instance ToPath UpdateSnapshotSchedule where
        toPath = const "/"

instance ToQuery UpdateSnapshotSchedule where
        toQuery = const mempty

-- | A JSON object containing the of the updated storage volume.
--
-- /See:/ 'updateSnapshotScheduleResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ussrsVolumeARN'
--
-- * 'ussrsStatus'
data UpdateSnapshotScheduleResponse = UpdateSnapshotScheduleResponse'
    { _ussrsVolumeARN :: !(Maybe Text)
    , _ussrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'UpdateSnapshotScheduleResponse' smart constructor.
updateSnapshotScheduleResponse :: Int -> UpdateSnapshotScheduleResponse
updateSnapshotScheduleResponse pStatus =
    UpdateSnapshotScheduleResponse'
    { _ussrsVolumeARN = Nothing
    , _ussrsStatus = pStatus
    }

-- | FIXME: Undocumented member.
ussrsVolumeARN :: Lens' UpdateSnapshotScheduleResponse (Maybe Text)
ussrsVolumeARN = lens _ussrsVolumeARN (\ s a -> s{_ussrsVolumeARN = a});

-- | FIXME: Undocumented member.
ussrsStatus :: Lens' UpdateSnapshotScheduleResponse Int
ussrsStatus = lens _ussrsStatus (\ s a -> s{_ussrsStatus = a});
