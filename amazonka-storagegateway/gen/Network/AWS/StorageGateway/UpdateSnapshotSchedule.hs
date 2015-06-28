{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.StorageGateway.UpdateSnapshotSchedule
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

-- | This operation updates a snapshot schedule configured for a gateway
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
    , ussDescription
    , ussVolumeARN
    , ussStartAt
    , ussRecurrenceInHours

    -- * Response
    , UpdateSnapshotScheduleResponse
    -- ** Response constructor
    , updateSnapshotScheduleResponse
    -- ** Response lenses
    , ussrVolumeARN
    , ussrStatus
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
-- * 'ussDescription'
--
-- * 'ussVolumeARN'
--
-- * 'ussStartAt'
--
-- * 'ussRecurrenceInHours'
data UpdateSnapshotSchedule = UpdateSnapshotSchedule'
    { _ussDescription       :: !(Maybe Text)
    , _ussVolumeARN         :: !Text
    , _ussStartAt           :: !Nat
    , _ussRecurrenceInHours :: !Nat
    } deriving (Eq,Read,Show)

-- | 'UpdateSnapshotSchedule' smart constructor.
updateSnapshotSchedule :: Text -> Natural -> Natural -> UpdateSnapshotSchedule
updateSnapshotSchedule pVolumeARN pStartAt pRecurrenceInHours =
    UpdateSnapshotSchedule'
    { _ussDescription = Nothing
    , _ussVolumeARN = pVolumeARN
    , _ussStartAt = _Nat # pStartAt
    , _ussRecurrenceInHours = _Nat # pRecurrenceInHours
    }

-- | Optional description of the snapshot that overwrites the existing
-- description.
ussDescription :: Lens' UpdateSnapshotSchedule (Maybe Text)
ussDescription = lens _ussDescription (\ s a -> s{_ussDescription = a});

-- | The Amazon Resource Name (ARN) of the volume. Use the ListVolumes
-- operation to return a list of gateway volumes.
ussVolumeARN :: Lens' UpdateSnapshotSchedule Text
ussVolumeARN = lens _ussVolumeARN (\ s a -> s{_ussVolumeARN = a});

-- | The hour of the day at which the snapshot schedule begins represented as
-- /hh/, where /hh/ is the hour (0 to 23). The hour of the day is in the
-- time zone of the gateway.
ussStartAt :: Lens' UpdateSnapshotSchedule Natural
ussStartAt = lens _ussStartAt (\ s a -> s{_ussStartAt = a}) . _Nat;

-- | Frequency of snapshots. Specify the number of hours between snapshots.
ussRecurrenceInHours :: Lens' UpdateSnapshotSchedule Natural
ussRecurrenceInHours = lens _ussRecurrenceInHours (\ s a -> s{_ussRecurrenceInHours = a}) . _Nat;

instance AWSRequest UpdateSnapshotSchedule where
        type Sv UpdateSnapshotSchedule = StorageGateway
        type Rs UpdateSnapshotSchedule =
             UpdateSnapshotScheduleResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 UpdateSnapshotScheduleResponse' <$>
                   (x .?> "VolumeARN") <*> (pure s))

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
              ["Description" .= _ussDescription,
               "VolumeARN" .= _ussVolumeARN,
               "StartAt" .= _ussStartAt,
               "RecurrenceInHours" .= _ussRecurrenceInHours]

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
-- * 'ussrVolumeARN'
--
-- * 'ussrStatus'
data UpdateSnapshotScheduleResponse = UpdateSnapshotScheduleResponse'
    { _ussrVolumeARN :: !(Maybe Text)
    , _ussrStatus    :: !Status
    } deriving (Eq,Show)

-- | 'UpdateSnapshotScheduleResponse' smart constructor.
updateSnapshotScheduleResponse :: Status -> UpdateSnapshotScheduleResponse
updateSnapshotScheduleResponse pStatus =
    UpdateSnapshotScheduleResponse'
    { _ussrVolumeARN = Nothing
    , _ussrStatus = pStatus
    }

-- | FIXME: Undocumented member.
ussrVolumeARN :: Lens' UpdateSnapshotScheduleResponse (Maybe Text)
ussrVolumeARN = lens _ussrVolumeARN (\ s a -> s{_ussrVolumeARN = a});

-- | FIXME: Undocumented member.
ussrStatus :: Lens' UpdateSnapshotScheduleResponse Status
ussrStatus = lens _ussrStatus (\ s a -> s{_ussrStatus = a});
