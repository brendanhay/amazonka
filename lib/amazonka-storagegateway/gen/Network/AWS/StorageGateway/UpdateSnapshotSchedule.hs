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
-- Module      : Network.AWS.StorageGateway.UpdateSnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a snapshot schedule configured for a gateway volume. This operation is only supported in the cached volume and stored volume gateway types.
--
--
-- The default snapshot schedule for volume is once every 24 hours, starting at the creation time of the volume. You can use this API to change the snapshot schedule configured for the volume.
--
-- In the request you must identify the gateway volume whose snapshot schedule you want to update, and the schedule information, including when you want the snapshot to begin on a day and the frequency (in hours) of snapshots.
module Network.AWS.StorageGateway.UpdateSnapshotSchedule
  ( -- * Creating a Request
    updateSnapshotSchedule,
    UpdateSnapshotSchedule,

    -- * Request Lenses
    ussDescription,
    ussTags,
    ussVolumeARN,
    ussStartAt,
    ussRecurrenceInHours,

    -- * Destructuring the Response
    updateSnapshotScheduleResponse,
    UpdateSnapshotScheduleResponse,

    -- * Response Lenses
    ussrsVolumeARN,
    ussrsResponseStatus,
  )
where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'UpdateSnapshotScheduleInput$Description'
--
--     * 'UpdateSnapshotScheduleInput$RecurrenceInHours'
--
--     * 'UpdateSnapshotScheduleInput$StartAt'
--
--     * 'UpdateSnapshotScheduleInput$VolumeARN'
--
--
--
--
-- /See:/ 'updateSnapshotSchedule' smart constructor.
data UpdateSnapshotSchedule = UpdateSnapshotSchedule'
  { _ussDescription ::
      !(Maybe Text),
    _ussTags :: !(Maybe [Tag]),
    _ussVolumeARN :: !Text,
    _ussStartAt :: !Nat,
    _ussRecurrenceInHours :: !Nat
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSnapshotSchedule' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussDescription' - Optional description of the snapshot that overwrites the existing description.
--
-- * 'ussTags' - A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
--
-- * 'ussVolumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- * 'ussStartAt' - The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
--
-- * 'ussRecurrenceInHours' - Frequency of snapshots. Specify the number of hours between snapshots.
updateSnapshotSchedule ::
  -- | 'ussVolumeARN'
  Text ->
  -- | 'ussStartAt'
  Natural ->
  -- | 'ussRecurrenceInHours'
  Natural ->
  UpdateSnapshotSchedule
updateSnapshotSchedule pVolumeARN_ pStartAt_ pRecurrenceInHours_ =
  UpdateSnapshotSchedule'
    { _ussDescription = Nothing,
      _ussTags = Nothing,
      _ussVolumeARN = pVolumeARN_,
      _ussStartAt = _Nat # pStartAt_,
      _ussRecurrenceInHours = _Nat # pRecurrenceInHours_
    }

-- | Optional description of the snapshot that overwrites the existing description.
ussDescription :: Lens' UpdateSnapshotSchedule (Maybe Text)
ussDescription = lens _ussDescription (\s a -> s {_ussDescription = a})

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
ussTags :: Lens' UpdateSnapshotSchedule [Tag]
ussTags = lens _ussTags (\s a -> s {_ussTags = a}) . _Default . _Coerce

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
ussVolumeARN :: Lens' UpdateSnapshotSchedule Text
ussVolumeARN = lens _ussVolumeARN (\s a -> s {_ussVolumeARN = a})

-- | The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
ussStartAt :: Lens' UpdateSnapshotSchedule Natural
ussStartAt = lens _ussStartAt (\s a -> s {_ussStartAt = a}) . _Nat

-- | Frequency of snapshots. Specify the number of hours between snapshots.
ussRecurrenceInHours :: Lens' UpdateSnapshotSchedule Natural
ussRecurrenceInHours = lens _ussRecurrenceInHours (\s a -> s {_ussRecurrenceInHours = a}) . _Nat

instance AWSRequest UpdateSnapshotSchedule where
  type Rs UpdateSnapshotSchedule = UpdateSnapshotScheduleResponse
  request = postJSON storageGateway
  response =
    receiveJSON
      ( \s h x ->
          UpdateSnapshotScheduleResponse'
            <$> (x .?> "VolumeARN") <*> (pure (fromEnum s))
      )

instance Hashable UpdateSnapshotSchedule

instance NFData UpdateSnapshotSchedule

instance ToHeaders UpdateSnapshotSchedule where
  toHeaders =
    const
      ( mconcat
          [ "X-Amz-Target"
              =# ("StorageGateway_20130630.UpdateSnapshotSchedule" :: ByteString),
            "Content-Type" =# ("application/x-amz-json-1.1" :: ByteString)
          ]
      )

instance ToJSON UpdateSnapshotSchedule where
  toJSON UpdateSnapshotSchedule' {..} =
    object
      ( catMaybes
          [ ("Description" .=) <$> _ussDescription,
            ("Tags" .=) <$> _ussTags,
            Just ("VolumeARN" .= _ussVolumeARN),
            Just ("StartAt" .= _ussStartAt),
            Just ("RecurrenceInHours" .= _ussRecurrenceInHours)
          ]
      )

instance ToPath UpdateSnapshotSchedule where
  toPath = const "/"

instance ToQuery UpdateSnapshotSchedule where
  toQuery = const mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the updated storage volume.
--
--
--
-- /See:/ 'updateSnapshotScheduleResponse' smart constructor.
data UpdateSnapshotScheduleResponse = UpdateSnapshotScheduleResponse'
  { _ussrsVolumeARN ::
      !(Maybe Text),
    _ussrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'UpdateSnapshotScheduleResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ussrsVolumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- * 'ussrsResponseStatus' - -- | The response status code.
updateSnapshotScheduleResponse ::
  -- | 'ussrsResponseStatus'
  Int ->
  UpdateSnapshotScheduleResponse
updateSnapshotScheduleResponse pResponseStatus_ =
  UpdateSnapshotScheduleResponse'
    { _ussrsVolumeARN = Nothing,
      _ussrsResponseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
ussrsVolumeARN :: Lens' UpdateSnapshotScheduleResponse (Maybe Text)
ussrsVolumeARN = lens _ussrsVolumeARN (\s a -> s {_ussrsVolumeARN = a})

-- | -- | The response status code.
ussrsResponseStatus :: Lens' UpdateSnapshotScheduleResponse Int
ussrsResponseStatus = lens _ussrsResponseStatus (\s a -> s {_ussrsResponseStatus = a})

instance NFData UpdateSnapshotScheduleResponse
