{-# OPTIONS_GHC -fno-warn-deprecations #-}
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
-- The default snapshot schedule for volume is once every 24 hours, starting at the creation time of the volume. You can use this API to change the snapshot schedule configured for the volume.
-- In the request you must identify the gateway volume whose snapshot schedule you want to update, and the schedule information, including when you want the snapshot to begin on a day and the frequency (in hours) of snapshots.
module Network.AWS.StorageGateway.UpdateSnapshotSchedule
  ( -- * Creating a request
    UpdateSnapshotSchedule (..),
    mkUpdateSnapshotSchedule,

    -- ** Request lenses
    ussStartAt,
    ussVolumeARN,
    ussRecurrenceInHours,
    ussDescription,
    ussTags,

    -- * Destructuring the response
    UpdateSnapshotScheduleResponse (..),
    mkUpdateSnapshotScheduleResponse,

    -- ** Response lenses
    ussrsVolumeARN,
    ussrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
--
--     * 'UpdateSnapshotScheduleInput$Description'
--
--
--     * 'UpdateSnapshotScheduleInput$RecurrenceInHours'
--
--
--     * 'UpdateSnapshotScheduleInput$StartAt'
--
--
--     * 'UpdateSnapshotScheduleInput$VolumeARN'
--
--
--
-- /See:/ 'mkUpdateSnapshotSchedule' smart constructor.
data UpdateSnapshotSchedule = UpdateSnapshotSchedule'
  { -- | The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
    startAt :: Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
    volumeARN :: Lude.Text,
    -- | Frequency of snapshots. Specify the number of hours between snapshots.
    recurrenceInHours :: Lude.Natural,
    -- | Optional description of the snapshot that overwrites the existing description.
    description :: Lude.Maybe Lude.Text,
    -- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
    tags :: Lude.Maybe [Tag]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSnapshotSchedule' with the minimum fields required to make a request.
--
-- * 'startAt' - The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
-- * 'recurrenceInHours' - Frequency of snapshots. Specify the number of hours between snapshots.
-- * 'description' - Optional description of the snapshot that overwrites the existing description.
-- * 'tags' - A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
mkUpdateSnapshotSchedule ::
  -- | 'startAt'
  Lude.Natural ->
  -- | 'volumeARN'
  Lude.Text ->
  -- | 'recurrenceInHours'
  Lude.Natural ->
  UpdateSnapshotSchedule
mkUpdateSnapshotSchedule pStartAt_ pVolumeARN_ pRecurrenceInHours_ =
  UpdateSnapshotSchedule'
    { startAt = pStartAt_,
      volumeARN = pVolumeARN_,
      recurrenceInHours = pRecurrenceInHours_,
      description = Lude.Nothing,
      tags = Lude.Nothing
    }

-- | The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
--
-- /Note:/ Consider using 'startAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussStartAt :: Lens.Lens' UpdateSnapshotSchedule Lude.Natural
ussStartAt = Lens.lens (startAt :: UpdateSnapshotSchedule -> Lude.Natural) (\s a -> s {startAt = a} :: UpdateSnapshotSchedule)
{-# DEPRECATED ussStartAt "Use generic-lens or generic-optics with 'startAt' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussVolumeARN :: Lens.Lens' UpdateSnapshotSchedule Lude.Text
ussVolumeARN = Lens.lens (volumeARN :: UpdateSnapshotSchedule -> Lude.Text) (\s a -> s {volumeARN = a} :: UpdateSnapshotSchedule)
{-# DEPRECATED ussVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | Frequency of snapshots. Specify the number of hours between snapshots.
--
-- /Note:/ Consider using 'recurrenceInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussRecurrenceInHours :: Lens.Lens' UpdateSnapshotSchedule Lude.Natural
ussRecurrenceInHours = Lens.lens (recurrenceInHours :: UpdateSnapshotSchedule -> Lude.Natural) (\s a -> s {recurrenceInHours = a} :: UpdateSnapshotSchedule)
{-# DEPRECATED ussRecurrenceInHours "Use generic-lens or generic-optics with 'recurrenceInHours' instead." #-}

-- | Optional description of the snapshot that overwrites the existing description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussDescription :: Lens.Lens' UpdateSnapshotSchedule (Lude.Maybe Lude.Text)
ussDescription = Lens.lens (description :: UpdateSnapshotSchedule -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: UpdateSnapshotSchedule)
{-# DEPRECATED ussDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussTags :: Lens.Lens' UpdateSnapshotSchedule (Lude.Maybe [Tag])
ussTags = Lens.lens (tags :: UpdateSnapshotSchedule -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: UpdateSnapshotSchedule)
{-# DEPRECATED ussTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Lude.AWSRequest UpdateSnapshotSchedule where
  type Rs UpdateSnapshotSchedule = UpdateSnapshotScheduleResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateSnapshotScheduleResponse'
            Lude.<$> (x Lude..?> "VolumeARN") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateSnapshotSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.UpdateSnapshotSchedule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateSnapshotSchedule where
  toJSON UpdateSnapshotSchedule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("StartAt" Lude..= startAt),
            Lude.Just ("VolumeARN" Lude..= volumeARN),
            Lude.Just ("RecurrenceInHours" Lude..= recurrenceInHours),
            ("Description" Lude..=) Lude.<$> description,
            ("Tags" Lude..=) Lude.<$> tags
          ]
      )

instance Lude.ToPath UpdateSnapshotSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateSnapshotSchedule where
  toQuery = Lude.const Lude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the updated storage volume.
--
-- /See:/ 'mkUpdateSnapshotScheduleResponse' smart constructor.
data UpdateSnapshotScheduleResponse = UpdateSnapshotScheduleResponse'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
    volumeARN :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateSnapshotScheduleResponse' with the minimum fields required to make a request.
--
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
-- * 'responseStatus' - The response status code.
mkUpdateSnapshotScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateSnapshotScheduleResponse
mkUpdateSnapshotScheduleResponse pResponseStatus_ =
  UpdateSnapshotScheduleResponse'
    { volumeARN = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussrsVolumeARN :: Lens.Lens' UpdateSnapshotScheduleResponse (Lude.Maybe Lude.Text)
ussrsVolumeARN = Lens.lens (volumeARN :: UpdateSnapshotScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: UpdateSnapshotScheduleResponse)
{-# DEPRECATED ussrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussrsResponseStatus :: Lens.Lens' UpdateSnapshotScheduleResponse Lude.Int
ussrsResponseStatus = Lens.lens (responseStatus :: UpdateSnapshotScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateSnapshotScheduleResponse)
{-# DEPRECATED ussrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
