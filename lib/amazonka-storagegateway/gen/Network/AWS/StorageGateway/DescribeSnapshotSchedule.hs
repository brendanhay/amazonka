{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeSnapshotSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the snapshot schedule for the specified gateway volume. The snapshot schedule information includes intervals at which snapshots are automatically initiated on the volume. This operation is only supported in the cached volume and stored volume types.
module Network.AWS.StorageGateway.DescribeSnapshotSchedule
  ( -- * Creating a request
    DescribeSnapshotSchedule (..),
    mkDescribeSnapshotSchedule,

    -- ** Request lenses
    dssVolumeARN,

    -- * Destructuring the response
    DescribeSnapshotScheduleResponse (..),
    mkDescribeSnapshotScheduleResponse,

    -- ** Response lenses
    drsStartAt,
    drsVolumeARN,
    drsRecurrenceInHours,
    drsTimezone,
    drsDescription,
    drsTags,
    drsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the 'DescribeSnapshotScheduleInput$VolumeARN' of the volume.
--
-- /See:/ 'mkDescribeSnapshotSchedule' smart constructor.
newtype DescribeSnapshotSchedule = DescribeSnapshotSchedule'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
    volumeARN :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotSchedule' with the minimum fields required to make a request.
--
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
mkDescribeSnapshotSchedule ::
  -- | 'volumeARN'
  Lude.Text ->
  DescribeSnapshotSchedule
mkDescribeSnapshotSchedule pVolumeARN_ =
  DescribeSnapshotSchedule' {volumeARN = pVolumeARN_}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssVolumeARN :: Lens.Lens' DescribeSnapshotSchedule Lude.Text
dssVolumeARN = Lens.lens (volumeARN :: DescribeSnapshotSchedule -> Lude.Text) (\s a -> s {volumeARN = a} :: DescribeSnapshotSchedule)
{-# DEPRECATED dssVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

instance Lude.AWSRequest DescribeSnapshotSchedule where
  type Rs DescribeSnapshotSchedule = DescribeSnapshotScheduleResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeSnapshotScheduleResponse'
            Lude.<$> (x Lude..?> "StartAt")
            Lude.<*> (x Lude..?> "VolumeARN")
            Lude.<*> (x Lude..?> "RecurrenceInHours")
            Lude.<*> (x Lude..?> "Timezone")
            Lude.<*> (x Lude..?> "Description")
            Lude.<*> (x Lude..?> "Tags" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeSnapshotSchedule where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "StorageGateway_20130630.DescribeSnapshotSchedule" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeSnapshotSchedule where
  toJSON DescribeSnapshotSchedule' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("VolumeARN" Lude..= volumeARN)])

instance Lude.ToPath DescribeSnapshotSchedule where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeSnapshotSchedule where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeSnapshotScheduleResponse' smart constructor.
data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse'
  { -- | The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
    startAt :: Lude.Maybe Lude.Natural,
    -- | The Amazon Resource Name (ARN) of the volume that was specified in the request.
    volumeARN :: Lude.Maybe Lude.Text,
    -- | The number of hours between snapshots.
    recurrenceInHours :: Lude.Maybe Lude.Natural,
    -- | A value that indicates the time zone of the gateway.
    timezone :: Lude.Maybe Lude.Text,
    -- | The snapshot description.
    description :: Lude.Maybe Lude.Text,
    -- | A list of up to 50 tags assigned to the snapshot schedule, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
    tags :: Lude.Maybe [Tag],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotScheduleResponse' with the minimum fields required to make a request.
--
-- * 'startAt' - The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume that was specified in the request.
-- * 'recurrenceInHours' - The number of hours between snapshots.
-- * 'timezone' - A value that indicates the time zone of the gateway.
-- * 'description' - The snapshot description.
-- * 'tags' - A list of up to 50 tags assigned to the snapshot schedule, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
-- * 'responseStatus' - The response status code.
mkDescribeSnapshotScheduleResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeSnapshotScheduleResponse
mkDescribeSnapshotScheduleResponse pResponseStatus_ =
  DescribeSnapshotScheduleResponse'
    { startAt = Lude.Nothing,
      volumeARN = Lude.Nothing,
      recurrenceInHours = Lude.Nothing,
      timezone = Lude.Nothing,
      description = Lude.Nothing,
      tags = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
--
-- /Note:/ Consider using 'startAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsStartAt :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe Lude.Natural)
drsStartAt = Lens.lens (startAt :: DescribeSnapshotScheduleResponse -> Lude.Maybe Lude.Natural) (\s a -> s {startAt = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED drsStartAt "Use generic-lens or generic-optics with 'startAt' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume that was specified in the request.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsVolumeARN :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe Lude.Text)
drsVolumeARN = Lens.lens (volumeARN :: DescribeSnapshotScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED drsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The number of hours between snapshots.
--
-- /Note:/ Consider using 'recurrenceInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsRecurrenceInHours :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe Lude.Natural)
drsRecurrenceInHours = Lens.lens (recurrenceInHours :: DescribeSnapshotScheduleResponse -> Lude.Maybe Lude.Natural) (\s a -> s {recurrenceInHours = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED drsRecurrenceInHours "Use generic-lens or generic-optics with 'recurrenceInHours' instead." #-}

-- | A value that indicates the time zone of the gateway.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTimezone :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe Lude.Text)
drsTimezone = Lens.lens (timezone :: DescribeSnapshotScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED drsTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The snapshot description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsDescription :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe Lude.Text)
drsDescription = Lens.lens (description :: DescribeSnapshotScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED drsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of up to 50 tags assigned to the snapshot schedule, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsTags :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe [Tag])
drsTags = Lens.lens (tags :: DescribeSnapshotScheduleResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED drsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeSnapshotScheduleResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeSnapshotScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
