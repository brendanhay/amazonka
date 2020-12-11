{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dssrsStartAt,
    dssrsVolumeARN,
    dssrsRecurrenceInHours,
    dssrsTimezone,
    dssrsDescription,
    dssrsTags,
    dssrsResponseStatus,
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
  { volumeARN ::
      Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
  { startAt ::
      Lude.Maybe Lude.Natural,
    volumeARN ::
      Lude.Maybe Lude.Text,
    recurrenceInHours ::
      Lude.Maybe Lude.Natural,
    timezone ::
      Lude.Maybe Lude.Text,
    description ::
      Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeSnapshotScheduleResponse' with the minimum fields required to make a request.
--
-- * 'description' - The snapshot description.
-- * 'recurrenceInHours' - The number of hours between snapshots.
-- * 'responseStatus' - The response status code.
-- * 'startAt' - The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
-- * 'tags' - A list of up to 50 tags assigned to the snapshot schedule, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
-- * 'timezone' - A value that indicates the time zone of the gateway.
-- * 'volumeARN' - The Amazon Resource Name (ARN) of the volume that was specified in the request.
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
dssrsStartAt :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe Lude.Natural)
dssrsStartAt = Lens.lens (startAt :: DescribeSnapshotScheduleResponse -> Lude.Maybe Lude.Natural) (\s a -> s {startAt = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED dssrsStartAt "Use generic-lens or generic-optics with 'startAt' instead." #-}

-- | The Amazon Resource Name (ARN) of the volume that was specified in the request.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsVolumeARN :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe Lude.Text)
dssrsVolumeARN = Lens.lens (volumeARN :: DescribeSnapshotScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {volumeARN = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED dssrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The number of hours between snapshots.
--
-- /Note:/ Consider using 'recurrenceInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsRecurrenceInHours :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe Lude.Natural)
dssrsRecurrenceInHours = Lens.lens (recurrenceInHours :: DescribeSnapshotScheduleResponse -> Lude.Maybe Lude.Natural) (\s a -> s {recurrenceInHours = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED dssrsRecurrenceInHours "Use generic-lens or generic-optics with 'recurrenceInHours' instead." #-}

-- | A value that indicates the time zone of the gateway.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsTimezone :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe Lude.Text)
dssrsTimezone = Lens.lens (timezone :: DescribeSnapshotScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {timezone = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED dssrsTimezone "Use generic-lens or generic-optics with 'timezone' instead." #-}

-- | The snapshot description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsDescription :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe Lude.Text)
dssrsDescription = Lens.lens (description :: DescribeSnapshotScheduleResponse -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED dssrsDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of up to 50 tags assigned to the snapshot schedule, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsTags :: Lens.Lens' DescribeSnapshotScheduleResponse (Lude.Maybe [Tag])
dssrsTags = Lens.lens (tags :: DescribeSnapshotScheduleResponse -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED dssrsTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrsResponseStatus :: Lens.Lens' DescribeSnapshotScheduleResponse Lude.Int
dssrsResponseStatus = Lens.lens (responseStatus :: DescribeSnapshotScheduleResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeSnapshotScheduleResponse)
{-# DEPRECATED dssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
