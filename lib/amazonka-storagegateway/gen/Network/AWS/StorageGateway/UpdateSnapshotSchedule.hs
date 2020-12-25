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
    ussVolumeARN,
    ussStartAt,
    ussRecurrenceInHours,
    ussDescription,
    ussTags,

    -- * Destructuring the response
    UpdateSnapshotScheduleResponse (..),
    mkUpdateSnapshotScheduleResponse,

    -- ** Response lenses
    ussrrsVolumeARN,
    ussrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

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
  { -- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
    volumeARN :: Types.VolumeARN,
    -- | The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
    startAt :: Core.Natural,
    -- | Frequency of snapshots. Specify the number of hours between snapshots.
    recurrenceInHours :: Core.Natural,
    -- | Optional description of the snapshot that overwrites the existing description.
    description :: Core.Maybe Types.Description,
    -- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSnapshotSchedule' value with any optional fields omitted.
mkUpdateSnapshotSchedule ::
  -- | 'volumeARN'
  Types.VolumeARN ->
  -- | 'startAt'
  Core.Natural ->
  -- | 'recurrenceInHours'
  Core.Natural ->
  UpdateSnapshotSchedule
mkUpdateSnapshotSchedule volumeARN startAt recurrenceInHours =
  UpdateSnapshotSchedule'
    { volumeARN,
      startAt,
      recurrenceInHours,
      description = Core.Nothing,
      tags = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussVolumeARN :: Lens.Lens' UpdateSnapshotSchedule Types.VolumeARN
ussVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED ussVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
--
-- /Note:/ Consider using 'startAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussStartAt :: Lens.Lens' UpdateSnapshotSchedule Core.Natural
ussStartAt = Lens.field @"startAt"
{-# DEPRECATED ussStartAt "Use generic-lens or generic-optics with 'startAt' instead." #-}

-- | Frequency of snapshots. Specify the number of hours between snapshots.
--
-- /Note:/ Consider using 'recurrenceInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussRecurrenceInHours :: Lens.Lens' UpdateSnapshotSchedule Core.Natural
ussRecurrenceInHours = Lens.field @"recurrenceInHours"
{-# DEPRECATED ussRecurrenceInHours "Use generic-lens or generic-optics with 'recurrenceInHours' instead." #-}

-- | Optional description of the snapshot that overwrites the existing description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussDescription :: Lens.Lens' UpdateSnapshotSchedule (Core.Maybe Types.Description)
ussDescription = Lens.field @"description"
{-# DEPRECATED ussDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussTags :: Lens.Lens' UpdateSnapshotSchedule (Core.Maybe [Types.Tag])
ussTags = Lens.field @"tags"
{-# DEPRECATED ussTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromJSON UpdateSnapshotSchedule where
  toJSON UpdateSnapshotSchedule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("VolumeARN" Core..= volumeARN),
            Core.Just ("StartAt" Core..= startAt),
            Core.Just ("RecurrenceInHours" Core..= recurrenceInHours),
            ("Description" Core..=) Core.<$> description,
            ("Tags" Core..=) Core.<$> tags
          ]
      )

instance Core.AWSRequest UpdateSnapshotSchedule where
  type Rs UpdateSnapshotSchedule = UpdateSnapshotScheduleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "StorageGateway_20130630.UpdateSnapshotSchedule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateSnapshotScheduleResponse'
            Core.<$> (x Core..:? "VolumeARN") Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | A JSON object containing the Amazon Resource Name (ARN) of the updated storage volume.
--
-- /See:/ 'mkUpdateSnapshotScheduleResponse' smart constructor.
data UpdateSnapshotScheduleResponse = UpdateSnapshotScheduleResponse'
  { -- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
    volumeARN :: Core.Maybe Types.VolumeARN,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSnapshotScheduleResponse' value with any optional fields omitted.
mkUpdateSnapshotScheduleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  UpdateSnapshotScheduleResponse
mkUpdateSnapshotScheduleResponse responseStatus =
  UpdateSnapshotScheduleResponse'
    { volumeARN = Core.Nothing,
      responseStatus
    }

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussrrsVolumeARN :: Lens.Lens' UpdateSnapshotScheduleResponse (Core.Maybe Types.VolumeARN)
ussrrsVolumeARN = Lens.field @"volumeARN"
{-# DEPRECATED ussrrsVolumeARN "Use generic-lens or generic-optics with 'volumeARN' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussrrsResponseStatus :: Lens.Lens' UpdateSnapshotScheduleResponse Core.Int
ussrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ussrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
