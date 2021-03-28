{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      UpdateSnapshotSchedule (..)
    , mkUpdateSnapshotSchedule
    -- ** Request lenses
    , ussVolumeARN
    , ussStartAt
    , ussRecurrenceInHours
    , ussDescription
    , ussTags

    -- * Destructuring the response
    , UpdateSnapshotScheduleResponse (..)
    , mkUpdateSnapshotScheduleResponse
    -- ** Response lenses
    , ussrrsVolumeARN
    , ussrrsResponseStatus
    ) where

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
  { volumeARN :: Types.VolumeARN
    -- ^ The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
  , startAt :: Core.Natural
    -- ^ The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
  , recurrenceInHours :: Core.Natural
    -- ^ Frequency of snapshots. Specify the number of hours between snapshots.
  , description :: Core.Maybe Types.Description
    -- ^ Optional description of the snapshot that overwrites the existing description.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSnapshotSchedule' value with any optional fields omitted.
mkUpdateSnapshotSchedule
    :: Types.VolumeARN -- ^ 'volumeARN'
    -> Core.Natural -- ^ 'startAt'
    -> Core.Natural -- ^ 'recurrenceInHours'
    -> UpdateSnapshotSchedule
mkUpdateSnapshotSchedule volumeARN startAt recurrenceInHours
  = UpdateSnapshotSchedule'{volumeARN, startAt, recurrenceInHours,
                            description = Core.Nothing, tags = Core.Nothing}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussVolumeARN :: Lens.Lens' UpdateSnapshotSchedule Types.VolumeARN
ussVolumeARN = Lens.field @"volumeARN"
{-# INLINEABLE ussVolumeARN #-}
{-# DEPRECATED volumeARN "Use generic-lens or generic-optics with 'volumeARN' instead"  #-}

-- | The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
--
-- /Note:/ Consider using 'startAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussStartAt :: Lens.Lens' UpdateSnapshotSchedule Core.Natural
ussStartAt = Lens.field @"startAt"
{-# INLINEABLE ussStartAt #-}
{-# DEPRECATED startAt "Use generic-lens or generic-optics with 'startAt' instead"  #-}

-- | Frequency of snapshots. Specify the number of hours between snapshots.
--
-- /Note:/ Consider using 'recurrenceInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussRecurrenceInHours :: Lens.Lens' UpdateSnapshotSchedule Core.Natural
ussRecurrenceInHours = Lens.field @"recurrenceInHours"
{-# INLINEABLE ussRecurrenceInHours #-}
{-# DEPRECATED recurrenceInHours "Use generic-lens or generic-optics with 'recurrenceInHours' instead"  #-}

-- | Optional description of the snapshot that overwrites the existing description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussDescription :: Lens.Lens' UpdateSnapshotSchedule (Core.Maybe Types.Description)
ussDescription = Lens.field @"description"
{-# INLINEABLE ussDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | A list of up to 50 tags that can be assigned to a snapshot. Each tag is a key-value pair.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussTags :: Lens.Lens' UpdateSnapshotSchedule (Core.Maybe [Types.Tag])
ussTags = Lens.field @"tags"
{-# INLINEABLE ussTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.ToQuery UpdateSnapshotSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateSnapshotSchedule where
        toHeaders UpdateSnapshotSchedule{..}
          = Core.pure
              ("X-Amz-Target", "StorageGateway_20130630.UpdateSnapshotSchedule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateSnapshotSchedule where
        toJSON UpdateSnapshotSchedule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("VolumeARN" Core..= volumeARN),
                  Core.Just ("StartAt" Core..= startAt),
                  Core.Just ("RecurrenceInHours" Core..= recurrenceInHours),
                  ("Description" Core..=) Core.<$> description,
                  ("Tags" Core..=) Core.<$> tags])

instance Core.AWSRequest UpdateSnapshotSchedule where
        type Rs UpdateSnapshotSchedule = UpdateSnapshotScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 UpdateSnapshotScheduleResponse' Core.<$>
                   (x Core..:? "VolumeARN") Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | A JSON object containing the Amazon Resource Name (ARN) of the updated storage volume.
--
-- /See:/ 'mkUpdateSnapshotScheduleResponse' smart constructor.
data UpdateSnapshotScheduleResponse = UpdateSnapshotScheduleResponse'
  { volumeARN :: Core.Maybe Types.VolumeARN
    -- ^ The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateSnapshotScheduleResponse' value with any optional fields omitted.
mkUpdateSnapshotScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateSnapshotScheduleResponse
mkUpdateSnapshotScheduleResponse responseStatus
  = UpdateSnapshotScheduleResponse'{volumeARN = Core.Nothing,
                                    responseStatus}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussrrsVolumeARN :: Lens.Lens' UpdateSnapshotScheduleResponse (Core.Maybe Types.VolumeARN)
ussrrsVolumeARN = Lens.field @"volumeARN"
{-# INLINEABLE ussrrsVolumeARN #-}
{-# DEPRECATED volumeARN "Use generic-lens or generic-optics with 'volumeARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ussrrsResponseStatus :: Lens.Lens' UpdateSnapshotScheduleResponse Core.Int
ussrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ussrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
