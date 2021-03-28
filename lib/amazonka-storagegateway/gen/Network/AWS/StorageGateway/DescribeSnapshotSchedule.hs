{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      DescribeSnapshotSchedule (..)
    , mkDescribeSnapshotSchedule
    -- ** Request lenses
    , dssVolumeARN

    -- * Destructuring the response
    , DescribeSnapshotScheduleResponse (..)
    , mkDescribeSnapshotScheduleResponse
    -- ** Response lenses
    , dssrrsDescription
    , dssrrsRecurrenceInHours
    , dssrrsStartAt
    , dssrrsTags
    , dssrrsTimezone
    , dssrrsVolumeARN
    , dssrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object containing the 'DescribeSnapshotScheduleInput$VolumeARN' of the volume.
--
-- /See:/ 'mkDescribeSnapshotSchedule' smart constructor.
newtype DescribeSnapshotSchedule = DescribeSnapshotSchedule'
  { volumeARN :: Types.VolumeARN
    -- ^ The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotSchedule' value with any optional fields omitted.
mkDescribeSnapshotSchedule
    :: Types.VolumeARN -- ^ 'volumeARN'
    -> DescribeSnapshotSchedule
mkDescribeSnapshotSchedule volumeARN
  = DescribeSnapshotSchedule'{volumeARN}

-- | The Amazon Resource Name (ARN) of the volume. Use the 'ListVolumes' operation to return a list of gateway volumes.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssVolumeARN :: Lens.Lens' DescribeSnapshotSchedule Types.VolumeARN
dssVolumeARN = Lens.field @"volumeARN"
{-# INLINEABLE dssVolumeARN #-}
{-# DEPRECATED volumeARN "Use generic-lens or generic-optics with 'volumeARN' instead"  #-}

instance Core.ToQuery DescribeSnapshotSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeSnapshotSchedule where
        toHeaders DescribeSnapshotSchedule{..}
          = Core.pure
              ("X-Amz-Target",
               "StorageGateway_20130630.DescribeSnapshotSchedule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeSnapshotSchedule where
        toJSON DescribeSnapshotSchedule{..}
          = Core.object
              (Core.catMaybes [Core.Just ("VolumeARN" Core..= volumeARN)])

instance Core.AWSRequest DescribeSnapshotSchedule where
        type Rs DescribeSnapshotSchedule = DescribeSnapshotScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeSnapshotScheduleResponse' Core.<$>
                   (x Core..:? "Description") Core.<*> x Core..:? "RecurrenceInHours"
                     Core.<*> x Core..:? "StartAt"
                     Core.<*> x Core..:? "Tags"
                     Core.<*> x Core..:? "Timezone"
                     Core.<*> x Core..:? "VolumeARN"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeSnapshotScheduleResponse' smart constructor.
data DescribeSnapshotScheduleResponse = DescribeSnapshotScheduleResponse'
  { description :: Core.Maybe Types.Description
    -- ^ The snapshot description.
  , recurrenceInHours :: Core.Maybe Core.Natural
    -- ^ The number of hours between snapshots.
  , startAt :: Core.Maybe Core.Natural
    -- ^ The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ A list of up to 50 tags assigned to the snapshot schedule, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
  , timezone :: Core.Maybe Types.Timezone
    -- ^ A value that indicates the time zone of the gateway.
  , volumeARN :: Core.Maybe Types.VolumeARN
    -- ^ The Amazon Resource Name (ARN) of the volume that was specified in the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSnapshotScheduleResponse' value with any optional fields omitted.
mkDescribeSnapshotScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeSnapshotScheduleResponse
mkDescribeSnapshotScheduleResponse responseStatus
  = DescribeSnapshotScheduleResponse'{description = Core.Nothing,
                                      recurrenceInHours = Core.Nothing, startAt = Core.Nothing,
                                      tags = Core.Nothing, timezone = Core.Nothing,
                                      volumeARN = Core.Nothing, responseStatus}

-- | The snapshot description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsDescription :: Lens.Lens' DescribeSnapshotScheduleResponse (Core.Maybe Types.Description)
dssrrsDescription = Lens.field @"description"
{-# INLINEABLE dssrrsDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The number of hours between snapshots.
--
-- /Note:/ Consider using 'recurrenceInHours' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsRecurrenceInHours :: Lens.Lens' DescribeSnapshotScheduleResponse (Core.Maybe Core.Natural)
dssrrsRecurrenceInHours = Lens.field @"recurrenceInHours"
{-# INLINEABLE dssrrsRecurrenceInHours #-}
{-# DEPRECATED recurrenceInHours "Use generic-lens or generic-optics with 'recurrenceInHours' instead"  #-}

-- | The hour of the day at which the snapshot schedule begins represented as /hh/ , where /hh/ is the hour (0 to 23). The hour of the day is in the time zone of the gateway.
--
-- /Note:/ Consider using 'startAt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsStartAt :: Lens.Lens' DescribeSnapshotScheduleResponse (Core.Maybe Core.Natural)
dssrrsStartAt = Lens.field @"startAt"
{-# INLINEABLE dssrrsStartAt #-}
{-# DEPRECATED startAt "Use generic-lens or generic-optics with 'startAt' instead"  #-}

-- | A list of up to 50 tags assigned to the snapshot schedule, sorted alphabetically by key name. Each tag is a key-value pair. For a gateway with more than 10 tags assigned, you can view all tags using the @ListTagsForResource@ API operation.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsTags :: Lens.Lens' DescribeSnapshotScheduleResponse (Core.Maybe [Types.Tag])
dssrrsTags = Lens.field @"tags"
{-# INLINEABLE dssrrsTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

-- | A value that indicates the time zone of the gateway.
--
-- /Note:/ Consider using 'timezone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsTimezone :: Lens.Lens' DescribeSnapshotScheduleResponse (Core.Maybe Types.Timezone)
dssrrsTimezone = Lens.field @"timezone"
{-# INLINEABLE dssrrsTimezone #-}
{-# DEPRECATED timezone "Use generic-lens or generic-optics with 'timezone' instead"  #-}

-- | The Amazon Resource Name (ARN) of the volume that was specified in the request.
--
-- /Note:/ Consider using 'volumeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsVolumeARN :: Lens.Lens' DescribeSnapshotScheduleResponse (Core.Maybe Types.VolumeARN)
dssrrsVolumeARN = Lens.field @"volumeARN"
{-# INLINEABLE dssrrsVolumeARN #-}
{-# DEPRECATED volumeARN "Use generic-lens or generic-optics with 'volumeARN' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dssrrsResponseStatus :: Lens.Lens' DescribeSnapshotScheduleResponse Core.Int
dssrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dssrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
