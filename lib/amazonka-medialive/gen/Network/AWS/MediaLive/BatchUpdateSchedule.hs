{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.BatchUpdateSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Update a channel schedule
module Network.AWS.MediaLive.BatchUpdateSchedule
    (
    -- * Creating a request
      BatchUpdateSchedule (..)
    , mkBatchUpdateSchedule
    -- ** Request lenses
    , busChannelId
    , busCreates
    , busDeletes

    -- * Destructuring the response
    , BatchUpdateScheduleResponse (..)
    , mkBatchUpdateScheduleResponse
    -- ** Response lenses
    , busrrsCreates
    , busrrsDeletes
    , busrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | List of actions to create and list of actions to delete.
--
-- /See:/ 'mkBatchUpdateSchedule' smart constructor.
data BatchUpdateSchedule = BatchUpdateSchedule'
  { channelId :: Core.Text
    -- ^ Id of the channel whose schedule is being updated.
  , creates :: Core.Maybe Types.BatchScheduleActionCreateRequest
    -- ^ Schedule actions to create in the schedule.
  , deletes :: Core.Maybe Types.BatchScheduleActionDeleteRequest
    -- ^ Schedule actions to delete from the schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchUpdateSchedule' value with any optional fields omitted.
mkBatchUpdateSchedule
    :: Core.Text -- ^ 'channelId'
    -> BatchUpdateSchedule
mkBatchUpdateSchedule channelId
  = BatchUpdateSchedule'{channelId, creates = Core.Nothing,
                         deletes = Core.Nothing}

-- | Id of the channel whose schedule is being updated.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busChannelId :: Lens.Lens' BatchUpdateSchedule Core.Text
busChannelId = Lens.field @"channelId"
{-# INLINEABLE busChannelId #-}
{-# DEPRECATED channelId "Use generic-lens or generic-optics with 'channelId' instead"  #-}

-- | Schedule actions to create in the schedule.
--
-- /Note:/ Consider using 'creates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busCreates :: Lens.Lens' BatchUpdateSchedule (Core.Maybe Types.BatchScheduleActionCreateRequest)
busCreates = Lens.field @"creates"
{-# INLINEABLE busCreates #-}
{-# DEPRECATED creates "Use generic-lens or generic-optics with 'creates' instead"  #-}

-- | Schedule actions to delete from the schedule.
--
-- /Note:/ Consider using 'deletes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busDeletes :: Lens.Lens' BatchUpdateSchedule (Core.Maybe Types.BatchScheduleActionDeleteRequest)
busDeletes = Lens.field @"deletes"
{-# INLINEABLE busDeletes #-}
{-# DEPRECATED deletes "Use generic-lens or generic-optics with 'deletes' instead"  #-}

instance Core.ToQuery BatchUpdateSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders BatchUpdateSchedule where
        toHeaders BatchUpdateSchedule{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON BatchUpdateSchedule where
        toJSON BatchUpdateSchedule{..}
          = Core.object
              (Core.catMaybes
                 [("creates" Core..=) Core.<$> creates,
                  ("deletes" Core..=) Core.<$> deletes])

instance Core.AWSRequest BatchUpdateSchedule where
        type Rs BatchUpdateSchedule = BatchUpdateScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.PUT,
                         Core._rqPath =
                           "/prod/channels/" Core.<> Core.toText channelId Core.<>
                             "/schedule",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 BatchUpdateScheduleResponse' Core.<$>
                   (x Core..:? "creates") Core.<*> x Core..:? "deletes" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Placeholder documentation for BatchUpdateScheduleResponse
--
-- /See:/ 'mkBatchUpdateScheduleResponse' smart constructor.
data BatchUpdateScheduleResponse = BatchUpdateScheduleResponse'
  { creates :: Core.Maybe Types.BatchScheduleActionCreateResult
    -- ^ Schedule actions created in the schedule.
  , deletes :: Core.Maybe Types.BatchScheduleActionDeleteResult
    -- ^ Schedule actions deleted from the schedule.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BatchUpdateScheduleResponse' value with any optional fields omitted.
mkBatchUpdateScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> BatchUpdateScheduleResponse
mkBatchUpdateScheduleResponse responseStatus
  = BatchUpdateScheduleResponse'{creates = Core.Nothing,
                                 deletes = Core.Nothing, responseStatus}

-- | Schedule actions created in the schedule.
--
-- /Note:/ Consider using 'creates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busrrsCreates :: Lens.Lens' BatchUpdateScheduleResponse (Core.Maybe Types.BatchScheduleActionCreateResult)
busrrsCreates = Lens.field @"creates"
{-# INLINEABLE busrrsCreates #-}
{-# DEPRECATED creates "Use generic-lens or generic-optics with 'creates' instead"  #-}

-- | Schedule actions deleted from the schedule.
--
-- /Note:/ Consider using 'deletes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busrrsDeletes :: Lens.Lens' BatchUpdateScheduleResponse (Core.Maybe Types.BatchScheduleActionDeleteResult)
busrrsDeletes = Lens.field @"deletes"
{-# INLINEABLE busrrsDeletes #-}
{-# DEPRECATED deletes "Use generic-lens or generic-optics with 'deletes' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
busrrsResponseStatus :: Lens.Lens' BatchUpdateScheduleResponse Core.Int
busrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE busrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
