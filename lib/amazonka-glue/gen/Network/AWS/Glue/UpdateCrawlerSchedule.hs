{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.UpdateCrawlerSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the schedule of a crawler using a @cron@ expression. 
module Network.AWS.Glue.UpdateCrawlerSchedule
    (
    -- * Creating a request
      UpdateCrawlerSchedule (..)
    , mkUpdateCrawlerSchedule
    -- ** Request lenses
    , ucsCrawlerName
    , ucsSchedule

    -- * Destructuring the response
    , UpdateCrawlerScheduleResponse (..)
    , mkUpdateCrawlerScheduleResponse
    -- ** Response lenses
    , ucsrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateCrawlerSchedule' smart constructor.
data UpdateCrawlerSchedule = UpdateCrawlerSchedule'
  { crawlerName :: Types.NameString
    -- ^ The name of the crawler whose schedule to update.
  , schedule :: Core.Maybe Types.CronExpression
    -- ^ The updated @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCrawlerSchedule' value with any optional fields omitted.
mkUpdateCrawlerSchedule
    :: Types.NameString -- ^ 'crawlerName'
    -> UpdateCrawlerSchedule
mkUpdateCrawlerSchedule crawlerName
  = UpdateCrawlerSchedule'{crawlerName, schedule = Core.Nothing}

-- | The name of the crawler whose schedule to update.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsCrawlerName :: Lens.Lens' UpdateCrawlerSchedule Types.NameString
ucsCrawlerName = Lens.field @"crawlerName"
{-# INLINEABLE ucsCrawlerName #-}
{-# DEPRECATED crawlerName "Use generic-lens or generic-optics with 'crawlerName' instead"  #-}

-- | The updated @cron@ expression used to specify the schedule (see <https://docs.aws.amazon.com/glue/latest/dg/monitor-data-warehouse-schedule.html Time-Based Schedules for Jobs and Crawlers> . For example, to run something every day at 12:15 UTC, you would specify: @cron(15 12 * * ? *)@ .
--
-- /Note:/ Consider using 'schedule' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsSchedule :: Lens.Lens' UpdateCrawlerSchedule (Core.Maybe Types.CronExpression)
ucsSchedule = Lens.field @"schedule"
{-# INLINEABLE ucsSchedule #-}
{-# DEPRECATED schedule "Use generic-lens or generic-optics with 'schedule' instead"  #-}

instance Core.ToQuery UpdateCrawlerSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders UpdateCrawlerSchedule where
        toHeaders UpdateCrawlerSchedule{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.UpdateCrawlerSchedule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON UpdateCrawlerSchedule where
        toJSON UpdateCrawlerSchedule{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("CrawlerName" Core..= crawlerName),
                  ("Schedule" Core..=) Core.<$> schedule])

instance Core.AWSRequest UpdateCrawlerSchedule where
        type Rs UpdateCrawlerSchedule = UpdateCrawlerScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 UpdateCrawlerScheduleResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkUpdateCrawlerScheduleResponse' smart constructor.
newtype UpdateCrawlerScheduleResponse = UpdateCrawlerScheduleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateCrawlerScheduleResponse' value with any optional fields omitted.
mkUpdateCrawlerScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> UpdateCrawlerScheduleResponse
mkUpdateCrawlerScheduleResponse responseStatus
  = UpdateCrawlerScheduleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ucsrrsResponseStatus :: Lens.Lens' UpdateCrawlerScheduleResponse Core.Int
ucsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ucsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
