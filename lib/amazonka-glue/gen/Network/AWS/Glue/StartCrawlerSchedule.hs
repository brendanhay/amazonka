{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StartCrawlerSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the schedule state of the specified crawler to @SCHEDULED@ , unless the crawler is already running or the schedule state is already @SCHEDULED@ .
module Network.AWS.Glue.StartCrawlerSchedule
    (
    -- * Creating a request
      StartCrawlerSchedule (..)
    , mkStartCrawlerSchedule
    -- ** Request lenses
    , scsCrawlerName

    -- * Destructuring the response
    , StartCrawlerScheduleResponse (..)
    , mkStartCrawlerScheduleResponse
    -- ** Response lenses
    , scsrrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartCrawlerSchedule' smart constructor.
newtype StartCrawlerSchedule = StartCrawlerSchedule'
  { crawlerName :: Types.CrawlerName
    -- ^ Name of the crawler to schedule.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartCrawlerSchedule' value with any optional fields omitted.
mkStartCrawlerSchedule
    :: Types.CrawlerName -- ^ 'crawlerName'
    -> StartCrawlerSchedule
mkStartCrawlerSchedule crawlerName
  = StartCrawlerSchedule'{crawlerName}

-- | Name of the crawler to schedule.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsCrawlerName :: Lens.Lens' StartCrawlerSchedule Types.CrawlerName
scsCrawlerName = Lens.field @"crawlerName"
{-# INLINEABLE scsCrawlerName #-}
{-# DEPRECATED crawlerName "Use generic-lens or generic-optics with 'crawlerName' instead"  #-}

instance Core.ToQuery StartCrawlerSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StartCrawlerSchedule where
        toHeaders StartCrawlerSchedule{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.StartCrawlerSchedule")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StartCrawlerSchedule where
        toJSON StartCrawlerSchedule{..}
          = Core.object
              (Core.catMaybes [Core.Just ("CrawlerName" Core..= crawlerName)])

instance Core.AWSRequest StartCrawlerSchedule where
        type Rs StartCrawlerSchedule = StartCrawlerScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StartCrawlerScheduleResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStartCrawlerScheduleResponse' smart constructor.
newtype StartCrawlerScheduleResponse = StartCrawlerScheduleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartCrawlerScheduleResponse' value with any optional fields omitted.
mkStartCrawlerScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StartCrawlerScheduleResponse
mkStartCrawlerScheduleResponse responseStatus
  = StartCrawlerScheduleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsrrsResponseStatus :: Lens.Lens' StartCrawlerScheduleResponse Core.Int
scsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE scsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
