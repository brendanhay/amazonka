{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.StopCrawlerSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the schedule state of the specified crawler to @NOT_SCHEDULED@ , but does not stop the crawler if it is already running.
module Network.AWS.Glue.StopCrawlerSchedule
    (
    -- * Creating a request
      StopCrawlerSchedule (..)
    , mkStopCrawlerSchedule
    -- ** Request lenses
    , sCrawlerName

    -- * Destructuring the response
    , StopCrawlerScheduleResponse (..)
    , mkStopCrawlerScheduleResponse
    -- ** Response lenses
    , scsrfrsResponseStatus
    ) where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopCrawlerSchedule' smart constructor.
newtype StopCrawlerSchedule = StopCrawlerSchedule'
  { crawlerName :: Types.NameString
    -- ^ Name of the crawler whose schedule state to set.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopCrawlerSchedule' value with any optional fields omitted.
mkStopCrawlerSchedule
    :: Types.NameString -- ^ 'crawlerName'
    -> StopCrawlerSchedule
mkStopCrawlerSchedule crawlerName
  = StopCrawlerSchedule'{crawlerName}

-- | Name of the crawler whose schedule state to set.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCrawlerName :: Lens.Lens' StopCrawlerSchedule Types.NameString
sCrawlerName = Lens.field @"crawlerName"
{-# INLINEABLE sCrawlerName #-}
{-# DEPRECATED crawlerName "Use generic-lens or generic-optics with 'crawlerName' instead"  #-}

instance Core.ToQuery StopCrawlerSchedule where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders StopCrawlerSchedule where
        toHeaders StopCrawlerSchedule{..}
          = Core.pure ("X-Amz-Target", "AWSGlue.StopCrawlerSchedule") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON StopCrawlerSchedule where
        toJSON StopCrawlerSchedule{..}
          = Core.object
              (Core.catMaybes [Core.Just ("CrawlerName" Core..= crawlerName)])

instance Core.AWSRequest StopCrawlerSchedule where
        type Rs StopCrawlerSchedule = StopCrawlerScheduleResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 StopCrawlerScheduleResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkStopCrawlerScheduleResponse' smart constructor.
newtype StopCrawlerScheduleResponse = StopCrawlerScheduleResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopCrawlerScheduleResponse' value with any optional fields omitted.
mkStopCrawlerScheduleResponse
    :: Core.Int -- ^ 'responseStatus'
    -> StopCrawlerScheduleResponse
mkStopCrawlerScheduleResponse responseStatus
  = StopCrawlerScheduleResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsrfrsResponseStatus :: Lens.Lens' StopCrawlerScheduleResponse Core.Int
scsrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE scsrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
