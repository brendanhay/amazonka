{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StopCrawlerSchedule (..),
    mkStopCrawlerSchedule,

    -- ** Request lenses
    sCrawlerName,

    -- * Destructuring the response
    StopCrawlerScheduleResponse (..),
    mkStopCrawlerScheduleResponse,

    -- ** Response lenses
    scsrfrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStopCrawlerSchedule' smart constructor.
newtype StopCrawlerSchedule = StopCrawlerSchedule'
  { -- | Name of the crawler whose schedule state to set.
    crawlerName :: Types.NameString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopCrawlerSchedule' value with any optional fields omitted.
mkStopCrawlerSchedule ::
  -- | 'crawlerName'
  Types.NameString ->
  StopCrawlerSchedule
mkStopCrawlerSchedule crawlerName =
  StopCrawlerSchedule' {crawlerName}

-- | Name of the crawler whose schedule state to set.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sCrawlerName :: Lens.Lens' StopCrawlerSchedule Types.NameString
sCrawlerName = Lens.field @"crawlerName"
{-# DEPRECATED sCrawlerName "Use generic-lens or generic-optics with 'crawlerName' instead." #-}

instance Core.FromJSON StopCrawlerSchedule where
  toJSON StopCrawlerSchedule {..} =
    Core.object
      (Core.catMaybes [Core.Just ("CrawlerName" Core..= crawlerName)])

instance Core.AWSRequest StopCrawlerSchedule where
  type Rs StopCrawlerSchedule = StopCrawlerScheduleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.StopCrawlerSchedule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StopCrawlerScheduleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStopCrawlerScheduleResponse' smart constructor.
newtype StopCrawlerScheduleResponse = StopCrawlerScheduleResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StopCrawlerScheduleResponse' value with any optional fields omitted.
mkStopCrawlerScheduleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StopCrawlerScheduleResponse
mkStopCrawlerScheduleResponse responseStatus =
  StopCrawlerScheduleResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsrfrsResponseStatus :: Lens.Lens' StopCrawlerScheduleResponse Core.Int
scsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
