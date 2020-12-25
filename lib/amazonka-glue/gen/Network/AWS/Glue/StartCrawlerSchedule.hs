{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    StartCrawlerSchedule (..),
    mkStartCrawlerSchedule,

    -- ** Request lenses
    scsCrawlerName,

    -- * Destructuring the response
    StartCrawlerScheduleResponse (..),
    mkStartCrawlerScheduleResponse,

    -- ** Response lenses
    scsrrsResponseStatus,
  )
where

import qualified Network.AWS.Glue.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkStartCrawlerSchedule' smart constructor.
newtype StartCrawlerSchedule = StartCrawlerSchedule'
  { -- | Name of the crawler to schedule.
    crawlerName :: Types.CrawlerName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartCrawlerSchedule' value with any optional fields omitted.
mkStartCrawlerSchedule ::
  -- | 'crawlerName'
  Types.CrawlerName ->
  StartCrawlerSchedule
mkStartCrawlerSchedule crawlerName =
  StartCrawlerSchedule' {crawlerName}

-- | Name of the crawler to schedule.
--
-- /Note:/ Consider using 'crawlerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsCrawlerName :: Lens.Lens' StartCrawlerSchedule Types.CrawlerName
scsCrawlerName = Lens.field @"crawlerName"
{-# DEPRECATED scsCrawlerName "Use generic-lens or generic-optics with 'crawlerName' instead." #-}

instance Core.FromJSON StartCrawlerSchedule where
  toJSON StartCrawlerSchedule {..} =
    Core.object
      (Core.catMaybes [Core.Just ("CrawlerName" Core..= crawlerName)])

instance Core.AWSRequest StartCrawlerSchedule where
  type Rs StartCrawlerSchedule = StartCrawlerScheduleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "AWSGlue.StartCrawlerSchedule")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveEmpty
      ( \s h x ->
          StartCrawlerScheduleResponse'
            Core.<$> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkStartCrawlerScheduleResponse' smart constructor.
newtype StartCrawlerScheduleResponse = StartCrawlerScheduleResponse'
  { -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'StartCrawlerScheduleResponse' value with any optional fields omitted.
mkStartCrawlerScheduleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  StartCrawlerScheduleResponse
mkStartCrawlerScheduleResponse responseStatus =
  StartCrawlerScheduleResponse' {responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsrrsResponseStatus :: Lens.Lens' StartCrawlerScheduleResponse Core.Int
scsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED scsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
