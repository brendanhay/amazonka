{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MediaLive.DescribeSchedule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get a channel schedule
--
-- This operation returns paginated results.
module Network.AWS.MediaLive.DescribeSchedule
  ( -- * Creating a request
    DescribeSchedule (..),
    mkDescribeSchedule,

    -- ** Request lenses
    dChannelId,
    dMaxResults,
    dNextToken,

    -- * Destructuring the response
    DescribeScheduleResponse (..),
    mkDescribeScheduleResponse,

    -- ** Response lenses
    dsrfrsNextToken,
    dsrfrsScheduleActions,
    dsrfrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.MediaLive.Types as Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Placeholder documentation for DescribeScheduleRequest
--
-- /See:/ 'mkDescribeSchedule' smart constructor.
data DescribeSchedule = DescribeSchedule'
  { -- | Id of the channel whose schedule is being updated.
    channelId :: Core.Text,
    maxResults :: Core.Maybe Core.Natural,
    nextToken :: Core.Maybe Core.Text
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeSchedule' value with any optional fields omitted.
mkDescribeSchedule ::
  -- | 'channelId'
  Core.Text ->
  DescribeSchedule
mkDescribeSchedule channelId =
  DescribeSchedule'
    { channelId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | Id of the channel whose schedule is being updated.
--
-- /Note:/ Consider using 'channelId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dChannelId :: Lens.Lens' DescribeSchedule Core.Text
dChannelId = Lens.field @"channelId"
{-# DEPRECATED dChannelId "Use generic-lens or generic-optics with 'channelId' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaxResults :: Lens.Lens' DescribeSchedule (Core.Maybe Core.Natural)
dMaxResults = Lens.field @"maxResults"
{-# DEPRECATED dMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNextToken :: Lens.Lens' DescribeSchedule (Core.Maybe Core.Text)
dNextToken = Lens.field @"nextToken"
{-# DEPRECATED dNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest DescribeSchedule where
  type Rs DescribeSchedule = DescribeScheduleResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/prod/channels/" Core.<> (Core.toText channelId)
                Core.<> ("/schedule")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders =
          Core.pure ("Content-Type", "application/x-amz-json-1.1"),
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeScheduleResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "scheduleActions")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager DescribeSchedule where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"scheduleActions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | Placeholder documentation for DescribeScheduleResponse
--
-- /See:/ 'mkDescribeScheduleResponse' smart constructor.
data DescribeScheduleResponse = DescribeScheduleResponse'
  { -- | The next token; for use in pagination.
    nextToken :: Core.Maybe Core.Text,
    -- | The list of actions in the schedule.
    scheduleActions :: Core.Maybe [Types.ScheduleAction],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScheduleResponse' value with any optional fields omitted.
mkDescribeScheduleResponse ::
  -- | 'responseStatus'
  Core.Int ->
  DescribeScheduleResponse
mkDescribeScheduleResponse responseStatus =
  DescribeScheduleResponse'
    { nextToken = Core.Nothing,
      scheduleActions = Core.Nothing,
      responseStatus
    }

-- | The next token; for use in pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsNextToken :: Lens.Lens' DescribeScheduleResponse (Core.Maybe Core.Text)
dsrfrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED dsrfrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of actions in the schedule.
--
-- /Note:/ Consider using 'scheduleActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsScheduleActions :: Lens.Lens' DescribeScheduleResponse (Core.Maybe [Types.ScheduleAction])
dsrfrsScheduleActions = Lens.field @"scheduleActions"
{-# DEPRECATED dsrfrsScheduleActions "Use generic-lens or generic-optics with 'scheduleActions' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsrfrsResponseStatus :: Lens.Lens' DescribeScheduleResponse Core.Int
dsrfrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED dsrfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
