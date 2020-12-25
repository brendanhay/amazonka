{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AlexaBusiness.ListBusinessReportSchedules
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the details of the schedules that a user configured. A download URL of the report associated with each schedule is returned every time this action is called. A new download URL is returned each time, and is valid for 24 hours.
--
-- This operation returns paginated results.
module Network.AWS.AlexaBusiness.ListBusinessReportSchedules
  ( -- * Creating a request
    ListBusinessReportSchedules (..),
    mkListBusinessReportSchedules,

    -- ** Request lenses
    lbrsMaxResults,
    lbrsNextToken,

    -- * Destructuring the response
    ListBusinessReportSchedulesResponse (..),
    mkListBusinessReportSchedulesResponse,

    -- ** Response lenses
    lbrsrrsBusinessReportSchedules,
    lbrsrrsNextToken,
    lbrsrrsResponseStatus,
  )
where

import qualified Network.AWS.AlexaBusiness.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListBusinessReportSchedules' smart constructor.
data ListBusinessReportSchedules = ListBusinessReportSchedules'
  { -- | The maximum number of schedules listed in the call.
    maxResults :: Core.Maybe Core.Natural,
    -- | The token used to list the remaining schedules from the previous API call.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListBusinessReportSchedules' value with any optional fields omitted.
mkListBusinessReportSchedules ::
  ListBusinessReportSchedules
mkListBusinessReportSchedules =
  ListBusinessReportSchedules'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of schedules listed in the call.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsMaxResults :: Lens.Lens' ListBusinessReportSchedules (Core.Maybe Core.Natural)
lbrsMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lbrsMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The token used to list the remaining schedules from the previous API call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsNextToken :: Lens.Lens' ListBusinessReportSchedules (Core.Maybe Types.NextToken)
lbrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListBusinessReportSchedules where
  toJSON ListBusinessReportSchedules {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListBusinessReportSchedules where
  type
    Rs ListBusinessReportSchedules =
      ListBusinessReportSchedulesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AlexaForBusiness.ListBusinessReportSchedules")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListBusinessReportSchedulesResponse'
            Core.<$> (x Core..:? "BusinessReportSchedules")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListBusinessReportSchedules where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"businessReportSchedules" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListBusinessReportSchedulesResponse' smart constructor.
data ListBusinessReportSchedulesResponse = ListBusinessReportSchedulesResponse'
  { -- | The schedule of the reports.
    businessReportSchedules :: Core.Maybe [Types.BusinessReportSchedule],
    -- | The token used to list the remaining schedules from the previous API call.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListBusinessReportSchedulesResponse' value with any optional fields omitted.
mkListBusinessReportSchedulesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListBusinessReportSchedulesResponse
mkListBusinessReportSchedulesResponse responseStatus =
  ListBusinessReportSchedulesResponse'
    { businessReportSchedules =
        Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The schedule of the reports.
--
-- /Note:/ Consider using 'businessReportSchedules' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsrrsBusinessReportSchedules :: Lens.Lens' ListBusinessReportSchedulesResponse (Core.Maybe [Types.BusinessReportSchedule])
lbrsrrsBusinessReportSchedules = Lens.field @"businessReportSchedules"
{-# DEPRECATED lbrsrrsBusinessReportSchedules "Use generic-lens or generic-optics with 'businessReportSchedules' instead." #-}

-- | The token used to list the remaining schedules from the previous API call.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsrrsNextToken :: Lens.Lens' ListBusinessReportSchedulesResponse (Core.Maybe Types.NextToken)
lbrsrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lbrsrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbrsrrsResponseStatus :: Lens.Lens' ListBusinessReportSchedulesResponse Core.Int
lbrsrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lbrsrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
