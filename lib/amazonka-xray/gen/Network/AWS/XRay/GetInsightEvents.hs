{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetInsightEvents
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- X-Ray reevaluates insights periodically until they're resolved, and records each intermediate state as an event. You can review an insight's events in the Impact Timeline on the Inspect page in the X-Ray console.
module Network.AWS.XRay.GetInsightEvents
  ( -- * Creating a request
    GetInsightEvents (..),
    mkGetInsightEvents,

    -- ** Request lenses
    gieInsightId,
    gieMaxResults,
    gieNextToken,

    -- * Destructuring the response
    GetInsightEventsResponse (..),
    mkGetInsightEventsResponse,

    -- ** Response lenses
    gierrsInsightEvents,
    gierrsNextToken,
    gierrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetInsightEvents' smart constructor.
data GetInsightEvents = GetInsightEvents'
  { -- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
    insightId :: Types.InsightId,
    -- | Used to retrieve at most the specified value of events.
    maxResults :: Core.Maybe Core.Natural,
    -- | Specify the pagination token returned by a previous request to retrieve the next page of events.
    nextToken :: Core.Maybe Types.Token
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetInsightEvents' value with any optional fields omitted.
mkGetInsightEvents ::
  -- | 'insightId'
  Types.InsightId ->
  GetInsightEvents
mkGetInsightEvents insightId =
  GetInsightEvents'
    { insightId,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The insight's unique identifier. Use the GetInsightSummaries action to retrieve an InsightId.
--
-- /Note:/ Consider using 'insightId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gieInsightId :: Lens.Lens' GetInsightEvents Types.InsightId
gieInsightId = Lens.field @"insightId"
{-# DEPRECATED gieInsightId "Use generic-lens or generic-optics with 'insightId' instead." #-}

-- | Used to retrieve at most the specified value of events.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gieMaxResults :: Lens.Lens' GetInsightEvents (Core.Maybe Core.Natural)
gieMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gieMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Specify the pagination token returned by a previous request to retrieve the next page of events.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gieNextToken :: Lens.Lens' GetInsightEvents (Core.Maybe Types.Token)
gieNextToken = Lens.field @"nextToken"
{-# DEPRECATED gieNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON GetInsightEvents where
  toJSON GetInsightEvents {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("InsightId" Core..= insightId),
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest GetInsightEvents where
  type Rs GetInsightEvents = GetInsightEventsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/InsightEvents",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightEventsResponse'
            Core.<$> (x Core..:? "InsightEvents")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInsightEventsResponse' smart constructor.
data GetInsightEventsResponse = GetInsightEventsResponse'
  { -- | A detailed description of the event. This includes the time of the event, client and root cause impact statistics, and the top anomalous service at the time of the event.
    insightEvents :: Core.Maybe [Types.InsightEvent],
    -- | Use this token to retrieve the next page of insight events.
    nextToken :: Core.Maybe Types.Token,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInsightEventsResponse' value with any optional fields omitted.
mkGetInsightEventsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInsightEventsResponse
mkGetInsightEventsResponse responseStatus =
  GetInsightEventsResponse'
    { insightEvents = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A detailed description of the event. This includes the time of the event, client and root cause impact statistics, and the top anomalous service at the time of the event.
--
-- /Note:/ Consider using 'insightEvents' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gierrsInsightEvents :: Lens.Lens' GetInsightEventsResponse (Core.Maybe [Types.InsightEvent])
gierrsInsightEvents = Lens.field @"insightEvents"
{-# DEPRECATED gierrsInsightEvents "Use generic-lens or generic-optics with 'insightEvents' instead." #-}

-- | Use this token to retrieve the next page of insight events.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gierrsNextToken :: Lens.Lens' GetInsightEventsResponse (Core.Maybe Types.Token)
gierrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gierrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gierrsResponseStatus :: Lens.Lens' GetInsightEventsResponse Core.Int
gierrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gierrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
