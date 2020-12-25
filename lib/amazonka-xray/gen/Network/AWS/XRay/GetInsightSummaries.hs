{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetInsightSummaries
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the summaries of all insights in the specified group matching the provided filter values.
module Network.AWS.XRay.GetInsightSummaries
  ( -- * Creating a request
    GetInsightSummaries (..),
    mkGetInsightSummaries,

    -- ** Request lenses
    gisStartTime,
    gisEndTime,
    gisGroupARN,
    gisGroupName,
    gisMaxResults,
    gisNextToken,
    gisStates,

    -- * Destructuring the response
    GetInsightSummariesResponse (..),
    mkGetInsightSummariesResponse,

    -- ** Response lenses
    gisrrsInsightSummaries,
    gisrrsNextToken,
    gisrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetInsightSummaries' smart constructor.
data GetInsightSummaries = GetInsightSummaries'
  { -- | The beginning of the time frame in which the insights started. The start time can't be more than 30 days old.
    startTime :: Core.NominalDiffTime,
    -- | The end of the time frame in which the insights ended. The end time can't be more than 30 days old.
    endTime :: Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of the group. Required if the GroupName isn't provided.
    groupARN :: Core.Maybe Types.GroupARN,
    -- | The name of the group. Required if the GroupARN isn't provided.
    groupName :: Core.Maybe Types.GroupName,
    -- | The maximum number of results to display.
    maxResults :: Core.Maybe Core.Natural,
    -- | Pagination token.
    nextToken :: Core.Maybe Types.Token,
    -- | The list of insight states.
    states :: Core.Maybe [Types.InsightState]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInsightSummaries' value with any optional fields omitted.
mkGetInsightSummaries ::
  -- | 'startTime'
  Core.NominalDiffTime ->
  -- | 'endTime'
  Core.NominalDiffTime ->
  GetInsightSummaries
mkGetInsightSummaries startTime endTime =
  GetInsightSummaries'
    { startTime,
      endTime,
      groupARN = Core.Nothing,
      groupName = Core.Nothing,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      states = Core.Nothing
    }

-- | The beginning of the time frame in which the insights started. The start time can't be more than 30 days old.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisStartTime :: Lens.Lens' GetInsightSummaries Core.NominalDiffTime
gisStartTime = Lens.field @"startTime"
{-# DEPRECATED gisStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end of the time frame in which the insights ended. The end time can't be more than 30 days old.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisEndTime :: Lens.Lens' GetInsightSummaries Core.NominalDiffTime
gisEndTime = Lens.field @"endTime"
{-# DEPRECATED gisEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the group. Required if the GroupName isn't provided.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisGroupARN :: Lens.Lens' GetInsightSummaries (Core.Maybe Types.GroupARN)
gisGroupARN = Lens.field @"groupARN"
{-# DEPRECATED gisGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The name of the group. Required if the GroupARN isn't provided.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisGroupName :: Lens.Lens' GetInsightSummaries (Core.Maybe Types.GroupName)
gisGroupName = Lens.field @"groupName"
{-# DEPRECATED gisGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The maximum number of results to display.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisMaxResults :: Lens.Lens' GetInsightSummaries (Core.Maybe Core.Natural)
gisMaxResults = Lens.field @"maxResults"
{-# DEPRECATED gisMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisNextToken :: Lens.Lens' GetInsightSummaries (Core.Maybe Types.Token)
gisNextToken = Lens.field @"nextToken"
{-# DEPRECATED gisNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The list of insight states.
--
-- /Note:/ Consider using 'states' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisStates :: Lens.Lens' GetInsightSummaries (Core.Maybe [Types.InsightState])
gisStates = Lens.field @"states"
{-# DEPRECATED gisStates "Use generic-lens or generic-optics with 'states' instead." #-}

instance Core.FromJSON GetInsightSummaries where
  toJSON GetInsightSummaries {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StartTime" Core..= startTime),
            Core.Just ("EndTime" Core..= endTime),
            ("GroupARN" Core..=) Core.<$> groupARN,
            ("GroupName" Core..=) Core.<$> groupName,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("States" Core..=) Core.<$> states
          ]
      )

instance Core.AWSRequest GetInsightSummaries where
  type Rs GetInsightSummaries = GetInsightSummariesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/InsightSummaries",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInsightSummariesResponse'
            Core.<$> (x Core..:? "InsightSummaries")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetInsightSummariesResponse' smart constructor.
data GetInsightSummariesResponse = GetInsightSummariesResponse'
  { -- | The summary of each insight within the group matching the provided filters. The summary contains the InsightID, start and end time, the root cause service, the root cause and client impact statistics, the top anomalous services, and the status of the insight.
    insightSummaries :: Core.Maybe [Types.InsightSummary],
    -- | Pagination token.
    nextToken :: Core.Maybe Types.Token,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetInsightSummariesResponse' value with any optional fields omitted.
mkGetInsightSummariesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetInsightSummariesResponse
mkGetInsightSummariesResponse responseStatus =
  GetInsightSummariesResponse'
    { insightSummaries = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | The summary of each insight within the group matching the provided filters. The summary contains the InsightID, start and end time, the root cause service, the root cause and client impact statistics, the top anomalous services, and the status of the insight.
--
-- /Note:/ Consider using 'insightSummaries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsInsightSummaries :: Lens.Lens' GetInsightSummariesResponse (Core.Maybe [Types.InsightSummary])
gisrrsInsightSummaries = Lens.field @"insightSummaries"
{-# DEPRECATED gisrrsInsightSummaries "Use generic-lens or generic-optics with 'insightSummaries' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsNextToken :: Lens.Lens' GetInsightSummariesResponse (Core.Maybe Types.Token)
gisrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gisrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gisrrsResponseStatus :: Lens.Lens' GetInsightSummariesResponse Core.Int
gisrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gisrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
