{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.GetTimeSeriesServiceStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get an aggregation of service statistics defined by a specific time range.
--
-- This operation returns paginated results.
module Network.AWS.XRay.GetTimeSeriesServiceStatistics
  ( -- * Creating a request
    GetTimeSeriesServiceStatistics (..),
    mkGetTimeSeriesServiceStatistics,

    -- ** Request lenses
    gtsssStartTime,
    gtsssEndTime,
    gtsssEntitySelectorExpression,
    gtsssForecastStatistics,
    gtsssGroupARN,
    gtsssGroupName,
    gtsssNextToken,
    gtsssPeriod,

    -- * Destructuring the response
    GetTimeSeriesServiceStatisticsResponse (..),
    mkGetTimeSeriesServiceStatisticsResponse,

    -- ** Response lenses
    gtsssrrsContainsOldGroupVersions,
    gtsssrrsNextToken,
    gtsssrrsTimeSeriesServiceStatistics,
    gtsssrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.XRay.Types as Types

-- | /See:/ 'mkGetTimeSeriesServiceStatistics' smart constructor.
data GetTimeSeriesServiceStatistics = GetTimeSeriesServiceStatistics'
  { -- | The start of the time frame for which to aggregate statistics.
    startTime :: Core.NominalDiffTime,
    -- | The end of the time frame for which to aggregate statistics.
    endTime :: Core.NominalDiffTime,
    -- | A filter expression defining entities that will be aggregated for statistics. Supports ID, service, and edge functions. If no selector expression is specified, edge statistics are returned.
    entitySelectorExpression :: Core.Maybe Types.EntitySelectorExpression,
    -- | The forecasted high and low fault count values. Forecast enabled requests require the EntitySelectorExpression ID be provided.
    forecastStatistics :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the group for which to pull statistics from.
    groupARN :: Core.Maybe Types.GroupARN,
    -- | The case-sensitive name of the group for which to pull statistics from.
    groupName :: Core.Maybe Types.GroupName,
    -- | Pagination token.
    nextToken :: Core.Maybe Types.String,
    -- | Aggregation period in seconds.
    period :: Core.Maybe Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetTimeSeriesServiceStatistics' value with any optional fields omitted.
mkGetTimeSeriesServiceStatistics ::
  -- | 'startTime'
  Core.NominalDiffTime ->
  -- | 'endTime'
  Core.NominalDiffTime ->
  GetTimeSeriesServiceStatistics
mkGetTimeSeriesServiceStatistics startTime endTime =
  GetTimeSeriesServiceStatistics'
    { startTime,
      endTime,
      entitySelectorExpression = Core.Nothing,
      forecastStatistics = Core.Nothing,
      groupARN = Core.Nothing,
      groupName = Core.Nothing,
      nextToken = Core.Nothing,
      period = Core.Nothing
    }

-- | The start of the time frame for which to aggregate statistics.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssStartTime :: Lens.Lens' GetTimeSeriesServiceStatistics Core.NominalDiffTime
gtsssStartTime = Lens.field @"startTime"
{-# DEPRECATED gtsssStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end of the time frame for which to aggregate statistics.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssEndTime :: Lens.Lens' GetTimeSeriesServiceStatistics Core.NominalDiffTime
gtsssEndTime = Lens.field @"endTime"
{-# DEPRECATED gtsssEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

-- | A filter expression defining entities that will be aggregated for statistics. Supports ID, service, and edge functions. If no selector expression is specified, edge statistics are returned.
--
-- /Note:/ Consider using 'entitySelectorExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssEntitySelectorExpression :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Types.EntitySelectorExpression)
gtsssEntitySelectorExpression = Lens.field @"entitySelectorExpression"
{-# DEPRECATED gtsssEntitySelectorExpression "Use generic-lens or generic-optics with 'entitySelectorExpression' instead." #-}

-- | The forecasted high and low fault count values. Forecast enabled requests require the EntitySelectorExpression ID be provided.
--
-- /Note:/ Consider using 'forecastStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssForecastStatistics :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Core.Bool)
gtsssForecastStatistics = Lens.field @"forecastStatistics"
{-# DEPRECATED gtsssForecastStatistics "Use generic-lens or generic-optics with 'forecastStatistics' instead." #-}

-- | The Amazon Resource Name (ARN) of the group for which to pull statistics from.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssGroupARN :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Types.GroupARN)
gtsssGroupARN = Lens.field @"groupARN"
{-# DEPRECATED gtsssGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The case-sensitive name of the group for which to pull statistics from.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssGroupName :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Types.GroupName)
gtsssGroupName = Lens.field @"groupName"
{-# DEPRECATED gtsssGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssNextToken :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Types.String)
gtsssNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtsssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Aggregation period in seconds.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssPeriod :: Lens.Lens' GetTimeSeriesServiceStatistics (Core.Maybe Core.Int)
gtsssPeriod = Lens.field @"period"
{-# DEPRECATED gtsssPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

instance Core.FromJSON GetTimeSeriesServiceStatistics where
  toJSON GetTimeSeriesServiceStatistics {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("StartTime" Core..= startTime),
            Core.Just ("EndTime" Core..= endTime),
            ("EntitySelectorExpression" Core..=)
              Core.<$> entitySelectorExpression,
            ("ForecastStatistics" Core..=) Core.<$> forecastStatistics,
            ("GroupARN" Core..=) Core.<$> groupARN,
            ("GroupName" Core..=) Core.<$> groupName,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("Period" Core..=) Core.<$> period
          ]
      )

instance Core.AWSRequest GetTimeSeriesServiceStatistics where
  type
    Rs GetTimeSeriesServiceStatistics =
      GetTimeSeriesServiceStatisticsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/TimeSeriesServiceStatistics",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders = Core.mempty,
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTimeSeriesServiceStatisticsResponse'
            Core.<$> (x Core..:? "ContainsOldGroupVersions")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (x Core..:? "TimeSeriesServiceStatistics")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager GetTimeSeriesServiceStatistics where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        ( rs
            Lens.^? Lens.field @"timeSeriesServiceStatistics" Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkGetTimeSeriesServiceStatisticsResponse' smart constructor.
data GetTimeSeriesServiceStatisticsResponse = GetTimeSeriesServiceStatisticsResponse'
  { -- | A flag indicating whether or not a group's filter expression has been consistent, or if a returned aggregation might show statistics from an older version of the group's filter expression.
    containsOldGroupVersions :: Core.Maybe Core.Bool,
    -- | Pagination token.
    nextToken :: Core.Maybe Types.String,
    -- | The collection of statistics.
    timeSeriesServiceStatistics :: Core.Maybe [Types.TimeSeriesServiceStatistics],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'GetTimeSeriesServiceStatisticsResponse' value with any optional fields omitted.
mkGetTimeSeriesServiceStatisticsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  GetTimeSeriesServiceStatisticsResponse
mkGetTimeSeriesServiceStatisticsResponse responseStatus =
  GetTimeSeriesServiceStatisticsResponse'
    { containsOldGroupVersions =
        Core.Nothing,
      nextToken = Core.Nothing,
      timeSeriesServiceStatistics = Core.Nothing,
      responseStatus
    }

-- | A flag indicating whether or not a group's filter expression has been consistent, or if a returned aggregation might show statistics from an older version of the group's filter expression.
--
-- /Note:/ Consider using 'containsOldGroupVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssrrsContainsOldGroupVersions :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Core.Maybe Core.Bool)
gtsssrrsContainsOldGroupVersions = Lens.field @"containsOldGroupVersions"
{-# DEPRECATED gtsssrrsContainsOldGroupVersions "Use generic-lens or generic-optics with 'containsOldGroupVersions' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssrrsNextToken :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Core.Maybe Types.String)
gtsssrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED gtsssrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The collection of statistics.
--
-- /Note:/ Consider using 'timeSeriesServiceStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssrrsTimeSeriesServiceStatistics :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Core.Maybe [Types.TimeSeriesServiceStatistics])
gtsssrrsTimeSeriesServiceStatistics = Lens.field @"timeSeriesServiceStatistics"
{-# DEPRECATED gtsssrrsTimeSeriesServiceStatistics "Use generic-lens or generic-optics with 'timeSeriesServiceStatistics' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssrrsResponseStatus :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse Core.Int
gtsssrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gtsssrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
