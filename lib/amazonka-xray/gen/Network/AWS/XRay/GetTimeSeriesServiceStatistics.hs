{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    gtsssEntitySelectorExpression,
    gtsssPeriod,
    gtsssForecastStatistics,
    gtsssNextToken,
    gtsssGroupARN,
    gtsssGroupName,
    gtsssStartTime,
    gtsssEndTime,

    -- * Destructuring the response
    GetTimeSeriesServiceStatisticsResponse (..),
    mkGetTimeSeriesServiceStatisticsResponse,

    -- ** Response lenses
    gtsssrsContainsOldGroupVersions,
    gtsssrsTimeSeriesServiceStatistics,
    gtsssrsNextToken,
    gtsssrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.XRay.Types

-- | /See:/ 'mkGetTimeSeriesServiceStatistics' smart constructor.
data GetTimeSeriesServiceStatistics = GetTimeSeriesServiceStatistics'
  { entitySelectorExpression ::
      Lude.Maybe Lude.Text,
    period :: Lude.Maybe Lude.Int,
    forecastStatistics ::
      Lude.Maybe Lude.Bool,
    nextToken ::
      Lude.Maybe Lude.Text,
    groupARN ::
      Lude.Maybe Lude.Text,
    groupName ::
      Lude.Maybe Lude.Text,
    startTime :: Lude.Timestamp,
    endTime :: Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTimeSeriesServiceStatistics' with the minimum fields required to make a request.
--
-- * 'endTime' - The end of the time frame for which to aggregate statistics.
-- * 'entitySelectorExpression' - A filter expression defining entities that will be aggregated for statistics. Supports ID, service, and edge functions. If no selector expression is specified, edge statistics are returned.
-- * 'forecastStatistics' - The forecasted high and low fault count values. Forecast enabled requests require the EntitySelectorExpression ID be provided.
-- * 'groupARN' - The Amazon Resource Name (ARN) of the group for which to pull statistics from.
-- * 'groupName' - The case-sensitive name of the group for which to pull statistics from.
-- * 'nextToken' - Pagination token.
-- * 'period' - Aggregation period in seconds.
-- * 'startTime' - The start of the time frame for which to aggregate statistics.
mkGetTimeSeriesServiceStatistics ::
  -- | 'startTime'
  Lude.Timestamp ->
  -- | 'endTime'
  Lude.Timestamp ->
  GetTimeSeriesServiceStatistics
mkGetTimeSeriesServiceStatistics pStartTime_ pEndTime_ =
  GetTimeSeriesServiceStatistics'
    { entitySelectorExpression =
        Lude.Nothing,
      period = Lude.Nothing,
      forecastStatistics = Lude.Nothing,
      nextToken = Lude.Nothing,
      groupARN = Lude.Nothing,
      groupName = Lude.Nothing,
      startTime = pStartTime_,
      endTime = pEndTime_
    }

-- | A filter expression defining entities that will be aggregated for statistics. Supports ID, service, and edge functions. If no selector expression is specified, edge statistics are returned.
--
-- /Note:/ Consider using 'entitySelectorExpression' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssEntitySelectorExpression :: Lens.Lens' GetTimeSeriesServiceStatistics (Lude.Maybe Lude.Text)
gtsssEntitySelectorExpression = Lens.lens (entitySelectorExpression :: GetTimeSeriesServiceStatistics -> Lude.Maybe Lude.Text) (\s a -> s {entitySelectorExpression = a} :: GetTimeSeriesServiceStatistics)
{-# DEPRECATED gtsssEntitySelectorExpression "Use generic-lens or generic-optics with 'entitySelectorExpression' instead." #-}

-- | Aggregation period in seconds.
--
-- /Note:/ Consider using 'period' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssPeriod :: Lens.Lens' GetTimeSeriesServiceStatistics (Lude.Maybe Lude.Int)
gtsssPeriod = Lens.lens (period :: GetTimeSeriesServiceStatistics -> Lude.Maybe Lude.Int) (\s a -> s {period = a} :: GetTimeSeriesServiceStatistics)
{-# DEPRECATED gtsssPeriod "Use generic-lens or generic-optics with 'period' instead." #-}

-- | The forecasted high and low fault count values. Forecast enabled requests require the EntitySelectorExpression ID be provided.
--
-- /Note:/ Consider using 'forecastStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssForecastStatistics :: Lens.Lens' GetTimeSeriesServiceStatistics (Lude.Maybe Lude.Bool)
gtsssForecastStatistics = Lens.lens (forecastStatistics :: GetTimeSeriesServiceStatistics -> Lude.Maybe Lude.Bool) (\s a -> s {forecastStatistics = a} :: GetTimeSeriesServiceStatistics)
{-# DEPRECATED gtsssForecastStatistics "Use generic-lens or generic-optics with 'forecastStatistics' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssNextToken :: Lens.Lens' GetTimeSeriesServiceStatistics (Lude.Maybe Lude.Text)
gtsssNextToken = Lens.lens (nextToken :: GetTimeSeriesServiceStatistics -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTimeSeriesServiceStatistics)
{-# DEPRECATED gtsssNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The Amazon Resource Name (ARN) of the group for which to pull statistics from.
--
-- /Note:/ Consider using 'groupARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssGroupARN :: Lens.Lens' GetTimeSeriesServiceStatistics (Lude.Maybe Lude.Text)
gtsssGroupARN = Lens.lens (groupARN :: GetTimeSeriesServiceStatistics -> Lude.Maybe Lude.Text) (\s a -> s {groupARN = a} :: GetTimeSeriesServiceStatistics)
{-# DEPRECATED gtsssGroupARN "Use generic-lens or generic-optics with 'groupARN' instead." #-}

-- | The case-sensitive name of the group for which to pull statistics from.
--
-- /Note:/ Consider using 'groupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssGroupName :: Lens.Lens' GetTimeSeriesServiceStatistics (Lude.Maybe Lude.Text)
gtsssGroupName = Lens.lens (groupName :: GetTimeSeriesServiceStatistics -> Lude.Maybe Lude.Text) (\s a -> s {groupName = a} :: GetTimeSeriesServiceStatistics)
{-# DEPRECATED gtsssGroupName "Use generic-lens or generic-optics with 'groupName' instead." #-}

-- | The start of the time frame for which to aggregate statistics.
--
-- /Note:/ Consider using 'startTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssStartTime :: Lens.Lens' GetTimeSeriesServiceStatistics Lude.Timestamp
gtsssStartTime = Lens.lens (startTime :: GetTimeSeriesServiceStatistics -> Lude.Timestamp) (\s a -> s {startTime = a} :: GetTimeSeriesServiceStatistics)
{-# DEPRECATED gtsssStartTime "Use generic-lens or generic-optics with 'startTime' instead." #-}

-- | The end of the time frame for which to aggregate statistics.
--
-- /Note:/ Consider using 'endTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssEndTime :: Lens.Lens' GetTimeSeriesServiceStatistics Lude.Timestamp
gtsssEndTime = Lens.lens (endTime :: GetTimeSeriesServiceStatistics -> Lude.Timestamp) (\s a -> s {endTime = a} :: GetTimeSeriesServiceStatistics)
{-# DEPRECATED gtsssEndTime "Use generic-lens or generic-optics with 'endTime' instead." #-}

instance Page.AWSPager GetTimeSeriesServiceStatistics where
  page rq rs
    | Page.stop (rs Lens.^. gtsssrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. gtsssrsTimeSeriesServiceStatistics) =
      Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& gtsssNextToken Lens..~ rs Lens.^. gtsssrsNextToken

instance Lude.AWSRequest GetTimeSeriesServiceStatistics where
  type
    Rs GetTimeSeriesServiceStatistics =
      GetTimeSeriesServiceStatisticsResponse
  request = Req.postJSON xRayService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetTimeSeriesServiceStatisticsResponse'
            Lude.<$> (x Lude..?> "ContainsOldGroupVersions")
            Lude.<*> (x Lude..?> "TimeSeriesServiceStatistics" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetTimeSeriesServiceStatistics where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON GetTimeSeriesServiceStatistics where
  toJSON GetTimeSeriesServiceStatistics' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("EntitySelectorExpression" Lude..=)
              Lude.<$> entitySelectorExpression,
            ("Period" Lude..=) Lude.<$> period,
            ("ForecastStatistics" Lude..=) Lude.<$> forecastStatistics,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("GroupARN" Lude..=) Lude.<$> groupARN,
            ("GroupName" Lude..=) Lude.<$> groupName,
            Lude.Just ("StartTime" Lude..= startTime),
            Lude.Just ("EndTime" Lude..= endTime)
          ]
      )

instance Lude.ToPath GetTimeSeriesServiceStatistics where
  toPath = Lude.const "/TimeSeriesServiceStatistics"

instance Lude.ToQuery GetTimeSeriesServiceStatistics where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetTimeSeriesServiceStatisticsResponse' smart constructor.
data GetTimeSeriesServiceStatisticsResponse = GetTimeSeriesServiceStatisticsResponse'
  { containsOldGroupVersions ::
      Lude.Maybe
        Lude.Bool,
    timeSeriesServiceStatistics ::
      Lude.Maybe
        [TimeSeriesServiceStatistics],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetTimeSeriesServiceStatisticsResponse' with the minimum fields required to make a request.
--
-- * 'containsOldGroupVersions' - A flag indicating whether or not a group's filter expression has been consistent, or if a returned aggregation might show statistics from an older version of the group's filter expression.
-- * 'nextToken' - Pagination token.
-- * 'responseStatus' - The response status code.
-- * 'timeSeriesServiceStatistics' - The collection of statistics.
mkGetTimeSeriesServiceStatisticsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetTimeSeriesServiceStatisticsResponse
mkGetTimeSeriesServiceStatisticsResponse pResponseStatus_ =
  GetTimeSeriesServiceStatisticsResponse'
    { containsOldGroupVersions =
        Lude.Nothing,
      timeSeriesServiceStatistics = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A flag indicating whether or not a group's filter expression has been consistent, or if a returned aggregation might show statistics from an older version of the group's filter expression.
--
-- /Note:/ Consider using 'containsOldGroupVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssrsContainsOldGroupVersions :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Lude.Maybe Lude.Bool)
gtsssrsContainsOldGroupVersions = Lens.lens (containsOldGroupVersions :: GetTimeSeriesServiceStatisticsResponse -> Lude.Maybe Lude.Bool) (\s a -> s {containsOldGroupVersions = a} :: GetTimeSeriesServiceStatisticsResponse)
{-# DEPRECATED gtsssrsContainsOldGroupVersions "Use generic-lens or generic-optics with 'containsOldGroupVersions' instead." #-}

-- | The collection of statistics.
--
-- /Note:/ Consider using 'timeSeriesServiceStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssrsTimeSeriesServiceStatistics :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Lude.Maybe [TimeSeriesServiceStatistics])
gtsssrsTimeSeriesServiceStatistics = Lens.lens (timeSeriesServiceStatistics :: GetTimeSeriesServiceStatisticsResponse -> Lude.Maybe [TimeSeriesServiceStatistics]) (\s a -> s {timeSeriesServiceStatistics = a} :: GetTimeSeriesServiceStatisticsResponse)
{-# DEPRECATED gtsssrsTimeSeriesServiceStatistics "Use generic-lens or generic-optics with 'timeSeriesServiceStatistics' instead." #-}

-- | Pagination token.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssrsNextToken :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse (Lude.Maybe Lude.Text)
gtsssrsNextToken = Lens.lens (nextToken :: GetTimeSeriesServiceStatisticsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetTimeSeriesServiceStatisticsResponse)
{-# DEPRECATED gtsssrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gtsssrsResponseStatus :: Lens.Lens' GetTimeSeriesServiceStatisticsResponse Lude.Int
gtsssrsResponseStatus = Lens.lens (responseStatus :: GetTimeSeriesServiceStatisticsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetTimeSeriesServiceStatisticsResponse)
{-# DEPRECATED gtsssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
