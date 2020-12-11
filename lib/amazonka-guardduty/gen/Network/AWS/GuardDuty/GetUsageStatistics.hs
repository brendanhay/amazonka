{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GuardDuty.GetUsageStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon GuardDuty usage statistics over the last 30 days for the specified detector ID. For newly enabled detectors or data sources the cost returned will include only the usage so far under 30 days, this may differ from the cost metrics in the console, which projects usage over 30 days to provide a monthly cost estimate. For more information see <https://docs.aws.amazon.com/guardduty/latest/ug/monitoring_costs.html#usage-calculations Understanding How Usage Costs are Calculated> .
module Network.AWS.GuardDuty.GetUsageStatistics
  ( -- * Creating a request
    GetUsageStatistics (..),
    mkGetUsageStatistics,

    -- ** Request lenses
    gusNextToken,
    gusUnit,
    gusMaxResults,
    gusDetectorId,
    gusUsageStatisticType,
    gusUsageCriteria,

    -- * Destructuring the response
    GetUsageStatisticsResponse (..),
    mkGetUsageStatisticsResponse,

    -- ** Response lenses
    gusrsUsageStatistics,
    gusrsNextToken,
    gusrsResponseStatus,
  )
where

import Network.AWS.GuardDuty.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetUsageStatistics' smart constructor.
data GetUsageStatistics = GetUsageStatistics'
  { nextToken ::
      Lude.Maybe Lude.Text,
    unit :: Lude.Maybe Lude.Text,
    maxResults :: Lude.Maybe Lude.Natural,
    detectorId :: Lude.Text,
    usageStatisticType :: UsageStatisticType,
    usageCriteria :: UsageCriteria
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsageStatistics' with the minimum fields required to make a request.
--
-- * 'detectorId' - The ID of the detector that specifies the GuardDuty service whose usage statistics you want to retrieve.
-- * 'maxResults' - The maximum number of results to return in the response.
-- * 'nextToken' - A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the NextToken value returned from the previous request to continue listing results after the first page.
-- * 'unit' - The currency unit you would like to view your usage statistics in. Current valid values are USD.
-- * 'usageCriteria' - Represents the criteria used for querying usage.
-- * 'usageStatisticType' - The type of usage statistics to retrieve.
mkGetUsageStatistics ::
  -- | 'detectorId'
  Lude.Text ->
  -- | 'usageStatisticType'
  UsageStatisticType ->
  -- | 'usageCriteria'
  UsageCriteria ->
  GetUsageStatistics
mkGetUsageStatistics
  pDetectorId_
  pUsageStatisticType_
  pUsageCriteria_ =
    GetUsageStatistics'
      { nextToken = Lude.Nothing,
        unit = Lude.Nothing,
        maxResults = Lude.Nothing,
        detectorId = pDetectorId_,
        usageStatisticType = pUsageStatisticType_,
        usageCriteria = pUsageCriteria_
      }

-- | A token to use for paginating results that are returned in the response. Set the value of this parameter to null for the first request to a list action. For subsequent calls, use the NextToken value returned from the previous request to continue listing results after the first page.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusNextToken :: Lens.Lens' GetUsageStatistics (Lude.Maybe Lude.Text)
gusNextToken = Lens.lens (nextToken :: GetUsageStatistics -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetUsageStatistics)
{-# DEPRECATED gusNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The currency unit you would like to view your usage statistics in. Current valid values are USD.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusUnit :: Lens.Lens' GetUsageStatistics (Lude.Maybe Lude.Text)
gusUnit = Lens.lens (unit :: GetUsageStatistics -> Lude.Maybe Lude.Text) (\s a -> s {unit = a} :: GetUsageStatistics)
{-# DEPRECATED gusUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The maximum number of results to return in the response.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusMaxResults :: Lens.Lens' GetUsageStatistics (Lude.Maybe Lude.Natural)
gusMaxResults = Lens.lens (maxResults :: GetUsageStatistics -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetUsageStatistics)
{-# DEPRECATED gusMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The ID of the detector that specifies the GuardDuty service whose usage statistics you want to retrieve.
--
-- /Note:/ Consider using 'detectorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusDetectorId :: Lens.Lens' GetUsageStatistics Lude.Text
gusDetectorId = Lens.lens (detectorId :: GetUsageStatistics -> Lude.Text) (\s a -> s {detectorId = a} :: GetUsageStatistics)
{-# DEPRECATED gusDetectorId "Use generic-lens or generic-optics with 'detectorId' instead." #-}

-- | The type of usage statistics to retrieve.
--
-- /Note:/ Consider using 'usageStatisticType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusUsageStatisticType :: Lens.Lens' GetUsageStatistics UsageStatisticType
gusUsageStatisticType = Lens.lens (usageStatisticType :: GetUsageStatistics -> UsageStatisticType) (\s a -> s {usageStatisticType = a} :: GetUsageStatistics)
{-# DEPRECATED gusUsageStatisticType "Use generic-lens or generic-optics with 'usageStatisticType' instead." #-}

-- | Represents the criteria used for querying usage.
--
-- /Note:/ Consider using 'usageCriteria' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusUsageCriteria :: Lens.Lens' GetUsageStatistics UsageCriteria
gusUsageCriteria = Lens.lens (usageCriteria :: GetUsageStatistics -> UsageCriteria) (\s a -> s {usageCriteria = a} :: GetUsageStatistics)
{-# DEPRECATED gusUsageCriteria "Use generic-lens or generic-optics with 'usageCriteria' instead." #-}

instance Lude.AWSRequest GetUsageStatistics where
  type Rs GetUsageStatistics = GetUsageStatisticsResponse
  request = Req.postJSON guardDutyService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetUsageStatisticsResponse'
            Lude.<$> (x Lude..?> "usageStatistics")
            Lude.<*> (x Lude..?> "nextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetUsageStatistics where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetUsageStatistics where
  toJSON GetUsageStatistics' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("nextToken" Lude..=) Lude.<$> nextToken,
            ("unit" Lude..=) Lude.<$> unit,
            ("maxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("usageStatisticsType" Lude..= usageStatisticType),
            Lude.Just ("usageCriteria" Lude..= usageCriteria)
          ]
      )

instance Lude.ToPath GetUsageStatistics where
  toPath GetUsageStatistics' {..} =
    Lude.mconcat
      ["/detector/", Lude.toBS detectorId, "/usage/statistics"]

instance Lude.ToQuery GetUsageStatistics where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetUsageStatisticsResponse' smart constructor.
data GetUsageStatisticsResponse = GetUsageStatisticsResponse'
  { usageStatistics ::
      Lude.Maybe UsageStatistics,
    nextToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetUsageStatisticsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The pagination parameter to be used on the next list operation to retrieve more items.
-- * 'responseStatus' - The response status code.
-- * 'usageStatistics' - The usage statistics object. If a UsageStatisticType was provided, the objects representing other types will be null.
mkGetUsageStatisticsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetUsageStatisticsResponse
mkGetUsageStatisticsResponse pResponseStatus_ =
  GetUsageStatisticsResponse'
    { usageStatistics = Lude.Nothing,
      nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The usage statistics object. If a UsageStatisticType was provided, the objects representing other types will be null.
--
-- /Note:/ Consider using 'usageStatistics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrsUsageStatistics :: Lens.Lens' GetUsageStatisticsResponse (Lude.Maybe UsageStatistics)
gusrsUsageStatistics = Lens.lens (usageStatistics :: GetUsageStatisticsResponse -> Lude.Maybe UsageStatistics) (\s a -> s {usageStatistics = a} :: GetUsageStatisticsResponse)
{-# DEPRECATED gusrsUsageStatistics "Use generic-lens or generic-optics with 'usageStatistics' instead." #-}

-- | The pagination parameter to be used on the next list operation to retrieve more items.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrsNextToken :: Lens.Lens' GetUsageStatisticsResponse (Lude.Maybe Lude.Text)
gusrsNextToken = Lens.lens (nextToken :: GetUsageStatisticsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetUsageStatisticsResponse)
{-# DEPRECATED gusrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gusrsResponseStatus :: Lens.Lens' GetUsageStatisticsResponse Lude.Int
gusrsResponseStatus = Lens.lens (responseStatus :: GetUsageStatisticsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetUsageStatisticsResponse)
{-# DEPRECATED gusrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
