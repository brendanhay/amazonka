{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansCoverage
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Savings Plans covered for your account. This enables you to see how much of your cost is covered by a Savings Plan. An organization’s management account can see the coverage of the associated member accounts. This supports dimensions, Cost Categories, and nested expressions. For any time period, you can filter data for Savings Plans usage with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@
--
--
--     * @REGION@
--
--
--     * @SERVICE@
--
--
--     * @INSTANCE_FAMILY@
--
--
-- To determine valid values for a dimension, use the @GetDimensionValues@ operation.
module Network.AWS.CostExplorer.GetSavingsPlansCoverage
  ( -- * Creating a request
    GetSavingsPlansCoverage (..),
    mkGetSavingsPlansCoverage,

    -- ** Request lenses
    gspcGroupBy,
    gspcMetrics,
    gspcGranularity,
    gspcNextToken,
    gspcFilter,
    gspcMaxResults,
    gspcTimePeriod,

    -- * Destructuring the response
    GetSavingsPlansCoverageResponse (..),
    mkGetSavingsPlansCoverageResponse,

    -- ** Response lenses
    gspcrsNextToken,
    gspcrsResponseStatus,
    gspcrsSavingsPlansCoverages,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSavingsPlansCoverage' smart constructor.
data GetSavingsPlansCoverage = GetSavingsPlansCoverage'
  { groupBy ::
      Lude.Maybe [GroupDefinition],
    metrics :: Lude.Maybe [Lude.Text],
    granularity :: Lude.Maybe Granularity,
    nextToken :: Lude.Maybe Lude.Text,
    filter :: Lude.Maybe Expression,
    maxResults :: Lude.Maybe Lude.Natural,
    timePeriod :: DateInterval
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSavingsPlansCoverage' with the minimum fields required to make a request.
--
-- * 'filter' - Filters Savings Plans coverage data by dimensions. You can filter data for Savings Plans usage with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@
--
--
--     * @REGION@
--
--
--     * @SERVICE@
--
--
--     * @INSTANCE_FAMILY@
--
--
-- @GetSavingsPlansCoverage@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension. If there are multiple values for a dimension, they are OR'd together.
-- Cost category is also supported.
-- * 'granularity' - The granularity of the Amazon Web Services cost data for your Savings Plans. @Granularity@ can't be set if @GroupBy@ is set.
--
-- The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
-- * 'groupBy' - You can group the data using the attributes @INSTANCE_FAMILY@ , @REGION@ , or @SERVICE@ .
-- * 'maxResults' - The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
-- * 'metrics' - The measurement that you want your Savings Plans coverage reported in. The only valid value is @SpendCoveredBySavingsPlans@ .
-- * 'nextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
-- * 'timePeriod' - The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
mkGetSavingsPlansCoverage ::
  -- | 'timePeriod'
  DateInterval ->
  GetSavingsPlansCoverage
mkGetSavingsPlansCoverage pTimePeriod_ =
  GetSavingsPlansCoverage'
    { groupBy = Lude.Nothing,
      metrics = Lude.Nothing,
      granularity = Lude.Nothing,
      nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing,
      timePeriod = pTimePeriod_
    }

-- | You can group the data using the attributes @INSTANCE_FAMILY@ , @REGION@ , or @SERVICE@ .
--
-- /Note:/ Consider using 'groupBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcGroupBy :: Lens.Lens' GetSavingsPlansCoverage (Lude.Maybe [GroupDefinition])
gspcGroupBy = Lens.lens (groupBy :: GetSavingsPlansCoverage -> Lude.Maybe [GroupDefinition]) (\s a -> s {groupBy = a} :: GetSavingsPlansCoverage)
{-# DEPRECATED gspcGroupBy "Use generic-lens or generic-optics with 'groupBy' instead." #-}

-- | The measurement that you want your Savings Plans coverage reported in. The only valid value is @SpendCoveredBySavingsPlans@ .
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcMetrics :: Lens.Lens' GetSavingsPlansCoverage (Lude.Maybe [Lude.Text])
gspcMetrics = Lens.lens (metrics :: GetSavingsPlansCoverage -> Lude.Maybe [Lude.Text]) (\s a -> s {metrics = a} :: GetSavingsPlansCoverage)
{-# DEPRECATED gspcMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | The granularity of the Amazon Web Services cost data for your Savings Plans. @Granularity@ can't be set if @GroupBy@ is set.
--
-- The @GetSavingsPlansCoverage@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcGranularity :: Lens.Lens' GetSavingsPlansCoverage (Lude.Maybe Granularity)
gspcGranularity = Lens.lens (granularity :: GetSavingsPlansCoverage -> Lude.Maybe Granularity) (\s a -> s {granularity = a} :: GetSavingsPlansCoverage)
{-# DEPRECATED gspcGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcNextToken :: Lens.Lens' GetSavingsPlansCoverage (Lude.Maybe Lude.Text)
gspcNextToken = Lens.lens (nextToken :: GetSavingsPlansCoverage -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSavingsPlansCoverage)
{-# DEPRECATED gspcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters Savings Plans coverage data by dimensions. You can filter data for Savings Plans usage with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@
--
--
--     * @REGION@
--
--
--     * @SERVICE@
--
--
--     * @INSTANCE_FAMILY@
--
--
-- @GetSavingsPlansCoverage@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension. If there are multiple values for a dimension, they are OR'd together.
-- Cost category is also supported.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcFilter :: Lens.Lens' GetSavingsPlansCoverage (Lude.Maybe Expression)
gspcFilter = Lens.lens (filter :: GetSavingsPlansCoverage -> Lude.Maybe Expression) (\s a -> s {filter = a} :: GetSavingsPlansCoverage)
{-# DEPRECATED gspcFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcMaxResults :: Lens.Lens' GetSavingsPlansCoverage (Lude.Maybe Lude.Natural)
gspcMaxResults = Lens.lens (maxResults :: GetSavingsPlansCoverage -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetSavingsPlansCoverage)
{-# DEPRECATED gspcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcTimePeriod :: Lens.Lens' GetSavingsPlansCoverage DateInterval
gspcTimePeriod = Lens.lens (timePeriod :: GetSavingsPlansCoverage -> DateInterval) (\s a -> s {timePeriod = a} :: GetSavingsPlansCoverage)
{-# DEPRECATED gspcTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

instance Lude.AWSRequest GetSavingsPlansCoverage where
  type Rs GetSavingsPlansCoverage = GetSavingsPlansCoverageResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSavingsPlansCoverageResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "SavingsPlansCoverages" Lude..!@ Lude.mempty)
      )

instance Lude.ToHeaders GetSavingsPlansCoverage where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.GetSavingsPlansCoverage" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSavingsPlansCoverage where
  toJSON GetSavingsPlansCoverage' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GroupBy" Lude..=) Lude.<$> groupBy,
            ("Metrics" Lude..=) Lude.<$> metrics,
            ("Granularity" Lude..=) Lude.<$> granularity,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("TimePeriod" Lude..= timePeriod)
          ]
      )

instance Lude.ToPath GetSavingsPlansCoverage where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSavingsPlansCoverage where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSavingsPlansCoverageResponse' smart constructor.
data GetSavingsPlansCoverageResponse = GetSavingsPlansCoverageResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int,
    savingsPlansCoverages ::
      [SavingsPlansCoverage]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSavingsPlansCoverageResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
-- * 'responseStatus' - The response status code.
-- * 'savingsPlansCoverages' - The amount of spend that your Savings Plans covered.
mkGetSavingsPlansCoverageResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetSavingsPlansCoverageResponse
mkGetSavingsPlansCoverageResponse pResponseStatus_ =
  GetSavingsPlansCoverageResponse'
    { nextToken = Lude.Nothing,
      responseStatus = pResponseStatus_,
      savingsPlansCoverages = Lude.mempty
    }

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcrsNextToken :: Lens.Lens' GetSavingsPlansCoverageResponse (Lude.Maybe Lude.Text)
gspcrsNextToken = Lens.lens (nextToken :: GetSavingsPlansCoverageResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSavingsPlansCoverageResponse)
{-# DEPRECATED gspcrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcrsResponseStatus :: Lens.Lens' GetSavingsPlansCoverageResponse Lude.Int
gspcrsResponseStatus = Lens.lens (responseStatus :: GetSavingsPlansCoverageResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSavingsPlansCoverageResponse)
{-# DEPRECATED gspcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | The amount of spend that your Savings Plans covered.
--
-- /Note:/ Consider using 'savingsPlansCoverages' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspcrsSavingsPlansCoverages :: Lens.Lens' GetSavingsPlansCoverageResponse [SavingsPlansCoverage]
gspcrsSavingsPlansCoverages = Lens.lens (savingsPlansCoverages :: GetSavingsPlansCoverageResponse -> [SavingsPlansCoverage]) (\s a -> s {savingsPlansCoverages = a} :: GetSavingsPlansCoverageResponse)
{-# DEPRECATED gspcrsSavingsPlansCoverages "Use generic-lens or generic-optics with 'savingsPlansCoverages' instead." #-}
