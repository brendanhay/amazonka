{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves attribute data along with aggregate utilization and savings data for a given time period. This doesn't support granular or grouped data (daily/monthly) in response. You can't retrieve data by dates in a single response similar to @GetSavingsPlanUtilization@ , but you have the option to make multiple calls to @GetSavingsPlanUtilizationDetails@ by providing individual dates. You can use @GetDimensionValues@ in @SAVINGS_PLANS@ to determine the possible dimension values.
module Network.AWS.CostExplorer.GetSavingsPlansUtilizationDetails
  ( -- * Creating a request
    GetSavingsPlansUtilizationDetails (..),
    mkGetSavingsPlansUtilizationDetails,

    -- ** Request lenses
    gspudNextToken,
    gspudFilter,
    gspudMaxResults,
    gspudTimePeriod,

    -- * Destructuring the response
    GetSavingsPlansUtilizationDetailsResponse (..),
    mkGetSavingsPlansUtilizationDetailsResponse,

    -- ** Response lenses
    gspudrsNextToken,
    gspudrsTotal,
    gspudrsResponseStatus,
    gspudrsSavingsPlansUtilizationDetails,
    gspudrsTimePeriod,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSavingsPlansUtilizationDetails' smart constructor.
data GetSavingsPlansUtilizationDetails = GetSavingsPlansUtilizationDetails'
  { nextToken ::
      Lude.Maybe Lude.Text,
    filter ::
      Lude.Maybe Expression,
    maxResults ::
      Lude.Maybe Lude.Natural,
    timePeriod ::
      DateInterval
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSavingsPlansUtilizationDetails' with the minimum fields required to make a request.
--
-- * 'filter' - Filters Savings Plans utilization coverage data for active Savings Plans dimensions. You can filter data with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@
--
--
--     * @SAVINGS_PLAN_ARN@
--
--
--     * @REGION@
--
--
--     * @PAYMENT_OPTION@
--
--
--     * @INSTANCE_TYPE_FAMILY@
--
--
-- @GetSavingsPlansUtilizationDetails@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension.
-- * 'maxResults' - The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
-- * 'nextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
-- * 'timePeriod' - The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
mkGetSavingsPlansUtilizationDetails ::
  -- | 'timePeriod'
  DateInterval ->
  GetSavingsPlansUtilizationDetails
mkGetSavingsPlansUtilizationDetails pTimePeriod_ =
  GetSavingsPlansUtilizationDetails'
    { nextToken = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing,
      timePeriod = pTimePeriod_
    }

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudNextToken :: Lens.Lens' GetSavingsPlansUtilizationDetails (Lude.Maybe Lude.Text)
gspudNextToken = Lens.lens (nextToken :: GetSavingsPlansUtilizationDetails -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSavingsPlansUtilizationDetails)
{-# DEPRECATED gspudNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Filters Savings Plans utilization coverage data for active Savings Plans dimensions. You can filter data with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@
--
--
--     * @SAVINGS_PLAN_ARN@
--
--
--     * @REGION@
--
--
--     * @PAYMENT_OPTION@
--
--
--     * @INSTANCE_TYPE_FAMILY@
--
--
-- @GetSavingsPlansUtilizationDetails@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudFilter :: Lens.Lens' GetSavingsPlansUtilizationDetails (Lude.Maybe Expression)
gspudFilter = Lens.lens (filter :: GetSavingsPlansUtilizationDetails -> Lude.Maybe Expression) (\s a -> s {filter = a} :: GetSavingsPlansUtilizationDetails)
{-# DEPRECATED gspudFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudMaxResults :: Lens.Lens' GetSavingsPlansUtilizationDetails (Lude.Maybe Lude.Natural)
gspudMaxResults = Lens.lens (maxResults :: GetSavingsPlansUtilizationDetails -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetSavingsPlansUtilizationDetails)
{-# DEPRECATED gspudMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudTimePeriod :: Lens.Lens' GetSavingsPlansUtilizationDetails DateInterval
gspudTimePeriod = Lens.lens (timePeriod :: GetSavingsPlansUtilizationDetails -> DateInterval) (\s a -> s {timePeriod = a} :: GetSavingsPlansUtilizationDetails)
{-# DEPRECATED gspudTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

instance Lude.AWSRequest GetSavingsPlansUtilizationDetails where
  type
    Rs GetSavingsPlansUtilizationDetails =
      GetSavingsPlansUtilizationDetailsResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSavingsPlansUtilizationDetailsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "Total")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
            Lude.<*> (x Lude..?> "SavingsPlansUtilizationDetails" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "TimePeriod")
      )

instance Lude.ToHeaders GetSavingsPlansUtilizationDetails where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.GetSavingsPlansUtilizationDetails" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSavingsPlansUtilizationDetails where
  toJSON GetSavingsPlansUtilizationDetails' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("TimePeriod" Lude..= timePeriod)
          ]
      )

instance Lude.ToPath GetSavingsPlansUtilizationDetails where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSavingsPlansUtilizationDetails where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSavingsPlansUtilizationDetailsResponse' smart constructor.
data GetSavingsPlansUtilizationDetailsResponse = GetSavingsPlansUtilizationDetailsResponse'
  { nextToken ::
      Lude.Maybe
        Lude.Text,
    total ::
      Lude.Maybe
        SavingsPlansUtilizationAggregates,
    responseStatus ::
      Lude.Int,
    savingsPlansUtilizationDetails ::
      [SavingsPlansUtilizationDetail],
    timePeriod ::
      DateInterval
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSavingsPlansUtilizationDetailsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
-- * 'responseStatus' - The response status code.
-- * 'savingsPlansUtilizationDetails' - Retrieves a single daily or monthly Savings Plans utilization rate and details for your account.
-- * 'timePeriod' - Undocumented field.
-- * 'total' - The total Savings Plans utilization, regardless of time period.
mkGetSavingsPlansUtilizationDetailsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'timePeriod'
  DateInterval ->
  GetSavingsPlansUtilizationDetailsResponse
mkGetSavingsPlansUtilizationDetailsResponse
  pResponseStatus_
  pTimePeriod_ =
    GetSavingsPlansUtilizationDetailsResponse'
      { nextToken =
          Lude.Nothing,
        total = Lude.Nothing,
        responseStatus = pResponseStatus_,
        savingsPlansUtilizationDetails = Lude.mempty,
        timePeriod = pTimePeriod_
      }

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudrsNextToken :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse (Lude.Maybe Lude.Text)
gspudrsNextToken = Lens.lens (nextToken :: GetSavingsPlansUtilizationDetailsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetSavingsPlansUtilizationDetailsResponse)
{-# DEPRECATED gspudrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The total Savings Plans utilization, regardless of time period.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudrsTotal :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse (Lude.Maybe SavingsPlansUtilizationAggregates)
gspudrsTotal = Lens.lens (total :: GetSavingsPlansUtilizationDetailsResponse -> Lude.Maybe SavingsPlansUtilizationAggregates) (\s a -> s {total = a} :: GetSavingsPlansUtilizationDetailsResponse)
{-# DEPRECATED gspudrsTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudrsResponseStatus :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse Lude.Int
gspudrsResponseStatus = Lens.lens (responseStatus :: GetSavingsPlansUtilizationDetailsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSavingsPlansUtilizationDetailsResponse)
{-# DEPRECATED gspudrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | Retrieves a single daily or monthly Savings Plans utilization rate and details for your account.
--
-- /Note:/ Consider using 'savingsPlansUtilizationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudrsSavingsPlansUtilizationDetails :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse [SavingsPlansUtilizationDetail]
gspudrsSavingsPlansUtilizationDetails = Lens.lens (savingsPlansUtilizationDetails :: GetSavingsPlansUtilizationDetailsResponse -> [SavingsPlansUtilizationDetail]) (\s a -> s {savingsPlansUtilizationDetails = a} :: GetSavingsPlansUtilizationDetailsResponse)
{-# DEPRECATED gspudrsSavingsPlansUtilizationDetails "Use generic-lens or generic-optics with 'savingsPlansUtilizationDetails' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudrsTimePeriod :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse DateInterval
gspudrsTimePeriod = Lens.lens (timePeriod :: GetSavingsPlansUtilizationDetailsResponse -> DateInterval) (\s a -> s {timePeriod = a} :: GetSavingsPlansUtilizationDetailsResponse)
{-# DEPRECATED gspudrsTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}
