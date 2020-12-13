{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansUtilization
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the Savings Plans utilization for your account across date ranges with daily or monthly granularity. Management account in an organization have access to member accounts. You can use @GetDimensionValues@ in @SAVINGS_PLANS@ to determine the possible dimension values.
module Network.AWS.CostExplorer.GetSavingsPlansUtilization
  ( -- * Creating a request
    GetSavingsPlansUtilization (..),
    mkGetSavingsPlansUtilization,

    -- ** Request lenses
    gspuTimePeriod,
    gspuGranularity,
    gspuFilter,

    -- * Destructuring the response
    GetSavingsPlansUtilizationResponse (..),
    mkGetSavingsPlansUtilizationResponse,

    -- ** Response lenses
    gspursSavingsPlansUtilizationsByTime,
    gspursTotal,
    gspursResponseStatus,
  )
where

import Network.AWS.CostExplorer.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetSavingsPlansUtilization' smart constructor.
data GetSavingsPlansUtilization = GetSavingsPlansUtilization'
  { -- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
    timePeriod :: DateInterval,
    -- | The granularity of the Amazon Web Services utillization data for your Savings Plans.
    --
    -- The @GetSavingsPlansUtilization@ operation supports only @DAILY@ and @MONTHLY@ granularities.
    granularity :: Lude.Maybe Granularity,
    -- | Filters Savings Plans utilization coverage data for active Savings Plans dimensions. You can filter data with the following dimensions:
    --
    --
    --     * @LINKED_ACCOUNT@
    --
    --
    --     * @SAVINGS_PLAN_ARN@
    --
    --
    --     * @SAVINGS_PLANS_TYPE@
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
    -- @GetSavingsPlansUtilization@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension.
    filter :: Lude.Maybe Expression
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSavingsPlansUtilization' with the minimum fields required to make a request.
--
-- * 'timePeriod' - The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
-- * 'granularity' - The granularity of the Amazon Web Services utillization data for your Savings Plans.
--
-- The @GetSavingsPlansUtilization@ operation supports only @DAILY@ and @MONTHLY@ granularities.
-- * 'filter' - Filters Savings Plans utilization coverage data for active Savings Plans dimensions. You can filter data with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@
--
--
--     * @SAVINGS_PLAN_ARN@
--
--
--     * @SAVINGS_PLANS_TYPE@
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
-- @GetSavingsPlansUtilization@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension.
mkGetSavingsPlansUtilization ::
  -- | 'timePeriod'
  DateInterval ->
  GetSavingsPlansUtilization
mkGetSavingsPlansUtilization pTimePeriod_ =
  GetSavingsPlansUtilization'
    { timePeriod = pTimePeriod_,
      granularity = Lude.Nothing,
      filter = Lude.Nothing
    }

-- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspuTimePeriod :: Lens.Lens' GetSavingsPlansUtilization DateInterval
gspuTimePeriod = Lens.lens (timePeriod :: GetSavingsPlansUtilization -> DateInterval) (\s a -> s {timePeriod = a} :: GetSavingsPlansUtilization)
{-# DEPRECATED gspuTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

-- | The granularity of the Amazon Web Services utillization data for your Savings Plans.
--
-- The @GetSavingsPlansUtilization@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspuGranularity :: Lens.Lens' GetSavingsPlansUtilization (Lude.Maybe Granularity)
gspuGranularity = Lens.lens (granularity :: GetSavingsPlansUtilization -> Lude.Maybe Granularity) (\s a -> s {granularity = a} :: GetSavingsPlansUtilization)
{-# DEPRECATED gspuGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

-- | Filters Savings Plans utilization coverage data for active Savings Plans dimensions. You can filter data with the following dimensions:
--
--
--     * @LINKED_ACCOUNT@
--
--
--     * @SAVINGS_PLAN_ARN@
--
--
--     * @SAVINGS_PLANS_TYPE@
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
-- @GetSavingsPlansUtilization@ uses the same <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_Expression.html Expression> object as the other operations, but only @AND@ is supported among each dimension.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspuFilter :: Lens.Lens' GetSavingsPlansUtilization (Lude.Maybe Expression)
gspuFilter = Lens.lens (filter :: GetSavingsPlansUtilization -> Lude.Maybe Expression) (\s a -> s {filter = a} :: GetSavingsPlansUtilization)
{-# DEPRECATED gspuFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

instance Lude.AWSRequest GetSavingsPlansUtilization where
  type
    Rs GetSavingsPlansUtilization =
      GetSavingsPlansUtilizationResponse
  request = Req.postJSON costExplorerService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetSavingsPlansUtilizationResponse'
            Lude.<$> (x Lude..?> "SavingsPlansUtilizationsByTime" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..:> "Total")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetSavingsPlansUtilization where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AWSInsightsIndexService.GetSavingsPlansUtilization" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetSavingsPlansUtilization where
  toJSON GetSavingsPlansUtilization' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TimePeriod" Lude..= timePeriod),
            ("Granularity" Lude..=) Lude.<$> granularity,
            ("Filter" Lude..=) Lude.<$> filter
          ]
      )

instance Lude.ToPath GetSavingsPlansUtilization where
  toPath = Lude.const "/"

instance Lude.ToQuery GetSavingsPlansUtilization where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetSavingsPlansUtilizationResponse' smart constructor.
data GetSavingsPlansUtilizationResponse = GetSavingsPlansUtilizationResponse'
  { -- | The amount of cost/commitment you used your Savings Plans. This allows you to specify date ranges.
    savingsPlansUtilizationsByTime :: Lude.Maybe [SavingsPlansUtilizationByTime],
    -- | The total amount of cost/commitment that you used your Savings Plans, regardless of date ranges.
    total :: SavingsPlansUtilizationAggregates,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetSavingsPlansUtilizationResponse' with the minimum fields required to make a request.
--
-- * 'savingsPlansUtilizationsByTime' - The amount of cost/commitment you used your Savings Plans. This allows you to specify date ranges.
-- * 'total' - The total amount of cost/commitment that you used your Savings Plans, regardless of date ranges.
-- * 'responseStatus' - The response status code.
mkGetSavingsPlansUtilizationResponse ::
  -- | 'total'
  SavingsPlansUtilizationAggregates ->
  -- | 'responseStatus'
  Lude.Int ->
  GetSavingsPlansUtilizationResponse
mkGetSavingsPlansUtilizationResponse pTotal_ pResponseStatus_ =
  GetSavingsPlansUtilizationResponse'
    { savingsPlansUtilizationsByTime =
        Lude.Nothing,
      total = pTotal_,
      responseStatus = pResponseStatus_
    }

-- | The amount of cost/commitment you used your Savings Plans. This allows you to specify date ranges.
--
-- /Note:/ Consider using 'savingsPlansUtilizationsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspursSavingsPlansUtilizationsByTime :: Lens.Lens' GetSavingsPlansUtilizationResponse (Lude.Maybe [SavingsPlansUtilizationByTime])
gspursSavingsPlansUtilizationsByTime = Lens.lens (savingsPlansUtilizationsByTime :: GetSavingsPlansUtilizationResponse -> Lude.Maybe [SavingsPlansUtilizationByTime]) (\s a -> s {savingsPlansUtilizationsByTime = a} :: GetSavingsPlansUtilizationResponse)
{-# DEPRECATED gspursSavingsPlansUtilizationsByTime "Use generic-lens or generic-optics with 'savingsPlansUtilizationsByTime' instead." #-}

-- | The total amount of cost/commitment that you used your Savings Plans, regardless of date ranges.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspursTotal :: Lens.Lens' GetSavingsPlansUtilizationResponse SavingsPlansUtilizationAggregates
gspursTotal = Lens.lens (total :: GetSavingsPlansUtilizationResponse -> SavingsPlansUtilizationAggregates) (\s a -> s {total = a} :: GetSavingsPlansUtilizationResponse)
{-# DEPRECATED gspursTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspursResponseStatus :: Lens.Lens' GetSavingsPlansUtilizationResponse Lude.Int
gspursResponseStatus = Lens.lens (responseStatus :: GetSavingsPlansUtilizationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetSavingsPlansUtilizationResponse)
{-# DEPRECATED gspursResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
