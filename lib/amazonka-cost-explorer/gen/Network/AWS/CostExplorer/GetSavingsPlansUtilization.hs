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
    gspuFilter,
    gspuGranularity,

    -- * Destructuring the response
    GetSavingsPlansUtilizationResponse (..),
    mkGetSavingsPlansUtilizationResponse,

    -- ** Response lenses
    gspurrsTotal,
    gspurrsSavingsPlansUtilizationsByTime,
    gspurrsResponseStatus,
  )
where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSavingsPlansUtilization' smart constructor.
data GetSavingsPlansUtilization = GetSavingsPlansUtilization'
  { -- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
    timePeriod :: Types.DateInterval,
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
    filter :: Core.Maybe Types.Expression,
    -- | The granularity of the Amazon Web Services utillization data for your Savings Plans.
    --
    -- The @GetSavingsPlansUtilization@ operation supports only @DAILY@ and @MONTHLY@ granularities.
    granularity :: Core.Maybe Types.Granularity
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSavingsPlansUtilization' value with any optional fields omitted.
mkGetSavingsPlansUtilization ::
  -- | 'timePeriod'
  Types.DateInterval ->
  GetSavingsPlansUtilization
mkGetSavingsPlansUtilization timePeriod =
  GetSavingsPlansUtilization'
    { timePeriod,
      filter = Core.Nothing,
      granularity = Core.Nothing
    }

-- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspuTimePeriod :: Lens.Lens' GetSavingsPlansUtilization Types.DateInterval
gspuTimePeriod = Lens.field @"timePeriod"
{-# DEPRECATED gspuTimePeriod "Use generic-lens or generic-optics with 'timePeriod' instead." #-}

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
gspuFilter :: Lens.Lens' GetSavingsPlansUtilization (Core.Maybe Types.Expression)
gspuFilter = Lens.field @"filter"
{-# DEPRECATED gspuFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The granularity of the Amazon Web Services utillization data for your Savings Plans.
--
-- The @GetSavingsPlansUtilization@ operation supports only @DAILY@ and @MONTHLY@ granularities.
--
-- /Note:/ Consider using 'granularity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspuGranularity :: Lens.Lens' GetSavingsPlansUtilization (Core.Maybe Types.Granularity)
gspuGranularity = Lens.field @"granularity"
{-# DEPRECATED gspuGranularity "Use generic-lens or generic-optics with 'granularity' instead." #-}

instance Core.FromJSON GetSavingsPlansUtilization where
  toJSON GetSavingsPlansUtilization {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TimePeriod" Core..= timePeriod),
            ("Filter" Core..=) Core.<$> filter,
            ("Granularity" Core..=) Core.<$> granularity
          ]
      )

instance Core.AWSRequest GetSavingsPlansUtilization where
  type
    Rs GetSavingsPlansUtilization =
      GetSavingsPlansUtilizationResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "X-Amz-Target",
              "AWSInsightsIndexService.GetSavingsPlansUtilization"
            )
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          GetSavingsPlansUtilizationResponse'
            Core.<$> (x Core..: "Total")
            Core.<*> (x Core..:? "SavingsPlansUtilizationsByTime")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

-- | /See:/ 'mkGetSavingsPlansUtilizationResponse' smart constructor.
data GetSavingsPlansUtilizationResponse = GetSavingsPlansUtilizationResponse'
  { -- | The total amount of cost/commitment that you used your Savings Plans, regardless of date ranges.
    total :: Types.SavingsPlansUtilizationAggregates,
    -- | The amount of cost/commitment you used your Savings Plans. This allows you to specify date ranges.
    savingsPlansUtilizationsByTime :: Core.Maybe [Types.SavingsPlansUtilizationByTime],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSavingsPlansUtilizationResponse' value with any optional fields omitted.
mkGetSavingsPlansUtilizationResponse ::
  -- | 'total'
  Types.SavingsPlansUtilizationAggregates ->
  -- | 'responseStatus'
  Core.Int ->
  GetSavingsPlansUtilizationResponse
mkGetSavingsPlansUtilizationResponse total responseStatus =
  GetSavingsPlansUtilizationResponse'
    { total,
      savingsPlansUtilizationsByTime = Core.Nothing,
      responseStatus
    }

-- | The total amount of cost/commitment that you used your Savings Plans, regardless of date ranges.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspurrsTotal :: Lens.Lens' GetSavingsPlansUtilizationResponse Types.SavingsPlansUtilizationAggregates
gspurrsTotal = Lens.field @"total"
{-# DEPRECATED gspurrsTotal "Use generic-lens or generic-optics with 'total' instead." #-}

-- | The amount of cost/commitment you used your Savings Plans. This allows you to specify date ranges.
--
-- /Note:/ Consider using 'savingsPlansUtilizationsByTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspurrsSavingsPlansUtilizationsByTime :: Lens.Lens' GetSavingsPlansUtilizationResponse (Core.Maybe [Types.SavingsPlansUtilizationByTime])
gspurrsSavingsPlansUtilizationsByTime = Lens.field @"savingsPlansUtilizationsByTime"
{-# DEPRECATED gspurrsSavingsPlansUtilizationsByTime "Use generic-lens or generic-optics with 'savingsPlansUtilizationsByTime' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspurrsResponseStatus :: Lens.Lens' GetSavingsPlansUtilizationResponse Core.Int
gspurrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED gspurrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
