{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      GetSavingsPlansUtilizationDetails (..)
    , mkGetSavingsPlansUtilizationDetails
    -- ** Request lenses
    , gspudTimePeriod
    , gspudFilter
    , gspudMaxResults
    , gspudNextToken

    -- * Destructuring the response
    , GetSavingsPlansUtilizationDetailsResponse (..)
    , mkGetSavingsPlansUtilizationDetailsResponse
    -- ** Response lenses
    , gspudrrsSavingsPlansUtilizationDetails
    , gspudrrsTimePeriod
    , gspudrrsNextToken
    , gspudrrsTotal
    , gspudrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSavingsPlansUtilizationDetails' smart constructor.
data GetSavingsPlansUtilizationDetails = GetSavingsPlansUtilizationDetails'
  { timePeriod :: Types.DateInterval
    -- ^ The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
  , filter :: Core.Maybe Types.Expression
    -- ^ Filters Savings Plans utilization coverage data for active Savings Plans dimensions. You can filter data with the following dimensions:
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
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
  , nextToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSavingsPlansUtilizationDetails' value with any optional fields omitted.
mkGetSavingsPlansUtilizationDetails
    :: Types.DateInterval -- ^ 'timePeriod'
    -> GetSavingsPlansUtilizationDetails
mkGetSavingsPlansUtilizationDetails timePeriod
  = GetSavingsPlansUtilizationDetails'{timePeriod,
                                       filter = Core.Nothing, maxResults = Core.Nothing,
                                       nextToken = Core.Nothing}

-- | The time period that you want the usage and costs for. The @Start@ date must be within 13 months. The @End@ date must be after the @Start@ date, and before the current date. Future dates can't be used as an @End@ date.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudTimePeriod :: Lens.Lens' GetSavingsPlansUtilizationDetails Types.DateInterval
gspudTimePeriod = Lens.field @"timePeriod"
{-# INLINEABLE gspudTimePeriod #-}
{-# DEPRECATED timePeriod "Use generic-lens or generic-optics with 'timePeriod' instead"  #-}

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
gspudFilter :: Lens.Lens' GetSavingsPlansUtilizationDetails (Core.Maybe Types.Expression)
gspudFilter = Lens.field @"filter"
{-# INLINEABLE gspudFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The number of items to be returned in a response. The default is @20@ , with a minimum value of @1@ .
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudMaxResults :: Lens.Lens' GetSavingsPlansUtilizationDetails (Core.Maybe Core.Natural)
gspudMaxResults = Lens.field @"maxResults"
{-# INLINEABLE gspudMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudNextToken :: Lens.Lens' GetSavingsPlansUtilizationDetails (Core.Maybe Types.NextPageToken)
gspudNextToken = Lens.field @"nextToken"
{-# INLINEABLE gspudNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetSavingsPlansUtilizationDetails where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSavingsPlansUtilizationDetails where
        toHeaders GetSavingsPlansUtilizationDetails{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSInsightsIndexService.GetSavingsPlansUtilizationDetails")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSavingsPlansUtilizationDetails where
        toJSON GetSavingsPlansUtilizationDetails{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("TimePeriod" Core..= timePeriod),
                  ("Filter" Core..=) Core.<$> filter,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetSavingsPlansUtilizationDetails where
        type Rs GetSavingsPlansUtilizationDetails =
             GetSavingsPlansUtilizationDetailsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSavingsPlansUtilizationDetailsResponse' Core.<$>
                   (x Core..:? "SavingsPlansUtilizationDetails" Core..!= Core.mempty)
                     Core.<*> x Core..: "TimePeriod"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> x Core..:? "Total"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSavingsPlansUtilizationDetailsResponse' smart constructor.
data GetSavingsPlansUtilizationDetailsResponse = GetSavingsPlansUtilizationDetailsResponse'
  { savingsPlansUtilizationDetails :: [Types.SavingsPlansUtilizationDetail]
    -- ^ Retrieves a single daily or monthly Savings Plans utilization rate and details for your account.
  , timePeriod :: Types.DateInterval
  , nextToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
  , total :: Core.Maybe Types.SavingsPlansUtilizationAggregates
    -- ^ The total Savings Plans utilization, regardless of time period.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSavingsPlansUtilizationDetailsResponse' value with any optional fields omitted.
mkGetSavingsPlansUtilizationDetailsResponse
    :: Types.DateInterval -- ^ 'timePeriod'
    -> Core.Int -- ^ 'responseStatus'
    -> GetSavingsPlansUtilizationDetailsResponse
mkGetSavingsPlansUtilizationDetailsResponse timePeriod
  responseStatus
  = GetSavingsPlansUtilizationDetailsResponse'{savingsPlansUtilizationDetails
                                                 = Core.mempty,
                                               timePeriod, nextToken = Core.Nothing,
                                               total = Core.Nothing, responseStatus}

-- | Retrieves a single daily or monthly Savings Plans utilization rate and details for your account.
--
-- /Note:/ Consider using 'savingsPlansUtilizationDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudrrsSavingsPlansUtilizationDetails :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse [Types.SavingsPlansUtilizationDetail]
gspudrrsSavingsPlansUtilizationDetails = Lens.field @"savingsPlansUtilizationDetails"
{-# INLINEABLE gspudrrsSavingsPlansUtilizationDetails #-}
{-# DEPRECATED savingsPlansUtilizationDetails "Use generic-lens or generic-optics with 'savingsPlansUtilizationDetails' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'timePeriod' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudrrsTimePeriod :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse Types.DateInterval
gspudrrsTimePeriod = Lens.field @"timePeriod"
{-# INLINEABLE gspudrrsTimePeriod #-}
{-# DEPRECATED timePeriod "Use generic-lens or generic-optics with 'timePeriod' instead"  #-}

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudrrsNextToken :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse (Core.Maybe Types.NextPageToken)
gspudrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gspudrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The total Savings Plans utilization, regardless of time period.
--
-- /Note:/ Consider using 'total' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudrrsTotal :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse (Core.Maybe Types.SavingsPlansUtilizationAggregates)
gspudrrsTotal = Lens.field @"total"
{-# INLINEABLE gspudrrsTotal #-}
{-# DEPRECATED total "Use generic-lens or generic-optics with 'total' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspudrrsResponseStatus :: Lens.Lens' GetSavingsPlansUtilizationDetailsResponse Core.Int
gspudrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gspudrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
