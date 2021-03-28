{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves your request parameters, Savings Plan Recommendations Summary and Details. 
module Network.AWS.CostExplorer.GetSavingsPlansPurchaseRecommendation
    (
    -- * Creating a request
      GetSavingsPlansPurchaseRecommendation (..)
    , mkGetSavingsPlansPurchaseRecommendation
    -- ** Request lenses
    , gspprSavingsPlansType
    , gspprTermInYears
    , gspprPaymentOption
    , gspprLookbackPeriodInDays
    , gspprAccountScope
    , gspprFilter
    , gspprNextPageToken
    , gspprPageSize

    -- * Destructuring the response
    , GetSavingsPlansPurchaseRecommendationResponse (..)
    , mkGetSavingsPlansPurchaseRecommendationResponse
    -- ** Response lenses
    , gspprrrsMetadata
    , gspprrrsNextPageToken
    , gspprrrsSavingsPlansPurchaseRecommendation
    , gspprrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetSavingsPlansPurchaseRecommendation' smart constructor.
data GetSavingsPlansPurchaseRecommendation = GetSavingsPlansPurchaseRecommendation'
  { savingsPlansType :: Types.SupportedSavingsPlansType
    -- ^ The Savings Plans recommendation type requested.
  , termInYears :: Types.TermInYears
    -- ^ The savings plan recommendation term used to generate these recommendations.
  , paymentOption :: Types.PaymentOption
    -- ^ The payment option used to generate these recommendations.
  , lookbackPeriodInDays :: Types.LookbackPeriodInDays
    -- ^ The lookback period used to generate the recommendation.
  , accountScope :: Core.Maybe Types.AccountScope
    -- ^ The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
  , filter :: Core.Maybe Types.Expression
    -- ^ You can filter your recommendations by Account ID with the @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated Acount ID(s) for which you want to see Savings Plans purchase recommendations.
--
-- For GetSavingsPlansPurchaseRecommendation, the @Filter@ does not include @CostCategories@ or @Tags@ . It only includes @Dimensions@ . With @Dimensions@ , @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single Account ID or multiple comma-separated Account IDs for which you want to see Savings Plans Purchase Recommendations. @AND@ and @OR@ operators are not supported.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The number of recommendations that you want returned in a single response object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSavingsPlansPurchaseRecommendation' value with any optional fields omitted.
mkGetSavingsPlansPurchaseRecommendation
    :: Types.SupportedSavingsPlansType -- ^ 'savingsPlansType'
    -> Types.TermInYears -- ^ 'termInYears'
    -> Types.PaymentOption -- ^ 'paymentOption'
    -> Types.LookbackPeriodInDays -- ^ 'lookbackPeriodInDays'
    -> GetSavingsPlansPurchaseRecommendation
mkGetSavingsPlansPurchaseRecommendation savingsPlansType
  termInYears paymentOption lookbackPeriodInDays
  = GetSavingsPlansPurchaseRecommendation'{savingsPlansType,
                                           termInYears, paymentOption, lookbackPeriodInDays,
                                           accountScope = Core.Nothing, filter = Core.Nothing,
                                           nextPageToken = Core.Nothing, pageSize = Core.Nothing}

-- | The Savings Plans recommendation type requested.
--
-- /Note:/ Consider using 'savingsPlansType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprSavingsPlansType :: Lens.Lens' GetSavingsPlansPurchaseRecommendation Types.SupportedSavingsPlansType
gspprSavingsPlansType = Lens.field @"savingsPlansType"
{-# INLINEABLE gspprSavingsPlansType #-}
{-# DEPRECATED savingsPlansType "Use generic-lens or generic-optics with 'savingsPlansType' instead"  #-}

-- | The savings plan recommendation term used to generate these recommendations.
--
-- /Note:/ Consider using 'termInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprTermInYears :: Lens.Lens' GetSavingsPlansPurchaseRecommendation Types.TermInYears
gspprTermInYears = Lens.field @"termInYears"
{-# INLINEABLE gspprTermInYears #-}
{-# DEPRECATED termInYears "Use generic-lens or generic-optics with 'termInYears' instead"  #-}

-- | The payment option used to generate these recommendations.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprPaymentOption :: Lens.Lens' GetSavingsPlansPurchaseRecommendation Types.PaymentOption
gspprPaymentOption = Lens.field @"paymentOption"
{-# INLINEABLE gspprPaymentOption #-}
{-# DEPRECATED paymentOption "Use generic-lens or generic-optics with 'paymentOption' instead"  #-}

-- | The lookback period used to generate the recommendation.
--
-- /Note:/ Consider using 'lookbackPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprLookbackPeriodInDays :: Lens.Lens' GetSavingsPlansPurchaseRecommendation Types.LookbackPeriodInDays
gspprLookbackPeriodInDays = Lens.field @"lookbackPeriodInDays"
{-# INLINEABLE gspprLookbackPeriodInDays #-}
{-# DEPRECATED lookbackPeriodInDays "Use generic-lens or generic-optics with 'lookbackPeriodInDays' instead"  #-}

-- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
--
-- /Note:/ Consider using 'accountScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprAccountScope :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Core.Maybe Types.AccountScope)
gspprAccountScope = Lens.field @"accountScope"
{-# INLINEABLE gspprAccountScope #-}
{-# DEPRECATED accountScope "Use generic-lens or generic-optics with 'accountScope' instead"  #-}

-- | You can filter your recommendations by Account ID with the @LINKED_ACCOUNT@ dimension. To filter your recommendations by Account ID, specify @Key@ as @LINKED_ACCOUNT@ and @Value@ as the comma-separated Acount ID(s) for which you want to see Savings Plans purchase recommendations.
--
-- For GetSavingsPlansPurchaseRecommendation, the @Filter@ does not include @CostCategories@ or @Tags@ . It only includes @Dimensions@ . With @Dimensions@ , @Key@ must be @LINKED_ACCOUNT@ and @Value@ can be a single Account ID or multiple comma-separated Account IDs for which you want to see Savings Plans Purchase Recommendations. @AND@ and @OR@ operators are not supported.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprFilter :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Core.Maybe Types.Expression)
gspprFilter = Lens.field @"filter"
{-# INLINEABLE gspprFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The token to retrieve the next set of results. Amazon Web Services provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprNextPageToken :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Core.Maybe Types.NextPageToken)
gspprNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gspprNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The number of recommendations that you want returned in a single response object.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprPageSize :: Lens.Lens' GetSavingsPlansPurchaseRecommendation (Core.Maybe Core.Natural)
gspprPageSize = Lens.field @"pageSize"
{-# INLINEABLE gspprPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

instance Core.ToQuery GetSavingsPlansPurchaseRecommendation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetSavingsPlansPurchaseRecommendation where
        toHeaders GetSavingsPlansPurchaseRecommendation{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSInsightsIndexService.GetSavingsPlansPurchaseRecommendation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetSavingsPlansPurchaseRecommendation where
        toJSON GetSavingsPlansPurchaseRecommendation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("SavingsPlansType" Core..= savingsPlansType),
                  Core.Just ("TermInYears" Core..= termInYears),
                  Core.Just ("PaymentOption" Core..= paymentOption),
                  Core.Just ("LookbackPeriodInDays" Core..= lookbackPeriodInDays),
                  ("AccountScope" Core..=) Core.<$> accountScope,
                  ("Filter" Core..=) Core.<$> filter,
                  ("NextPageToken" Core..=) Core.<$> nextPageToken,
                  ("PageSize" Core..=) Core.<$> pageSize])

instance Core.AWSRequest GetSavingsPlansPurchaseRecommendation
         where
        type Rs GetSavingsPlansPurchaseRecommendation =
             GetSavingsPlansPurchaseRecommendationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetSavingsPlansPurchaseRecommendationResponse' Core.<$>
                   (x Core..:? "Metadata") Core.<*> x Core..:? "NextPageToken"
                     Core.<*> x Core..:? "SavingsPlansPurchaseRecommendation"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetSavingsPlansPurchaseRecommendationResponse' smart constructor.
data GetSavingsPlansPurchaseRecommendationResponse = GetSavingsPlansPurchaseRecommendationResponse'
  { metadata :: Core.Maybe Types.SavingsPlansPurchaseRecommendationMetadata
    -- ^ Information regarding this specific recommendation set.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
  , savingsPlansPurchaseRecommendation :: Core.Maybe Types.SavingsPlansPurchaseRecommendation
    -- ^ Contains your request parameters, Savings Plan Recommendations Summary, and Details.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetSavingsPlansPurchaseRecommendationResponse' value with any optional fields omitted.
mkGetSavingsPlansPurchaseRecommendationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetSavingsPlansPurchaseRecommendationResponse
mkGetSavingsPlansPurchaseRecommendationResponse responseStatus
  = GetSavingsPlansPurchaseRecommendationResponse'{metadata =
                                                     Core.Nothing,
                                                   nextPageToken = Core.Nothing,
                                                   savingsPlansPurchaseRecommendation =
                                                     Core.Nothing,
                                                   responseStatus}

-- | Information regarding this specific recommendation set.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprrrsMetadata :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Core.Maybe Types.SavingsPlansPurchaseRecommendationMetadata)
gspprrrsMetadata = Lens.field @"metadata"
{-# INLINEABLE gspprrrsMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | The token for the next set of retrievable results. AWS provides the token when the response from a previous call has more results than the maximum page size.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprrrsNextPageToken :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Core.Maybe Types.NextPageToken)
gspprrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE gspprrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Contains your request parameters, Savings Plan Recommendations Summary, and Details.
--
-- /Note:/ Consider using 'savingsPlansPurchaseRecommendation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprrrsSavingsPlansPurchaseRecommendation :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse (Core.Maybe Types.SavingsPlansPurchaseRecommendation)
gspprrrsSavingsPlansPurchaseRecommendation = Lens.field @"savingsPlansPurchaseRecommendation"
{-# INLINEABLE gspprrrsSavingsPlansPurchaseRecommendation #-}
{-# DEPRECATED savingsPlansPurchaseRecommendation "Use generic-lens or generic-optics with 'savingsPlansPurchaseRecommendation' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gspprrrsResponseStatus :: Lens.Lens' GetSavingsPlansPurchaseRecommendationResponse Core.Int
gspprrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gspprrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
