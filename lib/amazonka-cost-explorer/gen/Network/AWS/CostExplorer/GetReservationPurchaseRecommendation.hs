{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets recommendations for which reservations to purchase. These recommendations could help you reduce your costs. Reservations provide a discounted hourly rate (up to 75%) compared to On-Demand pricing.
--
-- AWS generates your recommendations by identifying your On-Demand usage during a specific time period and collecting your usage into categories that are eligible for a reservation. After AWS has these categories, it simulates every combination of reservations in each category of usage to identify the best number of each type of RI to purchase to maximize your estimated savings. 
-- For example, AWS automatically aggregates your Amazon EC2 Linux, shared tenancy, and c4 family usage in the US West (Oregon) Region and recommends that you buy size-flexible regional reservations to apply to the c4 family usage. AWS recommends the smallest size instance in an instance family. This makes it easier to purchase a size-flexible RI. AWS also shows the equal number of normalized units so that you can purchase any instance size that you want. For this example, your RI recommendation would be for @c4.large@ because that is the smallest size instance in the c4 instance family.
module Network.AWS.CostExplorer.GetReservationPurchaseRecommendation
    (
    -- * Creating a request
      GetReservationPurchaseRecommendation (..)
    , mkGetReservationPurchaseRecommendation
    -- ** Request lenses
    , grprService
    , grprAccountId
    , grprAccountScope
    , grprLookbackPeriodInDays
    , grprNextPageToken
    , grprPageSize
    , grprPaymentOption
    , grprServiceSpecification
    , grprTermInYears

    -- * Destructuring the response
    , GetReservationPurchaseRecommendationResponse (..)
    , mkGetReservationPurchaseRecommendationResponse
    -- ** Response lenses
    , grprrrsMetadata
    , grprrrsNextPageToken
    , grprrrsRecommendations
    , grprrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetReservationPurchaseRecommendation' smart constructor.
data GetReservationPurchaseRecommendation = GetReservationPurchaseRecommendation'
  { service :: Types.Service
    -- ^ The specific service that you want recommendations for.
  , accountId :: Core.Maybe Types.AccountId
    -- ^ The account ID that is associated with the recommendation. 
  , accountScope :: Core.Maybe Types.AccountScope
    -- ^ The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
  , lookbackPeriodInDays :: Core.Maybe Types.LookbackPeriodInDays
    -- ^ The number of previous days that you want AWS to consider when it calculates your recommendations.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The pagination token that indicates the next set of results that you want to retrieve.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The number of recommendations that you want returned in a single response object.
  , paymentOption :: Core.Maybe Types.PaymentOption
    -- ^ The reservation purchase option that you want recommendations for.
  , serviceSpecification :: Core.Maybe Types.ServiceSpecification
    -- ^ The hardware specifications for the service instances that you want recommendations for, such as standard or convertible Amazon EC2 instances.
  , termInYears :: Core.Maybe Types.TermInYears
    -- ^ The reservation term that you want recommendations for.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReservationPurchaseRecommendation' value with any optional fields omitted.
mkGetReservationPurchaseRecommendation
    :: Types.Service -- ^ 'service'
    -> GetReservationPurchaseRecommendation
mkGetReservationPurchaseRecommendation service
  = GetReservationPurchaseRecommendation'{service,
                                          accountId = Core.Nothing, accountScope = Core.Nothing,
                                          lookbackPeriodInDays = Core.Nothing,
                                          nextPageToken = Core.Nothing, pageSize = Core.Nothing,
                                          paymentOption = Core.Nothing,
                                          serviceSpecification = Core.Nothing,
                                          termInYears = Core.Nothing}

-- | The specific service that you want recommendations for.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprService :: Lens.Lens' GetReservationPurchaseRecommendation Types.Service
grprService = Lens.field @"service"
{-# INLINEABLE grprService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | The account ID that is associated with the recommendation. 
--
-- /Note:/ Consider using 'accountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprAccountId :: Lens.Lens' GetReservationPurchaseRecommendation (Core.Maybe Types.AccountId)
grprAccountId = Lens.field @"accountId"
{-# INLINEABLE grprAccountId #-}
{-# DEPRECATED accountId "Use generic-lens or generic-optics with 'accountId' instead"  #-}

-- | The account scope that you want your recommendations for. Amazon Web Services calculates recommendations including the management account and member accounts if the value is set to @PAYER@ . If the value is @LINKED@ , recommendations are calculated for individual member accounts only.
--
-- /Note:/ Consider using 'accountScope' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprAccountScope :: Lens.Lens' GetReservationPurchaseRecommendation (Core.Maybe Types.AccountScope)
grprAccountScope = Lens.field @"accountScope"
{-# INLINEABLE grprAccountScope #-}
{-# DEPRECATED accountScope "Use generic-lens or generic-optics with 'accountScope' instead"  #-}

-- | The number of previous days that you want AWS to consider when it calculates your recommendations.
--
-- /Note:/ Consider using 'lookbackPeriodInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprLookbackPeriodInDays :: Lens.Lens' GetReservationPurchaseRecommendation (Core.Maybe Types.LookbackPeriodInDays)
grprLookbackPeriodInDays = Lens.field @"lookbackPeriodInDays"
{-# INLINEABLE grprLookbackPeriodInDays #-}
{-# DEPRECATED lookbackPeriodInDays "Use generic-lens or generic-optics with 'lookbackPeriodInDays' instead"  #-}

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprNextPageToken :: Lens.Lens' GetReservationPurchaseRecommendation (Core.Maybe Types.NextPageToken)
grprNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE grprNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The number of recommendations that you want returned in a single response object.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprPageSize :: Lens.Lens' GetReservationPurchaseRecommendation (Core.Maybe Core.Natural)
grprPageSize = Lens.field @"pageSize"
{-# INLINEABLE grprPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

-- | The reservation purchase option that you want recommendations for.
--
-- /Note:/ Consider using 'paymentOption' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprPaymentOption :: Lens.Lens' GetReservationPurchaseRecommendation (Core.Maybe Types.PaymentOption)
grprPaymentOption = Lens.field @"paymentOption"
{-# INLINEABLE grprPaymentOption #-}
{-# DEPRECATED paymentOption "Use generic-lens or generic-optics with 'paymentOption' instead"  #-}

-- | The hardware specifications for the service instances that you want recommendations for, such as standard or convertible Amazon EC2 instances.
--
-- /Note:/ Consider using 'serviceSpecification' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprServiceSpecification :: Lens.Lens' GetReservationPurchaseRecommendation (Core.Maybe Types.ServiceSpecification)
grprServiceSpecification = Lens.field @"serviceSpecification"
{-# INLINEABLE grprServiceSpecification #-}
{-# DEPRECATED serviceSpecification "Use generic-lens or generic-optics with 'serviceSpecification' instead"  #-}

-- | The reservation term that you want recommendations for.
--
-- /Note:/ Consider using 'termInYears' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprTermInYears :: Lens.Lens' GetReservationPurchaseRecommendation (Core.Maybe Types.TermInYears)
grprTermInYears = Lens.field @"termInYears"
{-# INLINEABLE grprTermInYears #-}
{-# DEPRECATED termInYears "Use generic-lens or generic-optics with 'termInYears' instead"  #-}

instance Core.ToQuery GetReservationPurchaseRecommendation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetReservationPurchaseRecommendation where
        toHeaders GetReservationPurchaseRecommendation{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSInsightsIndexService.GetReservationPurchaseRecommendation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetReservationPurchaseRecommendation where
        toJSON GetReservationPurchaseRecommendation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Service" Core..= service),
                  ("AccountId" Core..=) Core.<$> accountId,
                  ("AccountScope" Core..=) Core.<$> accountScope,
                  ("LookbackPeriodInDays" Core..=) Core.<$> lookbackPeriodInDays,
                  ("NextPageToken" Core..=) Core.<$> nextPageToken,
                  ("PageSize" Core..=) Core.<$> pageSize,
                  ("PaymentOption" Core..=) Core.<$> paymentOption,
                  ("ServiceSpecification" Core..=) Core.<$> serviceSpecification,
                  ("TermInYears" Core..=) Core.<$> termInYears])

instance Core.AWSRequest GetReservationPurchaseRecommendation where
        type Rs GetReservationPurchaseRecommendation =
             GetReservationPurchaseRecommendationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetReservationPurchaseRecommendationResponse' Core.<$>
                   (x Core..:? "Metadata") Core.<*> x Core..:? "NextPageToken"
                     Core.<*> x Core..:? "Recommendations"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetReservationPurchaseRecommendationResponse' smart constructor.
data GetReservationPurchaseRecommendationResponse = GetReservationPurchaseRecommendationResponse'
  { metadata :: Core.Maybe Types.ReservationPurchaseRecommendationMetadata
    -- ^ Information about this specific recommendation call, such as the time stamp for when Cost Explorer generated this recommendation.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The pagination token for the next set of retrievable results.
  , recommendations :: Core.Maybe [Types.ReservationPurchaseRecommendation]
    -- ^ Recommendations for reservations to purchase.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetReservationPurchaseRecommendationResponse' value with any optional fields omitted.
mkGetReservationPurchaseRecommendationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetReservationPurchaseRecommendationResponse
mkGetReservationPurchaseRecommendationResponse responseStatus
  = GetReservationPurchaseRecommendationResponse'{metadata =
                                                    Core.Nothing,
                                                  nextPageToken = Core.Nothing,
                                                  recommendations = Core.Nothing, responseStatus}

-- | Information about this specific recommendation call, such as the time stamp for when Cost Explorer generated this recommendation.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrrsMetadata :: Lens.Lens' GetReservationPurchaseRecommendationResponse (Core.Maybe Types.ReservationPurchaseRecommendationMetadata)
grprrrsMetadata = Lens.field @"metadata"
{-# INLINEABLE grprrrsMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | The pagination token for the next set of retrievable results.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrrsNextPageToken :: Lens.Lens' GetReservationPurchaseRecommendationResponse (Core.Maybe Types.NextPageToken)
grprrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE grprrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Recommendations for reservations to purchase.
--
-- /Note:/ Consider using 'recommendations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrrsRecommendations :: Lens.Lens' GetReservationPurchaseRecommendationResponse (Core.Maybe [Types.ReservationPurchaseRecommendation])
grprrrsRecommendations = Lens.field @"recommendations"
{-# INLINEABLE grprrrsRecommendations #-}
{-# DEPRECATED recommendations "Use generic-lens or generic-optics with 'recommendations' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grprrrsResponseStatus :: Lens.Lens' GetReservationPurchaseRecommendationResponse Core.Int
grprrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grprrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
