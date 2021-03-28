{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CostExplorer.GetRightsizingRecommendation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates recommendations that help you save cost by identifying idle and underutilized Amazon EC2 instances.
--
-- Recommendations are generated to either downsize or terminate instances, along with providing savings detail and metrics. For details on calculation and function, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/ce-rightsizing.html Optimizing Your Cost with Rightsizing Recommendations> in the /AWS Billing and Cost Management User Guide/ .
module Network.AWS.CostExplorer.GetRightsizingRecommendation
    (
    -- * Creating a request
      GetRightsizingRecommendation (..)
    , mkGetRightsizingRecommendation
    -- ** Request lenses
    , grrService
    , grrConfiguration
    , grrFilter
    , grrNextPageToken
    , grrPageSize

    -- * Destructuring the response
    , GetRightsizingRecommendationResponse (..)
    , mkGetRightsizingRecommendationResponse
    -- ** Response lenses
    , grrrrsConfiguration
    , grrrrsMetadata
    , grrrrsNextPageToken
    , grrrrsRightsizingRecommendations
    , grrrrsSummary
    , grrrrsResponseStatus
    ) where

import qualified Network.AWS.CostExplorer.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetRightsizingRecommendation' smart constructor.
data GetRightsizingRecommendation = GetRightsizingRecommendation'
  { service :: Types.GenericString
    -- ^ The specific service that you want recommendations for. The only valid value for @GetRightsizingRecommendation@ is "@AmazonEC2@ ".
  , configuration :: Core.Maybe Types.RightsizingRecommendationConfiguration
    -- ^ Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither. 
  , filter :: Core.Maybe Types.Expression
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The pagination token that indicates the next set of results that you want to retrieve.
  , pageSize :: Core.Maybe Core.Natural
    -- ^ The number of recommendations that you want returned in a single response object.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRightsizingRecommendation' value with any optional fields omitted.
mkGetRightsizingRecommendation
    :: Types.GenericString -- ^ 'service'
    -> GetRightsizingRecommendation
mkGetRightsizingRecommendation service
  = GetRightsizingRecommendation'{service,
                                  configuration = Core.Nothing, filter = Core.Nothing,
                                  nextPageToken = Core.Nothing, pageSize = Core.Nothing}

-- | The specific service that you want recommendations for. The only valid value for @GetRightsizingRecommendation@ is "@AmazonEC2@ ".
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrService :: Lens.Lens' GetRightsizingRecommendation Types.GenericString
grrService = Lens.field @"service"
{-# INLINEABLE grrService #-}
{-# DEPRECATED service "Use generic-lens or generic-optics with 'service' instead"  #-}

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither. 
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrConfiguration :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Types.RightsizingRecommendationConfiguration)
grrConfiguration = Lens.field @"configuration"
{-# INLINEABLE grrConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrFilter :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Types.Expression)
grrFilter = Lens.field @"filter"
{-# INLINEABLE grrFilter #-}
{-# DEPRECATED filter "Use generic-lens or generic-optics with 'filter' instead"  #-}

-- | The pagination token that indicates the next set of results that you want to retrieve.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrNextPageToken :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Types.NextPageToken)
grrNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE grrNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | The number of recommendations that you want returned in a single response object.
--
-- /Note:/ Consider using 'pageSize' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrPageSize :: Lens.Lens' GetRightsizingRecommendation (Core.Maybe Core.Natural)
grrPageSize = Lens.field @"pageSize"
{-# INLINEABLE grrPageSize #-}
{-# DEPRECATED pageSize "Use generic-lens or generic-optics with 'pageSize' instead"  #-}

instance Core.ToQuery GetRightsizingRecommendation where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetRightsizingRecommendation where
        toHeaders GetRightsizingRecommendation{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSInsightsIndexService.GetRightsizingRecommendation")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetRightsizingRecommendation where
        toJSON GetRightsizingRecommendation{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("Service" Core..= service),
                  ("Configuration" Core..=) Core.<$> configuration,
                  ("Filter" Core..=) Core.<$> filter,
                  ("NextPageToken" Core..=) Core.<$> nextPageToken,
                  ("PageSize" Core..=) Core.<$> pageSize])

instance Core.AWSRequest GetRightsizingRecommendation where
        type Rs GetRightsizingRecommendation =
             GetRightsizingRecommendationResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetRightsizingRecommendationResponse' Core.<$>
                   (x Core..:? "Configuration") Core.<*> x Core..:? "Metadata"
                     Core.<*> x Core..:? "NextPageToken"
                     Core.<*> x Core..:? "RightsizingRecommendations"
                     Core.<*> x Core..:? "Summary"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetRightsizingRecommendationResponse' smart constructor.
data GetRightsizingRecommendationResponse = GetRightsizingRecommendationResponse'
  { configuration :: Core.Maybe Types.RightsizingRecommendationConfiguration
    -- ^ Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither. 
  , metadata :: Core.Maybe Types.RightsizingRecommendationMetadata
    -- ^ Information regarding this specific recommendation set.
  , nextPageToken :: Core.Maybe Types.NextPageToken
    -- ^ The token to retrieve the next set of results.
  , rightsizingRecommendations :: Core.Maybe [Types.RightsizingRecommendation]
    -- ^ Recommendations to rightsize resources.
  , summary :: Core.Maybe Types.RightsizingRecommendationSummary
    -- ^ Summary of this recommendation set.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetRightsizingRecommendationResponse' value with any optional fields omitted.
mkGetRightsizingRecommendationResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetRightsizingRecommendationResponse
mkGetRightsizingRecommendationResponse responseStatus
  = GetRightsizingRecommendationResponse'{configuration =
                                            Core.Nothing,
                                          metadata = Core.Nothing, nextPageToken = Core.Nothing,
                                          rightsizingRecommendations = Core.Nothing,
                                          summary = Core.Nothing, responseStatus}

-- | Enables you to customize recommendations across two attributes. You can choose to view recommendations for instances within the same instance families or across different instance families. You can also choose to view your estimated savings associated with recommendations with consideration of existing Savings Plans or RI benefits, or neither. 
--
-- /Note:/ Consider using 'configuration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsConfiguration :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe Types.RightsizingRecommendationConfiguration)
grrrrsConfiguration = Lens.field @"configuration"
{-# INLINEABLE grrrrsConfiguration #-}
{-# DEPRECATED configuration "Use generic-lens or generic-optics with 'configuration' instead"  #-}

-- | Information regarding this specific recommendation set.
--
-- /Note:/ Consider using 'metadata' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsMetadata :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe Types.RightsizingRecommendationMetadata)
grrrrsMetadata = Lens.field @"metadata"
{-# INLINEABLE grrrrsMetadata #-}
{-# DEPRECATED metadata "Use generic-lens or generic-optics with 'metadata' instead"  #-}

-- | The token to retrieve the next set of results.
--
-- /Note:/ Consider using 'nextPageToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsNextPageToken :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe Types.NextPageToken)
grrrrsNextPageToken = Lens.field @"nextPageToken"
{-# INLINEABLE grrrrsNextPageToken #-}
{-# DEPRECATED nextPageToken "Use generic-lens or generic-optics with 'nextPageToken' instead"  #-}

-- | Recommendations to rightsize resources.
--
-- /Note:/ Consider using 'rightsizingRecommendations' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsRightsizingRecommendations :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe [Types.RightsizingRecommendation])
grrrrsRightsizingRecommendations = Lens.field @"rightsizingRecommendations"
{-# INLINEABLE grrrrsRightsizingRecommendations #-}
{-# DEPRECATED rightsizingRecommendations "Use generic-lens or generic-optics with 'rightsizingRecommendations' instead"  #-}

-- | Summary of this recommendation set.
--
-- /Note:/ Consider using 'summary' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsSummary :: Lens.Lens' GetRightsizingRecommendationResponse (Core.Maybe Types.RightsizingRecommendationSummary)
grrrrsSummary = Lens.field @"summary"
{-# INLINEABLE grrrrsSummary #-}
{-# DEPRECATED summary "Use generic-lens or generic-optics with 'summary' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
grrrrsResponseStatus :: Lens.Lens' GetRightsizingRecommendationResponse Core.Int
grrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE grrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
