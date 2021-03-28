{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetConformancePackComplianceDetails
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details of a conformance pack for all AWS resources that are monitered by conformance pack.
module Network.AWS.Config.GetConformancePackComplianceDetails
    (
    -- * Creating a request
      GetConformancePackComplianceDetails (..)
    , mkGetConformancePackComplianceDetails
    -- ** Request lenses
    , gcpcdConformancePackName
    , gcpcdFilters
    , gcpcdLimit
    , gcpcdNextToken

    -- * Destructuring the response
    , GetConformancePackComplianceDetailsResponse (..)
    , mkGetConformancePackComplianceDetailsResponse
    -- ** Response lenses
    , gcpcdrrsConformancePackName
    , gcpcdrrsConformancePackRuleEvaluationResults
    , gcpcdrrsNextToken
    , gcpcdrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkGetConformancePackComplianceDetails' smart constructor.
data GetConformancePackComplianceDetails = GetConformancePackComplianceDetails'
  { conformancePackName :: Types.ConformancePackName
    -- ^ Name of the conformance pack.
  , filters :: Core.Maybe Types.ConformancePackEvaluationFilters
    -- ^ A @ConformancePackEvaluationFilters@ object.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of evaluation results returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetConformancePackComplianceDetails' value with any optional fields omitted.
mkGetConformancePackComplianceDetails
    :: Types.ConformancePackName -- ^ 'conformancePackName'
    -> GetConformancePackComplianceDetails
mkGetConformancePackComplianceDetails conformancePackName
  = GetConformancePackComplianceDetails'{conformancePackName,
                                         filters = Core.Nothing, limit = Core.Nothing,
                                         nextToken = Core.Nothing}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdConformancePackName :: Lens.Lens' GetConformancePackComplianceDetails Types.ConformancePackName
gcpcdConformancePackName = Lens.field @"conformancePackName"
{-# INLINEABLE gcpcdConformancePackName #-}
{-# DEPRECATED conformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead"  #-}

-- | A @ConformancePackEvaluationFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdFilters :: Lens.Lens' GetConformancePackComplianceDetails (Core.Maybe Types.ConformancePackEvaluationFilters)
gcpcdFilters = Lens.field @"filters"
{-# INLINEABLE gcpcdFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of evaluation results returned on each page. If you do no specify a number, AWS Config uses the default. The default is 100.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdLimit :: Lens.Lens' GetConformancePackComplianceDetails (Core.Maybe Core.Natural)
gcpcdLimit = Lens.field @"limit"
{-# INLINEABLE gcpcdLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdNextToken :: Lens.Lens' GetConformancePackComplianceDetails (Core.Maybe Types.NextToken)
gcpcdNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcpcdNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetConformancePackComplianceDetails where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetConformancePackComplianceDetails where
        toHeaders GetConformancePackComplianceDetails{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.GetConformancePackComplianceDetails")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetConformancePackComplianceDetails where
        toJSON GetConformancePackComplianceDetails{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConformancePackName" Core..= conformancePackName),
                  ("Filters" Core..=) Core.<$> filters,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetConformancePackComplianceDetails where
        type Rs GetConformancePackComplianceDetails =
             GetConformancePackComplianceDetailsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetConformancePackComplianceDetailsResponse' Core.<$>
                   (x Core..: "ConformancePackName") Core.<*>
                     x Core..:? "ConformancePackRuleEvaluationResults"
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkGetConformancePackComplianceDetailsResponse' smart constructor.
data GetConformancePackComplianceDetailsResponse = GetConformancePackComplianceDetailsResponse'
  { conformancePackName :: Types.ConformancePackName
    -- ^ Name of the conformance pack.
  , conformancePackRuleEvaluationResults :: Core.Maybe [Types.ConformancePackEvaluationResult]
    -- ^ Returns a list of @ConformancePackEvaluationResult@ objects.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetConformancePackComplianceDetailsResponse' value with any optional fields omitted.
mkGetConformancePackComplianceDetailsResponse
    :: Types.ConformancePackName -- ^ 'conformancePackName'
    -> Core.Int -- ^ 'responseStatus'
    -> GetConformancePackComplianceDetailsResponse
mkGetConformancePackComplianceDetailsResponse conformancePackName
  responseStatus
  = GetConformancePackComplianceDetailsResponse'{conformancePackName,
                                                 conformancePackRuleEvaluationResults =
                                                   Core.Nothing,
                                                 nextToken = Core.Nothing, responseStatus}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrrsConformancePackName :: Lens.Lens' GetConformancePackComplianceDetailsResponse Types.ConformancePackName
gcpcdrrsConformancePackName = Lens.field @"conformancePackName"
{-# INLINEABLE gcpcdrrsConformancePackName #-}
{-# DEPRECATED conformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead"  #-}

-- | Returns a list of @ConformancePackEvaluationResult@ objects.
--
-- /Note:/ Consider using 'conformancePackRuleEvaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrrsConformancePackRuleEvaluationResults :: Lens.Lens' GetConformancePackComplianceDetailsResponse (Core.Maybe [Types.ConformancePackEvaluationResult])
gcpcdrrsConformancePackRuleEvaluationResults = Lens.field @"conformancePackRuleEvaluationResults"
{-# INLINEABLE gcpcdrrsConformancePackRuleEvaluationResults #-}
{-# DEPRECATED conformancePackRuleEvaluationResults "Use generic-lens or generic-optics with 'conformancePackRuleEvaluationResults' instead"  #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrrsNextToken :: Lens.Lens' GetConformancePackComplianceDetailsResponse (Core.Maybe Types.NextToken)
gcpcdrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcpcdrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcpcdrrsResponseStatus :: Lens.Lens' GetConformancePackComplianceDetailsResponse Core.Int
gcpcdrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcpcdrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
