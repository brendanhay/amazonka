{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.GetComplianceDetailsByResource
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the evaluation results for the specified AWS resource. The results indicate which AWS Config rules were used to evaluate the resource, when each rule was last used, and whether the resource complies with each rule.
--
-- This operation returns paginated results.
module Network.AWS.Config.GetComplianceDetailsByResource
    (
    -- * Creating a request
      GetComplianceDetailsByResource (..)
    , mkGetComplianceDetailsByResource
    -- ** Request lenses
    , gcdbrResourceType
    , gcdbrResourceId
    , gcdbrComplianceTypes
    , gcdbrNextToken

    -- * Destructuring the response
    , GetComplianceDetailsByResourceResponse (..)
    , mkGetComplianceDetailsByResourceResponse
    -- ** Response lenses
    , gcdbrrrsEvaluationResults
    , gcdbrrrsNextToken
    , gcdbrrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | 
--
-- /See:/ 'mkGetComplianceDetailsByResource' smart constructor.
data GetComplianceDetailsByResource = GetComplianceDetailsByResource'
  { resourceType :: Types.StringWithCharLimit256
    -- ^ The type of the AWS resource for which you want compliance information.
  , resourceId :: Types.BaseResourceId
    -- ^ The ID of the AWS resource for which you want compliance information.
  , complianceTypes :: Core.Maybe [Types.ComplianceType]
    -- ^ Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
  , nextToken :: Core.Maybe Core.Text
    -- ^ The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'GetComplianceDetailsByResource' value with any optional fields omitted.
mkGetComplianceDetailsByResource
    :: Types.StringWithCharLimit256 -- ^ 'resourceType'
    -> Types.BaseResourceId -- ^ 'resourceId'
    -> GetComplianceDetailsByResource
mkGetComplianceDetailsByResource resourceType resourceId
  = GetComplianceDetailsByResource'{resourceType, resourceId,
                                    complianceTypes = Core.Nothing, nextToken = Core.Nothing}

-- | The type of the AWS resource for which you want compliance information.
--
-- /Note:/ Consider using 'resourceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrResourceType :: Lens.Lens' GetComplianceDetailsByResource Types.StringWithCharLimit256
gcdbrResourceType = Lens.field @"resourceType"
{-# INLINEABLE gcdbrResourceType #-}
{-# DEPRECATED resourceType "Use generic-lens or generic-optics with 'resourceType' instead"  #-}

-- | The ID of the AWS resource for which you want compliance information.
--
-- /Note:/ Consider using 'resourceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrResourceId :: Lens.Lens' GetComplianceDetailsByResource Types.BaseResourceId
gcdbrResourceId = Lens.field @"resourceId"
{-# INLINEABLE gcdbrResourceId #-}
{-# DEPRECATED resourceId "Use generic-lens or generic-optics with 'resourceId' instead"  #-}

-- | Filters the results by compliance.
--
-- The allowed values are @COMPLIANT@ , @NON_COMPLIANT@ , and @NOT_APPLICABLE@ .
--
-- /Note:/ Consider using 'complianceTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrComplianceTypes :: Lens.Lens' GetComplianceDetailsByResource (Core.Maybe [Types.ComplianceType])
gcdbrComplianceTypes = Lens.field @"complianceTypes"
{-# INLINEABLE gcdbrComplianceTypes #-}
{-# DEPRECATED complianceTypes "Use generic-lens or generic-optics with 'complianceTypes' instead"  #-}

-- | The @nextToken@ string returned on a previous page that you use to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrNextToken :: Lens.Lens' GetComplianceDetailsByResource (Core.Maybe Core.Text)
gcdbrNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcdbrNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery GetComplianceDetailsByResource where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders GetComplianceDetailsByResource where
        toHeaders GetComplianceDetailsByResource{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.GetComplianceDetailsByResource")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON GetComplianceDetailsByResource where
        toJSON GetComplianceDetailsByResource{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ResourceType" Core..= resourceType),
                  Core.Just ("ResourceId" Core..= resourceId),
                  ("ComplianceTypes" Core..=) Core.<$> complianceTypes,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest GetComplianceDetailsByResource where
        type Rs GetComplianceDetailsByResource =
             GetComplianceDetailsByResourceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 GetComplianceDetailsByResourceResponse' Core.<$>
                   (x Core..:? "EvaluationResults") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager GetComplianceDetailsByResource where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"evaluationResults" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | 
--
-- /See:/ 'mkGetComplianceDetailsByResourceResponse' smart constructor.
data GetComplianceDetailsByResourceResponse = GetComplianceDetailsByResourceResponse'
  { evaluationResults :: Core.Maybe [Types.EvaluationResult]
    -- ^ Indicates whether the specified AWS resource complies each AWS Config rule.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The string that you use in a subsequent request to get the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'GetComplianceDetailsByResourceResponse' value with any optional fields omitted.
mkGetComplianceDetailsByResourceResponse
    :: Core.Int -- ^ 'responseStatus'
    -> GetComplianceDetailsByResourceResponse
mkGetComplianceDetailsByResourceResponse responseStatus
  = GetComplianceDetailsByResourceResponse'{evaluationResults =
                                              Core.Nothing,
                                            nextToken = Core.Nothing, responseStatus}

-- | Indicates whether the specified AWS resource complies each AWS Config rule.
--
-- /Note:/ Consider using 'evaluationResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrrrsEvaluationResults :: Lens.Lens' GetComplianceDetailsByResourceResponse (Core.Maybe [Types.EvaluationResult])
gcdbrrrsEvaluationResults = Lens.field @"evaluationResults"
{-# INLINEABLE gcdbrrrsEvaluationResults #-}
{-# DEPRECATED evaluationResults "Use generic-lens or generic-optics with 'evaluationResults' instead"  #-}

-- | The string that you use in a subsequent request to get the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrrrsNextToken :: Lens.Lens' GetComplianceDetailsByResourceResponse (Core.Maybe Core.Text)
gcdbrrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE gcdbrrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gcdbrrrsResponseStatus :: Lens.Lens' GetComplianceDetailsByResourceResponse Core.Int
gcdbrrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE gcdbrrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
