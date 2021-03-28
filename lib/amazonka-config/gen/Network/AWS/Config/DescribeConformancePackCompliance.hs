{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConformancePackCompliance
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns compliance details for each rule in that conformance pack.
module Network.AWS.Config.DescribeConformancePackCompliance
    (
    -- * Creating a request
      DescribeConformancePackCompliance (..)
    , mkDescribeConformancePackCompliance
    -- ** Request lenses
    , dcpcConformancePackName
    , dcpcFilters
    , dcpcLimit
    , dcpcNextToken

    -- * Destructuring the response
    , DescribeConformancePackComplianceResponse (..)
    , mkDescribeConformancePackComplianceResponse
    -- ** Response lenses
    , dcpcrrsConformancePackName
    , dcpcrrsConformancePackRuleComplianceList
    , dcpcrrsNextToken
    , dcpcrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConformancePackCompliance' smart constructor.
data DescribeConformancePackCompliance = DescribeConformancePackCompliance'
  { conformancePackName :: Types.ConformancePackName
    -- ^ Name of the conformance pack.
  , filters :: Core.Maybe Types.ConformancePackComplianceFilters
    -- ^ A @ConformancePackComplianceFilters@ object.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of AWS Config rules within a conformance pack are returned on each page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConformancePackCompliance' value with any optional fields omitted.
mkDescribeConformancePackCompliance
    :: Types.ConformancePackName -- ^ 'conformancePackName'
    -> DescribeConformancePackCompliance
mkDescribeConformancePackCompliance conformancePackName
  = DescribeConformancePackCompliance'{conformancePackName,
                                       filters = Core.Nothing, limit = Core.Nothing,
                                       nextToken = Core.Nothing}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcConformancePackName :: Lens.Lens' DescribeConformancePackCompliance Types.ConformancePackName
dcpcConformancePackName = Lens.field @"conformancePackName"
{-# INLINEABLE dcpcConformancePackName #-}
{-# DEPRECATED conformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead"  #-}

-- | A @ConformancePackComplianceFilters@ object.
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcFilters :: Lens.Lens' DescribeConformancePackCompliance (Core.Maybe Types.ConformancePackComplianceFilters)
dcpcFilters = Lens.field @"filters"
{-# INLINEABLE dcpcFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of AWS Config rules within a conformance pack are returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcLimit :: Lens.Lens' DescribeConformancePackCompliance (Core.Maybe Core.Natural)
dcpcLimit = Lens.field @"limit"
{-# INLINEABLE dcpcLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcNextToken :: Lens.Lens' DescribeConformancePackCompliance (Core.Maybe Types.NextToken)
dcpcNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcpcNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeConformancePackCompliance where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeConformancePackCompliance where
        toHeaders DescribeConformancePackCompliance{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeConformancePackCompliance")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeConformancePackCompliance where
        toJSON DescribeConformancePackCompliance{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ConformancePackName" Core..= conformancePackName),
                  ("Filters" Core..=) Core.<$> filters,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeConformancePackCompliance where
        type Rs DescribeConformancePackCompliance =
             DescribeConformancePackComplianceResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeConformancePackComplianceResponse' Core.<$>
                   (x Core..: "ConformancePackName") Core.<*>
                     x Core..:? "ConformancePackRuleComplianceList" Core..!= Core.mempty
                     Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeConformancePackComplianceResponse' smart constructor.
data DescribeConformancePackComplianceResponse = DescribeConformancePackComplianceResponse'
  { conformancePackName :: Types.ConformancePackName
    -- ^ Name of the conformance pack.
  , conformancePackRuleComplianceList :: [Types.ConformancePackRuleCompliance]
    -- ^ Returns a list of @ConformancePackRuleCompliance@ objects.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConformancePackComplianceResponse' value with any optional fields omitted.
mkDescribeConformancePackComplianceResponse
    :: Types.ConformancePackName -- ^ 'conformancePackName'
    -> Core.Int -- ^ 'responseStatus'
    -> DescribeConformancePackComplianceResponse
mkDescribeConformancePackComplianceResponse conformancePackName
  responseStatus
  = DescribeConformancePackComplianceResponse'{conformancePackName,
                                               conformancePackRuleComplianceList = Core.mempty,
                                               nextToken = Core.Nothing, responseStatus}

-- | Name of the conformance pack.
--
-- /Note:/ Consider using 'conformancePackName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcrrsConformancePackName :: Lens.Lens' DescribeConformancePackComplianceResponse Types.ConformancePackName
dcpcrrsConformancePackName = Lens.field @"conformancePackName"
{-# INLINEABLE dcpcrrsConformancePackName #-}
{-# DEPRECATED conformancePackName "Use generic-lens or generic-optics with 'conformancePackName' instead"  #-}

-- | Returns a list of @ConformancePackRuleCompliance@ objects.
--
-- /Note:/ Consider using 'conformancePackRuleComplianceList' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcrrsConformancePackRuleComplianceList :: Lens.Lens' DescribeConformancePackComplianceResponse [Types.ConformancePackRuleCompliance]
dcpcrrsConformancePackRuleComplianceList = Lens.field @"conformancePackRuleComplianceList"
{-# INLINEABLE dcpcrrsConformancePackRuleComplianceList #-}
{-# DEPRECATED conformancePackRuleComplianceList "Use generic-lens or generic-optics with 'conformancePackRuleComplianceList' instead"  #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcrrsNextToken :: Lens.Lens' DescribeConformancePackComplianceResponse (Core.Maybe Types.NextToken)
dcpcrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcpcrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpcrrsResponseStatus :: Lens.Lens' DescribeConformancePackComplianceResponse Core.Int
dcpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
