{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the scalable resources in the specified scaling plan.
--
-- This operation returns paginated results.
module Network.AWS.AutoScalingPlans.DescribeScalingPlanResources
    (
    -- * Creating a request
      DescribeScalingPlanResources (..)
    , mkDescribeScalingPlanResources
    -- ** Request lenses
    , dsprScalingPlanName
    , dsprScalingPlanVersion
    , dsprMaxResults
    , dsprNextToken

    -- * Destructuring the response
    , DescribeScalingPlanResourcesResponse (..)
    , mkDescribeScalingPlanResourcesResponse
    -- ** Response lenses
    , dsprrrsNextToken
    , dsprrrsScalingPlanResources
    , dsprrrsResponseStatus
    ) where

import qualified Network.AWS.AutoScalingPlans.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeScalingPlanResources' smart constructor.
data DescribeScalingPlanResources = DescribeScalingPlanResources'
  { scalingPlanName :: Types.ScalingPlanName
    -- ^ The name of the scaling plan.
  , scalingPlanVersion :: Core.Integer
    -- ^ The version number of the scaling plan.
  , maxResults :: Core.Maybe Core.Int
    -- ^ The maximum number of scalable resources to return. The value must be between 1 and 50. The default value is 50.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingPlanResources' value with any optional fields omitted.
mkDescribeScalingPlanResources
    :: Types.ScalingPlanName -- ^ 'scalingPlanName'
    -> Core.Integer -- ^ 'scalingPlanVersion'
    -> DescribeScalingPlanResources
mkDescribeScalingPlanResources scalingPlanName scalingPlanVersion
  = DescribeScalingPlanResources'{scalingPlanName,
                                  scalingPlanVersion, maxResults = Core.Nothing,
                                  nextToken = Core.Nothing}

-- | The name of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprScalingPlanName :: Lens.Lens' DescribeScalingPlanResources Types.ScalingPlanName
dsprScalingPlanName = Lens.field @"scalingPlanName"
{-# INLINEABLE dsprScalingPlanName #-}
{-# DEPRECATED scalingPlanName "Use generic-lens or generic-optics with 'scalingPlanName' instead"  #-}

-- | The version number of the scaling plan.
--
-- /Note:/ Consider using 'scalingPlanVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprScalingPlanVersion :: Lens.Lens' DescribeScalingPlanResources Core.Integer
dsprScalingPlanVersion = Lens.field @"scalingPlanVersion"
{-# INLINEABLE dsprScalingPlanVersion #-}
{-# DEPRECATED scalingPlanVersion "Use generic-lens or generic-optics with 'scalingPlanVersion' instead"  #-}

-- | The maximum number of scalable resources to return. The value must be between 1 and 50. The default value is 50.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprMaxResults :: Lens.Lens' DescribeScalingPlanResources (Core.Maybe Core.Int)
dsprMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dsprMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprNextToken :: Lens.Lens' DescribeScalingPlanResources (Core.Maybe Types.NextToken)
dsprNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsprNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeScalingPlanResources where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeScalingPlanResources where
        toHeaders DescribeScalingPlanResources{..}
          = Core.pure
              ("X-Amz-Target",
               "AnyScaleScalingPlannerFrontendService.DescribeScalingPlanResources")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeScalingPlanResources where
        toJSON DescribeScalingPlanResources{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ScalingPlanName" Core..= scalingPlanName),
                  Core.Just ("ScalingPlanVersion" Core..= scalingPlanVersion),
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeScalingPlanResources where
        type Rs DescribeScalingPlanResources =
             DescribeScalingPlanResourcesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeScalingPlanResourcesResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "ScalingPlanResources"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeScalingPlanResources where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"scalingPlanResources" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeScalingPlanResourcesResponse' smart constructor.
data DescribeScalingPlanResourcesResponse = DescribeScalingPlanResourcesResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token required to get the next set of results. This value is @null@ if there are no more results to return.
  , scalingPlanResources :: Core.Maybe [Types.ScalingPlanResource]
    -- ^ Information about the scalable resources.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeScalingPlanResourcesResponse' value with any optional fields omitted.
mkDescribeScalingPlanResourcesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeScalingPlanResourcesResponse
mkDescribeScalingPlanResourcesResponse responseStatus
  = DescribeScalingPlanResourcesResponse'{nextToken = Core.Nothing,
                                          scalingPlanResources = Core.Nothing, responseStatus}

-- | The token required to get the next set of results. This value is @null@ if there are no more results to return.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrrsNextToken :: Lens.Lens' DescribeScalingPlanResourcesResponse (Core.Maybe Types.NextToken)
dsprrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dsprrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the scalable resources.
--
-- /Note:/ Consider using 'scalingPlanResources' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrrsScalingPlanResources :: Lens.Lens' DescribeScalingPlanResourcesResponse (Core.Maybe [Types.ScalingPlanResource])
dsprrrsScalingPlanResources = Lens.field @"scalingPlanResources"
{-# INLINEABLE dsprrrsScalingPlanResources #-}
{-# DEPRECATED scalingPlanResources "Use generic-lens or generic-optics with 'scalingPlanResources' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsprrrsResponseStatus :: Lens.Lens' DescribeScalingPlanResourcesResponse Core.Int
dsprrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dsprrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
