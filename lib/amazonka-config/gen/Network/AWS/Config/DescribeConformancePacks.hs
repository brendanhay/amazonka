{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConformancePacks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of one or more conformance packs.
module Network.AWS.Config.DescribeConformancePacks
    (
    -- * Creating a request
      DescribeConformancePacks (..)
    , mkDescribeConformancePacks
    -- ** Request lenses
    , dcpConformancePackNames
    , dcpLimit
    , dcpNextToken

    -- * Destructuring the response
    , DescribeConformancePacksResponse (..)
    , mkDescribeConformancePacksResponse
    -- ** Response lenses
    , dcprrsConformancePackDetails
    , dcprrsNextToken
    , dcprrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConformancePacks' smart constructor.
data DescribeConformancePacks = DescribeConformancePacks'
  { conformancePackNames :: Core.Maybe [Types.ConformancePackName]
    -- ^ Comma-separated list of conformance pack names for which you want details. If you do not specify any names, AWS Config returns details for all your conformance packs. 
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of conformance packs returned on each page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConformancePacks' value with any optional fields omitted.
mkDescribeConformancePacks
    :: DescribeConformancePacks
mkDescribeConformancePacks
  = DescribeConformancePacks'{conformancePackNames = Core.Nothing,
                              limit = Core.Nothing, nextToken = Core.Nothing}

-- | Comma-separated list of conformance pack names for which you want details. If you do not specify any names, AWS Config returns details for all your conformance packs. 
--
-- /Note:/ Consider using 'conformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpConformancePackNames :: Lens.Lens' DescribeConformancePacks (Core.Maybe [Types.ConformancePackName])
dcpConformancePackNames = Lens.field @"conformancePackNames"
{-# INLINEABLE dcpConformancePackNames #-}
{-# DEPRECATED conformancePackNames "Use generic-lens or generic-optics with 'conformancePackNames' instead"  #-}

-- | The maximum number of conformance packs returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpLimit :: Lens.Lens' DescribeConformancePacks (Core.Maybe Core.Natural)
dcpLimit = Lens.field @"limit"
{-# INLINEABLE dcpLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpNextToken :: Lens.Lens' DescribeConformancePacks (Core.Maybe Types.NextToken)
dcpNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeConformancePacks where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeConformancePacks where
        toHeaders DescribeConformancePacks{..}
          = Core.pure
              ("X-Amz-Target", "StarlingDoveService.DescribeConformancePacks")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeConformancePacks where
        toJSON DescribeConformancePacks{..}
          = Core.object
              (Core.catMaybes
                 [("ConformancePackNames" Core..=) Core.<$> conformancePackNames,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeConformancePacks where
        type Rs DescribeConformancePacks = DescribeConformancePacksResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeConformancePacksResponse' Core.<$>
                   (x Core..:? "ConformancePackDetails") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeConformancePacksResponse' smart constructor.
data DescribeConformancePacksResponse = DescribeConformancePacksResponse'
  { conformancePackDetails :: Core.Maybe [Types.ConformancePackDetail]
    -- ^ Returns a list of @ConformancePackDetail@ objects.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeConformancePacksResponse' value with any optional fields omitted.
mkDescribeConformancePacksResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeConformancePacksResponse
mkDescribeConformancePacksResponse responseStatus
  = DescribeConformancePacksResponse'{conformancePackDetails =
                                        Core.Nothing,
                                      nextToken = Core.Nothing, responseStatus}

-- | Returns a list of @ConformancePackDetail@ objects.
--
-- /Note:/ Consider using 'conformancePackDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsConformancePackDetails :: Lens.Lens' DescribeConformancePacksResponse (Core.Maybe [Types.ConformancePackDetail])
dcprrsConformancePackDetails = Lens.field @"conformancePackDetails"
{-# INLINEABLE dcprrsConformancePackDetails #-}
{-# DEPRECATED conformancePackDetails "Use generic-lens or generic-optics with 'conformancePackDetails' instead"  #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsNextToken :: Lens.Lens' DescribeConformancePacksResponse (Core.Maybe Types.NextToken)
dcprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcprrsResponseStatus :: Lens.Lens' DescribeConformancePacksResponse Core.Int
dcprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
