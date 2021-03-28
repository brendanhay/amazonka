{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Config.DescribeConformancePackStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides one or more conformance packs deployment status.
module Network.AWS.Config.DescribeConformancePackStatus
    (
    -- * Creating a request
      DescribeConformancePackStatus (..)
    , mkDescribeConformancePackStatus
    -- ** Request lenses
    , dcpsConformancePackNames
    , dcpsLimit
    , dcpsNextToken

    -- * Destructuring the response
    , DescribeConformancePackStatusResponse (..)
    , mkDescribeConformancePackStatusResponse
    -- ** Response lenses
    , dcpsrrsConformancePackStatusDetails
    , dcpsrrsNextToken
    , dcpsrrsResponseStatus
    ) where

import qualified Network.AWS.Config.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeConformancePackStatus' smart constructor.
data DescribeConformancePackStatus = DescribeConformancePackStatus'
  { conformancePackNames :: Core.Maybe [Types.ConformancePackName]
    -- ^ Comma-separated list of conformance pack names.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of conformance packs status returned on each page.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeConformancePackStatus' value with any optional fields omitted.
mkDescribeConformancePackStatus
    :: DescribeConformancePackStatus
mkDescribeConformancePackStatus
  = DescribeConformancePackStatus'{conformancePackNames =
                                     Core.Nothing,
                                   limit = Core.Nothing, nextToken = Core.Nothing}

-- | Comma-separated list of conformance pack names.
--
-- /Note:/ Consider using 'conformancePackNames' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsConformancePackNames :: Lens.Lens' DescribeConformancePackStatus (Core.Maybe [Types.ConformancePackName])
dcpsConformancePackNames = Lens.field @"conformancePackNames"
{-# INLINEABLE dcpsConformancePackNames #-}
{-# DEPRECATED conformancePackNames "Use generic-lens or generic-optics with 'conformancePackNames' instead"  #-}

-- | The maximum number of conformance packs status returned on each page.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsLimit :: Lens.Lens' DescribeConformancePackStatus (Core.Maybe Core.Natural)
dcpsLimit = Lens.field @"limit"
{-# INLINEABLE dcpsLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsNextToken :: Lens.Lens' DescribeConformancePackStatus (Core.Maybe Types.NextToken)
dcpsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcpsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeConformancePackStatus where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeConformancePackStatus where
        toHeaders DescribeConformancePackStatus{..}
          = Core.pure
              ("X-Amz-Target",
               "StarlingDoveService.DescribeConformancePackStatus")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeConformancePackStatus where
        toJSON DescribeConformancePackStatus{..}
          = Core.object
              (Core.catMaybes
                 [("ConformancePackNames" Core..=) Core.<$> conformancePackNames,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeConformancePackStatus where
        type Rs DescribeConformancePackStatus =
             DescribeConformancePackStatusResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeConformancePackStatusResponse' Core.<$>
                   (x Core..:? "ConformancePackStatusDetails") Core.<*>
                     x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkDescribeConformancePackStatusResponse' smart constructor.
data DescribeConformancePackStatusResponse = DescribeConformancePackStatusResponse'
  { conformancePackStatusDetails :: Core.Maybe [Types.ConformancePackStatusDetail]
    -- ^ A list of @ConformancePackStatusDetail@ objects.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeConformancePackStatusResponse' value with any optional fields omitted.
mkDescribeConformancePackStatusResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeConformancePackStatusResponse
mkDescribeConformancePackStatusResponse responseStatus
  = DescribeConformancePackStatusResponse'{conformancePackStatusDetails
                                             = Core.Nothing,
                                           nextToken = Core.Nothing, responseStatus}

-- | A list of @ConformancePackStatusDetail@ objects.
--
-- /Note:/ Consider using 'conformancePackStatusDetails' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsConformancePackStatusDetails :: Lens.Lens' DescribeConformancePackStatusResponse (Core.Maybe [Types.ConformancePackStatusDetail])
dcpsrrsConformancePackStatusDetails = Lens.field @"conformancePackStatusDetails"
{-# INLINEABLE dcpsrrsConformancePackStatusDetails #-}
{-# DEPRECATED conformancePackStatusDetails "Use generic-lens or generic-optics with 'conformancePackStatusDetails' instead"  #-}

-- | The @nextToken@ string returned in a previous request that you use to request the next page of results in a paginated response.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsNextToken :: Lens.Lens' DescribeConformancePackStatusResponse (Core.Maybe Types.NextToken)
dcpsrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcpsrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcpsrrsResponseStatus :: Lens.Lens' DescribeConformancePackStatusResponse Core.Int
dcpsrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcpsrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
