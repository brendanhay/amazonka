{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Discovery.DescribeContinuousExports
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists exports as specified by ID. All continuous exports associated with your user account can be listed if you call @DescribeContinuousExports@ as is without passing any parameters.
--
-- This operation returns paginated results.
module Network.AWS.Discovery.DescribeContinuousExports
    (
    -- * Creating a request
      DescribeContinuousExports (..)
    , mkDescribeContinuousExports
    -- ** Request lenses
    , dceExportIds
    , dceMaxResults
    , dceNextToken

    -- * Destructuring the response
    , DescribeContinuousExportsResponse (..)
    , mkDescribeContinuousExportsResponse
    -- ** Response lenses
    , dcerrsDescriptions
    , dcerrsNextToken
    , dcerrsResponseStatus
    ) where

import qualified Network.AWS.Discovery.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeContinuousExports' smart constructor.
data DescribeContinuousExports = DescribeContinuousExports'
  { exportIds :: Core.Maybe [Types.ConfigurationsExportId]
    -- ^ The unique IDs assigned to the exports.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ A number between 1 and 100 specifying the maximum number of continuous export descriptions returned.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token from the previous call to @DescribeExportTasks@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeContinuousExports' value with any optional fields omitted.
mkDescribeContinuousExports
    :: DescribeContinuousExports
mkDescribeContinuousExports
  = DescribeContinuousExports'{exportIds = Core.Nothing,
                               maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | The unique IDs assigned to the exports.
--
-- /Note:/ Consider using 'exportIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceExportIds :: Lens.Lens' DescribeContinuousExports (Core.Maybe [Types.ConfigurationsExportId])
dceExportIds = Lens.field @"exportIds"
{-# INLINEABLE dceExportIds #-}
{-# DEPRECATED exportIds "Use generic-lens or generic-optics with 'exportIds' instead"  #-}

-- | A number between 1 and 100 specifying the maximum number of continuous export descriptions returned.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceMaxResults :: Lens.Lens' DescribeContinuousExports (Core.Maybe Core.Natural)
dceMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dceMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token from the previous call to @DescribeExportTasks@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dceNextToken :: Lens.Lens' DescribeContinuousExports (Core.Maybe Types.NextToken)
dceNextToken = Lens.field @"nextToken"
{-# INLINEABLE dceNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeContinuousExports where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeContinuousExports where
        toHeaders DescribeContinuousExports{..}
          = Core.pure
              ("X-Amz-Target",
               "AWSPoseidonService_V2015_11_01.DescribeContinuousExports")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeContinuousExports where
        toJSON DescribeContinuousExports{..}
          = Core.object
              (Core.catMaybes
                 [("exportIds" Core..=) Core.<$> exportIds,
                  ("maxResults" Core..=) Core.<$> maxResults,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeContinuousExports where
        type Rs DescribeContinuousExports =
             DescribeContinuousExportsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeContinuousExportsResponse' Core.<$>
                   (x Core..:? "descriptions") Core.<*> x Core..:? "nextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeContinuousExports where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"descriptions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeContinuousExportsResponse' smart constructor.
data DescribeContinuousExportsResponse = DescribeContinuousExportsResponse'
  { descriptions :: Core.Maybe [Types.ContinuousExportDescription]
    -- ^ A list of continuous export descriptions.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token from the previous call to @DescribeExportTasks@ .
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeContinuousExportsResponse' value with any optional fields omitted.
mkDescribeContinuousExportsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeContinuousExportsResponse
mkDescribeContinuousExportsResponse responseStatus
  = DescribeContinuousExportsResponse'{descriptions = Core.Nothing,
                                       nextToken = Core.Nothing, responseStatus}

-- | A list of continuous export descriptions.
--
-- /Note:/ Consider using 'descriptions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcerrsDescriptions :: Lens.Lens' DescribeContinuousExportsResponse (Core.Maybe [Types.ContinuousExportDescription])
dcerrsDescriptions = Lens.field @"descriptions"
{-# INLINEABLE dcerrsDescriptions #-}
{-# DEPRECATED descriptions "Use generic-lens or generic-optics with 'descriptions' instead"  #-}

-- | The token from the previous call to @DescribeExportTasks@ .
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcerrsNextToken :: Lens.Lens' DescribeContinuousExportsResponse (Core.Maybe Types.NextToken)
dcerrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dcerrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcerrsResponseStatus :: Lens.Lens' DescribeContinuousExportsResponse Core.Int
dcerrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dcerrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
