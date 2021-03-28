{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.SSM.DescribeMaintenanceWindows
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the maintenance windows in an AWS account.
--
-- This operation returns paginated results.
module Network.AWS.SSM.DescribeMaintenanceWindows
    (
    -- * Creating a request
      DescribeMaintenanceWindows (..)
    , mkDescribeMaintenanceWindows
    -- ** Request lenses
    , dmwFilters
    , dmwMaxResults
    , dmwNextToken

    -- * Destructuring the response
    , DescribeMaintenanceWindowsResponse (..)
    , mkDescribeMaintenanceWindowsResponse
    -- ** Response lenses
    , dmwrfrsNextToken
    , dmwrfrsWindowIdentities
    , dmwrfrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.SSM.Types as Types

-- | /See:/ 'mkDescribeMaintenanceWindows' smart constructor.
data DescribeMaintenanceWindows = DescribeMaintenanceWindows'
  { filters :: Core.Maybe [Types.MaintenanceWindowFilter]
    -- ^ Optional filters used to narrow down the scope of the returned maintenance windows. Supported filter keys are __Name__ and __Enabled__ .
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ The token for the next set of items to return. (You received this token from a previous call.)
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindows' value with any optional fields omitted.
mkDescribeMaintenanceWindows
    :: DescribeMaintenanceWindows
mkDescribeMaintenanceWindows
  = DescribeMaintenanceWindows'{filters = Core.Nothing,
                                maxResults = Core.Nothing, nextToken = Core.Nothing}

-- | Optional filters used to narrow down the scope of the returned maintenance windows. Supported filter keys are __Name__ and __Enabled__ .
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwFilters :: Lens.Lens' DescribeMaintenanceWindows (Core.Maybe [Types.MaintenanceWindowFilter])
dmwFilters = Lens.field @"filters"
{-# INLINEABLE dmwFilters #-}
{-# DEPRECATED filters "Use generic-lens or generic-optics with 'filters' instead"  #-}

-- | The maximum number of items to return for this call. The call also returns a token that you can specify in a subsequent call to get the next set of results.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwMaxResults :: Lens.Lens' DescribeMaintenanceWindows (Core.Maybe Core.Natural)
dmwMaxResults = Lens.field @"maxResults"
{-# INLINEABLE dmwMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwNextToken :: Lens.Lens' DescribeMaintenanceWindows (Core.Maybe Types.NextToken)
dmwNextToken = Lens.field @"nextToken"
{-# INLINEABLE dmwNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeMaintenanceWindows where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeMaintenanceWindows where
        toHeaders DescribeMaintenanceWindows{..}
          = Core.pure
              ("X-Amz-Target", "AmazonSSM.DescribeMaintenanceWindows")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeMaintenanceWindows where
        toJSON DescribeMaintenanceWindows{..}
          = Core.object
              (Core.catMaybes
                 [("Filters" Core..=) Core.<$> filters,
                  ("MaxResults" Core..=) Core.<$> maxResults,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeMaintenanceWindows where
        type Rs DescribeMaintenanceWindows =
             DescribeMaintenanceWindowsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeMaintenanceWindowsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "WindowIdentities"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeMaintenanceWindows where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"windowIdentities" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeMaintenanceWindowsResponse' smart constructor.
data DescribeMaintenanceWindowsResponse = DescribeMaintenanceWindowsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
  , windowIdentities :: Core.Maybe [Types.MaintenanceWindowIdentity]
    -- ^ Information about the maintenance windows.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeMaintenanceWindowsResponse' value with any optional fields omitted.
mkDescribeMaintenanceWindowsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeMaintenanceWindowsResponse
mkDescribeMaintenanceWindowsResponse responseStatus
  = DescribeMaintenanceWindowsResponse'{nextToken = Core.Nothing,
                                        windowIdentities = Core.Nothing, responseStatus}

-- | The token to use when requesting the next set of items. If there are no additional items to return, the string is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwrfrsNextToken :: Lens.Lens' DescribeMaintenanceWindowsResponse (Core.Maybe Types.NextToken)
dmwrfrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dmwrfrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about the maintenance windows.
--
-- /Note:/ Consider using 'windowIdentities' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwrfrsWindowIdentities :: Lens.Lens' DescribeMaintenanceWindowsResponse (Core.Maybe [Types.MaintenanceWindowIdentity])
dmwrfrsWindowIdentities = Lens.field @"windowIdentities"
{-# INLINEABLE dmwrfrsWindowIdentities #-}
{-# DEPRECATED windowIdentities "Use generic-lens or generic-optics with 'windowIdentities' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dmwrfrsResponseStatus :: Lens.Lens' DescribeMaintenanceWindowsResponse Core.Int
dmwrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dmwrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
