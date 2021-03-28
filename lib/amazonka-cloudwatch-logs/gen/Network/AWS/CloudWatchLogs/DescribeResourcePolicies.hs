{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeResourcePolicies
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the resource policies in this account.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeResourcePolicies
    (
    -- * Creating a request
      DescribeResourcePolicies (..)
    , mkDescribeResourcePolicies
    -- ** Request lenses
    , drpLimit
    , drpNextToken

    -- * Destructuring the response
    , DescribeResourcePoliciesResponse (..)
    , mkDescribeResourcePoliciesResponse
    -- ** Response lenses
    , drprrsNextToken
    , drprrsResourcePolicies
    , drprrsResponseStatus
    ) where

import qualified Network.AWS.CloudWatchLogs.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeResourcePolicies' smart constructor.
data DescribeResourcePolicies = DescribeResourcePolicies'
  { limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of resource policies to be displayed with one call of this API.
  , nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResourcePolicies' value with any optional fields omitted.
mkDescribeResourcePolicies
    :: DescribeResourcePolicies
mkDescribeResourcePolicies
  = DescribeResourcePolicies'{limit = Core.Nothing,
                              nextToken = Core.Nothing}

-- | The maximum number of resource policies to be displayed with one call of this API.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpLimit :: Lens.Lens' DescribeResourcePolicies (Core.Maybe Core.Natural)
drpLimit = Lens.field @"limit"
{-# INLINEABLE drpLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drpNextToken :: Lens.Lens' DescribeResourcePolicies (Core.Maybe Types.NextToken)
drpNextToken = Lens.field @"nextToken"
{-# INLINEABLE drpNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeResourcePolicies where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeResourcePolicies where
        toHeaders DescribeResourcePolicies{..}
          = Core.pure
              ("X-Amz-Target", "Logs_20140328.DescribeResourcePolicies")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeResourcePolicies where
        toJSON DescribeResourcePolicies{..}
          = Core.object
              (Core.catMaybes
                 [("limit" Core..=) Core.<$> limit,
                  ("nextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeResourcePolicies where
        type Rs DescribeResourcePolicies = DescribeResourcePoliciesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeResourcePoliciesResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "resourcePolicies"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeResourcePolicies where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"resourcePolicies" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeResourcePoliciesResponse' smart constructor.
data DescribeResourcePoliciesResponse = DescribeResourcePoliciesResponse'
  { nextToken :: Core.Maybe Types.NextToken
  , resourcePolicies :: Core.Maybe [Types.ResourcePolicy]
    -- ^ The resource policies that exist in this account.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeResourcePoliciesResponse' value with any optional fields omitted.
mkDescribeResourcePoliciesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeResourcePoliciesResponse
mkDescribeResourcePoliciesResponse responseStatus
  = DescribeResourcePoliciesResponse'{nextToken = Core.Nothing,
                                      resourcePolicies = Core.Nothing, responseStatus}

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsNextToken :: Lens.Lens' DescribeResourcePoliciesResponse (Core.Maybe Types.NextToken)
drprrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE drprrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The resource policies that exist in this account.
--
-- /Note:/ Consider using 'resourcePolicies' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResourcePolicies :: Lens.Lens' DescribeResourcePoliciesResponse (Core.Maybe [Types.ResourcePolicy])
drprrsResourcePolicies = Lens.field @"resourcePolicies"
{-# INLINEABLE drprrsResourcePolicies #-}
{-# DEPRECATED resourcePolicies "Use generic-lens or generic-optics with 'resourcePolicies' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drprrsResponseStatus :: Lens.Lens' DescribeResourcePoliciesResponse Core.Int
drprrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE drprrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
