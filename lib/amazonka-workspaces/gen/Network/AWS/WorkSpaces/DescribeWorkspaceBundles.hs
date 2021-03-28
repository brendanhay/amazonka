{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.WorkSpaces.DescribeWorkspaceBundles
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list that describes the available WorkSpace bundles.
--
-- You can filter the results using either bundle ID or owner, but not both.
--
-- This operation returns paginated results.
module Network.AWS.WorkSpaces.DescribeWorkspaceBundles
    (
    -- * Creating a request
      DescribeWorkspaceBundles (..)
    , mkDescribeWorkspaceBundles
    -- ** Request lenses
    , dwbBundleIds
    , dwbNextToken
    , dwbOwner

    -- * Destructuring the response
    , DescribeWorkspaceBundlesResponse (..)
    , mkDescribeWorkspaceBundlesResponse
    -- ** Response lenses
    , dwbrrsBundles
    , dwbrrsNextToken
    , dwbrrsResponseStatus
    ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.WorkSpaces.Types as Types

-- | /See:/ 'mkDescribeWorkspaceBundles' smart constructor.
data DescribeWorkspaceBundles = DescribeWorkspaceBundles'
  { bundleIds :: Core.Maybe (Core.NonEmpty Types.BundleId)
    -- ^ The identifiers of the bundles. You cannot combine this parameter with any other filter.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token for the next set of results. (You received this token from a previous call.)
  , owner :: Core.Maybe Types.BundleOwner
    -- ^ The owner of the bundles. You cannot combine this parameter with any other filter.
--
-- Specify @AMAZON@ to describe the bundles provided by AWS or null to describe the bundles that belong to your account.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeWorkspaceBundles' value with any optional fields omitted.
mkDescribeWorkspaceBundles
    :: DescribeWorkspaceBundles
mkDescribeWorkspaceBundles
  = DescribeWorkspaceBundles'{bundleIds = Core.Nothing,
                              nextToken = Core.Nothing, owner = Core.Nothing}

-- | The identifiers of the bundles. You cannot combine this parameter with any other filter.
--
-- /Note:/ Consider using 'bundleIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbBundleIds :: Lens.Lens' DescribeWorkspaceBundles (Core.Maybe (Core.NonEmpty Types.BundleId))
dwbBundleIds = Lens.field @"bundleIds"
{-# INLINEABLE dwbBundleIds #-}
{-# DEPRECATED bundleIds "Use generic-lens or generic-optics with 'bundleIds' instead"  #-}

-- | The token for the next set of results. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbNextToken :: Lens.Lens' DescribeWorkspaceBundles (Core.Maybe Types.PaginationToken)
dwbNextToken = Lens.field @"nextToken"
{-# INLINEABLE dwbNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The owner of the bundles. You cannot combine this parameter with any other filter.
--
-- Specify @AMAZON@ to describe the bundles provided by AWS or null to describe the bundles that belong to your account.
--
-- /Note:/ Consider using 'owner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbOwner :: Lens.Lens' DescribeWorkspaceBundles (Core.Maybe Types.BundleOwner)
dwbOwner = Lens.field @"owner"
{-# INLINEABLE dwbOwner #-}
{-# DEPRECATED owner "Use generic-lens or generic-optics with 'owner' instead"  #-}

instance Core.ToQuery DescribeWorkspaceBundles where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeWorkspaceBundles where
        toHeaders DescribeWorkspaceBundles{..}
          = Core.pure
              ("X-Amz-Target", "WorkspacesService.DescribeWorkspaceBundles")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeWorkspaceBundles where
        toJSON DescribeWorkspaceBundles{..}
          = Core.object
              (Core.catMaybes
                 [("BundleIds" Core..=) Core.<$> bundleIds,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("Owner" Core..=) Core.<$> owner])

instance Core.AWSRequest DescribeWorkspaceBundles where
        type Rs DescribeWorkspaceBundles = DescribeWorkspaceBundlesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeWorkspaceBundlesResponse' Core.<$>
                   (x Core..:? "Bundles") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeWorkspaceBundles where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"bundles" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeWorkspaceBundlesResponse' smart constructor.
data DescribeWorkspaceBundlesResponse = DescribeWorkspaceBundlesResponse'
  { bundles :: Core.Maybe [Types.WorkspaceBundle]
    -- ^ Information about the bundles.
  , nextToken :: Core.Maybe Types.PaginationToken
    -- ^ The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeWorkspaceBundlesResponse' value with any optional fields omitted.
mkDescribeWorkspaceBundlesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeWorkspaceBundlesResponse
mkDescribeWorkspaceBundlesResponse responseStatus
  = DescribeWorkspaceBundlesResponse'{bundles = Core.Nothing,
                                      nextToken = Core.Nothing, responseStatus}

-- | Information about the bundles.
--
-- /Note:/ Consider using 'bundles' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbrrsBundles :: Lens.Lens' DescribeWorkspaceBundlesResponse (Core.Maybe [Types.WorkspaceBundle])
dwbrrsBundles = Lens.field @"bundles"
{-# INLINEABLE dwbrrsBundles #-}
{-# DEPRECATED bundles "Use generic-lens or generic-optics with 'bundles' instead"  #-}

-- | The token to use to retrieve the next set of results, or null if there are no more results available. This token is valid for one day and must be used within that time frame.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbrrsNextToken :: Lens.Lens' DescribeWorkspaceBundlesResponse (Core.Maybe Types.PaginationToken)
dwbrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dwbrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dwbrrsResponseStatus :: Lens.Lens' DescribeWorkspaceBundlesResponse Core.Int
dwbrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dwbrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
