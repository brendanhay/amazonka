{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Snowball.ListClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns an array of @ClusterListEntry@ objects of the specified length. Each @ClusterListEntry@ object contains a cluster's state, a cluster's ID, and other important status information.
--
-- This operation returns paginated results.
module Network.AWS.Snowball.ListClusters
  ( -- * Creating a request
    ListClusters (..),
    mkListClusters,

    -- ** Request lenses
    lcMaxResults,
    lcNextToken,

    -- * Destructuring the response
    ListClustersResponse (..),
    mkListClustersResponse,

    -- ** Response lenses
    lcrrsClusterListEntries,
    lcrrsNextToken,
    lcrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.Snowball.Types as Types

-- | /See:/ 'mkListClusters' smart constructor.
data ListClusters = ListClusters'
  { -- | The number of @ClusterListEntry@ objects to return.
    maxResults :: Core.Maybe Core.Natural,
    -- | HTTP requests are stateless. To identify what object comes "next" in the list of @ClusterListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
    nextToken :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListClusters' value with any optional fields omitted.
mkListClusters ::
  ListClusters
mkListClusters =
  ListClusters'
    { maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The number of @ClusterListEntry@ objects to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcMaxResults :: Lens.Lens' ListClusters (Core.Maybe Core.Natural)
lcMaxResults = Lens.field @"maxResults"
{-# DEPRECATED lcMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | HTTP requests are stateless. To identify what object comes "next" in the list of @ClusterListEntry@ objects, you have the option of specifying @NextToken@ as the starting point for your returned list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcNextToken :: Lens.Lens' ListClusters (Core.Maybe Types.String)
lcNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListClusters where
  toJSON ListClusters {..} =
    Core.object
      ( Core.catMaybes
          [ ("MaxResults" Core..=) Core.<$> maxResults,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListClusters where
  type Rs ListClusters = ListClustersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ("X-Amz-Target", "AWSIESnowballJobManagementService.ListClusters")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClustersResponse'
            Core.<$> (x Core..:? "ClusterListEntries")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListClusters where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"clusterListEntries" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { -- | Each @ClusterListEntry@ object contains a cluster's state, a cluster's ID, and other important status information.
    clusterListEntries :: Core.Maybe [Types.ClusterListEntry],
    -- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ClusterListEntry@ call, your list of returned clusters will start from this point in the array.
    nextToken :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListClustersResponse' value with any optional fields omitted.
mkListClustersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListClustersResponse
mkListClustersResponse responseStatus =
  ListClustersResponse'
    { clusterListEntries = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | Each @ClusterListEntry@ object contains a cluster's state, a cluster's ID, and other important status information.
--
-- /Note:/ Consider using 'clusterListEntries' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsClusterListEntries :: Lens.Lens' ListClustersResponse (Core.Maybe [Types.ClusterListEntry])
lcrrsClusterListEntries = Lens.field @"clusterListEntries"
{-# DEPRECATED lcrrsClusterListEntries "Use generic-lens or generic-optics with 'clusterListEntries' instead." #-}

-- | HTTP requests are stateless. If you use the automatically generated @NextToken@ value in your next @ClusterListEntry@ call, your list of returned clusters will start from this point in the array.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsNextToken :: Lens.Lens' ListClustersResponse (Core.Maybe Types.String)
lcrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lcrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcrrsResponseStatus :: Lens.Lens' ListClustersResponse Core.Int
lcrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lcrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
