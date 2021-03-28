{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Greengrass.ListGroupVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of a group.
--
-- This operation returns paginated results.
module Network.AWS.Greengrass.ListGroupVersions
    (
    -- * Creating a request
      ListGroupVersions (..)
    , mkListGroupVersions
    -- ** Request lenses
    , lgvGroupId
    , lgvMaxResults
    , lgvNextToken

    -- * Destructuring the response
    , ListGroupVersionsResponse (..)
    , mkListGroupVersionsResponse
    -- ** Response lenses
    , lgvrrsNextToken
    , lgvrrsVersions
    , lgvrrsResponseStatus
    ) where

import qualified Network.AWS.Greengrass.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGroupVersions' smart constructor.
data ListGroupVersions = ListGroupVersions'
  { groupId :: Core.Text
    -- ^ The ID of the Greengrass group.
  , maxResults :: Core.Maybe Core.Text
    -- ^ The maximum number of results to be returned per request.
  , nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroupVersions' value with any optional fields omitted.
mkListGroupVersions
    :: Core.Text -- ^ 'groupId'
    -> ListGroupVersions
mkListGroupVersions groupId
  = ListGroupVersions'{groupId, maxResults = Core.Nothing,
                       nextToken = Core.Nothing}

-- | The ID of the Greengrass group.
--
-- /Note:/ Consider using 'groupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvGroupId :: Lens.Lens' ListGroupVersions Core.Text
lgvGroupId = Lens.field @"groupId"
{-# INLINEABLE lgvGroupId #-}
{-# DEPRECATED groupId "Use generic-lens or generic-optics with 'groupId' instead"  #-}

-- | The maximum number of results to be returned per request.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvMaxResults :: Lens.Lens' ListGroupVersions (Core.Maybe Core.Text)
lgvMaxResults = Lens.field @"maxResults"
{-# INLINEABLE lgvMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvNextToken :: Lens.Lens' ListGroupVersions (Core.Maybe Core.Text)
lgvNextToken = Lens.field @"nextToken"
{-# INLINEABLE lgvNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery ListGroupVersions where
        toQuery ListGroupVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "MaxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NextToken") nextToken

instance Core.ToHeaders ListGroupVersions where
        toHeaders ListGroupVersions{..}
          = Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.AWSRequest ListGroupVersions where
        type Rs ListGroupVersions = ListGroupVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/greengrass/groups/" Core.<> Core.toText groupId Core.<>
                             "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListGroupVersionsResponse' Core.<$>
                   (x Core..:? "NextToken") Core.<*> x Core..:? "Versions" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListGroupVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"versions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListGroupVersionsResponse' smart constructor.
data ListGroupVersionsResponse = ListGroupVersionsResponse'
  { nextToken :: Core.Maybe Core.Text
    -- ^ The token for the next set of results, or ''null'' if there are no additional results.
  , versions :: Core.Maybe [Types.VersionInformation]
    -- ^ Information about a version.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGroupVersionsResponse' value with any optional fields omitted.
mkListGroupVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListGroupVersionsResponse
mkListGroupVersionsResponse responseStatus
  = ListGroupVersionsResponse'{nextToken = Core.Nothing,
                               versions = Core.Nothing, responseStatus}

-- | The token for the next set of results, or ''null'' if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvrrsNextToken :: Lens.Lens' ListGroupVersionsResponse (Core.Maybe Core.Text)
lgvrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lgvrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | Information about a version.
--
-- /Note:/ Consider using 'versions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvrrsVersions :: Lens.Lens' ListGroupVersionsResponse (Core.Maybe [Types.VersionInformation])
lgvrrsVersions = Lens.field @"versions"
{-# INLINEABLE lgvrrsVersions #-}
{-# DEPRECATED versions "Use generic-lens or generic-optics with 'versions' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgvrrsResponseStatus :: Lens.Lens' ListGroupVersionsResponse Core.Int
lgvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lgvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
