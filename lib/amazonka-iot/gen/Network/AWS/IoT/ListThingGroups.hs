{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the thing groups in your account.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingGroups
    (
    -- * Creating a request
      ListThingGroups (..)
    , mkListThingGroups
    -- ** Request lenses
    , ltgMaxResults
    , ltgNamePrefixFilter
    , ltgNextToken
    , ltgParentGroup
    , ltgRecursive

    -- * Destructuring the response
    , ListThingGroupsResponse (..)
    , mkListThingGroupsResponse
    -- ** Response lenses
    , ltgrrsNextToken
    , ltgrrsThingGroups
    , ltgrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListThingGroups' smart constructor.
data ListThingGroups = ListThingGroups'
  { maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time.
  , namePrefixFilter :: Core.Maybe Types.ThingGroupName
    -- ^ A filter that limits the results to those with the specified name prefix.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
  , parentGroup :: Core.Maybe Types.ThingGroupName
    -- ^ A filter that limits the results to those with the specified parent group.
  , recursive :: Core.Maybe Core.Bool
    -- ^ If true, return child groups as well.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingGroups' value with any optional fields omitted.
mkListThingGroups
    :: ListThingGroups
mkListThingGroups
  = ListThingGroups'{maxResults = Core.Nothing,
                     namePrefixFilter = Core.Nothing, nextToken = Core.Nothing,
                     parentGroup = Core.Nothing, recursive = Core.Nothing}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgMaxResults :: Lens.Lens' ListThingGroups (Core.Maybe Core.Natural)
ltgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | A filter that limits the results to those with the specified name prefix.
--
-- /Note:/ Consider using 'namePrefixFilter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgNamePrefixFilter :: Lens.Lens' ListThingGroups (Core.Maybe Types.ThingGroupName)
ltgNamePrefixFilter = Lens.field @"namePrefixFilter"
{-# INLINEABLE ltgNamePrefixFilter #-}
{-# DEPRECATED namePrefixFilter "Use generic-lens or generic-optics with 'namePrefixFilter' instead"  #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgNextToken :: Lens.Lens' ListThingGroups (Core.Maybe Types.NextToken)
ltgNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A filter that limits the results to those with the specified parent group.
--
-- /Note:/ Consider using 'parentGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgParentGroup :: Lens.Lens' ListThingGroups (Core.Maybe Types.ThingGroupName)
ltgParentGroup = Lens.field @"parentGroup"
{-# INLINEABLE ltgParentGroup #-}
{-# DEPRECATED parentGroup "Use generic-lens or generic-optics with 'parentGroup' instead"  #-}

-- | If true, return child groups as well.
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgRecursive :: Lens.Lens' ListThingGroups (Core.Maybe Core.Bool)
ltgRecursive = Lens.field @"recursive"
{-# INLINEABLE ltgRecursive #-}
{-# DEPRECATED recursive "Use generic-lens or generic-optics with 'recursive' instead"  #-}

instance Core.ToQuery ListThingGroups where
        toQuery ListThingGroups{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "namePrefixFilter")
                namePrefixFilter
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "parentGroup") parentGroup
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "recursive") recursive

instance Core.ToHeaders ListThingGroups where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListThingGroups where
        type Rs ListThingGroups = ListThingGroupsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET, Core._rqPath = "/thing-groups",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListThingGroupsResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "thingGroups" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListThingGroups where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"thingGroups" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListThingGroupsResponse' smart constructor.
data ListThingGroupsResponse = ListThingGroupsResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to get the next set of results. Will not be returned if operation has returned all results.
  , thingGroups :: Core.Maybe [Types.GroupNameAndArn]
    -- ^ The thing groups.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingGroupsResponse' value with any optional fields omitted.
mkListThingGroupsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListThingGroupsResponse
mkListThingGroupsResponse responseStatus
  = ListThingGroupsResponse'{nextToken = Core.Nothing,
                             thingGroups = Core.Nothing, responseStatus}

-- | The token to use to get the next set of results. Will not be returned if operation has returned all results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgrrsNextToken :: Lens.Lens' ListThingGroupsResponse (Core.Maybe Types.NextToken)
ltgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The thing groups.
--
-- /Note:/ Consider using 'thingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgrrsThingGroups :: Lens.Lens' ListThingGroupsResponse (Core.Maybe [Types.GroupNameAndArn])
ltgrrsThingGroups = Lens.field @"thingGroups"
{-# INLINEABLE ltgrrsThingGroups #-}
{-# DEPRECATED thingGroups "Use generic-lens or generic-optics with 'thingGroups' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgrrsResponseStatus :: Lens.Lens' ListThingGroupsResponse Core.Int
ltgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
