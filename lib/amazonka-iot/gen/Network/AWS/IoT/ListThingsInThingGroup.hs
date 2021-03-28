{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingsInThingGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the things in the specified group.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingsInThingGroup
    (
    -- * Creating a request
      ListThingsInThingGroup (..)
    , mkListThingsInThingGroup
    -- ** Request lenses
    , ltitgThingGroupName
    , ltitgMaxResults
    , ltitgNextToken
    , ltitgRecursive

    -- * Destructuring the response
    , ListThingsInThingGroupResponse (..)
    , mkListThingsInThingGroupResponse
    -- ** Response lenses
    , ltitgrrsNextToken
    , ltitgrrsThings
    , ltitgrrsResponseStatus
    ) where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListThingsInThingGroup' smart constructor.
data ListThingsInThingGroup = ListThingsInThingGroup'
  { thingGroupName :: Types.ThingGroupName
    -- ^ The thing group name.
  , maxResults :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return at one time.
  , nextToken :: Core.Maybe Types.NextToken
    -- ^ To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
  , recursive :: Core.Maybe Core.Bool
    -- ^ When true, list things in this thing group and in all child groups as well.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingsInThingGroup' value with any optional fields omitted.
mkListThingsInThingGroup
    :: Types.ThingGroupName -- ^ 'thingGroupName'
    -> ListThingsInThingGroup
mkListThingsInThingGroup thingGroupName
  = ListThingsInThingGroup'{thingGroupName,
                            maxResults = Core.Nothing, nextToken = Core.Nothing,
                            recursive = Core.Nothing}

-- | The thing group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgThingGroupName :: Lens.Lens' ListThingsInThingGroup Types.ThingGroupName
ltitgThingGroupName = Lens.field @"thingGroupName"
{-# INLINEABLE ltitgThingGroupName #-}
{-# DEPRECATED thingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead"  #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgMaxResults :: Lens.Lens' ListThingsInThingGroup (Core.Maybe Core.Natural)
ltitgMaxResults = Lens.field @"maxResults"
{-# INLINEABLE ltitgMaxResults #-}
{-# DEPRECATED maxResults "Use generic-lens or generic-optics with 'maxResults' instead"  #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgNextToken :: Lens.Lens' ListThingsInThingGroup (Core.Maybe Types.NextToken)
ltitgNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltitgNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | When true, list things in this thing group and in all child groups as well.
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgRecursive :: Lens.Lens' ListThingsInThingGroup (Core.Maybe Core.Bool)
ltitgRecursive = Lens.field @"recursive"
{-# INLINEABLE ltitgRecursive #-}
{-# DEPRECATED recursive "Use generic-lens or generic-optics with 'recursive' instead"  #-}

instance Core.ToQuery ListThingsInThingGroup where
        toQuery ListThingsInThingGroup{..}
          = Core.maybe Core.mempty (Core.toQueryPair "maxResults") maxResults
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "nextToken") nextToken
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "recursive") recursive

instance Core.ToHeaders ListThingsInThingGroup where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListThingsInThingGroup where
        type Rs ListThingsInThingGroup = ListThingsInThingGroupResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/thing-groups/" Core.<> Core.toText thingGroupName Core.<>
                             "/things",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListThingsInThingGroupResponse' Core.<$>
                   (x Core..:? "nextToken") Core.<*> x Core..:? "things" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListThingsInThingGroup where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"things" Core.. Lens._Just) =
            Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkListThingsInThingGroupResponse' smart constructor.
data ListThingsInThingGroupResponse = ListThingsInThingGroupResponse'
  { nextToken :: Core.Maybe Types.NextToken
    -- ^ The token to use to get the next set of results, or __null__ if there are no additional results.
  , things :: Core.Maybe [Types.ThingName]
    -- ^ The things in the specified thing group.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingsInThingGroupResponse' value with any optional fields omitted.
mkListThingsInThingGroupResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListThingsInThingGroupResponse
mkListThingsInThingGroupResponse responseStatus
  = ListThingsInThingGroupResponse'{nextToken = Core.Nothing,
                                    things = Core.Nothing, responseStatus}

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgrrsNextToken :: Lens.Lens' ListThingsInThingGroupResponse (Core.Maybe Types.NextToken)
ltitgrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE ltitgrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The things in the specified thing group.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgrrsThings :: Lens.Lens' ListThingsInThingGroupResponse (Core.Maybe [Types.ThingName])
ltitgrrsThings = Lens.field @"things"
{-# INLINEABLE ltitgrrsThings #-}
{-# DEPRECATED things "Use generic-lens or generic-optics with 'things' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgrrsResponseStatus :: Lens.Lens' ListThingsInThingGroupResponse Core.Int
ltitgrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE ltitgrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
