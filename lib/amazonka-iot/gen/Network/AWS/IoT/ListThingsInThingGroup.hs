{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
  ( -- * Creating a request
    ListThingsInThingGroup (..),
    mkListThingsInThingGroup,

    -- ** Request lenses
    ltitgThingGroupName,
    ltitgMaxResults,
    ltitgNextToken,
    ltitgRecursive,

    -- * Destructuring the response
    ListThingsInThingGroupResponse (..),
    mkListThingsInThingGroupResponse,

    -- ** Response lenses
    ltitgrrsNextToken,
    ltitgrrsThings,
    ltitgrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListThingsInThingGroup' smart constructor.
data ListThingsInThingGroup = ListThingsInThingGroup'
  { -- | The thing group name.
    thingGroupName :: Types.ThingGroupName,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | When true, list things in this thing group and in all child groups as well.
    recursive :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingsInThingGroup' value with any optional fields omitted.
mkListThingsInThingGroup ::
  -- | 'thingGroupName'
  Types.ThingGroupName ->
  ListThingsInThingGroup
mkListThingsInThingGroup thingGroupName =
  ListThingsInThingGroup'
    { thingGroupName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing,
      recursive = Core.Nothing
    }

-- | The thing group name.
--
-- /Note:/ Consider using 'thingGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgThingGroupName :: Lens.Lens' ListThingsInThingGroup Types.ThingGroupName
ltitgThingGroupName = Lens.field @"thingGroupName"
{-# DEPRECATED ltitgThingGroupName "Use generic-lens or generic-optics with 'thingGroupName' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgMaxResults :: Lens.Lens' ListThingsInThingGroup (Core.Maybe Core.Natural)
ltitgMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltitgMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgNextToken :: Lens.Lens' ListThingsInThingGroup (Core.Maybe Types.NextToken)
ltitgNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltitgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | When true, list things in this thing group and in all child groups as well.
--
-- /Note:/ Consider using 'recursive' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgRecursive :: Lens.Lens' ListThingsInThingGroup (Core.Maybe Core.Bool)
ltitgRecursive = Lens.field @"recursive"
{-# DEPRECATED ltitgRecursive "Use generic-lens or generic-optics with 'recursive' instead." #-}

instance Core.AWSRequest ListThingsInThingGroup where
  type Rs ListThingsInThingGroup = ListThingsInThingGroupResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/thing-groups/" Core.<> (Core.toText thingGroupName)
                Core.<> ("/things")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken)
            Core.<> (Core.toQueryValue "recursive" Core.<$> recursive),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingsInThingGroupResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "things")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListThingsInThingGroup where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"things" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListThingsInThingGroupResponse' smart constructor.
data ListThingsInThingGroupResponse = ListThingsInThingGroupResponse'
  { -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The things in the specified thing group.
    things :: Core.Maybe [Types.ThingName],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingsInThingGroupResponse' value with any optional fields omitted.
mkListThingsInThingGroupResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListThingsInThingGroupResponse
mkListThingsInThingGroupResponse responseStatus =
  ListThingsInThingGroupResponse'
    { nextToken = Core.Nothing,
      things = Core.Nothing,
      responseStatus
    }

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgrrsNextToken :: Lens.Lens' ListThingsInThingGroupResponse (Core.Maybe Types.NextToken)
ltitgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltitgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The things in the specified thing group.
--
-- /Note:/ Consider using 'things' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgrrsThings :: Lens.Lens' ListThingsInThingGroupResponse (Core.Maybe [Types.ThingName])
ltitgrrsThings = Lens.field @"things"
{-# DEPRECATED ltitgrrsThings "Use generic-lens or generic-optics with 'things' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltitgrrsResponseStatus :: Lens.Lens' ListThingsInThingGroupResponse Core.Int
ltitgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltitgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
