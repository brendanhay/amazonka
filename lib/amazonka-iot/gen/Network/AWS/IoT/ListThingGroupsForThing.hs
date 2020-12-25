{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.ListThingGroupsForThing
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the thing groups to which the specified thing belongs.
--
-- This operation returns paginated results.
module Network.AWS.IoT.ListThingGroupsForThing
  ( -- * Creating a request
    ListThingGroupsForThing (..),
    mkListThingGroupsForThing,

    -- ** Request lenses
    ltgftThingName,
    ltgftMaxResults,
    ltgftNextToken,

    -- * Destructuring the response
    ListThingGroupsForThingResponse (..),
    mkListThingGroupsForThingResponse,

    -- ** Response lenses
    ltgftrrsNextToken,
    ltgftrrsThingGroups,
    ltgftrrsResponseStatus,
  )
where

import qualified Network.AWS.IoT.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListThingGroupsForThing' smart constructor.
data ListThingGroupsForThing = ListThingGroupsForThing'
  { -- | The thing name.
    thingName :: Types.ThingName,
    -- | The maximum number of results to return at one time.
    maxResults :: Core.Maybe Core.Natural,
    -- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
    nextToken :: Core.Maybe Types.NextToken
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingGroupsForThing' value with any optional fields omitted.
mkListThingGroupsForThing ::
  -- | 'thingName'
  Types.ThingName ->
  ListThingGroupsForThing
mkListThingGroupsForThing thingName =
  ListThingGroupsForThing'
    { thingName,
      maxResults = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The thing name.
--
-- /Note:/ Consider using 'thingName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftThingName :: Lens.Lens' ListThingGroupsForThing Types.ThingName
ltgftThingName = Lens.field @"thingName"
{-# DEPRECATED ltgftThingName "Use generic-lens or generic-optics with 'thingName' instead." #-}

-- | The maximum number of results to return at one time.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftMaxResults :: Lens.Lens' ListThingGroupsForThing (Core.Maybe Core.Natural)
ltgftMaxResults = Lens.field @"maxResults"
{-# DEPRECATED ltgftMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | To retrieve the next set of results, the @nextToken@ value from a previous response; otherwise __null__ to receive the first set of results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftNextToken :: Lens.Lens' ListThingGroupsForThing (Core.Maybe Types.NextToken)
ltgftNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltgftNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.AWSRequest ListThingGroupsForThing where
  type Rs ListThingGroupsForThing = ListThingGroupsForThingResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/things/" Core.<> (Core.toText thingName)
                Core.<> ("/thing-groups")
            ),
        Core._rqQuery =
          Core.toQueryValue "maxResults" Core.<$> maxResults
            Core.<> (Core.toQueryValue "nextToken" Core.<$> nextToken),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListThingGroupsForThingResponse'
            Core.<$> (x Core..:? "nextToken")
            Core.<*> (x Core..:? "thingGroups")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListThingGroupsForThing where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"thingGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListThingGroupsForThingResponse' smart constructor.
data ListThingGroupsForThingResponse = ListThingGroupsForThingResponse'
  { -- | The token to use to get the next set of results, or __null__ if there are no additional results.
    nextToken :: Core.Maybe Types.NextToken,
    -- | The thing groups.
    thingGroups :: Core.Maybe [Types.GroupNameAndArn],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListThingGroupsForThingResponse' value with any optional fields omitted.
mkListThingGroupsForThingResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListThingGroupsForThingResponse
mkListThingGroupsForThingResponse responseStatus =
  ListThingGroupsForThingResponse'
    { nextToken = Core.Nothing,
      thingGroups = Core.Nothing,
      responseStatus
    }

-- | The token to use to get the next set of results, or __null__ if there are no additional results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftrrsNextToken :: Lens.Lens' ListThingGroupsForThingResponse (Core.Maybe Types.NextToken)
ltgftrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED ltgftrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The thing groups.
--
-- /Note:/ Consider using 'thingGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftrrsThingGroups :: Lens.Lens' ListThingGroupsForThingResponse (Core.Maybe [Types.GroupNameAndArn])
ltgftrrsThingGroups = Lens.field @"thingGroups"
{-# DEPRECATED ltgftrrsThingGroups "Use generic-lens or generic-optics with 'thingGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltgftrrsResponseStatus :: Lens.Lens' ListThingGroupsForThingResponse Core.Int
ltgftrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED ltgftrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
