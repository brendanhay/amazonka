{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListGameServerGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__
--
-- Retrieves information on all game servers groups that exist in the current AWS account for the selected Region. Use the pagination parameters to retrieve results in a set of sequential segments.
-- __Learn more__
-- <https://docs.aws.amazon.com/gamelift/latest/fleetiqguide/gsg-intro.html GameLift FleetIQ Guide>
-- __Related operations__
--
--     * 'CreateGameServerGroup'
--
--
--     * 'ListGameServerGroups'
--
--
--     * 'DescribeGameServerGroup'
--
--
--     * 'UpdateGameServerGroup'
--
--
--     * 'DeleteGameServerGroup'
--
--
--     * 'ResumeGameServerGroup'
--
--
--     * 'SuspendGameServerGroup'
--
--
--     * 'DescribeGameServerInstances'
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListGameServerGroups
  ( -- * Creating a request
    ListGameServerGroups (..),
    mkListGameServerGroups,

    -- ** Request lenses
    lgsgLimit,
    lgsgNextToken,

    -- * Destructuring the response
    ListGameServerGroupsResponse (..),
    mkListGameServerGroupsResponse,

    -- ** Response lenses
    lgsgrrsGameServerGroups,
    lgsgrrsNextToken,
    lgsgrrsResponseStatus,
  )
where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListGameServerGroups' smart constructor.
data ListGameServerGroups = ListGameServerGroups'
  { -- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
    limit :: Core.Maybe Core.Natural,
    -- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListGameServerGroups' value with any optional fields omitted.
mkListGameServerGroups ::
  ListGameServerGroups
mkListGameServerGroups =
  ListGameServerGroups'
    { limit = Core.Nothing,
      nextToken = Core.Nothing
    }

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsgLimit :: Lens.Lens' ListGameServerGroups (Core.Maybe Core.Natural)
lgsgLimit = Lens.field @"limit"
{-# DEPRECATED lgsgLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsgNextToken :: Lens.Lens' ListGameServerGroups (Core.Maybe Types.NonZeroAndMaxString)
lgsgNextToken = Lens.field @"nextToken"
{-# DEPRECATED lgsgNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

instance Core.FromJSON ListGameServerGroups where
  toJSON ListGameServerGroups {..} =
    Core.object
      ( Core.catMaybes
          [ ("Limit" Core..=) Core.<$> limit,
            ("NextToken" Core..=) Core.<$> nextToken
          ]
      )

instance Core.AWSRequest ListGameServerGroups where
  type Rs ListGameServerGroups = ListGameServerGroupsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "GameLift.ListGameServerGroups")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListGameServerGroupsResponse'
            Core.<$> (x Core..:? "GameServerGroups")
            Core.<*> (x Core..:? "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListGameServerGroups where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"gameServerGroups" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken"
        )

-- | /See:/ 'mkListGameServerGroupsResponse' smart constructor.
data ListGameServerGroupsResponse = ListGameServerGroupsResponse'
  { -- | A collection of game server group objects that match the request.
    gameServerGroups :: Core.Maybe [Types.GameServerGroup],
    -- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
    nextToken :: Core.Maybe Types.NonZeroAndMaxString,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ListGameServerGroupsResponse' value with any optional fields omitted.
mkListGameServerGroupsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListGameServerGroupsResponse
mkListGameServerGroupsResponse responseStatus =
  ListGameServerGroupsResponse'
    { gameServerGroups = Core.Nothing,
      nextToken = Core.Nothing,
      responseStatus
    }

-- | A collection of game server group objects that match the request.
--
-- /Note:/ Consider using 'gameServerGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsgrrsGameServerGroups :: Lens.Lens' ListGameServerGroupsResponse (Core.Maybe [Types.GameServerGroup])
lgsgrrsGameServerGroups = Lens.field @"gameServerGroups"
{-# DEPRECATED lgsgrrsGameServerGroups "Use generic-lens or generic-optics with 'gameServerGroups' instead." #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsgrrsNextToken :: Lens.Lens' ListGameServerGroupsResponse (Core.Maybe Types.NonZeroAndMaxString)
lgsgrrsNextToken = Lens.field @"nextToken"
{-# DEPRECATED lgsgrrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lgsgrrsResponseStatus :: Lens.Lens' ListGameServerGroupsResponse Core.Int
lgsgrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lgsgrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
