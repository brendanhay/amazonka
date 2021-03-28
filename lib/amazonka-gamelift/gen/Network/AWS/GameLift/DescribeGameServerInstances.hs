{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeGameServerInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- __This operation is used with the Amazon GameLift FleetIQ solution and game server groups.__ 
--
-- Retrieves status information about the Amazon EC2 instances associated with a GameLift FleetIQ game server group. Use this operation to detect when instances are active or not available to host new game servers. If you are looking for instance configuration information, call 'DescribeGameServerGroup' or access the corresponding Auto Scaling group properties.
-- To request status for all instances in the game server group, provide a game server group ID only. To request status for specific instances, provide the game server group ID and one or more instance IDs. Use the pagination parameters to retrieve results in sequential segments. If successful, a collection of @GameServerInstance@ objects is returned. 
-- This operation is not designed to be called with every game server claim request; this practice can cause you to exceed your API limit, which results in errors. Instead, as a best practice, cache the results and refresh your cache no more than once every 10 seconds.
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
module Network.AWS.GameLift.DescribeGameServerInstances
    (
    -- * Creating a request
      DescribeGameServerInstances (..)
    , mkDescribeGameServerInstances
    -- ** Request lenses
    , dgsiGameServerGroupName
    , dgsiInstanceIds
    , dgsiLimit
    , dgsiNextToken

    -- * Destructuring the response
    , DescribeGameServerInstancesResponse (..)
    , mkDescribeGameServerInstancesResponse
    -- ** Response lenses
    , dgsirrsGameServerInstances
    , dgsirrsNextToken
    , dgsirrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkDescribeGameServerInstances' smart constructor.
data DescribeGameServerInstances = DescribeGameServerInstances'
  { gameServerGroupName :: Types.GameServerGroupName
    -- ^ A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
  , instanceIds :: Core.Maybe (Core.NonEmpty Types.GameServerInstanceId)
    -- ^ The EC2 instance IDs that you want to retrieve status on. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ . To retrieve all instances in the game server group, leave this parameter empty. 
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments. 
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value. 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGameServerInstances' value with any optional fields omitted.
mkDescribeGameServerInstances
    :: Types.GameServerGroupName -- ^ 'gameServerGroupName'
    -> DescribeGameServerInstances
mkDescribeGameServerInstances gameServerGroupName
  = DescribeGameServerInstances'{gameServerGroupName,
                                 instanceIds = Core.Nothing, limit = Core.Nothing,
                                 nextToken = Core.Nothing}

-- | A unique identifier for the game server group. Use either the 'GameServerGroup' name or ARN value.
--
-- /Note:/ Consider using 'gameServerGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsiGameServerGroupName :: Lens.Lens' DescribeGameServerInstances Types.GameServerGroupName
dgsiGameServerGroupName = Lens.field @"gameServerGroupName"
{-# INLINEABLE dgsiGameServerGroupName #-}
{-# DEPRECATED gameServerGroupName "Use generic-lens or generic-optics with 'gameServerGroupName' instead"  #-}

-- | The EC2 instance IDs that you want to retrieve status on. EC2 instance IDs use a 17-character format, for example: @i-1234567890abcdef0@ . To retrieve all instances in the game server group, leave this parameter empty. 
--
-- /Note:/ Consider using 'instanceIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsiInstanceIds :: Lens.Lens' DescribeGameServerInstances (Core.Maybe (Core.NonEmpty Types.GameServerInstanceId))
dgsiInstanceIds = Lens.field @"instanceIds"
{-# INLINEABLE dgsiInstanceIds #-}
{-# DEPRECATED instanceIds "Use generic-lens or generic-optics with 'instanceIds' instead"  #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential segments. 
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsiLimit :: Lens.Lens' DescribeGameServerInstances (Core.Maybe Core.Natural)
dgsiLimit = Lens.field @"limit"
{-# INLINEABLE dgsiLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | A token that indicates the start of the next sequential segment of results. Use the token returned with the previous call to this operation. To start at the beginning of the result set, do not specify a value. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsiNextToken :: Lens.Lens' DescribeGameServerInstances (Core.Maybe Types.NonZeroAndMaxString)
dgsiNextToken = Lens.field @"nextToken"
{-# INLINEABLE dgsiNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeGameServerInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeGameServerInstances where
        toHeaders DescribeGameServerInstances{..}
          = Core.pure
              ("X-Amz-Target", "GameLift.DescribeGameServerInstances")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeGameServerInstances where
        toJSON DescribeGameServerInstances{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("GameServerGroupName" Core..= gameServerGroupName),
                  ("InstanceIds" Core..=) Core.<$> instanceIds,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeGameServerInstances where
        type Rs DescribeGameServerInstances =
             DescribeGameServerInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeGameServerInstancesResponse' Core.<$>
                   (x Core..:? "GameServerInstances") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeGameServerInstances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"gameServerInstances" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | /See:/ 'mkDescribeGameServerInstancesResponse' smart constructor.
data DescribeGameServerInstancesResponse = DescribeGameServerInstancesResponse'
  { gameServerInstances :: Core.Maybe [Types.GameServerInstance]
    -- ^ The collection of requested game server instances. 
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list. 
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeGameServerInstancesResponse' value with any optional fields omitted.
mkDescribeGameServerInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeGameServerInstancesResponse
mkDescribeGameServerInstancesResponse responseStatus
  = DescribeGameServerInstancesResponse'{gameServerInstances =
                                           Core.Nothing,
                                         nextToken = Core.Nothing, responseStatus}

-- | The collection of requested game server instances. 
--
-- /Note:/ Consider using 'gameServerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsirrsGameServerInstances :: Lens.Lens' DescribeGameServerInstancesResponse (Core.Maybe [Types.GameServerInstance])
dgsirrsGameServerInstances = Lens.field @"gameServerInstances"
{-# INLINEABLE dgsirrsGameServerInstances #-}
{-# DEPRECATED gameServerInstances "Use generic-lens or generic-optics with 'gameServerInstances' instead"  #-}

-- | A token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list. 
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsirrsNextToken :: Lens.Lens' DescribeGameServerInstancesResponse (Core.Maybe Types.NonZeroAndMaxString)
dgsirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dgsirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dgsirrsResponseStatus :: Lens.Lens' DescribeGameServerInstancesResponse Core.Int
dgsirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dgsirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
