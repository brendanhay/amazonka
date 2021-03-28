{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information about a fleet's instances, including instance IDs. Use this operation to get details on all instances in the fleet or get details on one specific instance.
--
-- To get a specific instance, specify fleet ID and instance ID. To get all instances in a fleet, specify a fleet ID only. Use the pagination parameters to retrieve results as a set of sequential pages. If successful, an 'Instance' object is returned for each result.
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-remote-access.html Remotely Access Fleet Instances> 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-creating-debug.html Debug Fleet Issues> 
-- __Related operations__ 
--
--     * 'DescribeInstances' 
--
--
--     * 'GetInstanceAccess' 
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.DescribeInstances
    (
    -- * Creating a request
      DescribeInstances (..)
    , mkDescribeInstances
    -- ** Request lenses
    , diFleetId
    , diInstanceId
    , diLimit
    , diNextToken

    -- * Destructuring the response
    , DescribeInstancesResponse (..)
    , mkDescribeInstancesResponse
    -- ** Response lenses
    , dirrsInstances
    , dirrsNextToken
    , dirrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeInstances' smart constructor.
data DescribeInstances = DescribeInstances'
  { fleetId :: Types.FleetIdOrArn
    -- ^ A unique identifier for a fleet to retrieve instance information for. You can use either the fleet ID or ARN value.
  , instanceId :: Core.Maybe Types.InstanceId
    -- ^ A unique identifier for an instance to retrieve. Specify an instance ID or leave blank to retrieve all instances in the fleet.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeInstances' value with any optional fields omitted.
mkDescribeInstances
    :: Types.FleetIdOrArn -- ^ 'fleetId'
    -> DescribeInstances
mkDescribeInstances fleetId
  = DescribeInstances'{fleetId, instanceId = Core.Nothing,
                       limit = Core.Nothing, nextToken = Core.Nothing}

-- | A unique identifier for a fleet to retrieve instance information for. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diFleetId :: Lens.Lens' DescribeInstances Types.FleetIdOrArn
diFleetId = Lens.field @"fleetId"
{-# INLINEABLE diFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | A unique identifier for an instance to retrieve. Specify an instance ID or leave blank to retrieve all instances in the fleet.
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diInstanceId :: Lens.Lens' DescribeInstances (Core.Maybe Types.InstanceId)
diInstanceId = Lens.field @"instanceId"
{-# INLINEABLE diInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diLimit :: Lens.Lens' DescribeInstances (Core.Maybe Core.Natural)
diLimit = Lens.field @"limit"
{-# INLINEABLE diLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diNextToken :: Lens.Lens' DescribeInstances (Core.Maybe Types.NonZeroAndMaxString)
diNextToken = Lens.field @"nextToken"
{-# INLINEABLE diNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeInstances where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeInstances where
        toHeaders DescribeInstances{..}
          = Core.pure ("X-Amz-Target", "GameLift.DescribeInstances") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeInstances where
        toJSON DescribeInstances{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FleetId" Core..= fleetId),
                  ("InstanceId" Core..=) Core.<$> instanceId,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeInstances where
        type Rs DescribeInstances = DescribeInstancesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeInstancesResponse' Core.<$>
                   (x Core..:? "Instances") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeInstances where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"instances" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeInstancesResponse' smart constructor.
data DescribeInstancesResponse = DescribeInstancesResponse'
  { instances :: Core.Maybe [Types.Instance]
    -- ^ A collection of objects containing properties for each instance returned.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeInstancesResponse' value with any optional fields omitted.
mkDescribeInstancesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeInstancesResponse
mkDescribeInstancesResponse responseStatus
  = DescribeInstancesResponse'{instances = Core.Nothing,
                               nextToken = Core.Nothing, responseStatus}

-- | A collection of objects containing properties for each instance returned.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsInstances :: Lens.Lens' DescribeInstancesResponse (Core.Maybe [Types.Instance])
dirrsInstances = Lens.field @"instances"
{-# INLINEABLE dirrsInstances #-}
{-# DEPRECATED instances "Use generic-lens or generic-optics with 'instances' instead"  #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsNextToken :: Lens.Lens' DescribeInstancesResponse (Core.Maybe Types.NonZeroAndMaxString)
dirrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dirrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dirrsResponseStatus :: Lens.Lens' DescribeInstancesResponse Core.Int
dirrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dirrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
