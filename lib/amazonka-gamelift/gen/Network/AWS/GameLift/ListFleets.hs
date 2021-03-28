{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.ListFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a collection of fleet resources for this AWS account. You can filter the result set to find only those fleets that are deployed with a specific build or script. Use the pagination parameters to retrieve results in sequential pages.
--
-- __Learn more__ 
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/fleets-intro.html Setting up GameLift Fleets> 
-- __Related operations__ 
--
--     * 'CreateFleet' 
--
--
--     * 'ListFleets' 
--
--
--     * 'DeleteFleet' 
--
--
--     * 'DescribeFleetAttributes' 
--
--
--     * 'UpdateFleetAttributes' 
--
--
--     * 'StartFleetActions' or 'StopFleetActions' 
--
--
--
-- This operation returns paginated results.
module Network.AWS.GameLift.ListFleets
    (
    -- * Creating a request
      ListFleets (..)
    , mkListFleets
    -- ** Request lenses
    , lfBuildId
    , lfLimit
    , lfNextToken
    , lfScriptId

    -- * Destructuring the response
    , ListFleetsResponse (..)
    , mkListFleetsResponse
    -- ** Response lenses
    , lfrrsFleetIds
    , lfrrsNextToken
    , lfrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkListFleets' smart constructor.
data ListFleets = ListFleets'
  { buildId :: Core.Maybe Types.BuildIdOrArn
    -- ^ A unique identifier for a build to return fleets for. Use this parameter to return only fleets using a specified build. Use either the build ID or ARN value. To retrieve all fleets, do not include either a BuildId and ScriptID parameter.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
  , scriptId :: Core.Maybe Types.ScriptIdOrArn
    -- ^ A unique identifier for a Realtime script to return fleets for. Use this parameter to return only fleets using a specified script. Use either the script ID or ARN value. To retrieve all fleets, leave this parameter empty.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFleets' value with any optional fields omitted.
mkListFleets
    :: ListFleets
mkListFleets
  = ListFleets'{buildId = Core.Nothing, limit = Core.Nothing,
                nextToken = Core.Nothing, scriptId = Core.Nothing}

-- | A unique identifier for a build to return fleets for. Use this parameter to return only fleets using a specified build. Use either the build ID or ARN value. To retrieve all fleets, do not include either a BuildId and ScriptID parameter.
--
-- /Note:/ Consider using 'buildId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfBuildId :: Lens.Lens' ListFleets (Core.Maybe Types.BuildIdOrArn)
lfBuildId = Lens.field @"buildId"
{-# INLINEABLE lfBuildId #-}
{-# DEPRECATED buildId "Use generic-lens or generic-optics with 'buildId' instead"  #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfLimit :: Lens.Lens' ListFleets (Core.Maybe Core.Natural)
lfLimit = Lens.field @"limit"
{-# INLINEABLE lfLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfNextToken :: Lens.Lens' ListFleets (Core.Maybe Types.NonZeroAndMaxString)
lfNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | A unique identifier for a Realtime script to return fleets for. Use this parameter to return only fleets using a specified script. Use either the script ID or ARN value. To retrieve all fleets, leave this parameter empty.
--
-- /Note:/ Consider using 'scriptId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfScriptId :: Lens.Lens' ListFleets (Core.Maybe Types.ScriptIdOrArn)
lfScriptId = Lens.field @"scriptId"
{-# INLINEABLE lfScriptId #-}
{-# DEPRECATED scriptId "Use generic-lens or generic-optics with 'scriptId' instead"  #-}

instance Core.ToQuery ListFleets where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders ListFleets where
        toHeaders ListFleets{..}
          = Core.pure ("X-Amz-Target", "GameLift.ListFleets") Core.<>
              Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON ListFleets where
        toJSON ListFleets{..}
          = Core.object
              (Core.catMaybes
                 [("BuildId" Core..=) Core.<$> buildId,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken,
                  ("ScriptId" Core..=) Core.<$> scriptId])

instance Core.AWSRequest ListFleets where
        type Rs ListFleets = ListFleetsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListFleetsResponse' Core.<$>
                   (x Core..:? "FleetIds") Core.<*> x Core..:? "NextToken" Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListFleets where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop (rs Lens.^? Lens.field @"fleetIds" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkListFleetsResponse' smart constructor.
data ListFleetsResponse = ListFleetsResponse'
  { fleetIds :: Core.Maybe (Core.NonEmpty Types.FleetId)
    -- ^ Set of fleet IDs matching the list request. You can retrieve additional information about all returned fleets by passing this result set to a call to 'DescribeFleetAttributes' , 'DescribeFleetCapacity' , or 'DescribeFleetUtilization' .
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListFleetsResponse' value with any optional fields omitted.
mkListFleetsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListFleetsResponse
mkListFleetsResponse responseStatus
  = ListFleetsResponse'{fleetIds = Core.Nothing,
                        nextToken = Core.Nothing, responseStatus}

-- | Set of fleet IDs matching the list request. You can retrieve additional information about all returned fleets by passing this result set to a call to 'DescribeFleetAttributes' , 'DescribeFleetCapacity' , or 'DescribeFleetUtilization' .
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsFleetIds :: Lens.Lens' ListFleetsResponse (Core.Maybe (Core.NonEmpty Types.FleetId))
lfrrsFleetIds = Lens.field @"fleetIds"
{-# INLINEABLE lfrrsFleetIds #-}
{-# DEPRECATED fleetIds "Use generic-lens or generic-optics with 'fleetIds' instead"  #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsNextToken :: Lens.Lens' ListFleetsResponse (Core.Maybe Types.NonZeroAndMaxString)
lfrrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE lfrrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lfrrsResponseStatus :: Lens.Lens' ListFleetsResponse Core.Int
lfrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE lfrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
