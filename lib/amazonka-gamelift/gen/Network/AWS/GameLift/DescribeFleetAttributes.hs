{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeFleetAttributes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves core properties, including configuration, status, and metadata, for a fleet. 
--
-- To get attributes for one or more fleets, provide a list of fleet IDs or fleet ARNs. To get attributes for all fleets, do not specify a fleet identifier. When requesting attributes for multiple fleets, use the pagination parameters to retrieve results as a set of sequential pages. If successful, a 'FleetAttributes' object is returned for each fleet requested, unless the fleet identifier is not found.
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
--     * Describe fleets:
--
--     * 'DescribeFleetAttributes' 
--
--
--     * 'DescribeFleetCapacity' 
--
--
--     * 'DescribeFleetPortSettings' 
--
--
--     * 'DescribeFleetUtilization' 
--
--
--     * 'DescribeRuntimeConfiguration' 
--
--
--     * 'DescribeEC2InstanceLimits' 
--
--
--     * 'DescribeFleetEvents' 
--
--
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
module Network.AWS.GameLift.DescribeFleetAttributes
    (
    -- * Creating a request
      DescribeFleetAttributes (..)
    , mkDescribeFleetAttributes
    -- ** Request lenses
    , dfaFleetIds
    , dfaLimit
    , dfaNextToken

    -- * Destructuring the response
    , DescribeFleetAttributesResponse (..)
    , mkDescribeFleetAttributesResponse
    -- ** Response lenses
    , dfarrsFleetAttributes
    , dfarrsNextToken
    , dfarrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeFleetAttributes' smart constructor.
data DescribeFleetAttributes = DescribeFleetAttributes'
  { fleetIds :: Core.Maybe (Core.NonEmpty Types.FleetIdOrArn)
    -- ^ A list of unique fleet identifiers to retrieve attributes for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
  , limit :: Core.Maybe Core.Natural
    -- ^ The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeFleetAttributes' value with any optional fields omitted.
mkDescribeFleetAttributes
    :: DescribeFleetAttributes
mkDescribeFleetAttributes
  = DescribeFleetAttributes'{fleetIds = Core.Nothing,
                             limit = Core.Nothing, nextToken = Core.Nothing}

-- | A list of unique fleet identifiers to retrieve attributes for. You can use either the fleet ID or ARN value. To retrieve attributes for all current fleets, do not include this parameter. If the list of fleet identifiers includes fleets that don't currently exist, the request succeeds but no attributes for that fleet are returned.
--
-- /Note:/ Consider using 'fleetIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfaFleetIds :: Lens.Lens' DescribeFleetAttributes (Core.Maybe (Core.NonEmpty Types.FleetIdOrArn))
dfaFleetIds = Lens.field @"fleetIds"
{-# INLINEABLE dfaFleetIds #-}
{-# DEPRECATED fleetIds "Use generic-lens or generic-optics with 'fleetIds' instead"  #-}

-- | The maximum number of results to return. Use this parameter with @NextToken@ to get results as a set of sequential pages. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfaLimit :: Lens.Lens' DescribeFleetAttributes (Core.Maybe Core.Natural)
dfaLimit = Lens.field @"limit"
{-# INLINEABLE dfaLimit #-}
{-# DEPRECATED limit "Use generic-lens or generic-optics with 'limit' instead"  #-}

-- | Token that indicates the start of the next sequential page of results. Use the token that is returned with a previous call to this operation. To start at the beginning of the result set, do not specify a value. This parameter is ignored when the request specifies one or a list of fleet IDs.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfaNextToken :: Lens.Lens' DescribeFleetAttributes (Core.Maybe Types.NonZeroAndMaxString)
dfaNextToken = Lens.field @"nextToken"
{-# INLINEABLE dfaNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

instance Core.ToQuery DescribeFleetAttributes where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeFleetAttributes where
        toHeaders DescribeFleetAttributes{..}
          = Core.pure ("X-Amz-Target", "GameLift.DescribeFleetAttributes")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeFleetAttributes where
        toJSON DescribeFleetAttributes{..}
          = Core.object
              (Core.catMaybes
                 [("FleetIds" Core..=) Core.<$> fleetIds,
                  ("Limit" Core..=) Core.<$> limit,
                  ("NextToken" Core..=) Core.<$> nextToken])

instance Core.AWSRequest DescribeFleetAttributes where
        type Rs DescribeFleetAttributes = DescribeFleetAttributesResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeFleetAttributesResponse' Core.<$>
                   (x Core..:? "FleetAttributes") Core.<*> x Core..:? "NextToken"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager DescribeFleetAttributes where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextToken") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"fleetAttributes" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"nextToken" Lens..~ rs Lens.^. Lens.field @"nextToken")

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeFleetAttributesResponse' smart constructor.
data DescribeFleetAttributesResponse = DescribeFleetAttributesResponse'
  { fleetAttributes :: Core.Maybe [Types.FleetAttributes]
    -- ^ A collection of objects containing attribute metadata for each requested fleet ID. Attribute objects are returned only for fleets that currently exist.
  , nextToken :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DescribeFleetAttributesResponse' value with any optional fields omitted.
mkDescribeFleetAttributesResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeFleetAttributesResponse
mkDescribeFleetAttributesResponse responseStatus
  = DescribeFleetAttributesResponse'{fleetAttributes = Core.Nothing,
                                     nextToken = Core.Nothing, responseStatus}

-- | A collection of objects containing attribute metadata for each requested fleet ID. Attribute objects are returned only for fleets that currently exist.
--
-- /Note:/ Consider using 'fleetAttributes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfarrsFleetAttributes :: Lens.Lens' DescribeFleetAttributesResponse (Core.Maybe [Types.FleetAttributes])
dfarrsFleetAttributes = Lens.field @"fleetAttributes"
{-# INLINEABLE dfarrsFleetAttributes #-}
{-# DEPRECATED fleetAttributes "Use generic-lens or generic-optics with 'fleetAttributes' instead"  #-}

-- | Token that indicates where to resume retrieving results on the next call to this operation. If no token is returned, these results represent the end of the list.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfarrsNextToken :: Lens.Lens' DescribeFleetAttributesResponse (Core.Maybe Types.NonZeroAndMaxString)
dfarrsNextToken = Lens.field @"nextToken"
{-# INLINEABLE dfarrsNextToken #-}
{-# DEPRECATED nextToken "Use generic-lens or generic-optics with 'nextToken' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dfarrsResponseStatus :: Lens.Lens' DescribeFleetAttributesResponse Core.Int
dfarrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dfarrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
