{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeVpcPeeringConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information on VPC peering connections. Use this operation to get peering information for all fleets or for one specific fleet ID. 
--
-- To retrieve connection information, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Specify a fleet ID or leave the parameter empty to retrieve all connection records. If successful, the retrieved information includes both active and pending connections. Active connections identify the IpV4 CIDR block that the VPC uses to connect. 
--
--     * 'CreateVpcPeeringAuthorization' 
--
--
--     * 'DescribeVpcPeeringAuthorizations' 
--
--
--     * 'DeleteVpcPeeringAuthorization' 
--
--
--     * 'CreateVpcPeeringConnection' 
--
--
--     * 'DescribeVpcPeeringConnections' 
--
--
--     * 'DeleteVpcPeeringConnection' 
--
--
module Network.AWS.GameLift.DescribeVpcPeeringConnections
    (
    -- * Creating a request
      DescribeVpcPeeringConnections (..)
    , mkDescribeVpcPeeringConnections
    -- ** Request lenses
    , dvpcsFleetId

    -- * Destructuring the response
    , DescribeVpcPeeringConnectionsResponse (..)
    , mkDescribeVpcPeeringConnectionsResponse
    -- ** Response lenses
    , dvpcrfrsVpcPeeringConnections
    , dvpcrfrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeVpcPeeringConnections' smart constructor.
newtype DescribeVpcPeeringConnections = DescribeVpcPeeringConnections'
  { fleetId :: Core.Maybe Types.FleetId
    -- ^ A unique identifier for a fleet. You can use either the fleet ID or ARN value.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcPeeringConnections' value with any optional fields omitted.
mkDescribeVpcPeeringConnections
    :: DescribeVpcPeeringConnections
mkDescribeVpcPeeringConnections
  = DescribeVpcPeeringConnections'{fleetId = Core.Nothing}

-- | A unique identifier for a fleet. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcsFleetId :: Lens.Lens' DescribeVpcPeeringConnections (Core.Maybe Types.FleetId)
dvpcsFleetId = Lens.field @"fleetId"
{-# INLINEABLE dvpcsFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

instance Core.ToQuery DescribeVpcPeeringConnections where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders DescribeVpcPeeringConnections where
        toHeaders DescribeVpcPeeringConnections{..}
          = Core.pure
              ("X-Amz-Target", "GameLift.DescribeVpcPeeringConnections")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON DescribeVpcPeeringConnections where
        toJSON DescribeVpcPeeringConnections{..}
          = Core.object
              (Core.catMaybes [("FleetId" Core..=) Core.<$> fleetId])

instance Core.AWSRequest DescribeVpcPeeringConnections where
        type Rs DescribeVpcPeeringConnections =
             DescribeVpcPeeringConnectionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 DescribeVpcPeeringConnectionsResponse' Core.<$>
                   (x Core..:? "VpcPeeringConnections") Core.<*>
                     Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeVpcPeeringConnectionsResponse' smart constructor.
data DescribeVpcPeeringConnectionsResponse = DescribeVpcPeeringConnectionsResponse'
  { vpcPeeringConnections :: Core.Maybe [Types.VpcPeeringConnection]
    -- ^ A collection of VPC peering connection records that match the request.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DescribeVpcPeeringConnectionsResponse' value with any optional fields omitted.
mkDescribeVpcPeeringConnectionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> DescribeVpcPeeringConnectionsResponse
mkDescribeVpcPeeringConnectionsResponse responseStatus
  = DescribeVpcPeeringConnectionsResponse'{vpcPeeringConnections =
                                             Core.Nothing,
                                           responseStatus}

-- | A collection of VPC peering connection records that match the request.
--
-- /Note:/ Consider using 'vpcPeeringConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrfrsVpcPeeringConnections :: Lens.Lens' DescribeVpcPeeringConnectionsResponse (Core.Maybe [Types.VpcPeeringConnection])
dvpcrfrsVpcPeeringConnections = Lens.field @"vpcPeeringConnections"
{-# INLINEABLE dvpcrfrsVpcPeeringConnections #-}
{-# DEPRECATED vpcPeeringConnections "Use generic-lens or generic-optics with 'vpcPeeringConnections' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrfrsResponseStatus :: Lens.Lens' DescribeVpcPeeringConnectionsResponse Core.Int
dvpcrfrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE dvpcrfrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
