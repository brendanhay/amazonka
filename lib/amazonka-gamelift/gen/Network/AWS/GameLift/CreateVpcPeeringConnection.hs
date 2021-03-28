{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.CreateVpcPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Establishes a VPC peering connection between a virtual private cloud (VPC) in an AWS account with the VPC for your Amazon GameLift fleet. VPC peering enables the game servers on your fleet to communicate directly with other AWS resources. You can peer with VPCs in any AWS account that you have access to, including the account that you use to manage your Amazon GameLift fleets. You cannot peer with VPCs that are in different Regions. For more information, see <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- Before calling this operation to establish the peering connection, you first need to call 'CreateVpcPeeringAuthorization' and identify the VPC you want to peer with. Once the authorization for the specified VPC is issued, you have 24 hours to establish the connection. These two operations handle all tasks necessary to peer the two VPCs, including acceptance, updating routing tables, etc. 
-- To establish the connection, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Identify the following values: (1) The ID of the fleet you want to be enable a VPC peering connection for; (2) The AWS account with the VPC that you want to peer with; and (3) The ID of the VPC you want to peer with. This operation is asynchronous. If successful, a 'VpcPeeringConnection' request is created. You can use continuous polling to track the request's status using 'DescribeVpcPeeringConnections' , or by monitoring fleet events for success or failure using 'DescribeFleetEvents' . 
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
module Network.AWS.GameLift.CreateVpcPeeringConnection
    (
    -- * Creating a request
      CreateVpcPeeringConnection (..)
    , mkCreateVpcPeeringConnection
    -- ** Request lenses
    , cvpcFleetId
    , cvpcPeerVpcAwsAccountId
    , cvpcPeerVpcId

    -- * Destructuring the response
    , CreateVpcPeeringConnectionResponse (..)
    , mkCreateVpcPeeringConnectionResponse
    -- ** Response lenses
    , cvpcrrsResponseStatus
    ) where

import qualified Network.AWS.GameLift.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'mkCreateVpcPeeringConnection' smart constructor.
data CreateVpcPeeringConnection = CreateVpcPeeringConnection'
  { fleetId :: Types.FleetId
    -- ^ A unique identifier for a fleet. You can use either the fleet ID or ARN value. This tells Amazon GameLift which GameLift VPC to peer with. 
  , peerVpcAwsAccountId :: Types.NonZeroAndMaxString
    -- ^ A unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your Account ID in the AWS Management Console under account settings.
  , peerVpcId :: Types.NonZeroAndMaxString
    -- ^ A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpcPeeringConnection' value with any optional fields omitted.
mkCreateVpcPeeringConnection
    :: Types.FleetId -- ^ 'fleetId'
    -> Types.NonZeroAndMaxString -- ^ 'peerVpcAwsAccountId'
    -> Types.NonZeroAndMaxString -- ^ 'peerVpcId'
    -> CreateVpcPeeringConnection
mkCreateVpcPeeringConnection fleetId peerVpcAwsAccountId peerVpcId
  = CreateVpcPeeringConnection'{fleetId, peerVpcAwsAccountId,
                                peerVpcId}

-- | A unique identifier for a fleet. You can use either the fleet ID or ARN value. This tells Amazon GameLift which GameLift VPC to peer with. 
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcFleetId :: Lens.Lens' CreateVpcPeeringConnection Types.FleetId
cvpcFleetId = Lens.field @"fleetId"
{-# INLINEABLE cvpcFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | A unique identifier for the AWS account with the VPC that you want to peer your Amazon GameLift fleet with. You can find your Account ID in the AWS Management Console under account settings.
--
-- /Note:/ Consider using 'peerVpcAwsAccountId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcPeerVpcAwsAccountId :: Lens.Lens' CreateVpcPeeringConnection Types.NonZeroAndMaxString
cvpcPeerVpcAwsAccountId = Lens.field @"peerVpcAwsAccountId"
{-# INLINEABLE cvpcPeerVpcAwsAccountId #-}
{-# DEPRECATED peerVpcAwsAccountId "Use generic-lens or generic-optics with 'peerVpcAwsAccountId' instead"  #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcPeerVpcId :: Lens.Lens' CreateVpcPeeringConnection Types.NonZeroAndMaxString
cvpcPeerVpcId = Lens.field @"peerVpcId"
{-# INLINEABLE cvpcPeerVpcId #-}
{-# DEPRECATED peerVpcId "Use generic-lens or generic-optics with 'peerVpcId' instead"  #-}

instance Core.ToQuery CreateVpcPeeringConnection where
        toQuery _ = Core.pure Core.mempty

instance Core.ToHeaders CreateVpcPeeringConnection where
        toHeaders CreateVpcPeeringConnection{..}
          = Core.pure ("X-Amz-Target", "GameLift.CreateVpcPeeringConnection")
              Core.<> Core.pure ("Content-Type", "application/x-amz-json-1.1")

instance Core.FromJSON CreateVpcPeeringConnection where
        toJSON CreateVpcPeeringConnection{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("FleetId" Core..= fleetId),
                  Core.Just ("PeerVpcAwsAccountId" Core..= peerVpcAwsAccountId),
                  Core.Just ("PeerVpcId" Core..= peerVpcId)])

instance Core.AWSRequest CreateVpcPeeringConnection where
        type Rs CreateVpcPeeringConnection =
             CreateVpcPeeringConnectionResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.POST, Core._rqPath = "/",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = Core.toJSONBody x}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveEmpty
              (\ s h x ->
                 CreateVpcPeeringConnectionResponse' Core.<$>
                   (Core.pure (Core.fromEnum s)))
        
        {-# INLINE parseResponse #-}

-- | /See:/ 'mkCreateVpcPeeringConnectionResponse' smart constructor.
newtype CreateVpcPeeringConnectionResponse = CreateVpcPeeringConnectionResponse'
  { responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'CreateVpcPeeringConnectionResponse' value with any optional fields omitted.
mkCreateVpcPeeringConnectionResponse
    :: Core.Int -- ^ 'responseStatus'
    -> CreateVpcPeeringConnectionResponse
mkCreateVpcPeeringConnectionResponse responseStatus
  = CreateVpcPeeringConnectionResponse'{responseStatus}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cvpcrrsResponseStatus :: Lens.Lens' CreateVpcPeeringConnectionResponse Core.Int
cvpcrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE cvpcrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
