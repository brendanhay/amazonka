{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.VpcPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.GameLift.Types.VpcPeeringConnection
  ( VpcPeeringConnection (..)
  -- * Smart constructor
  , mkVpcPeeringConnection
  -- * Lenses
  , vpcFleetArn
  , vpcFleetId
  , vpcGameLiftVpcId
  , vpcIpV4CidrBlock
  , vpcPeerVpcId
  , vpcStatus
  , vpcVpcPeeringConnectionId
  ) where

import qualified Network.AWS.GameLift.Types.FleetArn as Types
import qualified Network.AWS.GameLift.Types.FleetId as Types
import qualified Network.AWS.GameLift.Types.NonZeroAndMaxString as Types
import qualified Network.AWS.GameLift.Types.VpcPeeringConnectionStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents a peering connection between a VPC on one of your AWS accounts and the VPC for your Amazon GameLift fleets. This record may be for an active peering connection or a pending connection that has not yet been established.
--
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
--
-- /See:/ 'mkVpcPeeringConnection' smart constructor.
data VpcPeeringConnection = VpcPeeringConnection'
  { fleetArn :: Core.Maybe Types.FleetArn
    -- ^ The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource for this connection. 
  , fleetId :: Core.Maybe Types.FleetId
    -- ^ A unique identifier for a fleet. This ID determines the ID of the Amazon GameLift VPC for your fleet.
  , gameLiftVpcId :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A unique identifier for the VPC that contains the Amazon GameLift fleet for this connection. This VPC is managed by Amazon GameLift and does not appear in your AWS account. 
  , ipV4CidrBlock :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ CIDR block of IPv4 addresses assigned to the VPC peering connection for the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated with it; these blocks cannot overlap or the peering connection cannot be created. 
  , peerVpcId :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
  , status :: Core.Maybe Types.VpcPeeringConnectionStatus
    -- ^ The status information about the connection. Status indicates if a connection is pending, successful, or failed.
  , vpcPeeringConnectionId :: Core.Maybe Types.NonZeroAndMaxString
    -- ^ A unique identifier that is automatically assigned to the connection record. This ID is referenced in VPC peering connection events, and is used when deleting a connection with 'DeleteVpcPeeringConnection' . 
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'VpcPeeringConnection' value with any optional fields omitted.
mkVpcPeeringConnection
    :: VpcPeeringConnection
mkVpcPeeringConnection
  = VpcPeeringConnection'{fleetArn = Core.Nothing,
                          fleetId = Core.Nothing, gameLiftVpcId = Core.Nothing,
                          ipV4CidrBlock = Core.Nothing, peerVpcId = Core.Nothing,
                          status = Core.Nothing, vpcPeeringConnectionId = Core.Nothing}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource for this connection. 
--
-- /Note:/ Consider using 'fleetArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcFleetArn :: Lens.Lens' VpcPeeringConnection (Core.Maybe Types.FleetArn)
vpcFleetArn = Lens.field @"fleetArn"
{-# INLINEABLE vpcFleetArn #-}
{-# DEPRECATED fleetArn "Use generic-lens or generic-optics with 'fleetArn' instead"  #-}

-- | A unique identifier for a fleet. This ID determines the ID of the Amazon GameLift VPC for your fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcFleetId :: Lens.Lens' VpcPeeringConnection (Core.Maybe Types.FleetId)
vpcFleetId = Lens.field @"fleetId"
{-# INLINEABLE vpcFleetId #-}
{-# DEPRECATED fleetId "Use generic-lens or generic-optics with 'fleetId' instead"  #-}

-- | A unique identifier for the VPC that contains the Amazon GameLift fleet for this connection. This VPC is managed by Amazon GameLift and does not appear in your AWS account. 
--
-- /Note:/ Consider using 'gameLiftVpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcGameLiftVpcId :: Lens.Lens' VpcPeeringConnection (Core.Maybe Types.NonZeroAndMaxString)
vpcGameLiftVpcId = Lens.field @"gameLiftVpcId"
{-# INLINEABLE vpcGameLiftVpcId #-}
{-# DEPRECATED gameLiftVpcId "Use generic-lens or generic-optics with 'gameLiftVpcId' instead"  #-}

-- | CIDR block of IPv4 addresses assigned to the VPC peering connection for the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated with it; these blocks cannot overlap or the peering connection cannot be created. 
--
-- /Note:/ Consider using 'ipV4CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcIpV4CidrBlock :: Lens.Lens' VpcPeeringConnection (Core.Maybe Types.NonZeroAndMaxString)
vpcIpV4CidrBlock = Lens.field @"ipV4CidrBlock"
{-# INLINEABLE vpcIpV4CidrBlock #-}
{-# DEPRECATED ipV4CidrBlock "Use generic-lens or generic-optics with 'ipV4CidrBlock' instead"  #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcPeerVpcId :: Lens.Lens' VpcPeeringConnection (Core.Maybe Types.NonZeroAndMaxString)
vpcPeerVpcId = Lens.field @"peerVpcId"
{-# INLINEABLE vpcPeerVpcId #-}
{-# DEPRECATED peerVpcId "Use generic-lens or generic-optics with 'peerVpcId' instead"  #-}

-- | The status information about the connection. Status indicates if a connection is pending, successful, or failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcStatus :: Lens.Lens' VpcPeeringConnection (Core.Maybe Types.VpcPeeringConnectionStatus)
vpcStatus = Lens.field @"status"
{-# INLINEABLE vpcStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | A unique identifier that is automatically assigned to the connection record. This ID is referenced in VPC peering connection events, and is used when deleting a connection with 'DeleteVpcPeeringConnection' . 
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcVpcPeeringConnectionId :: Lens.Lens' VpcPeeringConnection (Core.Maybe Types.NonZeroAndMaxString)
vpcVpcPeeringConnectionId = Lens.field @"vpcPeeringConnectionId"
{-# INLINEABLE vpcVpcPeeringConnectionId #-}
{-# DEPRECATED vpcPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead"  #-}

instance Core.FromJSON VpcPeeringConnection where
        parseJSON
          = Core.withObject "VpcPeeringConnection" Core.$
              \ x ->
                VpcPeeringConnection' Core.<$>
                  (x Core..:? "FleetArn") Core.<*> x Core..:? "FleetId" Core.<*>
                    x Core..:? "GameLiftVpcId"
                    Core.<*> x Core..:? "IpV4CidrBlock"
                    Core.<*> x Core..:? "PeerVpcId"
                    Core.<*> x Core..:? "Status"
                    Core.<*> x Core..:? "VpcPeeringConnectionId"
