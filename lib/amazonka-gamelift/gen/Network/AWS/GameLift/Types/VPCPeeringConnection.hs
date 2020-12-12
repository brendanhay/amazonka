{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.VPCPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.VPCPeeringConnection
  ( VPCPeeringConnection (..),

    -- * Smart constructor
    mkVPCPeeringConnection,

    -- * Lenses
    vpcVPCPeeringConnectionId,
    vpcStatus,
    vpcPeerVPCId,
    vpcFleetARN,
    vpcIPV4CidrBlock,
    vpcGameLiftVPCId,
    vpcFleetId,
  )
where

import Network.AWS.GameLift.Types.VPCPeeringConnectionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

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
-- /See:/ 'mkVPCPeeringConnection' smart constructor.
data VPCPeeringConnection = VPCPeeringConnection'
  { vpcPeeringConnectionId ::
      Lude.Maybe Lude.Text,
    status :: Lude.Maybe VPCPeeringConnectionStatus,
    peerVPCId :: Lude.Maybe Lude.Text,
    fleetARN :: Lude.Maybe Lude.Text,
    ipV4CidrBlock :: Lude.Maybe Lude.Text,
    gameLiftVPCId :: Lude.Maybe Lude.Text,
    fleetId :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'VPCPeeringConnection' with the minimum fields required to make a request.
--
-- * 'fleetARN' - The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource for this connection.
-- * 'fleetId' - A unique identifier for a fleet. This ID determines the ID of the Amazon GameLift VPC for your fleet.
-- * 'gameLiftVPCId' - A unique identifier for the VPC that contains the Amazon GameLift fleet for this connection. This VPC is managed by Amazon GameLift and does not appear in your AWS account.
-- * 'ipV4CidrBlock' - CIDR block of IPv4 addresses assigned to the VPC peering connection for the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated with it; these blocks cannot overlap or the peering connection cannot be created.
-- * 'peerVPCId' - A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
-- * 'status' - The status information about the connection. Status indicates if a connection is pending, successful, or failed.
-- * 'vpcPeeringConnectionId' - A unique identifier that is automatically assigned to the connection record. This ID is referenced in VPC peering connection events, and is used when deleting a connection with 'DeleteVpcPeeringConnection' .
mkVPCPeeringConnection ::
  VPCPeeringConnection
mkVPCPeeringConnection =
  VPCPeeringConnection'
    { vpcPeeringConnectionId = Lude.Nothing,
      status = Lude.Nothing,
      peerVPCId = Lude.Nothing,
      fleetARN = Lude.Nothing,
      ipV4CidrBlock = Lude.Nothing,
      gameLiftVPCId = Lude.Nothing,
      fleetId = Lude.Nothing
    }

-- | A unique identifier that is automatically assigned to the connection record. This ID is referenced in VPC peering connection events, and is used when deleting a connection with 'DeleteVpcPeeringConnection' .
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcVPCPeeringConnectionId :: Lens.Lens' VPCPeeringConnection (Lude.Maybe Lude.Text)
vpcVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: VPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | The status information about the connection. Status indicates if a connection is pending, successful, or failed.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcStatus :: Lens.Lens' VPCPeeringConnection (Lude.Maybe VPCPeeringConnectionStatus)
vpcStatus = Lens.lens (status :: VPCPeeringConnection -> Lude.Maybe VPCPeeringConnectionStatus) (\s a -> s {status = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | A unique identifier for a VPC with resources to be accessed by your Amazon GameLift fleet. The VPC must be in the same Region where your fleet is deployed. Look up a VPC ID using the <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS Management Console. Learn more about VPC peering in <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets> .
--
-- /Note:/ Consider using 'peerVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcPeerVPCId :: Lens.Lens' VPCPeeringConnection (Lude.Maybe Lude.Text)
vpcPeerVPCId = Lens.lens (peerVPCId :: VPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {peerVPCId = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcPeerVPCId "Use generic-lens or generic-optics with 'peerVPCId' instead." #-}

-- | The Amazon Resource Name (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN> ) associated with the GameLift fleet resource for this connection.
--
-- /Note:/ Consider using 'fleetARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcFleetARN :: Lens.Lens' VPCPeeringConnection (Lude.Maybe Lude.Text)
vpcFleetARN = Lens.lens (fleetARN :: VPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {fleetARN = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcFleetARN "Use generic-lens or generic-optics with 'fleetARN' instead." #-}

-- | CIDR block of IPv4 addresses assigned to the VPC peering connection for the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated with it; these blocks cannot overlap or the peering connection cannot be created.
--
-- /Note:/ Consider using 'ipV4CidrBlock' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcIPV4CidrBlock :: Lens.Lens' VPCPeeringConnection (Lude.Maybe Lude.Text)
vpcIPV4CidrBlock = Lens.lens (ipV4CidrBlock :: VPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {ipV4CidrBlock = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcIPV4CidrBlock "Use generic-lens or generic-optics with 'ipV4CidrBlock' instead." #-}

-- | A unique identifier for the VPC that contains the Amazon GameLift fleet for this connection. This VPC is managed by Amazon GameLift and does not appear in your AWS account.
--
-- /Note:/ Consider using 'gameLiftVPCId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcGameLiftVPCId :: Lens.Lens' VPCPeeringConnection (Lude.Maybe Lude.Text)
vpcGameLiftVPCId = Lens.lens (gameLiftVPCId :: VPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {gameLiftVPCId = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcGameLiftVPCId "Use generic-lens or generic-optics with 'gameLiftVPCId' instead." #-}

-- | A unique identifier for a fleet. This ID determines the ID of the Amazon GameLift VPC for your fleet.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
vpcFleetId :: Lens.Lens' VPCPeeringConnection (Lude.Maybe Lude.Text)
vpcFleetId = Lens.lens (fleetId :: VPCPeeringConnection -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: VPCPeeringConnection)
{-# DEPRECATED vpcFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.FromJSON VPCPeeringConnection where
  parseJSON =
    Lude.withObject
      "VPCPeeringConnection"
      ( \x ->
          VPCPeeringConnection'
            Lude.<$> (x Lude..:? "VpcPeeringConnectionId")
            Lude.<*> (x Lude..:? "Status")
            Lude.<*> (x Lude..:? "PeerVpcId")
            Lude.<*> (x Lude..:? "FleetArn")
            Lude.<*> (x Lude..:? "IpV4CidrBlock")
            Lude.<*> (x Lude..:? "GameLiftVpcId")
            Lude.<*> (x Lude..:? "FleetId")
      )
