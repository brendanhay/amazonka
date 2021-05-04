{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.Types.VpcPeeringConnection
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.VpcPeeringConnection where

import Network.AWS.GameLift.Types.VpcPeeringConnectionStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents a peering connection between a VPC on one of your AWS
-- accounts and the VPC for your Amazon GameLift fleets. This record may be
-- for an active peering connection or a pending connection that has not
-- yet been established.
--
-- -   CreateVpcPeeringAuthorization
--
-- -   DescribeVpcPeeringAuthorizations
--
-- -   DeleteVpcPeeringAuthorization
--
-- -   CreateVpcPeeringConnection
--
-- -   DescribeVpcPeeringConnections
--
-- -   DeleteVpcPeeringConnection
--
-- /See:/ 'newVpcPeeringConnection' smart constructor.
data VpcPeeringConnection = VpcPeeringConnection'
  { -- | The status information about the connection. Status indicates if a
    -- connection is pending, successful, or failed.
    status :: Prelude.Maybe VpcPeeringConnectionStatus,
    -- | A unique identifier that is automatically assigned to the connection
    -- record. This ID is referenced in VPC peering connection events, and is
    -- used when deleting a connection with DeleteVpcPeeringConnection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | CIDR block of IPv4 addresses assigned to the VPC peering connection for
    -- the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated
    -- with it; these blocks cannot overlap or the peering connection cannot be
    -- created.
    ipV4CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
    -- associated with the GameLift fleet resource for this connection.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a fleet. This ID determines the ID of the Amazon
    -- GameLift VPC for your fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the VPC that contains the Amazon GameLift fleet
    -- for this connection. This VPC is managed by Amazon GameLift and does not
    -- appear in your AWS account.
    gameLiftVpcId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- Amazon GameLift fleet. The VPC must be in the same Region where your
    -- fleet is deployed. Look up a VPC ID using the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
    -- Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
    peerVpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'VpcPeeringConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'vpcPeeringConnection_status' - The status information about the connection. Status indicates if a
-- connection is pending, successful, or failed.
--
-- 'vpcPeeringConnectionId', 'vpcPeeringConnection_vpcPeeringConnectionId' - A unique identifier that is automatically assigned to the connection
-- record. This ID is referenced in VPC peering connection events, and is
-- used when deleting a connection with DeleteVpcPeeringConnection.
--
-- 'ipV4CidrBlock', 'vpcPeeringConnection_ipV4CidrBlock' - CIDR block of IPv4 addresses assigned to the VPC peering connection for
-- the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated
-- with it; these blocks cannot overlap or the peering connection cannot be
-- created.
--
-- 'fleetArn', 'vpcPeeringConnection_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift fleet resource for this connection.
--
-- 'fleetId', 'vpcPeeringConnection_fleetId' - A unique identifier for a fleet. This ID determines the ID of the Amazon
-- GameLift VPC for your fleet.
--
-- 'gameLiftVpcId', 'vpcPeeringConnection_gameLiftVpcId' - A unique identifier for the VPC that contains the Amazon GameLift fleet
-- for this connection. This VPC is managed by Amazon GameLift and does not
-- appear in your AWS account.
--
-- 'peerVpcId', 'vpcPeeringConnection_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region where your
-- fleet is deployed. Look up a VPC ID using the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
newVpcPeeringConnection ::
  VpcPeeringConnection
newVpcPeeringConnection =
  VpcPeeringConnection'
    { status = Prelude.Nothing,
      vpcPeeringConnectionId = Prelude.Nothing,
      ipV4CidrBlock = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      gameLiftVpcId = Prelude.Nothing,
      peerVpcId = Prelude.Nothing
    }

-- | The status information about the connection. Status indicates if a
-- connection is pending, successful, or failed.
vpcPeeringConnection_status :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe VpcPeeringConnectionStatus)
vpcPeeringConnection_status = Lens.lens (\VpcPeeringConnection' {status} -> status) (\s@VpcPeeringConnection' {} a -> s {status = a} :: VpcPeeringConnection)

-- | A unique identifier that is automatically assigned to the connection
-- record. This ID is referenced in VPC peering connection events, and is
-- used when deleting a connection with DeleteVpcPeeringConnection.
vpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\VpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@VpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: VpcPeeringConnection)

-- | CIDR block of IPv4 addresses assigned to the VPC peering connection for
-- the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated
-- with it; these blocks cannot overlap or the peering connection cannot be
-- created.
vpcPeeringConnection_ipV4CidrBlock :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_ipV4CidrBlock = Lens.lens (\VpcPeeringConnection' {ipV4CidrBlock} -> ipV4CidrBlock) (\s@VpcPeeringConnection' {} a -> s {ipV4CidrBlock = a} :: VpcPeeringConnection)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/general/latest/gr/aws-arns-and-namespaces.html ARN>)
-- associated with the GameLift fleet resource for this connection.
vpcPeeringConnection_fleetArn :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_fleetArn = Lens.lens (\VpcPeeringConnection' {fleetArn} -> fleetArn) (\s@VpcPeeringConnection' {} a -> s {fleetArn = a} :: VpcPeeringConnection)

-- | A unique identifier for a fleet. This ID determines the ID of the Amazon
-- GameLift VPC for your fleet.
vpcPeeringConnection_fleetId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_fleetId = Lens.lens (\VpcPeeringConnection' {fleetId} -> fleetId) (\s@VpcPeeringConnection' {} a -> s {fleetId = a} :: VpcPeeringConnection)

-- | A unique identifier for the VPC that contains the Amazon GameLift fleet
-- for this connection. This VPC is managed by Amazon GameLift and does not
-- appear in your AWS account.
vpcPeeringConnection_gameLiftVpcId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_gameLiftVpcId = Lens.lens (\VpcPeeringConnection' {gameLiftVpcId} -> gameLiftVpcId) (\s@VpcPeeringConnection' {} a -> s {gameLiftVpcId = a} :: VpcPeeringConnection)

-- | A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region where your
-- fleet is deployed. Look up a VPC ID using the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
vpcPeeringConnection_peerVpcId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_peerVpcId = Lens.lens (\VpcPeeringConnection' {peerVpcId} -> peerVpcId) (\s@VpcPeeringConnection' {} a -> s {peerVpcId = a} :: VpcPeeringConnection)

instance Prelude.FromJSON VpcPeeringConnection where
  parseJSON =
    Prelude.withObject
      "VpcPeeringConnection"
      ( \x ->
          VpcPeeringConnection'
            Prelude.<$> (x Prelude..:? "Status")
            Prelude.<*> (x Prelude..:? "VpcPeeringConnectionId")
            Prelude.<*> (x Prelude..:? "IpV4CidrBlock")
            Prelude.<*> (x Prelude..:? "FleetArn")
            Prelude.<*> (x Prelude..:? "FleetId")
            Prelude.<*> (x Prelude..:? "GameLiftVpcId")
            Prelude.<*> (x Prelude..:? "PeerVpcId")
      )

instance Prelude.Hashable VpcPeeringConnection

instance Prelude.NFData VpcPeeringConnection
