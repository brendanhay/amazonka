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
-- Module      : Amazonka.GameLift.Types.VpcPeeringConnection
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.VpcPeeringConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types.VpcPeeringConnectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Represents a peering connection between a VPC on one of your Amazon Web
-- Services accounts and the VPC for your Amazon GameLift fleets. This
-- record may be for an active peering connection or a pending connection
-- that has not yet been established.
--
-- __Related actions__
--
-- CreateVpcPeeringAuthorization | DescribeVpcPeeringAuthorizations |
-- DeleteVpcPeeringAuthorization | CreateVpcPeeringConnection |
-- DescribeVpcPeeringConnections | DeleteVpcPeeringConnection |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newVpcPeeringConnection' smart constructor.
data VpcPeeringConnection = VpcPeeringConnection'
  { -- | A unique identifier for the fleet. This ID determines the ID of the
    -- Amazon GameLift VPC for your fleet.
    fleetId :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the VPC that contains the Amazon GameLift fleet
    -- for this connection. This VPC is managed by Amazon GameLift and does not
    -- appear in your Amazon Web Services account.
    gameLiftVpcId :: Prelude.Maybe Prelude.Text,
    -- | CIDR block of IPv4 addresses assigned to the VPC peering connection for
    -- the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated
    -- with it; these blocks cannot overlap or the peering connection cannot be
    -- created.
    ipV4CidrBlock :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier that is automatically assigned to the connection
    -- record. This ID is referenced in VPC peering connection events, and is
    -- used when deleting a connection with DeleteVpcPeeringConnection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The status information about the connection. Status indicates if a
    -- connection is pending, successful, or failed.
    status :: Prelude.Maybe VpcPeeringConnectionStatus,
    -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift fleet resource for this connection.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- GameLift fleet. The VPC must be in the same Region as your fleet. To
    -- look up a VPC ID, use the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
    -- Services Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
    peerVpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcPeeringConnection' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'vpcPeeringConnection_fleetId' - A unique identifier for the fleet. This ID determines the ID of the
-- Amazon GameLift VPC for your fleet.
--
-- 'gameLiftVpcId', 'vpcPeeringConnection_gameLiftVpcId' - A unique identifier for the VPC that contains the Amazon GameLift fleet
-- for this connection. This VPC is managed by Amazon GameLift and does not
-- appear in your Amazon Web Services account.
--
-- 'ipV4CidrBlock', 'vpcPeeringConnection_ipV4CidrBlock' - CIDR block of IPv4 addresses assigned to the VPC peering connection for
-- the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated
-- with it; these blocks cannot overlap or the peering connection cannot be
-- created.
--
-- 'vpcPeeringConnectionId', 'vpcPeeringConnection_vpcPeeringConnectionId' - A unique identifier that is automatically assigned to the connection
-- record. This ID is referenced in VPC peering connection events, and is
-- used when deleting a connection with DeleteVpcPeeringConnection.
--
-- 'status', 'vpcPeeringConnection_status' - The status information about the connection. Status indicates if a
-- connection is pending, successful, or failed.
--
-- 'fleetArn', 'vpcPeeringConnection_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift fleet resource for this connection.
--
-- 'peerVpcId', 'vpcPeeringConnection_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
newVpcPeeringConnection ::
  VpcPeeringConnection
newVpcPeeringConnection =
  VpcPeeringConnection'
    { fleetId = Prelude.Nothing,
      gameLiftVpcId = Prelude.Nothing,
      ipV4CidrBlock = Prelude.Nothing,
      vpcPeeringConnectionId = Prelude.Nothing,
      status = Prelude.Nothing,
      fleetArn = Prelude.Nothing,
      peerVpcId = Prelude.Nothing
    }

-- | A unique identifier for the fleet. This ID determines the ID of the
-- Amazon GameLift VPC for your fleet.
vpcPeeringConnection_fleetId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_fleetId = Lens.lens (\VpcPeeringConnection' {fleetId} -> fleetId) (\s@VpcPeeringConnection' {} a -> s {fleetId = a} :: VpcPeeringConnection)

-- | A unique identifier for the VPC that contains the Amazon GameLift fleet
-- for this connection. This VPC is managed by Amazon GameLift and does not
-- appear in your Amazon Web Services account.
vpcPeeringConnection_gameLiftVpcId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_gameLiftVpcId = Lens.lens (\VpcPeeringConnection' {gameLiftVpcId} -> gameLiftVpcId) (\s@VpcPeeringConnection' {} a -> s {gameLiftVpcId = a} :: VpcPeeringConnection)

-- | CIDR block of IPv4 addresses assigned to the VPC peering connection for
-- the GameLift VPC. The peered VPC also has an IPv4 CIDR block associated
-- with it; these blocks cannot overlap or the peering connection cannot be
-- created.
vpcPeeringConnection_ipV4CidrBlock :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_ipV4CidrBlock = Lens.lens (\VpcPeeringConnection' {ipV4CidrBlock} -> ipV4CidrBlock) (\s@VpcPeeringConnection' {} a -> s {ipV4CidrBlock = a} :: VpcPeeringConnection)

-- | A unique identifier that is automatically assigned to the connection
-- record. This ID is referenced in VPC peering connection events, and is
-- used when deleting a connection with DeleteVpcPeeringConnection.
vpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\VpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@VpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: VpcPeeringConnection)

-- | The status information about the connection. Status indicates if a
-- connection is pending, successful, or failed.
vpcPeeringConnection_status :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe VpcPeeringConnectionStatus)
vpcPeeringConnection_status = Lens.lens (\VpcPeeringConnection' {status} -> status) (\s@VpcPeeringConnection' {} a -> s {status = a} :: VpcPeeringConnection)

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift fleet resource for this connection.
vpcPeeringConnection_fleetArn :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_fleetArn = Lens.lens (\VpcPeeringConnection' {fleetArn} -> fleetArn) (\s@VpcPeeringConnection' {} a -> s {fleetArn = a} :: VpcPeeringConnection)

-- | A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
vpcPeeringConnection_peerVpcId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_peerVpcId = Lens.lens (\VpcPeeringConnection' {peerVpcId} -> peerVpcId) (\s@VpcPeeringConnection' {} a -> s {peerVpcId = a} :: VpcPeeringConnection)

instance Core.FromJSON VpcPeeringConnection where
  parseJSON =
    Core.withObject
      "VpcPeeringConnection"
      ( \x ->
          VpcPeeringConnection'
            Prelude.<$> (x Core..:? "FleetId")
            Prelude.<*> (x Core..:? "GameLiftVpcId")
            Prelude.<*> (x Core..:? "IpV4CidrBlock")
            Prelude.<*> (x Core..:? "VpcPeeringConnectionId")
            Prelude.<*> (x Core..:? "Status")
            Prelude.<*> (x Core..:? "FleetArn")
            Prelude.<*> (x Core..:? "PeerVpcId")
      )

instance Prelude.Hashable VpcPeeringConnection where
  hashWithSalt _salt VpcPeeringConnection' {..} =
    _salt `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` gameLiftVpcId
      `Prelude.hashWithSalt` ipV4CidrBlock
      `Prelude.hashWithSalt` vpcPeeringConnectionId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` peerVpcId

instance Prelude.NFData VpcPeeringConnection where
  rnf VpcPeeringConnection' {..} =
    Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf gameLiftVpcId
      `Prelude.seq` Prelude.rnf ipV4CidrBlock
      `Prelude.seq` Prelude.rnf vpcPeeringConnectionId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf peerVpcId
