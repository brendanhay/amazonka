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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.VpcPeeringConnection where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.GameLift.Types.VpcPeeringConnectionStatus
import qualified Amazonka.Prelude as Prelude

-- | Represents a peering connection between a VPC on one of your Amazon Web
-- Services accounts and the VPC for your Amazon GameLift fleets. This
-- record may be for an active peering connection or a pending connection
-- that has not yet been established.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newVpcPeeringConnection' smart constructor.
data VpcPeeringConnection = VpcPeeringConnection'
  { -- | The Amazon Resource Name
    -- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
    -- associated with the GameLift fleet resource for this connection.
    fleetArn :: Prelude.Maybe Prelude.Text,
    -- | A unique identifier for the fleet. This ID determines the ID of the
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
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- GameLift fleet. The VPC must be in the same Region as your fleet. To
    -- look up a VPC ID, use the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
    -- Services Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
    peerVpcId :: Prelude.Maybe Prelude.Text,
    -- | The status information about the connection. Status indicates if a
    -- connection is pending, successful, or failed.
    status :: Prelude.Maybe VpcPeeringConnectionStatus,
    -- | A unique identifier that is automatically assigned to the connection
    -- record. This ID is referenced in VPC peering connection events, and is
    -- used when deleting a connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text
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
-- 'fleetArn', 'vpcPeeringConnection_fleetArn' - The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift fleet resource for this connection.
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
-- 'peerVpcId', 'vpcPeeringConnection_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
--
-- 'status', 'vpcPeeringConnection_status' - The status information about the connection. Status indicates if a
-- connection is pending, successful, or failed.
--
-- 'vpcPeeringConnectionId', 'vpcPeeringConnection_vpcPeeringConnectionId' - A unique identifier that is automatically assigned to the connection
-- record. This ID is referenced in VPC peering connection events, and is
-- used when deleting a connection.
newVpcPeeringConnection ::
  VpcPeeringConnection
newVpcPeeringConnection =
  VpcPeeringConnection'
    { fleetArn = Prelude.Nothing,
      fleetId = Prelude.Nothing,
      gameLiftVpcId = Prelude.Nothing,
      ipV4CidrBlock = Prelude.Nothing,
      peerVpcId = Prelude.Nothing,
      status = Prelude.Nothing,
      vpcPeeringConnectionId = Prelude.Nothing
    }

-- | The Amazon Resource Name
-- (<https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-arn-format.html ARN>)
-- associated with the GameLift fleet resource for this connection.
vpcPeeringConnection_fleetArn :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_fleetArn = Lens.lens (\VpcPeeringConnection' {fleetArn} -> fleetArn) (\s@VpcPeeringConnection' {} a -> s {fleetArn = a} :: VpcPeeringConnection)

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

-- | A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
vpcPeeringConnection_peerVpcId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_peerVpcId = Lens.lens (\VpcPeeringConnection' {peerVpcId} -> peerVpcId) (\s@VpcPeeringConnection' {} a -> s {peerVpcId = a} :: VpcPeeringConnection)

-- | The status information about the connection. Status indicates if a
-- connection is pending, successful, or failed.
vpcPeeringConnection_status :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe VpcPeeringConnectionStatus)
vpcPeeringConnection_status = Lens.lens (\VpcPeeringConnection' {status} -> status) (\s@VpcPeeringConnection' {} a -> s {status = a} :: VpcPeeringConnection)

-- | A unique identifier that is automatically assigned to the connection
-- record. This ID is referenced in VPC peering connection events, and is
-- used when deleting a connection.
vpcPeeringConnection_vpcPeeringConnectionId :: Lens.Lens' VpcPeeringConnection (Prelude.Maybe Prelude.Text)
vpcPeeringConnection_vpcPeeringConnectionId = Lens.lens (\VpcPeeringConnection' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@VpcPeeringConnection' {} a -> s {vpcPeeringConnectionId = a} :: VpcPeeringConnection)

instance Data.FromJSON VpcPeeringConnection where
  parseJSON =
    Data.withObject
      "VpcPeeringConnection"
      ( \x ->
          VpcPeeringConnection'
            Prelude.<$> (x Data..:? "FleetArn")
            Prelude.<*> (x Data..:? "FleetId")
            Prelude.<*> (x Data..:? "GameLiftVpcId")
            Prelude.<*> (x Data..:? "IpV4CidrBlock")
            Prelude.<*> (x Data..:? "PeerVpcId")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "VpcPeeringConnectionId")
      )

instance Prelude.Hashable VpcPeeringConnection where
  hashWithSalt _salt VpcPeeringConnection' {..} =
    _salt
      `Prelude.hashWithSalt` fleetArn
      `Prelude.hashWithSalt` fleetId
      `Prelude.hashWithSalt` gameLiftVpcId
      `Prelude.hashWithSalt` ipV4CidrBlock
      `Prelude.hashWithSalt` peerVpcId
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` vpcPeeringConnectionId

instance Prelude.NFData VpcPeeringConnection where
  rnf VpcPeeringConnection' {..} =
    Prelude.rnf fleetArn
      `Prelude.seq` Prelude.rnf fleetId
      `Prelude.seq` Prelude.rnf gameLiftVpcId
      `Prelude.seq` Prelude.rnf ipV4CidrBlock
      `Prelude.seq` Prelude.rnf peerVpcId
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf vpcPeeringConnectionId
