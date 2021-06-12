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
-- Module      : Network.AWS.GameLift.Types.VpcPeeringAuthorization
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.GameLift.Types.VpcPeeringAuthorization where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents an authorization for a VPC peering connection between the VPC
-- for an Amazon GameLift fleet and another VPC on an account you have
-- access to. This authorization must exist and be valid for the peering
-- connection to be established. Authorizations are valid for 24 hours
-- after they are issued.
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
-- /See:/ 'newVpcPeeringAuthorization' smart constructor.
data VpcPeeringAuthorization = VpcPeeringAuthorization'
  { -- | Time stamp indicating when this authorization was issued. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- \"1469498468.057\").
    creationTime :: Core.Maybe Core.POSIX,
    peerVpcAwsAccountId :: Core.Maybe Core.Text,
    -- | Time stamp indicating when this authorization expires (24 hours after
    -- issuance). Format is a number expressed in Unix time as milliseconds
    -- (for example \"1469498468.057\").
    expirationTime :: Core.Maybe Core.POSIX,
    -- | A unique identifier for the AWS account that you use to manage your
    -- Amazon GameLift fleet. You can find your Account ID in the AWS
    -- Management Console under account settings.
    gameLiftAwsAccountId :: Core.Maybe Core.Text,
    -- | A unique identifier for a VPC with resources to be accessed by your
    -- Amazon GameLift fleet. The VPC must be in the same Region where your
    -- fleet is deployed. Look up a VPC ID using the
    -- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
    -- Management Console. Learn more about VPC peering in
    -- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
    peerVpcId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpcPeeringAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'vpcPeeringAuthorization_creationTime' - Time stamp indicating when this authorization was issued. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
--
-- 'peerVpcAwsAccountId', 'vpcPeeringAuthorization_peerVpcAwsAccountId' -
--
-- 'expirationTime', 'vpcPeeringAuthorization_expirationTime' - Time stamp indicating when this authorization expires (24 hours after
-- issuance). Format is a number expressed in Unix time as milliseconds
-- (for example \"1469498468.057\").
--
-- 'gameLiftAwsAccountId', 'vpcPeeringAuthorization_gameLiftAwsAccountId' - A unique identifier for the AWS account that you use to manage your
-- Amazon GameLift fleet. You can find your Account ID in the AWS
-- Management Console under account settings.
--
-- 'peerVpcId', 'vpcPeeringAuthorization_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region where your
-- fleet is deployed. Look up a VPC ID using the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
newVpcPeeringAuthorization ::
  VpcPeeringAuthorization
newVpcPeeringAuthorization =
  VpcPeeringAuthorization'
    { creationTime =
        Core.Nothing,
      peerVpcAwsAccountId = Core.Nothing,
      expirationTime = Core.Nothing,
      gameLiftAwsAccountId = Core.Nothing,
      peerVpcId = Core.Nothing
    }

-- | Time stamp indicating when this authorization was issued. Format is a
-- number expressed in Unix time as milliseconds (for example
-- \"1469498468.057\").
vpcPeeringAuthorization_creationTime :: Lens.Lens' VpcPeeringAuthorization (Core.Maybe Core.UTCTime)
vpcPeeringAuthorization_creationTime = Lens.lens (\VpcPeeringAuthorization' {creationTime} -> creationTime) (\s@VpcPeeringAuthorization' {} a -> s {creationTime = a} :: VpcPeeringAuthorization) Core.. Lens.mapping Core._Time

-- |
vpcPeeringAuthorization_peerVpcAwsAccountId :: Lens.Lens' VpcPeeringAuthorization (Core.Maybe Core.Text)
vpcPeeringAuthorization_peerVpcAwsAccountId = Lens.lens (\VpcPeeringAuthorization' {peerVpcAwsAccountId} -> peerVpcAwsAccountId) (\s@VpcPeeringAuthorization' {} a -> s {peerVpcAwsAccountId = a} :: VpcPeeringAuthorization)

-- | Time stamp indicating when this authorization expires (24 hours after
-- issuance). Format is a number expressed in Unix time as milliseconds
-- (for example \"1469498468.057\").
vpcPeeringAuthorization_expirationTime :: Lens.Lens' VpcPeeringAuthorization (Core.Maybe Core.UTCTime)
vpcPeeringAuthorization_expirationTime = Lens.lens (\VpcPeeringAuthorization' {expirationTime} -> expirationTime) (\s@VpcPeeringAuthorization' {} a -> s {expirationTime = a} :: VpcPeeringAuthorization) Core.. Lens.mapping Core._Time

-- | A unique identifier for the AWS account that you use to manage your
-- Amazon GameLift fleet. You can find your Account ID in the AWS
-- Management Console under account settings.
vpcPeeringAuthorization_gameLiftAwsAccountId :: Lens.Lens' VpcPeeringAuthorization (Core.Maybe Core.Text)
vpcPeeringAuthorization_gameLiftAwsAccountId = Lens.lens (\VpcPeeringAuthorization' {gameLiftAwsAccountId} -> gameLiftAwsAccountId) (\s@VpcPeeringAuthorization' {} a -> s {gameLiftAwsAccountId = a} :: VpcPeeringAuthorization)

-- | A unique identifier for a VPC with resources to be accessed by your
-- Amazon GameLift fleet. The VPC must be in the same Region where your
-- fleet is deployed. Look up a VPC ID using the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the AWS
-- Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with Amazon GameLift Fleets>.
vpcPeeringAuthorization_peerVpcId :: Lens.Lens' VpcPeeringAuthorization (Core.Maybe Core.Text)
vpcPeeringAuthorization_peerVpcId = Lens.lens (\VpcPeeringAuthorization' {peerVpcId} -> peerVpcId) (\s@VpcPeeringAuthorization' {} a -> s {peerVpcId = a} :: VpcPeeringAuthorization)

instance Core.FromJSON VpcPeeringAuthorization where
  parseJSON =
    Core.withObject
      "VpcPeeringAuthorization"
      ( \x ->
          VpcPeeringAuthorization'
            Core.<$> (x Core..:? "CreationTime")
            Core.<*> (x Core..:? "PeerVpcAwsAccountId")
            Core.<*> (x Core..:? "ExpirationTime")
            Core.<*> (x Core..:? "GameLiftAwsAccountId")
            Core.<*> (x Core..:? "PeerVpcId")
      )

instance Core.Hashable VpcPeeringAuthorization

instance Core.NFData VpcPeeringAuthorization
