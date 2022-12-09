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
-- Module      : Amazonka.GameLift.Types.VpcPeeringAuthorization
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.GameLift.Types.VpcPeeringAuthorization where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Represents an authorization for a VPC peering connection between the VPC
-- for an Amazon GameLift fleet and another VPC on an account you have
-- access to. This authorization must exist and be valid for the peering
-- connection to be established. Authorizations are valid for 24 hours
-- after they are issued.
--
-- __Related actions__
--
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
--
-- /See:/ 'newVpcPeeringAuthorization' smart constructor.
data VpcPeeringAuthorization = VpcPeeringAuthorization'
  { -- | Time stamp indicating when this authorization was issued. Format is a
    -- number expressed in Unix time as milliseconds (for example
    -- @\"1469498468.057\"@).
    creationTime :: Prelude.Maybe Data.POSIX,
    -- | Time stamp indicating when this authorization expires (24 hours after
    -- issuance). Format is a number expressed in Unix time as milliseconds
    -- (for example @\"1469498468.057\"@).
    expirationTime :: Prelude.Maybe Data.POSIX,
    -- | A unique identifier for the Amazon Web Services account that you use to
    -- manage your GameLift fleet. You can find your Account ID in the Amazon
    -- Web Services Management Console under account settings.
    gameLiftAwsAccountId :: Prelude.Maybe Prelude.Text,
    -- | The authorization\'s peer VPC Amazon Web Services account ID.
    peerVpcAwsAccountId :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'VpcPeeringAuthorization' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTime', 'vpcPeeringAuthorization_creationTime' - Time stamp indicating when this authorization was issued. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
--
-- 'expirationTime', 'vpcPeeringAuthorization_expirationTime' - Time stamp indicating when this authorization expires (24 hours after
-- issuance). Format is a number expressed in Unix time as milliseconds
-- (for example @\"1469498468.057\"@).
--
-- 'gameLiftAwsAccountId', 'vpcPeeringAuthorization_gameLiftAwsAccountId' - A unique identifier for the Amazon Web Services account that you use to
-- manage your GameLift fleet. You can find your Account ID in the Amazon
-- Web Services Management Console under account settings.
--
-- 'peerVpcAwsAccountId', 'vpcPeeringAuthorization_peerVpcAwsAccountId' - The authorization\'s peer VPC Amazon Web Services account ID.
--
-- 'peerVpcId', 'vpcPeeringAuthorization_peerVpcId' - A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
newVpcPeeringAuthorization ::
  VpcPeeringAuthorization
newVpcPeeringAuthorization =
  VpcPeeringAuthorization'
    { creationTime =
        Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      gameLiftAwsAccountId = Prelude.Nothing,
      peerVpcAwsAccountId = Prelude.Nothing,
      peerVpcId = Prelude.Nothing
    }

-- | Time stamp indicating when this authorization was issued. Format is a
-- number expressed in Unix time as milliseconds (for example
-- @\"1469498468.057\"@).
vpcPeeringAuthorization_creationTime :: Lens.Lens' VpcPeeringAuthorization (Prelude.Maybe Prelude.UTCTime)
vpcPeeringAuthorization_creationTime = Lens.lens (\VpcPeeringAuthorization' {creationTime} -> creationTime) (\s@VpcPeeringAuthorization' {} a -> s {creationTime = a} :: VpcPeeringAuthorization) Prelude.. Lens.mapping Data._Time

-- | Time stamp indicating when this authorization expires (24 hours after
-- issuance). Format is a number expressed in Unix time as milliseconds
-- (for example @\"1469498468.057\"@).
vpcPeeringAuthorization_expirationTime :: Lens.Lens' VpcPeeringAuthorization (Prelude.Maybe Prelude.UTCTime)
vpcPeeringAuthorization_expirationTime = Lens.lens (\VpcPeeringAuthorization' {expirationTime} -> expirationTime) (\s@VpcPeeringAuthorization' {} a -> s {expirationTime = a} :: VpcPeeringAuthorization) Prelude.. Lens.mapping Data._Time

-- | A unique identifier for the Amazon Web Services account that you use to
-- manage your GameLift fleet. You can find your Account ID in the Amazon
-- Web Services Management Console under account settings.
vpcPeeringAuthorization_gameLiftAwsAccountId :: Lens.Lens' VpcPeeringAuthorization (Prelude.Maybe Prelude.Text)
vpcPeeringAuthorization_gameLiftAwsAccountId = Lens.lens (\VpcPeeringAuthorization' {gameLiftAwsAccountId} -> gameLiftAwsAccountId) (\s@VpcPeeringAuthorization' {} a -> s {gameLiftAwsAccountId = a} :: VpcPeeringAuthorization)

-- | The authorization\'s peer VPC Amazon Web Services account ID.
vpcPeeringAuthorization_peerVpcAwsAccountId :: Lens.Lens' VpcPeeringAuthorization (Prelude.Maybe Prelude.Text)
vpcPeeringAuthorization_peerVpcAwsAccountId = Lens.lens (\VpcPeeringAuthorization' {peerVpcAwsAccountId} -> peerVpcAwsAccountId) (\s@VpcPeeringAuthorization' {} a -> s {peerVpcAwsAccountId = a} :: VpcPeeringAuthorization)

-- | A unique identifier for a VPC with resources to be accessed by your
-- GameLift fleet. The VPC must be in the same Region as your fleet. To
-- look up a VPC ID, use the
-- <https://console.aws.amazon.com/vpc/ VPC Dashboard> in the Amazon Web
-- Services Management Console. Learn more about VPC peering in
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/vpc-peering.html VPC Peering with GameLift Fleets>.
vpcPeeringAuthorization_peerVpcId :: Lens.Lens' VpcPeeringAuthorization (Prelude.Maybe Prelude.Text)
vpcPeeringAuthorization_peerVpcId = Lens.lens (\VpcPeeringAuthorization' {peerVpcId} -> peerVpcId) (\s@VpcPeeringAuthorization' {} a -> s {peerVpcId = a} :: VpcPeeringAuthorization)

instance Data.FromJSON VpcPeeringAuthorization where
  parseJSON =
    Data.withObject
      "VpcPeeringAuthorization"
      ( \x ->
          VpcPeeringAuthorization'
            Prelude.<$> (x Data..:? "CreationTime")
            Prelude.<*> (x Data..:? "ExpirationTime")
            Prelude.<*> (x Data..:? "GameLiftAwsAccountId")
            Prelude.<*> (x Data..:? "PeerVpcAwsAccountId")
            Prelude.<*> (x Data..:? "PeerVpcId")
      )

instance Prelude.Hashable VpcPeeringAuthorization where
  hashWithSalt _salt VpcPeeringAuthorization' {..} =
    _salt `Prelude.hashWithSalt` creationTime
      `Prelude.hashWithSalt` expirationTime
      `Prelude.hashWithSalt` gameLiftAwsAccountId
      `Prelude.hashWithSalt` peerVpcAwsAccountId
      `Prelude.hashWithSalt` peerVpcId

instance Prelude.NFData VpcPeeringAuthorization where
  rnf VpcPeeringAuthorization' {..} =
    Prelude.rnf creationTime
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf gameLiftAwsAccountId
      `Prelude.seq` Prelude.rnf peerVpcAwsAccountId
      `Prelude.seq` Prelude.rnf peerVpcId
