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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionDetails
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionStatusDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionVpcInfoDetails

-- | Provides information about a VPC peering connection between two VPCs: a
-- requester VPC that you own and an accepter VPC with which to create the
-- connection.
--
-- /See:/ 'newAwsEc2VpcPeeringConnectionDetails' smart constructor.
data AwsEc2VpcPeeringConnectionDetails = AwsEc2VpcPeeringConnectionDetails'
  { -- | Information about the requester VPC.
    requesterVpcInfo :: Prelude.Maybe AwsEc2VpcPeeringConnectionVpcInfoDetails,
    -- | The time at which an unaccepted VPC peering connection will expire.
    expirationTime :: Prelude.Maybe Prelude.Text,
    -- | Information about the accepter VPC.
    accepterVpcInfo :: Prelude.Maybe AwsEc2VpcPeeringConnectionVpcInfoDetails,
    -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text,
    -- | The status of the VPC peering connection.
    status :: Prelude.Maybe AwsEc2VpcPeeringConnectionStatusDetails
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpcPeeringConnectionDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requesterVpcInfo', 'awsEc2VpcPeeringConnectionDetails_requesterVpcInfo' - Information about the requester VPC.
--
-- 'expirationTime', 'awsEc2VpcPeeringConnectionDetails_expirationTime' - The time at which an unaccepted VPC peering connection will expire.
--
-- 'accepterVpcInfo', 'awsEc2VpcPeeringConnectionDetails_accepterVpcInfo' - Information about the accepter VPC.
--
-- 'vpcPeeringConnectionId', 'awsEc2VpcPeeringConnectionDetails_vpcPeeringConnectionId' - The ID of the VPC peering connection.
--
-- 'status', 'awsEc2VpcPeeringConnectionDetails_status' - The status of the VPC peering connection.
newAwsEc2VpcPeeringConnectionDetails ::
  AwsEc2VpcPeeringConnectionDetails
newAwsEc2VpcPeeringConnectionDetails =
  AwsEc2VpcPeeringConnectionDetails'
    { requesterVpcInfo =
        Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      accepterVpcInfo = Prelude.Nothing,
      vpcPeeringConnectionId = Prelude.Nothing,
      status = Prelude.Nothing
    }

-- | Information about the requester VPC.
awsEc2VpcPeeringConnectionDetails_requesterVpcInfo :: Lens.Lens' AwsEc2VpcPeeringConnectionDetails (Prelude.Maybe AwsEc2VpcPeeringConnectionVpcInfoDetails)
awsEc2VpcPeeringConnectionDetails_requesterVpcInfo = Lens.lens (\AwsEc2VpcPeeringConnectionDetails' {requesterVpcInfo} -> requesterVpcInfo) (\s@AwsEc2VpcPeeringConnectionDetails' {} a -> s {requesterVpcInfo = a} :: AwsEc2VpcPeeringConnectionDetails)

-- | The time at which an unaccepted VPC peering connection will expire.
awsEc2VpcPeeringConnectionDetails_expirationTime :: Lens.Lens' AwsEc2VpcPeeringConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcPeeringConnectionDetails_expirationTime = Lens.lens (\AwsEc2VpcPeeringConnectionDetails' {expirationTime} -> expirationTime) (\s@AwsEc2VpcPeeringConnectionDetails' {} a -> s {expirationTime = a} :: AwsEc2VpcPeeringConnectionDetails)

-- | Information about the accepter VPC.
awsEc2VpcPeeringConnectionDetails_accepterVpcInfo :: Lens.Lens' AwsEc2VpcPeeringConnectionDetails (Prelude.Maybe AwsEc2VpcPeeringConnectionVpcInfoDetails)
awsEc2VpcPeeringConnectionDetails_accepterVpcInfo = Lens.lens (\AwsEc2VpcPeeringConnectionDetails' {accepterVpcInfo} -> accepterVpcInfo) (\s@AwsEc2VpcPeeringConnectionDetails' {} a -> s {accepterVpcInfo = a} :: AwsEc2VpcPeeringConnectionDetails)

-- | The ID of the VPC peering connection.
awsEc2VpcPeeringConnectionDetails_vpcPeeringConnectionId :: Lens.Lens' AwsEc2VpcPeeringConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcPeeringConnectionDetails_vpcPeeringConnectionId = Lens.lens (\AwsEc2VpcPeeringConnectionDetails' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@AwsEc2VpcPeeringConnectionDetails' {} a -> s {vpcPeeringConnectionId = a} :: AwsEc2VpcPeeringConnectionDetails)

-- | The status of the VPC peering connection.
awsEc2VpcPeeringConnectionDetails_status :: Lens.Lens' AwsEc2VpcPeeringConnectionDetails (Prelude.Maybe AwsEc2VpcPeeringConnectionStatusDetails)
awsEc2VpcPeeringConnectionDetails_status = Lens.lens (\AwsEc2VpcPeeringConnectionDetails' {status} -> status) (\s@AwsEc2VpcPeeringConnectionDetails' {} a -> s {status = a} :: AwsEc2VpcPeeringConnectionDetails)

instance
  Core.FromJSON
    AwsEc2VpcPeeringConnectionDetails
  where
  parseJSON =
    Core.withObject
      "AwsEc2VpcPeeringConnectionDetails"
      ( \x ->
          AwsEc2VpcPeeringConnectionDetails'
            Prelude.<$> (x Core..:? "RequesterVpcInfo")
            Prelude.<*> (x Core..:? "ExpirationTime")
            Prelude.<*> (x Core..:? "AccepterVpcInfo")
            Prelude.<*> (x Core..:? "VpcPeeringConnectionId")
            Prelude.<*> (x Core..:? "Status")
      )

instance
  Prelude.Hashable
    AwsEc2VpcPeeringConnectionDetails
  where
  hashWithSalt
    _salt
    AwsEc2VpcPeeringConnectionDetails' {..} =
      _salt `Prelude.hashWithSalt` requesterVpcInfo
        `Prelude.hashWithSalt` expirationTime
        `Prelude.hashWithSalt` accepterVpcInfo
        `Prelude.hashWithSalt` vpcPeeringConnectionId
        `Prelude.hashWithSalt` status

instance
  Prelude.NFData
    AwsEc2VpcPeeringConnectionDetails
  where
  rnf AwsEc2VpcPeeringConnectionDetails' {..} =
    Prelude.rnf requesterVpcInfo
      `Prelude.seq` Prelude.rnf expirationTime
      `Prelude.seq` Prelude.rnf accepterVpcInfo
      `Prelude.seq` Prelude.rnf vpcPeeringConnectionId
      `Prelude.seq` Prelude.rnf status

instance
  Core.ToJSON
    AwsEc2VpcPeeringConnectionDetails
  where
  toJSON AwsEc2VpcPeeringConnectionDetails' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("RequesterVpcInfo" Core..=)
              Prelude.<$> requesterVpcInfo,
            ("ExpirationTime" Core..=)
              Prelude.<$> expirationTime,
            ("AccepterVpcInfo" Core..=)
              Prelude.<$> accepterVpcInfo,
            ("VpcPeeringConnectionId" Core..=)
              Prelude.<$> vpcPeeringConnectionId,
            ("Status" Core..=) Prelude.<$> status
          ]
      )
