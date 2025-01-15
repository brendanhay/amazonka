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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionStatusDetails
import Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionVpcInfoDetails

-- | Provides information about a VPC peering connection between two VPCs: a
-- requester VPC that you own and an accepter VPC with which to create the
-- connection.
--
-- /See:/ 'newAwsEc2VpcPeeringConnectionDetails' smart constructor.
data AwsEc2VpcPeeringConnectionDetails = AwsEc2VpcPeeringConnectionDetails'
  { -- | Information about the accepter VPC.
    accepterVpcInfo :: Prelude.Maybe AwsEc2VpcPeeringConnectionVpcInfoDetails,
    -- | The time at which an unaccepted VPC peering connection will expire.
    expirationTime :: Prelude.Maybe Prelude.Text,
    -- | Information about the requester VPC.
    requesterVpcInfo :: Prelude.Maybe AwsEc2VpcPeeringConnectionVpcInfoDetails,
    -- | The status of the VPC peering connection.
    status :: Prelude.Maybe AwsEc2VpcPeeringConnectionStatusDetails,
    -- | The ID of the VPC peering connection.
    vpcPeeringConnectionId :: Prelude.Maybe Prelude.Text
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
-- 'accepterVpcInfo', 'awsEc2VpcPeeringConnectionDetails_accepterVpcInfo' - Information about the accepter VPC.
--
-- 'expirationTime', 'awsEc2VpcPeeringConnectionDetails_expirationTime' - The time at which an unaccepted VPC peering connection will expire.
--
-- 'requesterVpcInfo', 'awsEc2VpcPeeringConnectionDetails_requesterVpcInfo' - Information about the requester VPC.
--
-- 'status', 'awsEc2VpcPeeringConnectionDetails_status' - The status of the VPC peering connection.
--
-- 'vpcPeeringConnectionId', 'awsEc2VpcPeeringConnectionDetails_vpcPeeringConnectionId' - The ID of the VPC peering connection.
newAwsEc2VpcPeeringConnectionDetails ::
  AwsEc2VpcPeeringConnectionDetails
newAwsEc2VpcPeeringConnectionDetails =
  AwsEc2VpcPeeringConnectionDetails'
    { accepterVpcInfo =
        Prelude.Nothing,
      expirationTime = Prelude.Nothing,
      requesterVpcInfo = Prelude.Nothing,
      status = Prelude.Nothing,
      vpcPeeringConnectionId = Prelude.Nothing
    }

-- | Information about the accepter VPC.
awsEc2VpcPeeringConnectionDetails_accepterVpcInfo :: Lens.Lens' AwsEc2VpcPeeringConnectionDetails (Prelude.Maybe AwsEc2VpcPeeringConnectionVpcInfoDetails)
awsEc2VpcPeeringConnectionDetails_accepterVpcInfo = Lens.lens (\AwsEc2VpcPeeringConnectionDetails' {accepterVpcInfo} -> accepterVpcInfo) (\s@AwsEc2VpcPeeringConnectionDetails' {} a -> s {accepterVpcInfo = a} :: AwsEc2VpcPeeringConnectionDetails)

-- | The time at which an unaccepted VPC peering connection will expire.
awsEc2VpcPeeringConnectionDetails_expirationTime :: Lens.Lens' AwsEc2VpcPeeringConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcPeeringConnectionDetails_expirationTime = Lens.lens (\AwsEc2VpcPeeringConnectionDetails' {expirationTime} -> expirationTime) (\s@AwsEc2VpcPeeringConnectionDetails' {} a -> s {expirationTime = a} :: AwsEc2VpcPeeringConnectionDetails)

-- | Information about the requester VPC.
awsEc2VpcPeeringConnectionDetails_requesterVpcInfo :: Lens.Lens' AwsEc2VpcPeeringConnectionDetails (Prelude.Maybe AwsEc2VpcPeeringConnectionVpcInfoDetails)
awsEc2VpcPeeringConnectionDetails_requesterVpcInfo = Lens.lens (\AwsEc2VpcPeeringConnectionDetails' {requesterVpcInfo} -> requesterVpcInfo) (\s@AwsEc2VpcPeeringConnectionDetails' {} a -> s {requesterVpcInfo = a} :: AwsEc2VpcPeeringConnectionDetails)

-- | The status of the VPC peering connection.
awsEc2VpcPeeringConnectionDetails_status :: Lens.Lens' AwsEc2VpcPeeringConnectionDetails (Prelude.Maybe AwsEc2VpcPeeringConnectionStatusDetails)
awsEc2VpcPeeringConnectionDetails_status = Lens.lens (\AwsEc2VpcPeeringConnectionDetails' {status} -> status) (\s@AwsEc2VpcPeeringConnectionDetails' {} a -> s {status = a} :: AwsEc2VpcPeeringConnectionDetails)

-- | The ID of the VPC peering connection.
awsEc2VpcPeeringConnectionDetails_vpcPeeringConnectionId :: Lens.Lens' AwsEc2VpcPeeringConnectionDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcPeeringConnectionDetails_vpcPeeringConnectionId = Lens.lens (\AwsEc2VpcPeeringConnectionDetails' {vpcPeeringConnectionId} -> vpcPeeringConnectionId) (\s@AwsEc2VpcPeeringConnectionDetails' {} a -> s {vpcPeeringConnectionId = a} :: AwsEc2VpcPeeringConnectionDetails)

instance
  Data.FromJSON
    AwsEc2VpcPeeringConnectionDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2VpcPeeringConnectionDetails"
      ( \x ->
          AwsEc2VpcPeeringConnectionDetails'
            Prelude.<$> (x Data..:? "AccepterVpcInfo")
            Prelude.<*> (x Data..:? "ExpirationTime")
            Prelude.<*> (x Data..:? "RequesterVpcInfo")
            Prelude.<*> (x Data..:? "Status")
            Prelude.<*> (x Data..:? "VpcPeeringConnectionId")
      )

instance
  Prelude.Hashable
    AwsEc2VpcPeeringConnectionDetails
  where
  hashWithSalt
    _salt
    AwsEc2VpcPeeringConnectionDetails' {..} =
      _salt
        `Prelude.hashWithSalt` accepterVpcInfo
        `Prelude.hashWithSalt` expirationTime
        `Prelude.hashWithSalt` requesterVpcInfo
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` vpcPeeringConnectionId

instance
  Prelude.NFData
    AwsEc2VpcPeeringConnectionDetails
  where
  rnf AwsEc2VpcPeeringConnectionDetails' {..} =
    Prelude.rnf accepterVpcInfo `Prelude.seq`
      Prelude.rnf expirationTime `Prelude.seq`
        Prelude.rnf requesterVpcInfo `Prelude.seq`
          Prelude.rnf status `Prelude.seq`
            Prelude.rnf vpcPeeringConnectionId

instance
  Data.ToJSON
    AwsEc2VpcPeeringConnectionDetails
  where
  toJSON AwsEc2VpcPeeringConnectionDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AccepterVpcInfo" Data..=)
              Prelude.<$> accepterVpcInfo,
            ("ExpirationTime" Data..=)
              Prelude.<$> expirationTime,
            ("RequesterVpcInfo" Data..=)
              Prelude.<$> requesterVpcInfo,
            ("Status" Data..=) Prelude.<$> status,
            ("VpcPeeringConnectionId" Data..=)
              Prelude.<$> vpcPeeringConnectionId
          ]
      )
