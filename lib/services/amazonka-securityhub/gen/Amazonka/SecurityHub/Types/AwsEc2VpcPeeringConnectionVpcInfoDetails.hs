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
-- Module      : Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionVpcInfoDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.SecurityHub.Types.AwsEc2VpcPeeringConnectionVpcInfoDetails where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.SecurityHub.Types.VpcInfoCidrBlockSetDetails
import Amazonka.SecurityHub.Types.VpcInfoIpv6CidrBlockSetDetails
import Amazonka.SecurityHub.Types.VpcInfoPeeringOptionsDetails

-- | Describes a VPC in a VPC peering connection.
--
-- /See:/ 'newAwsEc2VpcPeeringConnectionVpcInfoDetails' smart constructor.
data AwsEc2VpcPeeringConnectionVpcInfoDetails = AwsEc2VpcPeeringConnectionVpcInfoDetails'
  { -- | The IPv4 CIDR block for the VPC.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv4 CIDR blocks for the VPC.
    cidrBlockSet :: Prelude.Maybe [VpcInfoCidrBlockSetDetails],
    -- | The IPv6 CIDR block for the VPC.
    ipv6CidrBlockSet :: Prelude.Maybe [VpcInfoIpv6CidrBlockSetDetails],
    -- | The ID of the Amazon Web Services account that owns the VPC.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC peering connection options for the accepter or
    -- requester VPC.
    peeringOptions :: Prelude.Maybe VpcInfoPeeringOptionsDetails,
    -- | The Amazon Web Services Region in which the VPC is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'AwsEc2VpcPeeringConnectionVpcInfoDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlock', 'awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlock' - The IPv4 CIDR block for the VPC.
--
-- 'cidrBlockSet', 'awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlockSet' - Information about the IPv4 CIDR blocks for the VPC.
--
-- 'ipv6CidrBlockSet', 'awsEc2VpcPeeringConnectionVpcInfoDetails_ipv6CidrBlockSet' - The IPv6 CIDR block for the VPC.
--
-- 'ownerId', 'awsEc2VpcPeeringConnectionVpcInfoDetails_ownerId' - The ID of the Amazon Web Services account that owns the VPC.
--
-- 'peeringOptions', 'awsEc2VpcPeeringConnectionVpcInfoDetails_peeringOptions' - Information about the VPC peering connection options for the accepter or
-- requester VPC.
--
-- 'region', 'awsEc2VpcPeeringConnectionVpcInfoDetails_region' - The Amazon Web Services Region in which the VPC is located.
--
-- 'vpcId', 'awsEc2VpcPeeringConnectionVpcInfoDetails_vpcId' - The ID of the VPC.
newAwsEc2VpcPeeringConnectionVpcInfoDetails ::
  AwsEc2VpcPeeringConnectionVpcInfoDetails
newAwsEc2VpcPeeringConnectionVpcInfoDetails =
  AwsEc2VpcPeeringConnectionVpcInfoDetails'
    { cidrBlock =
        Prelude.Nothing,
      cidrBlockSet = Prelude.Nothing,
      ipv6CidrBlockSet =
        Prelude.Nothing,
      ownerId = Prelude.Nothing,
      peeringOptions = Prelude.Nothing,
      region = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The IPv4 CIDR block for the VPC.
awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlock :: Lens.Lens' AwsEc2VpcPeeringConnectionVpcInfoDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlock = Lens.lens (\AwsEc2VpcPeeringConnectionVpcInfoDetails' {cidrBlock} -> cidrBlock) (\s@AwsEc2VpcPeeringConnectionVpcInfoDetails' {} a -> s {cidrBlock = a} :: AwsEc2VpcPeeringConnectionVpcInfoDetails)

-- | Information about the IPv4 CIDR blocks for the VPC.
awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlockSet :: Lens.Lens' AwsEc2VpcPeeringConnectionVpcInfoDetails (Prelude.Maybe [VpcInfoCidrBlockSetDetails])
awsEc2VpcPeeringConnectionVpcInfoDetails_cidrBlockSet = Lens.lens (\AwsEc2VpcPeeringConnectionVpcInfoDetails' {cidrBlockSet} -> cidrBlockSet) (\s@AwsEc2VpcPeeringConnectionVpcInfoDetails' {} a -> s {cidrBlockSet = a} :: AwsEc2VpcPeeringConnectionVpcInfoDetails) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 CIDR block for the VPC.
awsEc2VpcPeeringConnectionVpcInfoDetails_ipv6CidrBlockSet :: Lens.Lens' AwsEc2VpcPeeringConnectionVpcInfoDetails (Prelude.Maybe [VpcInfoIpv6CidrBlockSetDetails])
awsEc2VpcPeeringConnectionVpcInfoDetails_ipv6CidrBlockSet = Lens.lens (\AwsEc2VpcPeeringConnectionVpcInfoDetails' {ipv6CidrBlockSet} -> ipv6CidrBlockSet) (\s@AwsEc2VpcPeeringConnectionVpcInfoDetails' {} a -> s {ipv6CidrBlockSet = a} :: AwsEc2VpcPeeringConnectionVpcInfoDetails) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that owns the VPC.
awsEc2VpcPeeringConnectionVpcInfoDetails_ownerId :: Lens.Lens' AwsEc2VpcPeeringConnectionVpcInfoDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcPeeringConnectionVpcInfoDetails_ownerId = Lens.lens (\AwsEc2VpcPeeringConnectionVpcInfoDetails' {ownerId} -> ownerId) (\s@AwsEc2VpcPeeringConnectionVpcInfoDetails' {} a -> s {ownerId = a} :: AwsEc2VpcPeeringConnectionVpcInfoDetails)

-- | Information about the VPC peering connection options for the accepter or
-- requester VPC.
awsEc2VpcPeeringConnectionVpcInfoDetails_peeringOptions :: Lens.Lens' AwsEc2VpcPeeringConnectionVpcInfoDetails (Prelude.Maybe VpcInfoPeeringOptionsDetails)
awsEc2VpcPeeringConnectionVpcInfoDetails_peeringOptions = Lens.lens (\AwsEc2VpcPeeringConnectionVpcInfoDetails' {peeringOptions} -> peeringOptions) (\s@AwsEc2VpcPeeringConnectionVpcInfoDetails' {} a -> s {peeringOptions = a} :: AwsEc2VpcPeeringConnectionVpcInfoDetails)

-- | The Amazon Web Services Region in which the VPC is located.
awsEc2VpcPeeringConnectionVpcInfoDetails_region :: Lens.Lens' AwsEc2VpcPeeringConnectionVpcInfoDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcPeeringConnectionVpcInfoDetails_region = Lens.lens (\AwsEc2VpcPeeringConnectionVpcInfoDetails' {region} -> region) (\s@AwsEc2VpcPeeringConnectionVpcInfoDetails' {} a -> s {region = a} :: AwsEc2VpcPeeringConnectionVpcInfoDetails)

-- | The ID of the VPC.
awsEc2VpcPeeringConnectionVpcInfoDetails_vpcId :: Lens.Lens' AwsEc2VpcPeeringConnectionVpcInfoDetails (Prelude.Maybe Prelude.Text)
awsEc2VpcPeeringConnectionVpcInfoDetails_vpcId = Lens.lens (\AwsEc2VpcPeeringConnectionVpcInfoDetails' {vpcId} -> vpcId) (\s@AwsEc2VpcPeeringConnectionVpcInfoDetails' {} a -> s {vpcId = a} :: AwsEc2VpcPeeringConnectionVpcInfoDetails)

instance
  Data.FromJSON
    AwsEc2VpcPeeringConnectionVpcInfoDetails
  where
  parseJSON =
    Data.withObject
      "AwsEc2VpcPeeringConnectionVpcInfoDetails"
      ( \x ->
          AwsEc2VpcPeeringConnectionVpcInfoDetails'
            Prelude.<$> (x Data..:? "CidrBlock")
            Prelude.<*> (x Data..:? "CidrBlockSet" Data..!= Prelude.mempty)
            Prelude.<*> ( x Data..:? "Ipv6CidrBlockSet"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "OwnerId")
            Prelude.<*> (x Data..:? "PeeringOptions")
            Prelude.<*> (x Data..:? "Region")
            Prelude.<*> (x Data..:? "VpcId")
      )

instance
  Prelude.Hashable
    AwsEc2VpcPeeringConnectionVpcInfoDetails
  where
  hashWithSalt
    _salt
    AwsEc2VpcPeeringConnectionVpcInfoDetails' {..} =
      _salt `Prelude.hashWithSalt` cidrBlock
        `Prelude.hashWithSalt` cidrBlockSet
        `Prelude.hashWithSalt` ipv6CidrBlockSet
        `Prelude.hashWithSalt` ownerId
        `Prelude.hashWithSalt` peeringOptions
        `Prelude.hashWithSalt` region
        `Prelude.hashWithSalt` vpcId

instance
  Prelude.NFData
    AwsEc2VpcPeeringConnectionVpcInfoDetails
  where
  rnf AwsEc2VpcPeeringConnectionVpcInfoDetails' {..} =
    Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf cidrBlockSet
      `Prelude.seq` Prelude.rnf ipv6CidrBlockSet
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf peeringOptions
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf vpcId

instance
  Data.ToJSON
    AwsEc2VpcPeeringConnectionVpcInfoDetails
  where
  toJSON AwsEc2VpcPeeringConnectionVpcInfoDetails' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CidrBlock" Data..=) Prelude.<$> cidrBlock,
            ("CidrBlockSet" Data..=) Prelude.<$> cidrBlockSet,
            ("Ipv6CidrBlockSet" Data..=)
              Prelude.<$> ipv6CidrBlockSet,
            ("OwnerId" Data..=) Prelude.<$> ownerId,
            ("PeeringOptions" Data..=)
              Prelude.<$> peeringOptions,
            ("Region" Data..=) Prelude.<$> region,
            ("VpcId" Data..=) Prelude.<$> vpcId
          ]
      )
