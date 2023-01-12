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
-- Module      : Amazonka.EC2.Types.VpcPeeringConnectionVpcInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.VpcPeeringConnectionVpcInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.CidrBlock
import Amazonka.EC2.Types.Ipv6CidrBlock
import Amazonka.EC2.Types.VpcPeeringConnectionOptionsDescription
import qualified Amazonka.Prelude as Prelude

-- | Describes a VPC in a VPC peering connection.
--
-- /See:/ 'newVpcPeeringConnectionVpcInfo' smart constructor.
data VpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo'
  { -- | The IPv4 CIDR block for the VPC.
    cidrBlock :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv4 CIDR blocks for the VPC.
    cidrBlockSet :: Prelude.Maybe [CidrBlock],
    -- | The IPv6 CIDR block for the VPC.
    ipv6CidrBlockSet :: Prelude.Maybe [Ipv6CidrBlock],
    -- | The ID of the Amazon Web Services account that owns the VPC.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Information about the VPC peering connection options for the accepter or
    -- requester VPC.
    peeringOptions :: Prelude.Maybe VpcPeeringConnectionOptionsDescription,
    -- | The Region in which the VPC is located.
    region :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'VpcPeeringConnectionVpcInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlock', 'vpcPeeringConnectionVpcInfo_cidrBlock' - The IPv4 CIDR block for the VPC.
--
-- 'cidrBlockSet', 'vpcPeeringConnectionVpcInfo_cidrBlockSet' - Information about the IPv4 CIDR blocks for the VPC.
--
-- 'ipv6CidrBlockSet', 'vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet' - The IPv6 CIDR block for the VPC.
--
-- 'ownerId', 'vpcPeeringConnectionVpcInfo_ownerId' - The ID of the Amazon Web Services account that owns the VPC.
--
-- 'peeringOptions', 'vpcPeeringConnectionVpcInfo_peeringOptions' - Information about the VPC peering connection options for the accepter or
-- requester VPC.
--
-- 'region', 'vpcPeeringConnectionVpcInfo_region' - The Region in which the VPC is located.
--
-- 'vpcId', 'vpcPeeringConnectionVpcInfo_vpcId' - The ID of the VPC.
newVpcPeeringConnectionVpcInfo ::
  VpcPeeringConnectionVpcInfo
newVpcPeeringConnectionVpcInfo =
  VpcPeeringConnectionVpcInfo'
    { cidrBlock =
        Prelude.Nothing,
      cidrBlockSet = Prelude.Nothing,
      ipv6CidrBlockSet = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      peeringOptions = Prelude.Nothing,
      region = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The IPv4 CIDR block for the VPC.
vpcPeeringConnectionVpcInfo_cidrBlock :: Lens.Lens' VpcPeeringConnectionVpcInfo (Prelude.Maybe Prelude.Text)
vpcPeeringConnectionVpcInfo_cidrBlock = Lens.lens (\VpcPeeringConnectionVpcInfo' {cidrBlock} -> cidrBlock) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {cidrBlock = a} :: VpcPeeringConnectionVpcInfo)

-- | Information about the IPv4 CIDR blocks for the VPC.
vpcPeeringConnectionVpcInfo_cidrBlockSet :: Lens.Lens' VpcPeeringConnectionVpcInfo (Prelude.Maybe [CidrBlock])
vpcPeeringConnectionVpcInfo_cidrBlockSet = Lens.lens (\VpcPeeringConnectionVpcInfo' {cidrBlockSet} -> cidrBlockSet) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {cidrBlockSet = a} :: VpcPeeringConnectionVpcInfo) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 CIDR block for the VPC.
vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet :: Lens.Lens' VpcPeeringConnectionVpcInfo (Prelude.Maybe [Ipv6CidrBlock])
vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet = Lens.lens (\VpcPeeringConnectionVpcInfo' {ipv6CidrBlockSet} -> ipv6CidrBlockSet) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {ipv6CidrBlockSet = a} :: VpcPeeringConnectionVpcInfo) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Amazon Web Services account that owns the VPC.
vpcPeeringConnectionVpcInfo_ownerId :: Lens.Lens' VpcPeeringConnectionVpcInfo (Prelude.Maybe Prelude.Text)
vpcPeeringConnectionVpcInfo_ownerId = Lens.lens (\VpcPeeringConnectionVpcInfo' {ownerId} -> ownerId) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {ownerId = a} :: VpcPeeringConnectionVpcInfo)

-- | Information about the VPC peering connection options for the accepter or
-- requester VPC.
vpcPeeringConnectionVpcInfo_peeringOptions :: Lens.Lens' VpcPeeringConnectionVpcInfo (Prelude.Maybe VpcPeeringConnectionOptionsDescription)
vpcPeeringConnectionVpcInfo_peeringOptions = Lens.lens (\VpcPeeringConnectionVpcInfo' {peeringOptions} -> peeringOptions) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {peeringOptions = a} :: VpcPeeringConnectionVpcInfo)

-- | The Region in which the VPC is located.
vpcPeeringConnectionVpcInfo_region :: Lens.Lens' VpcPeeringConnectionVpcInfo (Prelude.Maybe Prelude.Text)
vpcPeeringConnectionVpcInfo_region = Lens.lens (\VpcPeeringConnectionVpcInfo' {region} -> region) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {region = a} :: VpcPeeringConnectionVpcInfo)

-- | The ID of the VPC.
vpcPeeringConnectionVpcInfo_vpcId :: Lens.Lens' VpcPeeringConnectionVpcInfo (Prelude.Maybe Prelude.Text)
vpcPeeringConnectionVpcInfo_vpcId = Lens.lens (\VpcPeeringConnectionVpcInfo' {vpcId} -> vpcId) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {vpcId = a} :: VpcPeeringConnectionVpcInfo)

instance Data.FromXML VpcPeeringConnectionVpcInfo where
  parseXML x =
    VpcPeeringConnectionVpcInfo'
      Prelude.<$> (x Data..@? "cidrBlock")
      Prelude.<*> ( x Data..@? "cidrBlockSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "ipv6CidrBlockSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "peeringOptions")
      Prelude.<*> (x Data..@? "region")
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable VpcPeeringConnectionVpcInfo where
  hashWithSalt _salt VpcPeeringConnectionVpcInfo' {..} =
    _salt `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` cidrBlockSet
      `Prelude.hashWithSalt` ipv6CidrBlockSet
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` peeringOptions
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData VpcPeeringConnectionVpcInfo where
  rnf VpcPeeringConnectionVpcInfo' {..} =
    Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf cidrBlockSet
      `Prelude.seq` Prelude.rnf ipv6CidrBlockSet
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf peeringOptions
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf vpcId
