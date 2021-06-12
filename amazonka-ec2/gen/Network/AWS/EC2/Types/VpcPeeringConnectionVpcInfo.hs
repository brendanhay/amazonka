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
-- Module      : Network.AWS.EC2.Types.VpcPeeringConnectionVpcInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.VpcPeeringConnectionVpcInfo where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.CidrBlock
import Network.AWS.EC2.Types.Ipv6CidrBlock
import Network.AWS.EC2.Types.VpcPeeringConnectionOptionsDescription
import qualified Network.AWS.Lens as Lens

-- | Describes a VPC in a VPC peering connection.
--
-- /See:/ 'newVpcPeeringConnectionVpcInfo' smart constructor.
data VpcPeeringConnectionVpcInfo = VpcPeeringConnectionVpcInfo'
  { -- | Information about the IPv4 CIDR blocks for the VPC.
    cidrBlockSet :: Core.Maybe [CidrBlock],
    -- | The AWS account ID of the VPC owner.
    ownerId :: Core.Maybe Core.Text,
    -- | The IPv6 CIDR block for the VPC.
    ipv6CidrBlockSet :: Core.Maybe [Ipv6CidrBlock],
    -- | The Region in which the VPC is located.
    region :: Core.Maybe Core.Text,
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text,
    -- | The IPv4 CIDR block for the VPC.
    cidrBlock :: Core.Maybe Core.Text,
    -- | Information about the VPC peering connection options for the accepter or
    -- requester VPC.
    peeringOptions :: Core.Maybe VpcPeeringConnectionOptionsDescription
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'VpcPeeringConnectionVpcInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cidrBlockSet', 'vpcPeeringConnectionVpcInfo_cidrBlockSet' - Information about the IPv4 CIDR blocks for the VPC.
--
-- 'ownerId', 'vpcPeeringConnectionVpcInfo_ownerId' - The AWS account ID of the VPC owner.
--
-- 'ipv6CidrBlockSet', 'vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet' - The IPv6 CIDR block for the VPC.
--
-- 'region', 'vpcPeeringConnectionVpcInfo_region' - The Region in which the VPC is located.
--
-- 'vpcId', 'vpcPeeringConnectionVpcInfo_vpcId' - The ID of the VPC.
--
-- 'cidrBlock', 'vpcPeeringConnectionVpcInfo_cidrBlock' - The IPv4 CIDR block for the VPC.
--
-- 'peeringOptions', 'vpcPeeringConnectionVpcInfo_peeringOptions' - Information about the VPC peering connection options for the accepter or
-- requester VPC.
newVpcPeeringConnectionVpcInfo ::
  VpcPeeringConnectionVpcInfo
newVpcPeeringConnectionVpcInfo =
  VpcPeeringConnectionVpcInfo'
    { cidrBlockSet =
        Core.Nothing,
      ownerId = Core.Nothing,
      ipv6CidrBlockSet = Core.Nothing,
      region = Core.Nothing,
      vpcId = Core.Nothing,
      cidrBlock = Core.Nothing,
      peeringOptions = Core.Nothing
    }

-- | Information about the IPv4 CIDR blocks for the VPC.
vpcPeeringConnectionVpcInfo_cidrBlockSet :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe [CidrBlock])
vpcPeeringConnectionVpcInfo_cidrBlockSet = Lens.lens (\VpcPeeringConnectionVpcInfo' {cidrBlockSet} -> cidrBlockSet) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {cidrBlockSet = a} :: VpcPeeringConnectionVpcInfo) Core.. Lens.mapping Lens._Coerce

-- | The AWS account ID of the VPC owner.
vpcPeeringConnectionVpcInfo_ownerId :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe Core.Text)
vpcPeeringConnectionVpcInfo_ownerId = Lens.lens (\VpcPeeringConnectionVpcInfo' {ownerId} -> ownerId) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {ownerId = a} :: VpcPeeringConnectionVpcInfo)

-- | The IPv6 CIDR block for the VPC.
vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe [Ipv6CidrBlock])
vpcPeeringConnectionVpcInfo_ipv6CidrBlockSet = Lens.lens (\VpcPeeringConnectionVpcInfo' {ipv6CidrBlockSet} -> ipv6CidrBlockSet) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {ipv6CidrBlockSet = a} :: VpcPeeringConnectionVpcInfo) Core.. Lens.mapping Lens._Coerce

-- | The Region in which the VPC is located.
vpcPeeringConnectionVpcInfo_region :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe Core.Text)
vpcPeeringConnectionVpcInfo_region = Lens.lens (\VpcPeeringConnectionVpcInfo' {region} -> region) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {region = a} :: VpcPeeringConnectionVpcInfo)

-- | The ID of the VPC.
vpcPeeringConnectionVpcInfo_vpcId :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe Core.Text)
vpcPeeringConnectionVpcInfo_vpcId = Lens.lens (\VpcPeeringConnectionVpcInfo' {vpcId} -> vpcId) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {vpcId = a} :: VpcPeeringConnectionVpcInfo)

-- | The IPv4 CIDR block for the VPC.
vpcPeeringConnectionVpcInfo_cidrBlock :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe Core.Text)
vpcPeeringConnectionVpcInfo_cidrBlock = Lens.lens (\VpcPeeringConnectionVpcInfo' {cidrBlock} -> cidrBlock) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {cidrBlock = a} :: VpcPeeringConnectionVpcInfo)

-- | Information about the VPC peering connection options for the accepter or
-- requester VPC.
vpcPeeringConnectionVpcInfo_peeringOptions :: Lens.Lens' VpcPeeringConnectionVpcInfo (Core.Maybe VpcPeeringConnectionOptionsDescription)
vpcPeeringConnectionVpcInfo_peeringOptions = Lens.lens (\VpcPeeringConnectionVpcInfo' {peeringOptions} -> peeringOptions) (\s@VpcPeeringConnectionVpcInfo' {} a -> s {peeringOptions = a} :: VpcPeeringConnectionVpcInfo)

instance Core.FromXML VpcPeeringConnectionVpcInfo where
  parseXML x =
    VpcPeeringConnectionVpcInfo'
      Core.<$> ( x Core..@? "cidrBlockSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "ownerId")
      Core.<*> ( x Core..@? "ipv6CidrBlockSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "region")
      Core.<*> (x Core..@? "vpcId")
      Core.<*> (x Core..@? "cidrBlock")
      Core.<*> (x Core..@? "peeringOptions")

instance Core.Hashable VpcPeeringConnectionVpcInfo

instance Core.NFData VpcPeeringConnectionVpcInfo
