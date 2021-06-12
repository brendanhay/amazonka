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
-- Module      : Network.AWS.EC2.Types.Vpc
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Vpc where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.EC2.Types.VpcCidrBlockAssociation
import Network.AWS.EC2.Types.VpcIpv6CidrBlockAssociation
import Network.AWS.EC2.Types.VpcState
import qualified Network.AWS.Lens as Lens

-- | Describes a VPC.
--
-- /See:/ 'newVpc' smart constructor.
data Vpc = Vpc'
  { -- | The ID of the AWS account that owns the VPC.
    ownerId :: Core.Maybe Core.Text,
    -- | Indicates whether the VPC is the default VPC.
    isDefault :: Core.Maybe Core.Bool,
    -- | Information about the IPv4 CIDR blocks associated with the VPC.
    cidrBlockAssociationSet :: Core.Maybe [VpcCidrBlockAssociation],
    -- | Information about the IPv6 CIDR blocks associated with the VPC.
    ipv6CidrBlockAssociationSet :: Core.Maybe [VpcIpv6CidrBlockAssociation],
    -- | Any tags assigned to the VPC.
    tags :: Core.Maybe [Tag],
    -- | The primary IPv4 CIDR block for the VPC.
    cidrBlock :: Core.Text,
    -- | The ID of the set of DHCP options you\'ve associated with the VPC.
    dhcpOptionsId :: Core.Text,
    -- | The allowed tenancy of instances launched into the VPC.
    instanceTenancy :: Tenancy,
    -- | The current state of the VPC.
    state :: VpcState,
    -- | The ID of the VPC.
    vpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Vpc' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'vpc_ownerId' - The ID of the AWS account that owns the VPC.
--
-- 'isDefault', 'vpc_isDefault' - Indicates whether the VPC is the default VPC.
--
-- 'cidrBlockAssociationSet', 'vpc_cidrBlockAssociationSet' - Information about the IPv4 CIDR blocks associated with the VPC.
--
-- 'ipv6CidrBlockAssociationSet', 'vpc_ipv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the VPC.
--
-- 'tags', 'vpc_tags' - Any tags assigned to the VPC.
--
-- 'cidrBlock', 'vpc_cidrBlock' - The primary IPv4 CIDR block for the VPC.
--
-- 'dhcpOptionsId', 'vpc_dhcpOptionsId' - The ID of the set of DHCP options you\'ve associated with the VPC.
--
-- 'instanceTenancy', 'vpc_instanceTenancy' - The allowed tenancy of instances launched into the VPC.
--
-- 'state', 'vpc_state' - The current state of the VPC.
--
-- 'vpcId', 'vpc_vpcId' - The ID of the VPC.
newVpc ::
  -- | 'cidrBlock'
  Core.Text ->
  -- | 'dhcpOptionsId'
  Core.Text ->
  -- | 'instanceTenancy'
  Tenancy ->
  -- | 'state'
  VpcState ->
  -- | 'vpcId'
  Core.Text ->
  Vpc
newVpc
  pCidrBlock_
  pDhcpOptionsId_
  pInstanceTenancy_
  pState_
  pVpcId_ =
    Vpc'
      { ownerId = Core.Nothing,
        isDefault = Core.Nothing,
        cidrBlockAssociationSet = Core.Nothing,
        ipv6CidrBlockAssociationSet = Core.Nothing,
        tags = Core.Nothing,
        cidrBlock = pCidrBlock_,
        dhcpOptionsId = pDhcpOptionsId_,
        instanceTenancy = pInstanceTenancy_,
        state = pState_,
        vpcId = pVpcId_
      }

-- | The ID of the AWS account that owns the VPC.
vpc_ownerId :: Lens.Lens' Vpc (Core.Maybe Core.Text)
vpc_ownerId = Lens.lens (\Vpc' {ownerId} -> ownerId) (\s@Vpc' {} a -> s {ownerId = a} :: Vpc)

-- | Indicates whether the VPC is the default VPC.
vpc_isDefault :: Lens.Lens' Vpc (Core.Maybe Core.Bool)
vpc_isDefault = Lens.lens (\Vpc' {isDefault} -> isDefault) (\s@Vpc' {} a -> s {isDefault = a} :: Vpc)

-- | Information about the IPv4 CIDR blocks associated with the VPC.
vpc_cidrBlockAssociationSet :: Lens.Lens' Vpc (Core.Maybe [VpcCidrBlockAssociation])
vpc_cidrBlockAssociationSet = Lens.lens (\Vpc' {cidrBlockAssociationSet} -> cidrBlockAssociationSet) (\s@Vpc' {} a -> s {cidrBlockAssociationSet = a} :: Vpc) Core.. Lens.mapping Lens._Coerce

-- | Information about the IPv6 CIDR blocks associated with the VPC.
vpc_ipv6CidrBlockAssociationSet :: Lens.Lens' Vpc (Core.Maybe [VpcIpv6CidrBlockAssociation])
vpc_ipv6CidrBlockAssociationSet = Lens.lens (\Vpc' {ipv6CidrBlockAssociationSet} -> ipv6CidrBlockAssociationSet) (\s@Vpc' {} a -> s {ipv6CidrBlockAssociationSet = a} :: Vpc) Core.. Lens.mapping Lens._Coerce

-- | Any tags assigned to the VPC.
vpc_tags :: Lens.Lens' Vpc (Core.Maybe [Tag])
vpc_tags = Lens.lens (\Vpc' {tags} -> tags) (\s@Vpc' {} a -> s {tags = a} :: Vpc) Core.. Lens.mapping Lens._Coerce

-- | The primary IPv4 CIDR block for the VPC.
vpc_cidrBlock :: Lens.Lens' Vpc Core.Text
vpc_cidrBlock = Lens.lens (\Vpc' {cidrBlock} -> cidrBlock) (\s@Vpc' {} a -> s {cidrBlock = a} :: Vpc)

-- | The ID of the set of DHCP options you\'ve associated with the VPC.
vpc_dhcpOptionsId :: Lens.Lens' Vpc Core.Text
vpc_dhcpOptionsId = Lens.lens (\Vpc' {dhcpOptionsId} -> dhcpOptionsId) (\s@Vpc' {} a -> s {dhcpOptionsId = a} :: Vpc)

-- | The allowed tenancy of instances launched into the VPC.
vpc_instanceTenancy :: Lens.Lens' Vpc Tenancy
vpc_instanceTenancy = Lens.lens (\Vpc' {instanceTenancy} -> instanceTenancy) (\s@Vpc' {} a -> s {instanceTenancy = a} :: Vpc)

-- | The current state of the VPC.
vpc_state :: Lens.Lens' Vpc VpcState
vpc_state = Lens.lens (\Vpc' {state} -> state) (\s@Vpc' {} a -> s {state = a} :: Vpc)

-- | The ID of the VPC.
vpc_vpcId :: Lens.Lens' Vpc Core.Text
vpc_vpcId = Lens.lens (\Vpc' {vpcId} -> vpcId) (\s@Vpc' {} a -> s {vpcId = a} :: Vpc)

instance Core.FromXML Vpc where
  parseXML x =
    Vpc'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "isDefault")
      Core.<*> ( x Core..@? "cidrBlockAssociationSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "ipv6CidrBlockAssociationSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@ "cidrBlock")
      Core.<*> (x Core..@ "dhcpOptionsId")
      Core.<*> (x Core..@ "instanceTenancy")
      Core.<*> (x Core..@ "state")
      Core.<*> (x Core..@ "vpcId")

instance Core.Hashable Vpc

instance Core.NFData Vpc
