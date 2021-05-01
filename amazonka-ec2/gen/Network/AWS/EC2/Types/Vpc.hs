{-# LANGUAGE DeriveDataTypeable #-}
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

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.Tag
import Network.AWS.EC2.Types.Tenancy
import Network.AWS.EC2.Types.VpcCidrBlockAssociation
import Network.AWS.EC2.Types.VpcIpv6CidrBlockAssociation
import Network.AWS.EC2.Types.VpcState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a VPC.
--
-- /See:/ 'newVpc' smart constructor.
data Vpc = Vpc'
  { -- | The ID of the AWS account that owns the VPC.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the VPC is the default VPC.
    isDefault :: Prelude.Maybe Prelude.Bool,
    -- | Information about the IPv4 CIDR blocks associated with the VPC.
    cidrBlockAssociationSet :: Prelude.Maybe [VpcCidrBlockAssociation],
    -- | Information about the IPv6 CIDR blocks associated with the VPC.
    ipv6CidrBlockAssociationSet :: Prelude.Maybe [VpcIpv6CidrBlockAssociation],
    -- | Any tags assigned to the VPC.
    tags :: Prelude.Maybe [Tag],
    -- | The primary IPv4 CIDR block for the VPC.
    cidrBlock :: Prelude.Text,
    -- | The ID of the set of DHCP options you\'ve associated with the VPC.
    dhcpOptionsId :: Prelude.Text,
    -- | The allowed tenancy of instances launched into the VPC.
    instanceTenancy :: Tenancy,
    -- | The current state of the VPC.
    state :: VpcState,
    -- | The ID of the VPC.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'dhcpOptionsId'
  Prelude.Text ->
  -- | 'instanceTenancy'
  Tenancy ->
  -- | 'state'
  VpcState ->
  -- | 'vpcId'
  Prelude.Text ->
  Vpc
newVpc
  pCidrBlock_
  pDhcpOptionsId_
  pInstanceTenancy_
  pState_
  pVpcId_ =
    Vpc'
      { ownerId = Prelude.Nothing,
        isDefault = Prelude.Nothing,
        cidrBlockAssociationSet = Prelude.Nothing,
        ipv6CidrBlockAssociationSet = Prelude.Nothing,
        tags = Prelude.Nothing,
        cidrBlock = pCidrBlock_,
        dhcpOptionsId = pDhcpOptionsId_,
        instanceTenancy = pInstanceTenancy_,
        state = pState_,
        vpcId = pVpcId_
      }

-- | The ID of the AWS account that owns the VPC.
vpc_ownerId :: Lens.Lens' Vpc (Prelude.Maybe Prelude.Text)
vpc_ownerId = Lens.lens (\Vpc' {ownerId} -> ownerId) (\s@Vpc' {} a -> s {ownerId = a} :: Vpc)

-- | Indicates whether the VPC is the default VPC.
vpc_isDefault :: Lens.Lens' Vpc (Prelude.Maybe Prelude.Bool)
vpc_isDefault = Lens.lens (\Vpc' {isDefault} -> isDefault) (\s@Vpc' {} a -> s {isDefault = a} :: Vpc)

-- | Information about the IPv4 CIDR blocks associated with the VPC.
vpc_cidrBlockAssociationSet :: Lens.Lens' Vpc (Prelude.Maybe [VpcCidrBlockAssociation])
vpc_cidrBlockAssociationSet = Lens.lens (\Vpc' {cidrBlockAssociationSet} -> cidrBlockAssociationSet) (\s@Vpc' {} a -> s {cidrBlockAssociationSet = a} :: Vpc) Prelude.. Lens.mapping Prelude._Coerce

-- | Information about the IPv6 CIDR blocks associated with the VPC.
vpc_ipv6CidrBlockAssociationSet :: Lens.Lens' Vpc (Prelude.Maybe [VpcIpv6CidrBlockAssociation])
vpc_ipv6CidrBlockAssociationSet = Lens.lens (\Vpc' {ipv6CidrBlockAssociationSet} -> ipv6CidrBlockAssociationSet) (\s@Vpc' {} a -> s {ipv6CidrBlockAssociationSet = a} :: Vpc) Prelude.. Lens.mapping Prelude._Coerce

-- | Any tags assigned to the VPC.
vpc_tags :: Lens.Lens' Vpc (Prelude.Maybe [Tag])
vpc_tags = Lens.lens (\Vpc' {tags} -> tags) (\s@Vpc' {} a -> s {tags = a} :: Vpc) Prelude.. Lens.mapping Prelude._Coerce

-- | The primary IPv4 CIDR block for the VPC.
vpc_cidrBlock :: Lens.Lens' Vpc Prelude.Text
vpc_cidrBlock = Lens.lens (\Vpc' {cidrBlock} -> cidrBlock) (\s@Vpc' {} a -> s {cidrBlock = a} :: Vpc)

-- | The ID of the set of DHCP options you\'ve associated with the VPC.
vpc_dhcpOptionsId :: Lens.Lens' Vpc Prelude.Text
vpc_dhcpOptionsId = Lens.lens (\Vpc' {dhcpOptionsId} -> dhcpOptionsId) (\s@Vpc' {} a -> s {dhcpOptionsId = a} :: Vpc)

-- | The allowed tenancy of instances launched into the VPC.
vpc_instanceTenancy :: Lens.Lens' Vpc Tenancy
vpc_instanceTenancy = Lens.lens (\Vpc' {instanceTenancy} -> instanceTenancy) (\s@Vpc' {} a -> s {instanceTenancy = a} :: Vpc)

-- | The current state of the VPC.
vpc_state :: Lens.Lens' Vpc VpcState
vpc_state = Lens.lens (\Vpc' {state} -> state) (\s@Vpc' {} a -> s {state = a} :: Vpc)

-- | The ID of the VPC.
vpc_vpcId :: Lens.Lens' Vpc Prelude.Text
vpc_vpcId = Lens.lens (\Vpc' {vpcId} -> vpcId) (\s@Vpc' {} a -> s {vpcId = a} :: Vpc)

instance Prelude.FromXML Vpc where
  parseXML x =
    Vpc'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "isDefault")
      Prelude.<*> ( x Prelude..@? "cidrBlockAssociationSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "ipv6CidrBlockAssociationSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@ "cidrBlock")
      Prelude.<*> (x Prelude..@ "dhcpOptionsId")
      Prelude.<*> (x Prelude..@ "instanceTenancy")
      Prelude.<*> (x Prelude..@ "state")
      Prelude.<*> (x Prelude..@ "vpcId")

instance Prelude.Hashable Vpc

instance Prelude.NFData Vpc
