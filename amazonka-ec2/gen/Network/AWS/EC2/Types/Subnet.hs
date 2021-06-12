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
-- Module      : Network.AWS.EC2.Types.Subnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Subnet where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SubnetIpv6CidrBlockAssociation
import Network.AWS.EC2.Types.SubnetState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a subnet.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The ID of the AWS account that owns the subnet.
    ownerId :: Core.Maybe Core.Text,
    -- | The customer-owned IPv4 address pool associated with the subnet.
    customerOwnedIpv4Pool :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the subnet.
    subnetArn :: Core.Maybe Core.Text,
    -- | Indicates whether a network interface created in this subnet (including
    -- a network interface created by RunInstances) receives an IPv6 address.
    assignIpv6AddressOnCreation :: Core.Maybe Core.Bool,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Core.Maybe Core.Text,
    -- | Indicates whether instances launched in this subnet receive a public
    -- IPv4 address.
    mapPublicIpOnLaunch :: Core.Maybe Core.Bool,
    -- | The AZ ID of the subnet.
    availabilityZoneId :: Core.Maybe Core.Text,
    -- | Information about the IPv6 CIDR blocks associated with the subnet.
    ipv6CidrBlockAssociationSet :: Core.Maybe [SubnetIpv6CidrBlockAssociation],
    -- | Any tags assigned to the subnet.
    tags :: Core.Maybe [Tag],
    -- | Indicates whether this is the default subnet for the Availability Zone.
    defaultForAz :: Core.Maybe Core.Bool,
    -- | Indicates whether a network interface created in this subnet (including
    -- a network interface created by RunInstances) receives a customer-owned
    -- IPv4 address.
    mapCustomerOwnedIpOnLaunch :: Core.Maybe Core.Bool,
    -- | The Availability Zone of the subnet.
    availabilityZone :: Core.Text,
    -- | The number of unused private IPv4 addresses in the subnet. The IPv4
    -- addresses for any stopped instances are considered unavailable.
    availableIpAddressCount :: Core.Int,
    -- | The IPv4 CIDR block assigned to the subnet.
    cidrBlock :: Core.Text,
    -- | The current state of the subnet.
    state :: SubnetState,
    -- | The ID of the subnet.
    subnetId :: Core.Text,
    -- | The ID of the VPC the subnet is in.
    vpcId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Subnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ownerId', 'subnet_ownerId' - The ID of the AWS account that owns the subnet.
--
-- 'customerOwnedIpv4Pool', 'subnet_customerOwnedIpv4Pool' - The customer-owned IPv4 address pool associated with the subnet.
--
-- 'subnetArn', 'subnet_subnetArn' - The Amazon Resource Name (ARN) of the subnet.
--
-- 'assignIpv6AddressOnCreation', 'subnet_assignIpv6AddressOnCreation' - Indicates whether a network interface created in this subnet (including
-- a network interface created by RunInstances) receives an IPv6 address.
--
-- 'outpostArn', 'subnet_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'mapPublicIpOnLaunch', 'subnet_mapPublicIpOnLaunch' - Indicates whether instances launched in this subnet receive a public
-- IPv4 address.
--
-- 'availabilityZoneId', 'subnet_availabilityZoneId' - The AZ ID of the subnet.
--
-- 'ipv6CidrBlockAssociationSet', 'subnet_ipv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the subnet.
--
-- 'tags', 'subnet_tags' - Any tags assigned to the subnet.
--
-- 'defaultForAz', 'subnet_defaultForAz' - Indicates whether this is the default subnet for the Availability Zone.
--
-- 'mapCustomerOwnedIpOnLaunch', 'subnet_mapCustomerOwnedIpOnLaunch' - Indicates whether a network interface created in this subnet (including
-- a network interface created by RunInstances) receives a customer-owned
-- IPv4 address.
--
-- 'availabilityZone', 'subnet_availabilityZone' - The Availability Zone of the subnet.
--
-- 'availableIpAddressCount', 'subnet_availableIpAddressCount' - The number of unused private IPv4 addresses in the subnet. The IPv4
-- addresses for any stopped instances are considered unavailable.
--
-- 'cidrBlock', 'subnet_cidrBlock' - The IPv4 CIDR block assigned to the subnet.
--
-- 'state', 'subnet_state' - The current state of the subnet.
--
-- 'subnetId', 'subnet_subnetId' - The ID of the subnet.
--
-- 'vpcId', 'subnet_vpcId' - The ID of the VPC the subnet is in.
newSubnet ::
  -- | 'availabilityZone'
  Core.Text ->
  -- | 'availableIpAddressCount'
  Core.Int ->
  -- | 'cidrBlock'
  Core.Text ->
  -- | 'state'
  SubnetState ->
  -- | 'subnetId'
  Core.Text ->
  -- | 'vpcId'
  Core.Text ->
  Subnet
newSubnet
  pAvailabilityZone_
  pAvailableIpAddressCount_
  pCidrBlock_
  pState_
  pSubnetId_
  pVpcId_ =
    Subnet'
      { ownerId = Core.Nothing,
        customerOwnedIpv4Pool = Core.Nothing,
        subnetArn = Core.Nothing,
        assignIpv6AddressOnCreation = Core.Nothing,
        outpostArn = Core.Nothing,
        mapPublicIpOnLaunch = Core.Nothing,
        availabilityZoneId = Core.Nothing,
        ipv6CidrBlockAssociationSet = Core.Nothing,
        tags = Core.Nothing,
        defaultForAz = Core.Nothing,
        mapCustomerOwnedIpOnLaunch = Core.Nothing,
        availabilityZone = pAvailabilityZone_,
        availableIpAddressCount = pAvailableIpAddressCount_,
        cidrBlock = pCidrBlock_,
        state = pState_,
        subnetId = pSubnetId_,
        vpcId = pVpcId_
      }

-- | The ID of the AWS account that owns the subnet.
subnet_ownerId :: Lens.Lens' Subnet (Core.Maybe Core.Text)
subnet_ownerId = Lens.lens (\Subnet' {ownerId} -> ownerId) (\s@Subnet' {} a -> s {ownerId = a} :: Subnet)

-- | The customer-owned IPv4 address pool associated with the subnet.
subnet_customerOwnedIpv4Pool :: Lens.Lens' Subnet (Core.Maybe Core.Text)
subnet_customerOwnedIpv4Pool = Lens.lens (\Subnet' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@Subnet' {} a -> s {customerOwnedIpv4Pool = a} :: Subnet)

-- | The Amazon Resource Name (ARN) of the subnet.
subnet_subnetArn :: Lens.Lens' Subnet (Core.Maybe Core.Text)
subnet_subnetArn = Lens.lens (\Subnet' {subnetArn} -> subnetArn) (\s@Subnet' {} a -> s {subnetArn = a} :: Subnet)

-- | Indicates whether a network interface created in this subnet (including
-- a network interface created by RunInstances) receives an IPv6 address.
subnet_assignIpv6AddressOnCreation :: Lens.Lens' Subnet (Core.Maybe Core.Bool)
subnet_assignIpv6AddressOnCreation = Lens.lens (\Subnet' {assignIpv6AddressOnCreation} -> assignIpv6AddressOnCreation) (\s@Subnet' {} a -> s {assignIpv6AddressOnCreation = a} :: Subnet)

-- | The Amazon Resource Name (ARN) of the Outpost.
subnet_outpostArn :: Lens.Lens' Subnet (Core.Maybe Core.Text)
subnet_outpostArn = Lens.lens (\Subnet' {outpostArn} -> outpostArn) (\s@Subnet' {} a -> s {outpostArn = a} :: Subnet)

-- | Indicates whether instances launched in this subnet receive a public
-- IPv4 address.
subnet_mapPublicIpOnLaunch :: Lens.Lens' Subnet (Core.Maybe Core.Bool)
subnet_mapPublicIpOnLaunch = Lens.lens (\Subnet' {mapPublicIpOnLaunch} -> mapPublicIpOnLaunch) (\s@Subnet' {} a -> s {mapPublicIpOnLaunch = a} :: Subnet)

-- | The AZ ID of the subnet.
subnet_availabilityZoneId :: Lens.Lens' Subnet (Core.Maybe Core.Text)
subnet_availabilityZoneId = Lens.lens (\Subnet' {availabilityZoneId} -> availabilityZoneId) (\s@Subnet' {} a -> s {availabilityZoneId = a} :: Subnet)

-- | Information about the IPv6 CIDR blocks associated with the subnet.
subnet_ipv6CidrBlockAssociationSet :: Lens.Lens' Subnet (Core.Maybe [SubnetIpv6CidrBlockAssociation])
subnet_ipv6CidrBlockAssociationSet = Lens.lens (\Subnet' {ipv6CidrBlockAssociationSet} -> ipv6CidrBlockAssociationSet) (\s@Subnet' {} a -> s {ipv6CidrBlockAssociationSet = a} :: Subnet) Core.. Lens.mapping Lens._Coerce

-- | Any tags assigned to the subnet.
subnet_tags :: Lens.Lens' Subnet (Core.Maybe [Tag])
subnet_tags = Lens.lens (\Subnet' {tags} -> tags) (\s@Subnet' {} a -> s {tags = a} :: Subnet) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether this is the default subnet for the Availability Zone.
subnet_defaultForAz :: Lens.Lens' Subnet (Core.Maybe Core.Bool)
subnet_defaultForAz = Lens.lens (\Subnet' {defaultForAz} -> defaultForAz) (\s@Subnet' {} a -> s {defaultForAz = a} :: Subnet)

-- | Indicates whether a network interface created in this subnet (including
-- a network interface created by RunInstances) receives a customer-owned
-- IPv4 address.
subnet_mapCustomerOwnedIpOnLaunch :: Lens.Lens' Subnet (Core.Maybe Core.Bool)
subnet_mapCustomerOwnedIpOnLaunch = Lens.lens (\Subnet' {mapCustomerOwnedIpOnLaunch} -> mapCustomerOwnedIpOnLaunch) (\s@Subnet' {} a -> s {mapCustomerOwnedIpOnLaunch = a} :: Subnet)

-- | The Availability Zone of the subnet.
subnet_availabilityZone :: Lens.Lens' Subnet Core.Text
subnet_availabilityZone = Lens.lens (\Subnet' {availabilityZone} -> availabilityZone) (\s@Subnet' {} a -> s {availabilityZone = a} :: Subnet)

-- | The number of unused private IPv4 addresses in the subnet. The IPv4
-- addresses for any stopped instances are considered unavailable.
subnet_availableIpAddressCount :: Lens.Lens' Subnet Core.Int
subnet_availableIpAddressCount = Lens.lens (\Subnet' {availableIpAddressCount} -> availableIpAddressCount) (\s@Subnet' {} a -> s {availableIpAddressCount = a} :: Subnet)

-- | The IPv4 CIDR block assigned to the subnet.
subnet_cidrBlock :: Lens.Lens' Subnet Core.Text
subnet_cidrBlock = Lens.lens (\Subnet' {cidrBlock} -> cidrBlock) (\s@Subnet' {} a -> s {cidrBlock = a} :: Subnet)

-- | The current state of the subnet.
subnet_state :: Lens.Lens' Subnet SubnetState
subnet_state = Lens.lens (\Subnet' {state} -> state) (\s@Subnet' {} a -> s {state = a} :: Subnet)

-- | The ID of the subnet.
subnet_subnetId :: Lens.Lens' Subnet Core.Text
subnet_subnetId = Lens.lens (\Subnet' {subnetId} -> subnetId) (\s@Subnet' {} a -> s {subnetId = a} :: Subnet)

-- | The ID of the VPC the subnet is in.
subnet_vpcId :: Lens.Lens' Subnet Core.Text
subnet_vpcId = Lens.lens (\Subnet' {vpcId} -> vpcId) (\s@Subnet' {} a -> s {vpcId = a} :: Subnet)

instance Core.FromXML Subnet where
  parseXML x =
    Subnet'
      Core.<$> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "customerOwnedIpv4Pool")
      Core.<*> (x Core..@? "subnetArn")
      Core.<*> (x Core..@? "assignIpv6AddressOnCreation")
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> (x Core..@? "mapPublicIpOnLaunch")
      Core.<*> (x Core..@? "availabilityZoneId")
      Core.<*> ( x Core..@? "ipv6CidrBlockAssociationSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "defaultForAz")
      Core.<*> (x Core..@? "mapCustomerOwnedIpOnLaunch")
      Core.<*> (x Core..@ "availabilityZone")
      Core.<*> (x Core..@ "availableIpAddressCount")
      Core.<*> (x Core..@ "cidrBlock")
      Core.<*> (x Core..@ "state")
      Core.<*> (x Core..@ "subnetId")
      Core.<*> (x Core..@ "vpcId")

instance Core.Hashable Subnet

instance Core.NFData Subnet
