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
-- Module      : Network.AWS.EC2.Types.Subnet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Subnet where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.SubnetIpv6CidrBlockAssociation
import Network.AWS.EC2.Types.SubnetState
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a subnet.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The ID of the AWS account that owns the subnet.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The customer-owned IPv4 address pool associated with the subnet.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the subnet.
    subnetArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a network interface created in this subnet (including
    -- a network interface created by RunInstances) receives an IPv6 address.
    assignIpv6AddressOnCreation :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether instances launched in this subnet receive a public
    -- IPv4 address.
    mapPublicIpOnLaunch :: Prelude.Maybe Prelude.Bool,
    -- | The AZ ID of the subnet.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | Information about the IPv6 CIDR blocks associated with the subnet.
    ipv6CidrBlockAssociationSet :: Prelude.Maybe [SubnetIpv6CidrBlockAssociation],
    -- | Any tags assigned to the subnet.
    tags :: Prelude.Maybe [Tag],
    -- | Indicates whether this is the default subnet for the Availability Zone.
    defaultForAz :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a network interface created in this subnet (including
    -- a network interface created by RunInstances) receives a customer-owned
    -- IPv4 address.
    mapCustomerOwnedIpOnLaunch :: Prelude.Maybe Prelude.Bool,
    -- | The Availability Zone of the subnet.
    availabilityZone :: Prelude.Text,
    -- | The number of unused private IPv4 addresses in the subnet. The IPv4
    -- addresses for any stopped instances are considered unavailable.
    availableIpAddressCount :: Prelude.Int,
    -- | The IPv4 CIDR block assigned to the subnet.
    cidrBlock :: Prelude.Text,
    -- | The current state of the subnet.
    state :: SubnetState,
    -- | The ID of the subnet.
    subnetId :: Prelude.Text,
    -- | The ID of the VPC the subnet is in.
    vpcId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'availableIpAddressCount'
  Prelude.Int ->
  -- | 'cidrBlock'
  Prelude.Text ->
  -- | 'state'
  SubnetState ->
  -- | 'subnetId'
  Prelude.Text ->
  -- | 'vpcId'
  Prelude.Text ->
  Subnet
newSubnet
  pAvailabilityZone_
  pAvailableIpAddressCount_
  pCidrBlock_
  pState_
  pSubnetId_
  pVpcId_ =
    Subnet'
      { ownerId = Prelude.Nothing,
        customerOwnedIpv4Pool = Prelude.Nothing,
        subnetArn = Prelude.Nothing,
        assignIpv6AddressOnCreation = Prelude.Nothing,
        outpostArn = Prelude.Nothing,
        mapPublicIpOnLaunch = Prelude.Nothing,
        availabilityZoneId = Prelude.Nothing,
        ipv6CidrBlockAssociationSet = Prelude.Nothing,
        tags = Prelude.Nothing,
        defaultForAz = Prelude.Nothing,
        mapCustomerOwnedIpOnLaunch = Prelude.Nothing,
        availabilityZone = pAvailabilityZone_,
        availableIpAddressCount = pAvailableIpAddressCount_,
        cidrBlock = pCidrBlock_,
        state = pState_,
        subnetId = pSubnetId_,
        vpcId = pVpcId_
      }

-- | The ID of the AWS account that owns the subnet.
subnet_ownerId :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_ownerId = Lens.lens (\Subnet' {ownerId} -> ownerId) (\s@Subnet' {} a -> s {ownerId = a} :: Subnet)

-- | The customer-owned IPv4 address pool associated with the subnet.
subnet_customerOwnedIpv4Pool :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_customerOwnedIpv4Pool = Lens.lens (\Subnet' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@Subnet' {} a -> s {customerOwnedIpv4Pool = a} :: Subnet)

-- | The Amazon Resource Name (ARN) of the subnet.
subnet_subnetArn :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetArn = Lens.lens (\Subnet' {subnetArn} -> subnetArn) (\s@Subnet' {} a -> s {subnetArn = a} :: Subnet)

-- | Indicates whether a network interface created in this subnet (including
-- a network interface created by RunInstances) receives an IPv6 address.
subnet_assignIpv6AddressOnCreation :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Bool)
subnet_assignIpv6AddressOnCreation = Lens.lens (\Subnet' {assignIpv6AddressOnCreation} -> assignIpv6AddressOnCreation) (\s@Subnet' {} a -> s {assignIpv6AddressOnCreation = a} :: Subnet)

-- | The Amazon Resource Name (ARN) of the Outpost.
subnet_outpostArn :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_outpostArn = Lens.lens (\Subnet' {outpostArn} -> outpostArn) (\s@Subnet' {} a -> s {outpostArn = a} :: Subnet)

-- | Indicates whether instances launched in this subnet receive a public
-- IPv4 address.
subnet_mapPublicIpOnLaunch :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Bool)
subnet_mapPublicIpOnLaunch = Lens.lens (\Subnet' {mapPublicIpOnLaunch} -> mapPublicIpOnLaunch) (\s@Subnet' {} a -> s {mapPublicIpOnLaunch = a} :: Subnet)

-- | The AZ ID of the subnet.
subnet_availabilityZoneId :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_availabilityZoneId = Lens.lens (\Subnet' {availabilityZoneId} -> availabilityZoneId) (\s@Subnet' {} a -> s {availabilityZoneId = a} :: Subnet)

-- | Information about the IPv6 CIDR blocks associated with the subnet.
subnet_ipv6CidrBlockAssociationSet :: Lens.Lens' Subnet (Prelude.Maybe [SubnetIpv6CidrBlockAssociation])
subnet_ipv6CidrBlockAssociationSet = Lens.lens (\Subnet' {ipv6CidrBlockAssociationSet} -> ipv6CidrBlockAssociationSet) (\s@Subnet' {} a -> s {ipv6CidrBlockAssociationSet = a} :: Subnet) Prelude.. Lens.mapping Prelude._Coerce

-- | Any tags assigned to the subnet.
subnet_tags :: Lens.Lens' Subnet (Prelude.Maybe [Tag])
subnet_tags = Lens.lens (\Subnet' {tags} -> tags) (\s@Subnet' {} a -> s {tags = a} :: Subnet) Prelude.. Lens.mapping Prelude._Coerce

-- | Indicates whether this is the default subnet for the Availability Zone.
subnet_defaultForAz :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Bool)
subnet_defaultForAz = Lens.lens (\Subnet' {defaultForAz} -> defaultForAz) (\s@Subnet' {} a -> s {defaultForAz = a} :: Subnet)

-- | Indicates whether a network interface created in this subnet (including
-- a network interface created by RunInstances) receives a customer-owned
-- IPv4 address.
subnet_mapCustomerOwnedIpOnLaunch :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Bool)
subnet_mapCustomerOwnedIpOnLaunch = Lens.lens (\Subnet' {mapCustomerOwnedIpOnLaunch} -> mapCustomerOwnedIpOnLaunch) (\s@Subnet' {} a -> s {mapCustomerOwnedIpOnLaunch = a} :: Subnet)

-- | The Availability Zone of the subnet.
subnet_availabilityZone :: Lens.Lens' Subnet Prelude.Text
subnet_availabilityZone = Lens.lens (\Subnet' {availabilityZone} -> availabilityZone) (\s@Subnet' {} a -> s {availabilityZone = a} :: Subnet)

-- | The number of unused private IPv4 addresses in the subnet. The IPv4
-- addresses for any stopped instances are considered unavailable.
subnet_availableIpAddressCount :: Lens.Lens' Subnet Prelude.Int
subnet_availableIpAddressCount = Lens.lens (\Subnet' {availableIpAddressCount} -> availableIpAddressCount) (\s@Subnet' {} a -> s {availableIpAddressCount = a} :: Subnet)

-- | The IPv4 CIDR block assigned to the subnet.
subnet_cidrBlock :: Lens.Lens' Subnet Prelude.Text
subnet_cidrBlock = Lens.lens (\Subnet' {cidrBlock} -> cidrBlock) (\s@Subnet' {} a -> s {cidrBlock = a} :: Subnet)

-- | The current state of the subnet.
subnet_state :: Lens.Lens' Subnet SubnetState
subnet_state = Lens.lens (\Subnet' {state} -> state) (\s@Subnet' {} a -> s {state = a} :: Subnet)

-- | The ID of the subnet.
subnet_subnetId :: Lens.Lens' Subnet Prelude.Text
subnet_subnetId = Lens.lens (\Subnet' {subnetId} -> subnetId) (\s@Subnet' {} a -> s {subnetId = a} :: Subnet)

-- | The ID of the VPC the subnet is in.
subnet_vpcId :: Lens.Lens' Subnet Prelude.Text
subnet_vpcId = Lens.lens (\Subnet' {vpcId} -> vpcId) (\s@Subnet' {} a -> s {vpcId = a} :: Subnet)

instance Prelude.FromXML Subnet where
  parseXML x =
    Subnet'
      Prelude.<$> (x Prelude..@? "ownerId")
      Prelude.<*> (x Prelude..@? "customerOwnedIpv4Pool")
      Prelude.<*> (x Prelude..@? "subnetArn")
      Prelude.<*> (x Prelude..@? "assignIpv6AddressOnCreation")
      Prelude.<*> (x Prelude..@? "outpostArn")
      Prelude.<*> (x Prelude..@? "mapPublicIpOnLaunch")
      Prelude.<*> (x Prelude..@? "availabilityZoneId")
      Prelude.<*> ( x Prelude..@? "ipv6CidrBlockAssociationSet"
                      Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> ( x Prelude..@? "tagSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                  )
      Prelude.<*> (x Prelude..@? "defaultForAz")
      Prelude.<*> (x Prelude..@? "mapCustomerOwnedIpOnLaunch")
      Prelude.<*> (x Prelude..@ "availabilityZone")
      Prelude.<*> (x Prelude..@ "availableIpAddressCount")
      Prelude.<*> (x Prelude..@ "cidrBlock")
      Prelude.<*> (x Prelude..@ "state")
      Prelude.<*> (x Prelude..@ "subnetId")
      Prelude.<*> (x Prelude..@ "vpcId")

instance Prelude.Hashable Subnet

instance Prelude.NFData Subnet
