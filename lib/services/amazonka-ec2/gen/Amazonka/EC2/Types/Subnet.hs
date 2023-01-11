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
-- Module      : Amazonka.EC2.Types.Subnet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.Subnet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.PrivateDnsNameOptionsOnLaunch
import Amazonka.EC2.Types.SubnetIpv6CidrBlockAssociation
import Amazonka.EC2.Types.SubnetState
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a subnet.
--
-- /See:/ 'newSubnet' smart constructor.
data Subnet = Subnet'
  { -- | Indicates whether a network interface created in this subnet (including
    -- a network interface created by RunInstances) receives an IPv6 address.
    assignIpv6AddressOnCreation :: Prelude.Maybe Prelude.Bool,
    -- | The AZ ID of the subnet.
    availabilityZoneId :: Prelude.Maybe Prelude.Text,
    -- | The customer-owned IPv4 address pool associated with the subnet.
    customerOwnedIpv4Pool :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether this is the default subnet for the Availability Zone.
    defaultForAz :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether DNS queries made to the Amazon-provided DNS Resolver
    -- in this subnet should return synthetic IPv6 addresses for IPv4-only
    -- destinations.
    enableDns64 :: Prelude.Maybe Prelude.Bool,
    -- | Indicates the device position for local network interfaces in this
    -- subnet. For example, @1@ indicates local network interfaces in this
    -- subnet are the secondary network interface (eth1).
    enableLniAtDeviceIndex :: Prelude.Maybe Prelude.Int,
    -- | Information about the IPv6 CIDR blocks associated with the subnet.
    ipv6CidrBlockAssociationSet :: Prelude.Maybe [SubnetIpv6CidrBlockAssociation],
    -- | Indicates whether this is an IPv6 only subnet.
    ipv6Native :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether a network interface created in this subnet (including
    -- a network interface created by RunInstances) receives a customer-owned
    -- IPv4 address.
    mapCustomerOwnedIpOnLaunch :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether instances launched in this subnet receive a public
    -- IPv4 address.
    mapPublicIpOnLaunch :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that owns the subnet.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The type of hostnames to assign to instances in the subnet at launch. An
    -- instance hostname is based on the IPv4 address or ID of the instance.
    privateDnsNameOptionsOnLaunch :: Prelude.Maybe PrivateDnsNameOptionsOnLaunch,
    -- | The Amazon Resource Name (ARN) of the subnet.
    subnetArn :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the subnet.
    tags :: Prelude.Maybe [Tag],
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'Subnet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'assignIpv6AddressOnCreation', 'subnet_assignIpv6AddressOnCreation' - Indicates whether a network interface created in this subnet (including
-- a network interface created by RunInstances) receives an IPv6 address.
--
-- 'availabilityZoneId', 'subnet_availabilityZoneId' - The AZ ID of the subnet.
--
-- 'customerOwnedIpv4Pool', 'subnet_customerOwnedIpv4Pool' - The customer-owned IPv4 address pool associated with the subnet.
--
-- 'defaultForAz', 'subnet_defaultForAz' - Indicates whether this is the default subnet for the Availability Zone.
--
-- 'enableDns64', 'subnet_enableDns64' - Indicates whether DNS queries made to the Amazon-provided DNS Resolver
-- in this subnet should return synthetic IPv6 addresses for IPv4-only
-- destinations.
--
-- 'enableLniAtDeviceIndex', 'subnet_enableLniAtDeviceIndex' - Indicates the device position for local network interfaces in this
-- subnet. For example, @1@ indicates local network interfaces in this
-- subnet are the secondary network interface (eth1).
--
-- 'ipv6CidrBlockAssociationSet', 'subnet_ipv6CidrBlockAssociationSet' - Information about the IPv6 CIDR blocks associated with the subnet.
--
-- 'ipv6Native', 'subnet_ipv6Native' - Indicates whether this is an IPv6 only subnet.
--
-- 'mapCustomerOwnedIpOnLaunch', 'subnet_mapCustomerOwnedIpOnLaunch' - Indicates whether a network interface created in this subnet (including
-- a network interface created by RunInstances) receives a customer-owned
-- IPv4 address.
--
-- 'mapPublicIpOnLaunch', 'subnet_mapPublicIpOnLaunch' - Indicates whether instances launched in this subnet receive a public
-- IPv4 address.
--
-- 'outpostArn', 'subnet_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'ownerId', 'subnet_ownerId' - The ID of the Amazon Web Services account that owns the subnet.
--
-- 'privateDnsNameOptionsOnLaunch', 'subnet_privateDnsNameOptionsOnLaunch' - The type of hostnames to assign to instances in the subnet at launch. An
-- instance hostname is based on the IPv4 address or ID of the instance.
--
-- 'subnetArn', 'subnet_subnetArn' - The Amazon Resource Name (ARN) of the subnet.
--
-- 'tags', 'subnet_tags' - Any tags assigned to the subnet.
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
      { assignIpv6AddressOnCreation =
          Prelude.Nothing,
        availabilityZoneId = Prelude.Nothing,
        customerOwnedIpv4Pool = Prelude.Nothing,
        defaultForAz = Prelude.Nothing,
        enableDns64 = Prelude.Nothing,
        enableLniAtDeviceIndex = Prelude.Nothing,
        ipv6CidrBlockAssociationSet = Prelude.Nothing,
        ipv6Native = Prelude.Nothing,
        mapCustomerOwnedIpOnLaunch = Prelude.Nothing,
        mapPublicIpOnLaunch = Prelude.Nothing,
        outpostArn = Prelude.Nothing,
        ownerId = Prelude.Nothing,
        privateDnsNameOptionsOnLaunch = Prelude.Nothing,
        subnetArn = Prelude.Nothing,
        tags = Prelude.Nothing,
        availabilityZone = pAvailabilityZone_,
        availableIpAddressCount = pAvailableIpAddressCount_,
        cidrBlock = pCidrBlock_,
        state = pState_,
        subnetId = pSubnetId_,
        vpcId = pVpcId_
      }

-- | Indicates whether a network interface created in this subnet (including
-- a network interface created by RunInstances) receives an IPv6 address.
subnet_assignIpv6AddressOnCreation :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Bool)
subnet_assignIpv6AddressOnCreation = Lens.lens (\Subnet' {assignIpv6AddressOnCreation} -> assignIpv6AddressOnCreation) (\s@Subnet' {} a -> s {assignIpv6AddressOnCreation = a} :: Subnet)

-- | The AZ ID of the subnet.
subnet_availabilityZoneId :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_availabilityZoneId = Lens.lens (\Subnet' {availabilityZoneId} -> availabilityZoneId) (\s@Subnet' {} a -> s {availabilityZoneId = a} :: Subnet)

-- | The customer-owned IPv4 address pool associated with the subnet.
subnet_customerOwnedIpv4Pool :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_customerOwnedIpv4Pool = Lens.lens (\Subnet' {customerOwnedIpv4Pool} -> customerOwnedIpv4Pool) (\s@Subnet' {} a -> s {customerOwnedIpv4Pool = a} :: Subnet)

-- | Indicates whether this is the default subnet for the Availability Zone.
subnet_defaultForAz :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Bool)
subnet_defaultForAz = Lens.lens (\Subnet' {defaultForAz} -> defaultForAz) (\s@Subnet' {} a -> s {defaultForAz = a} :: Subnet)

-- | Indicates whether DNS queries made to the Amazon-provided DNS Resolver
-- in this subnet should return synthetic IPv6 addresses for IPv4-only
-- destinations.
subnet_enableDns64 :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Bool)
subnet_enableDns64 = Lens.lens (\Subnet' {enableDns64} -> enableDns64) (\s@Subnet' {} a -> s {enableDns64 = a} :: Subnet)

-- | Indicates the device position for local network interfaces in this
-- subnet. For example, @1@ indicates local network interfaces in this
-- subnet are the secondary network interface (eth1).
subnet_enableLniAtDeviceIndex :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Int)
subnet_enableLniAtDeviceIndex = Lens.lens (\Subnet' {enableLniAtDeviceIndex} -> enableLniAtDeviceIndex) (\s@Subnet' {} a -> s {enableLniAtDeviceIndex = a} :: Subnet)

-- | Information about the IPv6 CIDR blocks associated with the subnet.
subnet_ipv6CidrBlockAssociationSet :: Lens.Lens' Subnet (Prelude.Maybe [SubnetIpv6CidrBlockAssociation])
subnet_ipv6CidrBlockAssociationSet = Lens.lens (\Subnet' {ipv6CidrBlockAssociationSet} -> ipv6CidrBlockAssociationSet) (\s@Subnet' {} a -> s {ipv6CidrBlockAssociationSet = a} :: Subnet) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether this is an IPv6 only subnet.
subnet_ipv6Native :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Bool)
subnet_ipv6Native = Lens.lens (\Subnet' {ipv6Native} -> ipv6Native) (\s@Subnet' {} a -> s {ipv6Native = a} :: Subnet)

-- | Indicates whether a network interface created in this subnet (including
-- a network interface created by RunInstances) receives a customer-owned
-- IPv4 address.
subnet_mapCustomerOwnedIpOnLaunch :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Bool)
subnet_mapCustomerOwnedIpOnLaunch = Lens.lens (\Subnet' {mapCustomerOwnedIpOnLaunch} -> mapCustomerOwnedIpOnLaunch) (\s@Subnet' {} a -> s {mapCustomerOwnedIpOnLaunch = a} :: Subnet)

-- | Indicates whether instances launched in this subnet receive a public
-- IPv4 address.
subnet_mapPublicIpOnLaunch :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Bool)
subnet_mapPublicIpOnLaunch = Lens.lens (\Subnet' {mapPublicIpOnLaunch} -> mapPublicIpOnLaunch) (\s@Subnet' {} a -> s {mapPublicIpOnLaunch = a} :: Subnet)

-- | The Amazon Resource Name (ARN) of the Outpost.
subnet_outpostArn :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_outpostArn = Lens.lens (\Subnet' {outpostArn} -> outpostArn) (\s@Subnet' {} a -> s {outpostArn = a} :: Subnet)

-- | The ID of the Amazon Web Services account that owns the subnet.
subnet_ownerId :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_ownerId = Lens.lens (\Subnet' {ownerId} -> ownerId) (\s@Subnet' {} a -> s {ownerId = a} :: Subnet)

-- | The type of hostnames to assign to instances in the subnet at launch. An
-- instance hostname is based on the IPv4 address or ID of the instance.
subnet_privateDnsNameOptionsOnLaunch :: Lens.Lens' Subnet (Prelude.Maybe PrivateDnsNameOptionsOnLaunch)
subnet_privateDnsNameOptionsOnLaunch = Lens.lens (\Subnet' {privateDnsNameOptionsOnLaunch} -> privateDnsNameOptionsOnLaunch) (\s@Subnet' {} a -> s {privateDnsNameOptionsOnLaunch = a} :: Subnet)

-- | The Amazon Resource Name (ARN) of the subnet.
subnet_subnetArn :: Lens.Lens' Subnet (Prelude.Maybe Prelude.Text)
subnet_subnetArn = Lens.lens (\Subnet' {subnetArn} -> subnetArn) (\s@Subnet' {} a -> s {subnetArn = a} :: Subnet)

-- | Any tags assigned to the subnet.
subnet_tags :: Lens.Lens' Subnet (Prelude.Maybe [Tag])
subnet_tags = Lens.lens (\Subnet' {tags} -> tags) (\s@Subnet' {} a -> s {tags = a} :: Subnet) Prelude.. Lens.mapping Lens.coerced

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

instance Data.FromXML Subnet where
  parseXML x =
    Subnet'
      Prelude.<$> (x Data..@? "assignIpv6AddressOnCreation")
      Prelude.<*> (x Data..@? "availabilityZoneId")
      Prelude.<*> (x Data..@? "customerOwnedIpv4Pool")
      Prelude.<*> (x Data..@? "defaultForAz")
      Prelude.<*> (x Data..@? "enableDns64")
      Prelude.<*> (x Data..@? "enableLniAtDeviceIndex")
      Prelude.<*> ( x Data..@? "ipv6CidrBlockAssociationSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ipv6Native")
      Prelude.<*> (x Data..@? "mapCustomerOwnedIpOnLaunch")
      Prelude.<*> (x Data..@? "mapPublicIpOnLaunch")
      Prelude.<*> (x Data..@? "outpostArn")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "privateDnsNameOptionsOnLaunch")
      Prelude.<*> (x Data..@? "subnetArn")
      Prelude.<*> ( x Data..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@ "availabilityZone")
      Prelude.<*> (x Data..@ "availableIpAddressCount")
      Prelude.<*> (x Data..@ "cidrBlock")
      Prelude.<*> (x Data..@ "state")
      Prelude.<*> (x Data..@ "subnetId")
      Prelude.<*> (x Data..@ "vpcId")

instance Prelude.Hashable Subnet where
  hashWithSalt _salt Subnet' {..} =
    _salt
      `Prelude.hashWithSalt` assignIpv6AddressOnCreation
      `Prelude.hashWithSalt` availabilityZoneId
      `Prelude.hashWithSalt` customerOwnedIpv4Pool
      `Prelude.hashWithSalt` defaultForAz
      `Prelude.hashWithSalt` enableDns64
      `Prelude.hashWithSalt` enableLniAtDeviceIndex
      `Prelude.hashWithSalt` ipv6CidrBlockAssociationSet
      `Prelude.hashWithSalt` ipv6Native
      `Prelude.hashWithSalt` mapCustomerOwnedIpOnLaunch
      `Prelude.hashWithSalt` mapPublicIpOnLaunch
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` privateDnsNameOptionsOnLaunch
      `Prelude.hashWithSalt` subnetArn
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` availableIpAddressCount
      `Prelude.hashWithSalt` cidrBlock
      `Prelude.hashWithSalt` state
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData Subnet where
  rnf Subnet' {..} =
    Prelude.rnf assignIpv6AddressOnCreation
      `Prelude.seq` Prelude.rnf availabilityZoneId
      `Prelude.seq` Prelude.rnf customerOwnedIpv4Pool
      `Prelude.seq` Prelude.rnf defaultForAz
      `Prelude.seq` Prelude.rnf enableDns64
      `Prelude.seq` Prelude.rnf enableLniAtDeviceIndex
      `Prelude.seq` Prelude.rnf ipv6CidrBlockAssociationSet
      `Prelude.seq` Prelude.rnf ipv6Native
      `Prelude.seq` Prelude.rnf mapCustomerOwnedIpOnLaunch
      `Prelude.seq` Prelude.rnf mapPublicIpOnLaunch
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf privateDnsNameOptionsOnLaunch
      `Prelude.seq` Prelude.rnf subnetArn
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf availableIpAddressCount
      `Prelude.seq` Prelude.rnf cidrBlock
      `Prelude.seq` Prelude.rnf state
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf vpcId
