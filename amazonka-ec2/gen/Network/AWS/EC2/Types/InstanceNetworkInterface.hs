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
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterface where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.InstanceIpv6Address
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
import Network.AWS.EC2.Types.InstancePrivateIpAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import qualified Network.AWS.Lens as Lens

-- | Describes a network interface.
--
-- /See:/ 'newInstanceNetworkInterface' smart constructor.
data InstanceNetworkInterface = InstanceNetworkInterface'
  { -- | One or more security groups.
    groups :: Core.Maybe [GroupIdentifier],
    -- | The status of the network interface.
    status :: Core.Maybe NetworkInterfaceStatus,
    -- | The ID of the AWS account that created the network interface.
    ownerId :: Core.Maybe Core.Text,
    -- | One or more private IPv4 addresses associated with the network
    -- interface.
    privateIpAddresses :: Core.Maybe [InstancePrivateIpAddress],
    -- | The network interface attachment.
    attachment :: Core.Maybe InstanceNetworkInterfaceAttachment,
    -- | The MAC address.
    macAddress :: Core.Maybe Core.Text,
    -- | The association information for an Elastic IPv4 associated with the
    -- network interface.
    association :: Core.Maybe InstanceNetworkInterfaceAssociation,
    -- | One or more IPv6 addresses associated with the network interface.
    ipv6Addresses :: Core.Maybe [InstanceIpv6Address],
    -- | Describes the type of network interface.
    --
    -- Valid values: @interface@ | @efa@
    interfaceType :: Core.Maybe Core.Text,
    -- | Indicates whether to validate network traffic to or from this network
    -- interface.
    sourceDestCheck :: Core.Maybe Core.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Core.Text,
    -- | The description.
    description :: Core.Maybe Core.Text,
    -- | The private DNS name.
    privateDnsName :: Core.Maybe Core.Text,
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Core.Text,
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'InstanceNetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'instanceNetworkInterface_groups' - One or more security groups.
--
-- 'status', 'instanceNetworkInterface_status' - The status of the network interface.
--
-- 'ownerId', 'instanceNetworkInterface_ownerId' - The ID of the AWS account that created the network interface.
--
-- 'privateIpAddresses', 'instanceNetworkInterface_privateIpAddresses' - One or more private IPv4 addresses associated with the network
-- interface.
--
-- 'attachment', 'instanceNetworkInterface_attachment' - The network interface attachment.
--
-- 'macAddress', 'instanceNetworkInterface_macAddress' - The MAC address.
--
-- 'association', 'instanceNetworkInterface_association' - The association information for an Elastic IPv4 associated with the
-- network interface.
--
-- 'ipv6Addresses', 'instanceNetworkInterface_ipv6Addresses' - One or more IPv6 addresses associated with the network interface.
--
-- 'interfaceType', 'instanceNetworkInterface_interfaceType' - Describes the type of network interface.
--
-- Valid values: @interface@ | @efa@
--
-- 'sourceDestCheck', 'instanceNetworkInterface_sourceDestCheck' - Indicates whether to validate network traffic to or from this network
-- interface.
--
-- 'networkInterfaceId', 'instanceNetworkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'subnetId', 'instanceNetworkInterface_subnetId' - The ID of the subnet.
--
-- 'description', 'instanceNetworkInterface_description' - The description.
--
-- 'privateDnsName', 'instanceNetworkInterface_privateDnsName' - The private DNS name.
--
-- 'vpcId', 'instanceNetworkInterface_vpcId' - The ID of the VPC.
--
-- 'privateIpAddress', 'instanceNetworkInterface_privateIpAddress' - The IPv4 address of the network interface within the subnet.
newInstanceNetworkInterface ::
  InstanceNetworkInterface
newInstanceNetworkInterface =
  InstanceNetworkInterface'
    { groups = Core.Nothing,
      status = Core.Nothing,
      ownerId = Core.Nothing,
      privateIpAddresses = Core.Nothing,
      attachment = Core.Nothing,
      macAddress = Core.Nothing,
      association = Core.Nothing,
      ipv6Addresses = Core.Nothing,
      interfaceType = Core.Nothing,
      sourceDestCheck = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      subnetId = Core.Nothing,
      description = Core.Nothing,
      privateDnsName = Core.Nothing,
      vpcId = Core.Nothing,
      privateIpAddress = Core.Nothing
    }

-- | One or more security groups.
instanceNetworkInterface_groups :: Lens.Lens' InstanceNetworkInterface (Core.Maybe [GroupIdentifier])
instanceNetworkInterface_groups = Lens.lens (\InstanceNetworkInterface' {groups} -> groups) (\s@InstanceNetworkInterface' {} a -> s {groups = a} :: InstanceNetworkInterface) Core.. Lens.mapping Lens._Coerce

-- | The status of the network interface.
instanceNetworkInterface_status :: Lens.Lens' InstanceNetworkInterface (Core.Maybe NetworkInterfaceStatus)
instanceNetworkInterface_status = Lens.lens (\InstanceNetworkInterface' {status} -> status) (\s@InstanceNetworkInterface' {} a -> s {status = a} :: InstanceNetworkInterface)

-- | The ID of the AWS account that created the network interface.
instanceNetworkInterface_ownerId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
instanceNetworkInterface_ownerId = Lens.lens (\InstanceNetworkInterface' {ownerId} -> ownerId) (\s@InstanceNetworkInterface' {} a -> s {ownerId = a} :: InstanceNetworkInterface)

-- | One or more private IPv4 addresses associated with the network
-- interface.
instanceNetworkInterface_privateIpAddresses :: Lens.Lens' InstanceNetworkInterface (Core.Maybe [InstancePrivateIpAddress])
instanceNetworkInterface_privateIpAddresses = Lens.lens (\InstanceNetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@InstanceNetworkInterface' {} a -> s {privateIpAddresses = a} :: InstanceNetworkInterface) Core.. Lens.mapping Lens._Coerce

-- | The network interface attachment.
instanceNetworkInterface_attachment :: Lens.Lens' InstanceNetworkInterface (Core.Maybe InstanceNetworkInterfaceAttachment)
instanceNetworkInterface_attachment = Lens.lens (\InstanceNetworkInterface' {attachment} -> attachment) (\s@InstanceNetworkInterface' {} a -> s {attachment = a} :: InstanceNetworkInterface)

-- | The MAC address.
instanceNetworkInterface_macAddress :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
instanceNetworkInterface_macAddress = Lens.lens (\InstanceNetworkInterface' {macAddress} -> macAddress) (\s@InstanceNetworkInterface' {} a -> s {macAddress = a} :: InstanceNetworkInterface)

-- | The association information for an Elastic IPv4 associated with the
-- network interface.
instanceNetworkInterface_association :: Lens.Lens' InstanceNetworkInterface (Core.Maybe InstanceNetworkInterfaceAssociation)
instanceNetworkInterface_association = Lens.lens (\InstanceNetworkInterface' {association} -> association) (\s@InstanceNetworkInterface' {} a -> s {association = a} :: InstanceNetworkInterface)

-- | One or more IPv6 addresses associated with the network interface.
instanceNetworkInterface_ipv6Addresses :: Lens.Lens' InstanceNetworkInterface (Core.Maybe [InstanceIpv6Address])
instanceNetworkInterface_ipv6Addresses = Lens.lens (\InstanceNetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@InstanceNetworkInterface' {} a -> s {ipv6Addresses = a} :: InstanceNetworkInterface) Core.. Lens.mapping Lens._Coerce

-- | Describes the type of network interface.
--
-- Valid values: @interface@ | @efa@
instanceNetworkInterface_interfaceType :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
instanceNetworkInterface_interfaceType = Lens.lens (\InstanceNetworkInterface' {interfaceType} -> interfaceType) (\s@InstanceNetworkInterface' {} a -> s {interfaceType = a} :: InstanceNetworkInterface)

-- | Indicates whether to validate network traffic to or from this network
-- interface.
instanceNetworkInterface_sourceDestCheck :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Bool)
instanceNetworkInterface_sourceDestCheck = Lens.lens (\InstanceNetworkInterface' {sourceDestCheck} -> sourceDestCheck) (\s@InstanceNetworkInterface' {} a -> s {sourceDestCheck = a} :: InstanceNetworkInterface)

-- | The ID of the network interface.
instanceNetworkInterface_networkInterfaceId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
instanceNetworkInterface_networkInterfaceId = Lens.lens (\InstanceNetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@InstanceNetworkInterface' {} a -> s {networkInterfaceId = a} :: InstanceNetworkInterface)

-- | The ID of the subnet.
instanceNetworkInterface_subnetId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
instanceNetworkInterface_subnetId = Lens.lens (\InstanceNetworkInterface' {subnetId} -> subnetId) (\s@InstanceNetworkInterface' {} a -> s {subnetId = a} :: InstanceNetworkInterface)

-- | The description.
instanceNetworkInterface_description :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
instanceNetworkInterface_description = Lens.lens (\InstanceNetworkInterface' {description} -> description) (\s@InstanceNetworkInterface' {} a -> s {description = a} :: InstanceNetworkInterface)

-- | The private DNS name.
instanceNetworkInterface_privateDnsName :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
instanceNetworkInterface_privateDnsName = Lens.lens (\InstanceNetworkInterface' {privateDnsName} -> privateDnsName) (\s@InstanceNetworkInterface' {} a -> s {privateDnsName = a} :: InstanceNetworkInterface)

-- | The ID of the VPC.
instanceNetworkInterface_vpcId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
instanceNetworkInterface_vpcId = Lens.lens (\InstanceNetworkInterface' {vpcId} -> vpcId) (\s@InstanceNetworkInterface' {} a -> s {vpcId = a} :: InstanceNetworkInterface)

-- | The IPv4 address of the network interface within the subnet.
instanceNetworkInterface_privateIpAddress :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
instanceNetworkInterface_privateIpAddress = Lens.lens (\InstanceNetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@InstanceNetworkInterface' {} a -> s {privateIpAddress = a} :: InstanceNetworkInterface)

instance Core.FromXML InstanceNetworkInterface where
  parseXML x =
    InstanceNetworkInterface'
      Core.<$> ( x Core..@? "groupSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> ( x Core..@? "privateIpAddressesSet"
                   Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "attachment")
      Core.<*> (x Core..@? "macAddress")
      Core.<*> (x Core..@? "association")
      Core.<*> ( x Core..@? "ipv6AddressesSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "interfaceType")
      Core.<*> (x Core..@? "sourceDestCheck")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "privateDnsName")
      Core.<*> (x Core..@? "vpcId")
      Core.<*> (x Core..@? "privateIpAddress")

instance Core.Hashable InstanceNetworkInterface

instance Core.NFData InstanceNetworkInterface
