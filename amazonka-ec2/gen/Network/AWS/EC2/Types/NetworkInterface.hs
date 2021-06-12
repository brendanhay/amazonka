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
-- Module      : Network.AWS.EC2.Types.NetworkInterface
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterface where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.GroupIdentifier
import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import Network.AWS.EC2.Types.NetworkInterfaceAttachment
import Network.AWS.EC2.Types.NetworkInterfaceIpv6Address
import Network.AWS.EC2.Types.NetworkInterfacePrivateIpAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import Network.AWS.EC2.Types.NetworkInterfaceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens

-- | Describes a network interface.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | Any security groups for the network interface.
    groups :: Core.Maybe [GroupIdentifier],
    -- | The status of the network interface.
    status :: Core.Maybe NetworkInterfaceStatus,
    -- | The AWS account ID of the owner of the network interface.
    ownerId :: Core.Maybe Core.Text,
    -- | The private IPv4 addresses associated with the network interface.
    privateIpAddresses :: Core.Maybe [NetworkInterfacePrivateIpAddress],
    -- | The network interface attachment.
    attachment :: Core.Maybe NetworkInterfaceAttachment,
    -- | The MAC address.
    macAddress :: Core.Maybe Core.Text,
    -- | The association information for an Elastic IP address (IPv4) associated
    -- with the network interface.
    association :: Core.Maybe NetworkInterfaceAssociation,
    -- | The IPv6 addresses associated with the network interface.
    ipv6Addresses :: Core.Maybe [NetworkInterfaceIpv6Address],
    -- | Indicates whether the network interface is being managed by AWS.
    requesterManaged :: Core.Maybe Core.Bool,
    -- | The alias or AWS account ID of the principal or service that created the
    -- network interface.
    requesterId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Core.Maybe Core.Text,
    -- | Any tags assigned to the network interface.
    tagSet :: Core.Maybe [Tag],
    -- | The type of network interface.
    interfaceType :: Core.Maybe NetworkInterfaceType,
    -- | Indicates whether traffic to or from the instance is validated.
    sourceDestCheck :: Core.Maybe Core.Bool,
    -- | The Availability Zone.
    availabilityZone :: Core.Maybe Core.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Core.Text,
    -- | A description.
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
-- Create a value of 'NetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'networkInterface_groups' - Any security groups for the network interface.
--
-- 'status', 'networkInterface_status' - The status of the network interface.
--
-- 'ownerId', 'networkInterface_ownerId' - The AWS account ID of the owner of the network interface.
--
-- 'privateIpAddresses', 'networkInterface_privateIpAddresses' - The private IPv4 addresses associated with the network interface.
--
-- 'attachment', 'networkInterface_attachment' - The network interface attachment.
--
-- 'macAddress', 'networkInterface_macAddress' - The MAC address.
--
-- 'association', 'networkInterface_association' - The association information for an Elastic IP address (IPv4) associated
-- with the network interface.
--
-- 'ipv6Addresses', 'networkInterface_ipv6Addresses' - The IPv6 addresses associated with the network interface.
--
-- 'requesterManaged', 'networkInterface_requesterManaged' - Indicates whether the network interface is being managed by AWS.
--
-- 'requesterId', 'networkInterface_requesterId' - The alias or AWS account ID of the principal or service that created the
-- network interface.
--
-- 'outpostArn', 'networkInterface_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'tagSet', 'networkInterface_tagSet' - Any tags assigned to the network interface.
--
-- 'interfaceType', 'networkInterface_interfaceType' - The type of network interface.
--
-- 'sourceDestCheck', 'networkInterface_sourceDestCheck' - Indicates whether traffic to or from the instance is validated.
--
-- 'availabilityZone', 'networkInterface_availabilityZone' - The Availability Zone.
--
-- 'networkInterfaceId', 'networkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'subnetId', 'networkInterface_subnetId' - The ID of the subnet.
--
-- 'description', 'networkInterface_description' - A description.
--
-- 'privateDnsName', 'networkInterface_privateDnsName' - The private DNS name.
--
-- 'vpcId', 'networkInterface_vpcId' - The ID of the VPC.
--
-- 'privateIpAddress', 'networkInterface_privateIpAddress' - The IPv4 address of the network interface within the subnet.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { groups = Core.Nothing,
      status = Core.Nothing,
      ownerId = Core.Nothing,
      privateIpAddresses = Core.Nothing,
      attachment = Core.Nothing,
      macAddress = Core.Nothing,
      association = Core.Nothing,
      ipv6Addresses = Core.Nothing,
      requesterManaged = Core.Nothing,
      requesterId = Core.Nothing,
      outpostArn = Core.Nothing,
      tagSet = Core.Nothing,
      interfaceType = Core.Nothing,
      sourceDestCheck = Core.Nothing,
      availabilityZone = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      subnetId = Core.Nothing,
      description = Core.Nothing,
      privateDnsName = Core.Nothing,
      vpcId = Core.Nothing,
      privateIpAddress = Core.Nothing
    }

-- | Any security groups for the network interface.
networkInterface_groups :: Lens.Lens' NetworkInterface (Core.Maybe [GroupIdentifier])
networkInterface_groups = Lens.lens (\NetworkInterface' {groups} -> groups) (\s@NetworkInterface' {} a -> s {groups = a} :: NetworkInterface) Core.. Lens.mapping Lens._Coerce

-- | The status of the network interface.
networkInterface_status :: Lens.Lens' NetworkInterface (Core.Maybe NetworkInterfaceStatus)
networkInterface_status = Lens.lens (\NetworkInterface' {status} -> status) (\s@NetworkInterface' {} a -> s {status = a} :: NetworkInterface)

-- | The AWS account ID of the owner of the network interface.
networkInterface_ownerId :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_ownerId = Lens.lens (\NetworkInterface' {ownerId} -> ownerId) (\s@NetworkInterface' {} a -> s {ownerId = a} :: NetworkInterface)

-- | The private IPv4 addresses associated with the network interface.
networkInterface_privateIpAddresses :: Lens.Lens' NetworkInterface (Core.Maybe [NetworkInterfacePrivateIpAddress])
networkInterface_privateIpAddresses = Lens.lens (\NetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@NetworkInterface' {} a -> s {privateIpAddresses = a} :: NetworkInterface) Core.. Lens.mapping Lens._Coerce

-- | The network interface attachment.
networkInterface_attachment :: Lens.Lens' NetworkInterface (Core.Maybe NetworkInterfaceAttachment)
networkInterface_attachment = Lens.lens (\NetworkInterface' {attachment} -> attachment) (\s@NetworkInterface' {} a -> s {attachment = a} :: NetworkInterface)

-- | The MAC address.
networkInterface_macAddress :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_macAddress = Lens.lens (\NetworkInterface' {macAddress} -> macAddress) (\s@NetworkInterface' {} a -> s {macAddress = a} :: NetworkInterface)

-- | The association information for an Elastic IP address (IPv4) associated
-- with the network interface.
networkInterface_association :: Lens.Lens' NetworkInterface (Core.Maybe NetworkInterfaceAssociation)
networkInterface_association = Lens.lens (\NetworkInterface' {association} -> association) (\s@NetworkInterface' {} a -> s {association = a} :: NetworkInterface)

-- | The IPv6 addresses associated with the network interface.
networkInterface_ipv6Addresses :: Lens.Lens' NetworkInterface (Core.Maybe [NetworkInterfaceIpv6Address])
networkInterface_ipv6Addresses = Lens.lens (\NetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@NetworkInterface' {} a -> s {ipv6Addresses = a} :: NetworkInterface) Core.. Lens.mapping Lens._Coerce

-- | Indicates whether the network interface is being managed by AWS.
networkInterface_requesterManaged :: Lens.Lens' NetworkInterface (Core.Maybe Core.Bool)
networkInterface_requesterManaged = Lens.lens (\NetworkInterface' {requesterManaged} -> requesterManaged) (\s@NetworkInterface' {} a -> s {requesterManaged = a} :: NetworkInterface)

-- | The alias or AWS account ID of the principal or service that created the
-- network interface.
networkInterface_requesterId :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_requesterId = Lens.lens (\NetworkInterface' {requesterId} -> requesterId) (\s@NetworkInterface' {} a -> s {requesterId = a} :: NetworkInterface)

-- | The Amazon Resource Name (ARN) of the Outpost.
networkInterface_outpostArn :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_outpostArn = Lens.lens (\NetworkInterface' {outpostArn} -> outpostArn) (\s@NetworkInterface' {} a -> s {outpostArn = a} :: NetworkInterface)

-- | Any tags assigned to the network interface.
networkInterface_tagSet :: Lens.Lens' NetworkInterface (Core.Maybe [Tag])
networkInterface_tagSet = Lens.lens (\NetworkInterface' {tagSet} -> tagSet) (\s@NetworkInterface' {} a -> s {tagSet = a} :: NetworkInterface) Core.. Lens.mapping Lens._Coerce

-- | The type of network interface.
networkInterface_interfaceType :: Lens.Lens' NetworkInterface (Core.Maybe NetworkInterfaceType)
networkInterface_interfaceType = Lens.lens (\NetworkInterface' {interfaceType} -> interfaceType) (\s@NetworkInterface' {} a -> s {interfaceType = a} :: NetworkInterface)

-- | Indicates whether traffic to or from the instance is validated.
networkInterface_sourceDestCheck :: Lens.Lens' NetworkInterface (Core.Maybe Core.Bool)
networkInterface_sourceDestCheck = Lens.lens (\NetworkInterface' {sourceDestCheck} -> sourceDestCheck) (\s@NetworkInterface' {} a -> s {sourceDestCheck = a} :: NetworkInterface)

-- | The Availability Zone.
networkInterface_availabilityZone :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_availabilityZone = Lens.lens (\NetworkInterface' {availabilityZone} -> availabilityZone) (\s@NetworkInterface' {} a -> s {availabilityZone = a} :: NetworkInterface)

-- | The ID of the network interface.
networkInterface_networkInterfaceId :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_networkInterfaceId = Lens.lens (\NetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterface' {} a -> s {networkInterfaceId = a} :: NetworkInterface)

-- | The ID of the subnet.
networkInterface_subnetId :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_subnetId = Lens.lens (\NetworkInterface' {subnetId} -> subnetId) (\s@NetworkInterface' {} a -> s {subnetId = a} :: NetworkInterface)

-- | A description.
networkInterface_description :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_description = Lens.lens (\NetworkInterface' {description} -> description) (\s@NetworkInterface' {} a -> s {description = a} :: NetworkInterface)

-- | The private DNS name.
networkInterface_privateDnsName :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_privateDnsName = Lens.lens (\NetworkInterface' {privateDnsName} -> privateDnsName) (\s@NetworkInterface' {} a -> s {privateDnsName = a} :: NetworkInterface)

-- | The ID of the VPC.
networkInterface_vpcId :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_vpcId = Lens.lens (\NetworkInterface' {vpcId} -> vpcId) (\s@NetworkInterface' {} a -> s {vpcId = a} :: NetworkInterface)

-- | The IPv4 address of the network interface within the subnet.
networkInterface_privateIpAddress :: Lens.Lens' NetworkInterface (Core.Maybe Core.Text)
networkInterface_privateIpAddress = Lens.lens (\NetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@NetworkInterface' {} a -> s {privateIpAddress = a} :: NetworkInterface)

instance Core.FromXML NetworkInterface where
  parseXML x =
    NetworkInterface'
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
      Core.<*> (x Core..@? "requesterManaged")
      Core.<*> (x Core..@? "requesterId")
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> ( x Core..@? "tagSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "item")
               )
      Core.<*> (x Core..@? "interfaceType")
      Core.<*> (x Core..@? "sourceDestCheck")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "privateDnsName")
      Core.<*> (x Core..@? "vpcId")
      Core.<*> (x Core..@? "privateIpAddress")

instance Core.Hashable NetworkInterface

instance Core.NFData NetworkInterface
