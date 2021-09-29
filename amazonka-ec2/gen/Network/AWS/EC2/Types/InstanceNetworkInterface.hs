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
import Network.AWS.EC2.Types.InstanceIpv4Prefix
import Network.AWS.EC2.Types.InstanceIpv6Address
import Network.AWS.EC2.Types.InstanceIpv6Prefix
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation
import Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment
import Network.AWS.EC2.Types.InstancePrivateIpAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a network interface.
--
-- /See:/ 'newInstanceNetworkInterface' smart constructor.
data InstanceNetworkInterface = InstanceNetworkInterface'
  { -- | The status of the network interface.
    status :: Prelude.Maybe NetworkInterfaceStatus,
    -- | One or more security groups.
    groups :: Prelude.Maybe [GroupIdentifier],
    -- | The ID of the Amazon Web Services account that created the network
    -- interface.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | One or more private IPv4 addresses associated with the network
    -- interface.
    privateIpAddresses :: Prelude.Maybe [InstancePrivateIpAddress],
    -- | The MAC address.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The network interface attachment.
    attachment :: Prelude.Maybe InstanceNetworkInterfaceAttachment,
    -- | The association information for an Elastic IPv4 associated with the
    -- network interface.
    association :: Prelude.Maybe InstanceNetworkInterfaceAssociation,
    -- | One or more IPv6 addresses associated with the network interface.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6Address],
    -- | Indicates whether source\/destination checking is enabled.
    sourceDestCheck :: Prelude.Maybe Prelude.Bool,
    -- | Describes the type of network interface.
    --
    -- Valid values: @interface@ | @efa@ | @trunk@
    interfaceType :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 delegated prefixes that are assigned to the network interface.
    ipv6Prefixes :: Prelude.Maybe [InstanceIpv6Prefix],
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The private DNS name.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 delegated prefixes that are assigned to the network interface.
    ipv4Prefixes :: Prelude.Maybe [InstanceIpv4Prefix],
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'InstanceNetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'instanceNetworkInterface_status' - The status of the network interface.
--
-- 'groups', 'instanceNetworkInterface_groups' - One or more security groups.
--
-- 'ownerId', 'instanceNetworkInterface_ownerId' - The ID of the Amazon Web Services account that created the network
-- interface.
--
-- 'privateIpAddresses', 'instanceNetworkInterface_privateIpAddresses' - One or more private IPv4 addresses associated with the network
-- interface.
--
-- 'macAddress', 'instanceNetworkInterface_macAddress' - The MAC address.
--
-- 'attachment', 'instanceNetworkInterface_attachment' - The network interface attachment.
--
-- 'association', 'instanceNetworkInterface_association' - The association information for an Elastic IPv4 associated with the
-- network interface.
--
-- 'ipv6Addresses', 'instanceNetworkInterface_ipv6Addresses' - One or more IPv6 addresses associated with the network interface.
--
-- 'sourceDestCheck', 'instanceNetworkInterface_sourceDestCheck' - Indicates whether source\/destination checking is enabled.
--
-- 'interfaceType', 'instanceNetworkInterface_interfaceType' - Describes the type of network interface.
--
-- Valid values: @interface@ | @efa@ | @trunk@
--
-- 'ipv6Prefixes', 'instanceNetworkInterface_ipv6Prefixes' - The IPv6 delegated prefixes that are assigned to the network interface.
--
-- 'description', 'instanceNetworkInterface_description' - The description.
--
-- 'subnetId', 'instanceNetworkInterface_subnetId' - The ID of the subnet.
--
-- 'networkInterfaceId', 'instanceNetworkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'privateDnsName', 'instanceNetworkInterface_privateDnsName' - The private DNS name.
--
-- 'vpcId', 'instanceNetworkInterface_vpcId' - The ID of the VPC.
--
-- 'ipv4Prefixes', 'instanceNetworkInterface_ipv4Prefixes' - The IPv4 delegated prefixes that are assigned to the network interface.
--
-- 'privateIpAddress', 'instanceNetworkInterface_privateIpAddress' - The IPv4 address of the network interface within the subnet.
newInstanceNetworkInterface ::
  InstanceNetworkInterface
newInstanceNetworkInterface =
  InstanceNetworkInterface'
    { status = Prelude.Nothing,
      groups = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      attachment = Prelude.Nothing,
      association = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      sourceDestCheck = Prelude.Nothing,
      interfaceType = Prelude.Nothing,
      ipv6Prefixes = Prelude.Nothing,
      description = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      ipv4Prefixes = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | The status of the network interface.
instanceNetworkInterface_status :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe NetworkInterfaceStatus)
instanceNetworkInterface_status = Lens.lens (\InstanceNetworkInterface' {status} -> status) (\s@InstanceNetworkInterface' {} a -> s {status = a} :: InstanceNetworkInterface)

-- | One or more security groups.
instanceNetworkInterface_groups :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe [GroupIdentifier])
instanceNetworkInterface_groups = Lens.lens (\InstanceNetworkInterface' {groups} -> groups) (\s@InstanceNetworkInterface' {} a -> s {groups = a} :: InstanceNetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The ID of the Amazon Web Services account that created the network
-- interface.
instanceNetworkInterface_ownerId :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_ownerId = Lens.lens (\InstanceNetworkInterface' {ownerId} -> ownerId) (\s@InstanceNetworkInterface' {} a -> s {ownerId = a} :: InstanceNetworkInterface)

-- | One or more private IPv4 addresses associated with the network
-- interface.
instanceNetworkInterface_privateIpAddresses :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe [InstancePrivateIpAddress])
instanceNetworkInterface_privateIpAddresses = Lens.lens (\InstanceNetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@InstanceNetworkInterface' {} a -> s {privateIpAddresses = a} :: InstanceNetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The MAC address.
instanceNetworkInterface_macAddress :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_macAddress = Lens.lens (\InstanceNetworkInterface' {macAddress} -> macAddress) (\s@InstanceNetworkInterface' {} a -> s {macAddress = a} :: InstanceNetworkInterface)

-- | The network interface attachment.
instanceNetworkInterface_attachment :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe InstanceNetworkInterfaceAttachment)
instanceNetworkInterface_attachment = Lens.lens (\InstanceNetworkInterface' {attachment} -> attachment) (\s@InstanceNetworkInterface' {} a -> s {attachment = a} :: InstanceNetworkInterface)

-- | The association information for an Elastic IPv4 associated with the
-- network interface.
instanceNetworkInterface_association :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe InstanceNetworkInterfaceAssociation)
instanceNetworkInterface_association = Lens.lens (\InstanceNetworkInterface' {association} -> association) (\s@InstanceNetworkInterface' {} a -> s {association = a} :: InstanceNetworkInterface)

-- | One or more IPv6 addresses associated with the network interface.
instanceNetworkInterface_ipv6Addresses :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe [InstanceIpv6Address])
instanceNetworkInterface_ipv6Addresses = Lens.lens (\InstanceNetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@InstanceNetworkInterface' {} a -> s {ipv6Addresses = a} :: InstanceNetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates whether source\/destination checking is enabled.
instanceNetworkInterface_sourceDestCheck :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Bool)
instanceNetworkInterface_sourceDestCheck = Lens.lens (\InstanceNetworkInterface' {sourceDestCheck} -> sourceDestCheck) (\s@InstanceNetworkInterface' {} a -> s {sourceDestCheck = a} :: InstanceNetworkInterface)

-- | Describes the type of network interface.
--
-- Valid values: @interface@ | @efa@ | @trunk@
instanceNetworkInterface_interfaceType :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_interfaceType = Lens.lens (\InstanceNetworkInterface' {interfaceType} -> interfaceType) (\s@InstanceNetworkInterface' {} a -> s {interfaceType = a} :: InstanceNetworkInterface)

-- | The IPv6 delegated prefixes that are assigned to the network interface.
instanceNetworkInterface_ipv6Prefixes :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe [InstanceIpv6Prefix])
instanceNetworkInterface_ipv6Prefixes = Lens.lens (\InstanceNetworkInterface' {ipv6Prefixes} -> ipv6Prefixes) (\s@InstanceNetworkInterface' {} a -> s {ipv6Prefixes = a} :: InstanceNetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The description.
instanceNetworkInterface_description :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_description = Lens.lens (\InstanceNetworkInterface' {description} -> description) (\s@InstanceNetworkInterface' {} a -> s {description = a} :: InstanceNetworkInterface)

-- | The ID of the subnet.
instanceNetworkInterface_subnetId :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_subnetId = Lens.lens (\InstanceNetworkInterface' {subnetId} -> subnetId) (\s@InstanceNetworkInterface' {} a -> s {subnetId = a} :: InstanceNetworkInterface)

-- | The ID of the network interface.
instanceNetworkInterface_networkInterfaceId :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_networkInterfaceId = Lens.lens (\InstanceNetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@InstanceNetworkInterface' {} a -> s {networkInterfaceId = a} :: InstanceNetworkInterface)

-- | The private DNS name.
instanceNetworkInterface_privateDnsName :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_privateDnsName = Lens.lens (\InstanceNetworkInterface' {privateDnsName} -> privateDnsName) (\s@InstanceNetworkInterface' {} a -> s {privateDnsName = a} :: InstanceNetworkInterface)

-- | The ID of the VPC.
instanceNetworkInterface_vpcId :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_vpcId = Lens.lens (\InstanceNetworkInterface' {vpcId} -> vpcId) (\s@InstanceNetworkInterface' {} a -> s {vpcId = a} :: InstanceNetworkInterface)

-- | The IPv4 delegated prefixes that are assigned to the network interface.
instanceNetworkInterface_ipv4Prefixes :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe [InstanceIpv4Prefix])
instanceNetworkInterface_ipv4Prefixes = Lens.lens (\InstanceNetworkInterface' {ipv4Prefixes} -> ipv4Prefixes) (\s@InstanceNetworkInterface' {} a -> s {ipv4Prefixes = a} :: InstanceNetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The IPv4 address of the network interface within the subnet.
instanceNetworkInterface_privateIpAddress :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_privateIpAddress = Lens.lens (\InstanceNetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@InstanceNetworkInterface' {} a -> s {privateIpAddress = a} :: InstanceNetworkInterface)

instance Core.FromXML InstanceNetworkInterface where
  parseXML x =
    InstanceNetworkInterface'
      Prelude.<$> (x Core..@? "status")
      Prelude.<*> ( x Core..@? "groupSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "ownerId")
      Prelude.<*> ( x Core..@? "privateIpAddressesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "macAddress")
      Prelude.<*> (x Core..@? "attachment")
      Prelude.<*> (x Core..@? "association")
      Prelude.<*> ( x Core..@? "ipv6AddressesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "sourceDestCheck")
      Prelude.<*> (x Core..@? "interfaceType")
      Prelude.<*> ( x Core..@? "ipv6PrefixSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> (x Core..@? "subnetId")
      Prelude.<*> (x Core..@? "networkInterfaceId")
      Prelude.<*> (x Core..@? "privateDnsName")
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> ( x Core..@? "ipv4PrefixSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "privateIpAddress")

instance Prelude.Hashable InstanceNetworkInterface

instance Prelude.NFData InstanceNetworkInterface
