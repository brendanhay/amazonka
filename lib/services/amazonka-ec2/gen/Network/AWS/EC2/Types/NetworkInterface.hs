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
import Network.AWS.EC2.Types.Ipv4PrefixSpecification
import Network.AWS.EC2.Types.Ipv6PrefixSpecification
import Network.AWS.EC2.Types.NetworkInterfaceAssociation
import Network.AWS.EC2.Types.NetworkInterfaceAttachment
import Network.AWS.EC2.Types.NetworkInterfaceIpv6Address
import Network.AWS.EC2.Types.NetworkInterfacePrivateIpAddress
import Network.AWS.EC2.Types.NetworkInterfaceStatus
import Network.AWS.EC2.Types.NetworkInterfaceType
import Network.AWS.EC2.Types.Tag
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a network interface.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The status of the network interface.
    status :: Prelude.Maybe NetworkInterfaceStatus,
    -- | Any security groups for the network interface.
    groups :: Prelude.Maybe [GroupIdentifier],
    -- | The Amazon Web Services account ID of the owner of the network
    -- interface.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The private IPv4 addresses associated with the network interface.
    privateIpAddresses :: Prelude.Maybe [NetworkInterfacePrivateIpAddress],
    -- | The MAC address.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The network interface attachment.
    attachment :: Prelude.Maybe NetworkInterfaceAttachment,
    -- | The association information for an Elastic IP address (IPv4) associated
    -- with the network interface.
    association :: Prelude.Maybe NetworkInterfaceAssociation,
    -- | The IPv6 addresses associated with the network interface.
    ipv6Addresses :: Prelude.Maybe [NetworkInterfaceIpv6Address],
    -- | The alias or Amazon Web Services account ID of the principal or service
    -- that created the network interface.
    requesterId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the network interface is being managed by Amazon Web
    -- Services.
    requesterManaged :: Prelude.Maybe Prelude.Bool,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the network interface.
    tagSet :: Prelude.Maybe [Tag],
    -- | Indicates whether source\/destination checking is enabled.
    sourceDestCheck :: Prelude.Maybe Prelude.Bool,
    -- | The type of network interface.
    interfaceType :: Prelude.Maybe NetworkInterfaceType,
    -- | The IPv6 prefixes that are assigned to the network interface.
    ipv6Prefixes :: Prelude.Maybe [Ipv6PrefixSpecification],
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | A description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The private DNS name.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 prefixes that are assigned to the network interface.
    ipv4Prefixes :: Prelude.Maybe [Ipv4PrefixSpecification],
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'NetworkInterface' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'networkInterface_status' - The status of the network interface.
--
-- 'groups', 'networkInterface_groups' - Any security groups for the network interface.
--
-- 'ownerId', 'networkInterface_ownerId' - The Amazon Web Services account ID of the owner of the network
-- interface.
--
-- 'privateIpAddresses', 'networkInterface_privateIpAddresses' - The private IPv4 addresses associated with the network interface.
--
-- 'macAddress', 'networkInterface_macAddress' - The MAC address.
--
-- 'attachment', 'networkInterface_attachment' - The network interface attachment.
--
-- 'association', 'networkInterface_association' - The association information for an Elastic IP address (IPv4) associated
-- with the network interface.
--
-- 'ipv6Addresses', 'networkInterface_ipv6Addresses' - The IPv6 addresses associated with the network interface.
--
-- 'requesterId', 'networkInterface_requesterId' - The alias or Amazon Web Services account ID of the principal or service
-- that created the network interface.
--
-- 'requesterManaged', 'networkInterface_requesterManaged' - Indicates whether the network interface is being managed by Amazon Web
-- Services.
--
-- 'outpostArn', 'networkInterface_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'tagSet', 'networkInterface_tagSet' - Any tags assigned to the network interface.
--
-- 'sourceDestCheck', 'networkInterface_sourceDestCheck' - Indicates whether source\/destination checking is enabled.
--
-- 'interfaceType', 'networkInterface_interfaceType' - The type of network interface.
--
-- 'ipv6Prefixes', 'networkInterface_ipv6Prefixes' - The IPv6 prefixes that are assigned to the network interface.
--
-- 'availabilityZone', 'networkInterface_availabilityZone' - The Availability Zone.
--
-- 'description', 'networkInterface_description' - A description.
--
-- 'subnetId', 'networkInterface_subnetId' - The ID of the subnet.
--
-- 'networkInterfaceId', 'networkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'privateDnsName', 'networkInterface_privateDnsName' - The private DNS name.
--
-- 'vpcId', 'networkInterface_vpcId' - The ID of the VPC.
--
-- 'ipv4Prefixes', 'networkInterface_ipv4Prefixes' - The IPv4 prefixes that are assigned to the network interface.
--
-- 'privateIpAddress', 'networkInterface_privateIpAddress' - The IPv4 address of the network interface within the subnet.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { status = Prelude.Nothing,
      groups = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      attachment = Prelude.Nothing,
      association = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      requesterId = Prelude.Nothing,
      requesterManaged = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      tagSet = Prelude.Nothing,
      sourceDestCheck = Prelude.Nothing,
      interfaceType = Prelude.Nothing,
      ipv6Prefixes = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      description = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      vpcId = Prelude.Nothing,
      ipv4Prefixes = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing
    }

-- | The status of the network interface.
networkInterface_status :: Lens.Lens' NetworkInterface (Prelude.Maybe NetworkInterfaceStatus)
networkInterface_status = Lens.lens (\NetworkInterface' {status} -> status) (\s@NetworkInterface' {} a -> s {status = a} :: NetworkInterface)

-- | Any security groups for the network interface.
networkInterface_groups :: Lens.Lens' NetworkInterface (Prelude.Maybe [GroupIdentifier])
networkInterface_groups = Lens.lens (\NetworkInterface' {groups} -> groups) (\s@NetworkInterface' {} a -> s {groups = a} :: NetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The Amazon Web Services account ID of the owner of the network
-- interface.
networkInterface_ownerId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_ownerId = Lens.lens (\NetworkInterface' {ownerId} -> ownerId) (\s@NetworkInterface' {} a -> s {ownerId = a} :: NetworkInterface)

-- | The private IPv4 addresses associated with the network interface.
networkInterface_privateIpAddresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [NetworkInterfacePrivateIpAddress])
networkInterface_privateIpAddresses = Lens.lens (\NetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@NetworkInterface' {} a -> s {privateIpAddresses = a} :: NetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The MAC address.
networkInterface_macAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_macAddress = Lens.lens (\NetworkInterface' {macAddress} -> macAddress) (\s@NetworkInterface' {} a -> s {macAddress = a} :: NetworkInterface)

-- | The network interface attachment.
networkInterface_attachment :: Lens.Lens' NetworkInterface (Prelude.Maybe NetworkInterfaceAttachment)
networkInterface_attachment = Lens.lens (\NetworkInterface' {attachment} -> attachment) (\s@NetworkInterface' {} a -> s {attachment = a} :: NetworkInterface)

-- | The association information for an Elastic IP address (IPv4) associated
-- with the network interface.
networkInterface_association :: Lens.Lens' NetworkInterface (Prelude.Maybe NetworkInterfaceAssociation)
networkInterface_association = Lens.lens (\NetworkInterface' {association} -> association) (\s@NetworkInterface' {} a -> s {association = a} :: NetworkInterface)

-- | The IPv6 addresses associated with the network interface.
networkInterface_ipv6Addresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [NetworkInterfaceIpv6Address])
networkInterface_ipv6Addresses = Lens.lens (\NetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@NetworkInterface' {} a -> s {ipv6Addresses = a} :: NetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The alias or Amazon Web Services account ID of the principal or service
-- that created the network interface.
networkInterface_requesterId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_requesterId = Lens.lens (\NetworkInterface' {requesterId} -> requesterId) (\s@NetworkInterface' {} a -> s {requesterId = a} :: NetworkInterface)

-- | Indicates whether the network interface is being managed by Amazon Web
-- Services.
networkInterface_requesterManaged :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Bool)
networkInterface_requesterManaged = Lens.lens (\NetworkInterface' {requesterManaged} -> requesterManaged) (\s@NetworkInterface' {} a -> s {requesterManaged = a} :: NetworkInterface)

-- | The Amazon Resource Name (ARN) of the Outpost.
networkInterface_outpostArn :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_outpostArn = Lens.lens (\NetworkInterface' {outpostArn} -> outpostArn) (\s@NetworkInterface' {} a -> s {outpostArn = a} :: NetworkInterface)

-- | Any tags assigned to the network interface.
networkInterface_tagSet :: Lens.Lens' NetworkInterface (Prelude.Maybe [Tag])
networkInterface_tagSet = Lens.lens (\NetworkInterface' {tagSet} -> tagSet) (\s@NetworkInterface' {} a -> s {tagSet = a} :: NetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates whether source\/destination checking is enabled.
networkInterface_sourceDestCheck :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Bool)
networkInterface_sourceDestCheck = Lens.lens (\NetworkInterface' {sourceDestCheck} -> sourceDestCheck) (\s@NetworkInterface' {} a -> s {sourceDestCheck = a} :: NetworkInterface)

-- | The type of network interface.
networkInterface_interfaceType :: Lens.Lens' NetworkInterface (Prelude.Maybe NetworkInterfaceType)
networkInterface_interfaceType = Lens.lens (\NetworkInterface' {interfaceType} -> interfaceType) (\s@NetworkInterface' {} a -> s {interfaceType = a} :: NetworkInterface)

-- | The IPv6 prefixes that are assigned to the network interface.
networkInterface_ipv6Prefixes :: Lens.Lens' NetworkInterface (Prelude.Maybe [Ipv6PrefixSpecification])
networkInterface_ipv6Prefixes = Lens.lens (\NetworkInterface' {ipv6Prefixes} -> ipv6Prefixes) (\s@NetworkInterface' {} a -> s {ipv6Prefixes = a} :: NetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The Availability Zone.
networkInterface_availabilityZone :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_availabilityZone = Lens.lens (\NetworkInterface' {availabilityZone} -> availabilityZone) (\s@NetworkInterface' {} a -> s {availabilityZone = a} :: NetworkInterface)

-- | A description.
networkInterface_description :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_description = Lens.lens (\NetworkInterface' {description} -> description) (\s@NetworkInterface' {} a -> s {description = a} :: NetworkInterface)

-- | The ID of the subnet.
networkInterface_subnetId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_subnetId = Lens.lens (\NetworkInterface' {subnetId} -> subnetId) (\s@NetworkInterface' {} a -> s {subnetId = a} :: NetworkInterface)

-- | The ID of the network interface.
networkInterface_networkInterfaceId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_networkInterfaceId = Lens.lens (\NetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterface' {} a -> s {networkInterfaceId = a} :: NetworkInterface)

-- | The private DNS name.
networkInterface_privateDnsName :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateDnsName = Lens.lens (\NetworkInterface' {privateDnsName} -> privateDnsName) (\s@NetworkInterface' {} a -> s {privateDnsName = a} :: NetworkInterface)

-- | The ID of the VPC.
networkInterface_vpcId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_vpcId = Lens.lens (\NetworkInterface' {vpcId} -> vpcId) (\s@NetworkInterface' {} a -> s {vpcId = a} :: NetworkInterface)

-- | The IPv4 prefixes that are assigned to the network interface.
networkInterface_ipv4Prefixes :: Lens.Lens' NetworkInterface (Prelude.Maybe [Ipv4PrefixSpecification])
networkInterface_ipv4Prefixes = Lens.lens (\NetworkInterface' {ipv4Prefixes} -> ipv4Prefixes) (\s@NetworkInterface' {} a -> s {ipv4Prefixes = a} :: NetworkInterface) Prelude.. Lens.mapping Lens._Coerce

-- | The IPv4 address of the network interface within the subnet.
networkInterface_privateIpAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateIpAddress = Lens.lens (\NetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@NetworkInterface' {} a -> s {privateIpAddress = a} :: NetworkInterface)

instance Core.FromXML NetworkInterface where
  parseXML x =
    NetworkInterface'
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
      Prelude.<*> (x Core..@? "requesterId")
      Prelude.<*> (x Core..@? "requesterManaged")
      Prelude.<*> (x Core..@? "outpostArn")
      Prelude.<*> ( x Core..@? "tagSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "sourceDestCheck")
      Prelude.<*> (x Core..@? "interfaceType")
      Prelude.<*> ( x Core..@? "ipv6PrefixSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "availabilityZone")
      Prelude.<*> (x Core..@? "description")
      Prelude.<*> (x Core..@? "subnetId")
      Prelude.<*> (x Core..@? "networkInterfaceId")
      Prelude.<*> (x Core..@? "privateDnsName")
      Prelude.<*> (x Core..@? "vpcId")
      Prelude.<*> ( x Core..@? "ipv4PrefixSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Core.parseXMLList "item")
                  )
      Prelude.<*> (x Core..@? "privateIpAddress")

instance Prelude.Hashable NetworkInterface

instance Prelude.NFData NetworkInterface
