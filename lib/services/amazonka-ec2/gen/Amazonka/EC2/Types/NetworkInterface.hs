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
-- Module      : Amazonka.EC2.Types.NetworkInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.NetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.GroupIdentifier
import Amazonka.EC2.Types.Ipv4PrefixSpecification
import Amazonka.EC2.Types.Ipv6PrefixSpecification
import Amazonka.EC2.Types.NetworkInterfaceAssociation
import Amazonka.EC2.Types.NetworkInterfaceAttachment
import Amazonka.EC2.Types.NetworkInterfaceIpv6Address
import Amazonka.EC2.Types.NetworkInterfacePrivateIpAddress
import Amazonka.EC2.Types.NetworkInterfaceStatus
import Amazonka.EC2.Types.NetworkInterfaceType
import Amazonka.EC2.Types.Tag
import qualified Amazonka.Prelude as Prelude

-- | Describes a network interface.
--
-- /See:/ 'newNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The association information for an Elastic IP address (IPv4) associated
    -- with the network interface.
    association :: Prelude.Maybe NetworkInterfaceAssociation,
    -- | The network interface attachment.
    attachment :: Prelude.Maybe NetworkInterfaceAttachment,
    -- | The Availability Zone.
    availabilityZone :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether a network interface with an IPv6 address is
    -- unreachable from the public internet. If the value is @true@, inbound
    -- traffic from the internet is dropped and you cannot assign an elastic IP
    -- address to the network interface. The network interface is reachable
    -- from peered VPCs and resources connected through a transit gateway,
    -- including on-premises networks.
    denyAllIgwTraffic :: Prelude.Maybe Prelude.Bool,
    -- | A description.
    description :: Prelude.Maybe Prelude.Text,
    -- | Any security groups for the network interface.
    groups :: Prelude.Maybe [GroupIdentifier],
    -- | The type of network interface.
    interfaceType :: Prelude.Maybe NetworkInterfaceType,
    -- | The IPv4 prefixes that are assigned to the network interface.
    ipv4Prefixes :: Prelude.Maybe [Ipv4PrefixSpecification],
    -- | The IPv6 globally unique address associated with the network interface.
    ipv6Address :: Prelude.Maybe Prelude.Text,
    -- | The IPv6 addresses associated with the network interface.
    ipv6Addresses :: Prelude.Maybe [NetworkInterfaceIpv6Address],
    -- | Indicates whether this is an IPv6 only network interface.
    ipv6Native :: Prelude.Maybe Prelude.Bool,
    -- | The IPv6 prefixes that are assigned to the network interface.
    ipv6Prefixes :: Prelude.Maybe [Ipv6PrefixSpecification],
    -- | The MAC address.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the owner of the network
    -- interface.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The private DNS name.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The private IPv4 addresses associated with the network interface.
    privateIpAddresses :: Prelude.Maybe [NetworkInterfacePrivateIpAddress],
    -- | The alias or Amazon Web Services account ID of the principal or service
    -- that created the network interface.
    requesterId :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether the network interface is being managed by Amazon Web
    -- Services.
    requesterManaged :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether source\/destination checking is enabled.
    sourceDestCheck :: Prelude.Maybe Prelude.Bool,
    -- | The status of the network interface.
    status :: Prelude.Maybe NetworkInterfaceStatus,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | Any tags assigned to the network interface.
    tagSet :: Prelude.Maybe [Tag],
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
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
-- 'association', 'networkInterface_association' - The association information for an Elastic IP address (IPv4) associated
-- with the network interface.
--
-- 'attachment', 'networkInterface_attachment' - The network interface attachment.
--
-- 'availabilityZone', 'networkInterface_availabilityZone' - The Availability Zone.
--
-- 'denyAllIgwTraffic', 'networkInterface_denyAllIgwTraffic' - Indicates whether a network interface with an IPv6 address is
-- unreachable from the public internet. If the value is @true@, inbound
-- traffic from the internet is dropped and you cannot assign an elastic IP
-- address to the network interface. The network interface is reachable
-- from peered VPCs and resources connected through a transit gateway,
-- including on-premises networks.
--
-- 'description', 'networkInterface_description' - A description.
--
-- 'groups', 'networkInterface_groups' - Any security groups for the network interface.
--
-- 'interfaceType', 'networkInterface_interfaceType' - The type of network interface.
--
-- 'ipv4Prefixes', 'networkInterface_ipv4Prefixes' - The IPv4 prefixes that are assigned to the network interface.
--
-- 'ipv6Address', 'networkInterface_ipv6Address' - The IPv6 globally unique address associated with the network interface.
--
-- 'ipv6Addresses', 'networkInterface_ipv6Addresses' - The IPv6 addresses associated with the network interface.
--
-- 'ipv6Native', 'networkInterface_ipv6Native' - Indicates whether this is an IPv6 only network interface.
--
-- 'ipv6Prefixes', 'networkInterface_ipv6Prefixes' - The IPv6 prefixes that are assigned to the network interface.
--
-- 'macAddress', 'networkInterface_macAddress' - The MAC address.
--
-- 'networkInterfaceId', 'networkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'outpostArn', 'networkInterface_outpostArn' - The Amazon Resource Name (ARN) of the Outpost.
--
-- 'ownerId', 'networkInterface_ownerId' - The Amazon Web Services account ID of the owner of the network
-- interface.
--
-- 'privateDnsName', 'networkInterface_privateDnsName' - The private DNS name.
--
-- 'privateIpAddress', 'networkInterface_privateIpAddress' - The IPv4 address of the network interface within the subnet.
--
-- 'privateIpAddresses', 'networkInterface_privateIpAddresses' - The private IPv4 addresses associated with the network interface.
--
-- 'requesterId', 'networkInterface_requesterId' - The alias or Amazon Web Services account ID of the principal or service
-- that created the network interface.
--
-- 'requesterManaged', 'networkInterface_requesterManaged' - Indicates whether the network interface is being managed by Amazon Web
-- Services.
--
-- 'sourceDestCheck', 'networkInterface_sourceDestCheck' - Indicates whether source\/destination checking is enabled.
--
-- 'status', 'networkInterface_status' - The status of the network interface.
--
-- 'subnetId', 'networkInterface_subnetId' - The ID of the subnet.
--
-- 'tagSet', 'networkInterface_tagSet' - Any tags assigned to the network interface.
--
-- 'vpcId', 'networkInterface_vpcId' - The ID of the VPC.
newNetworkInterface ::
  NetworkInterface
newNetworkInterface =
  NetworkInterface'
    { association = Prelude.Nothing,
      attachment = Prelude.Nothing,
      availabilityZone = Prelude.Nothing,
      denyAllIgwTraffic = Prelude.Nothing,
      description = Prelude.Nothing,
      groups = Prelude.Nothing,
      interfaceType = Prelude.Nothing,
      ipv4Prefixes = Prelude.Nothing,
      ipv6Address = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      ipv6Native = Prelude.Nothing,
      ipv6Prefixes = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      outpostArn = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      requesterId = Prelude.Nothing,
      requesterManaged = Prelude.Nothing,
      sourceDestCheck = Prelude.Nothing,
      status = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      tagSet = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The association information for an Elastic IP address (IPv4) associated
-- with the network interface.
networkInterface_association :: Lens.Lens' NetworkInterface (Prelude.Maybe NetworkInterfaceAssociation)
networkInterface_association = Lens.lens (\NetworkInterface' {association} -> association) (\s@NetworkInterface' {} a -> s {association = a} :: NetworkInterface)

-- | The network interface attachment.
networkInterface_attachment :: Lens.Lens' NetworkInterface (Prelude.Maybe NetworkInterfaceAttachment)
networkInterface_attachment = Lens.lens (\NetworkInterface' {attachment} -> attachment) (\s@NetworkInterface' {} a -> s {attachment = a} :: NetworkInterface)

-- | The Availability Zone.
networkInterface_availabilityZone :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_availabilityZone = Lens.lens (\NetworkInterface' {availabilityZone} -> availabilityZone) (\s@NetworkInterface' {} a -> s {availabilityZone = a} :: NetworkInterface)

-- | Indicates whether a network interface with an IPv6 address is
-- unreachable from the public internet. If the value is @true@, inbound
-- traffic from the internet is dropped and you cannot assign an elastic IP
-- address to the network interface. The network interface is reachable
-- from peered VPCs and resources connected through a transit gateway,
-- including on-premises networks.
networkInterface_denyAllIgwTraffic :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Bool)
networkInterface_denyAllIgwTraffic = Lens.lens (\NetworkInterface' {denyAllIgwTraffic} -> denyAllIgwTraffic) (\s@NetworkInterface' {} a -> s {denyAllIgwTraffic = a} :: NetworkInterface)

-- | A description.
networkInterface_description :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_description = Lens.lens (\NetworkInterface' {description} -> description) (\s@NetworkInterface' {} a -> s {description = a} :: NetworkInterface)

-- | Any security groups for the network interface.
networkInterface_groups :: Lens.Lens' NetworkInterface (Prelude.Maybe [GroupIdentifier])
networkInterface_groups = Lens.lens (\NetworkInterface' {groups} -> groups) (\s@NetworkInterface' {} a -> s {groups = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The type of network interface.
networkInterface_interfaceType :: Lens.Lens' NetworkInterface (Prelude.Maybe NetworkInterfaceType)
networkInterface_interfaceType = Lens.lens (\NetworkInterface' {interfaceType} -> interfaceType) (\s@NetworkInterface' {} a -> s {interfaceType = a} :: NetworkInterface)

-- | The IPv4 prefixes that are assigned to the network interface.
networkInterface_ipv4Prefixes :: Lens.Lens' NetworkInterface (Prelude.Maybe [Ipv4PrefixSpecification])
networkInterface_ipv4Prefixes = Lens.lens (\NetworkInterface' {ipv4Prefixes} -> ipv4Prefixes) (\s@NetworkInterface' {} a -> s {ipv4Prefixes = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 globally unique address associated with the network interface.
networkInterface_ipv6Address :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_ipv6Address = Lens.lens (\NetworkInterface' {ipv6Address} -> ipv6Address) (\s@NetworkInterface' {} a -> s {ipv6Address = a} :: NetworkInterface)

-- | The IPv6 addresses associated with the network interface.
networkInterface_ipv6Addresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [NetworkInterfaceIpv6Address])
networkInterface_ipv6Addresses = Lens.lens (\NetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@NetworkInterface' {} a -> s {ipv6Addresses = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether this is an IPv6 only network interface.
networkInterface_ipv6Native :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Bool)
networkInterface_ipv6Native = Lens.lens (\NetworkInterface' {ipv6Native} -> ipv6Native) (\s@NetworkInterface' {} a -> s {ipv6Native = a} :: NetworkInterface)

-- | The IPv6 prefixes that are assigned to the network interface.
networkInterface_ipv6Prefixes :: Lens.Lens' NetworkInterface (Prelude.Maybe [Ipv6PrefixSpecification])
networkInterface_ipv6Prefixes = Lens.lens (\NetworkInterface' {ipv6Prefixes} -> ipv6Prefixes) (\s@NetworkInterface' {} a -> s {ipv6Prefixes = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The MAC address.
networkInterface_macAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_macAddress = Lens.lens (\NetworkInterface' {macAddress} -> macAddress) (\s@NetworkInterface' {} a -> s {macAddress = a} :: NetworkInterface)

-- | The ID of the network interface.
networkInterface_networkInterfaceId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_networkInterfaceId = Lens.lens (\NetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@NetworkInterface' {} a -> s {networkInterfaceId = a} :: NetworkInterface)

-- | The Amazon Resource Name (ARN) of the Outpost.
networkInterface_outpostArn :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_outpostArn = Lens.lens (\NetworkInterface' {outpostArn} -> outpostArn) (\s@NetworkInterface' {} a -> s {outpostArn = a} :: NetworkInterface)

-- | The Amazon Web Services account ID of the owner of the network
-- interface.
networkInterface_ownerId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_ownerId = Lens.lens (\NetworkInterface' {ownerId} -> ownerId) (\s@NetworkInterface' {} a -> s {ownerId = a} :: NetworkInterface)

-- | The private DNS name.
networkInterface_privateDnsName :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateDnsName = Lens.lens (\NetworkInterface' {privateDnsName} -> privateDnsName) (\s@NetworkInterface' {} a -> s {privateDnsName = a} :: NetworkInterface)

-- | The IPv4 address of the network interface within the subnet.
networkInterface_privateIpAddress :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_privateIpAddress = Lens.lens (\NetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@NetworkInterface' {} a -> s {privateIpAddress = a} :: NetworkInterface)

-- | The private IPv4 addresses associated with the network interface.
networkInterface_privateIpAddresses :: Lens.Lens' NetworkInterface (Prelude.Maybe [NetworkInterfacePrivateIpAddress])
networkInterface_privateIpAddresses = Lens.lens (\NetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@NetworkInterface' {} a -> s {privateIpAddresses = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The alias or Amazon Web Services account ID of the principal or service
-- that created the network interface.
networkInterface_requesterId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_requesterId = Lens.lens (\NetworkInterface' {requesterId} -> requesterId) (\s@NetworkInterface' {} a -> s {requesterId = a} :: NetworkInterface)

-- | Indicates whether the network interface is being managed by Amazon Web
-- Services.
networkInterface_requesterManaged :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Bool)
networkInterface_requesterManaged = Lens.lens (\NetworkInterface' {requesterManaged} -> requesterManaged) (\s@NetworkInterface' {} a -> s {requesterManaged = a} :: NetworkInterface)

-- | Indicates whether source\/destination checking is enabled.
networkInterface_sourceDestCheck :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Bool)
networkInterface_sourceDestCheck = Lens.lens (\NetworkInterface' {sourceDestCheck} -> sourceDestCheck) (\s@NetworkInterface' {} a -> s {sourceDestCheck = a} :: NetworkInterface)

-- | The status of the network interface.
networkInterface_status :: Lens.Lens' NetworkInterface (Prelude.Maybe NetworkInterfaceStatus)
networkInterface_status = Lens.lens (\NetworkInterface' {status} -> status) (\s@NetworkInterface' {} a -> s {status = a} :: NetworkInterface)

-- | The ID of the subnet.
networkInterface_subnetId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_subnetId = Lens.lens (\NetworkInterface' {subnetId} -> subnetId) (\s@NetworkInterface' {} a -> s {subnetId = a} :: NetworkInterface)

-- | Any tags assigned to the network interface.
networkInterface_tagSet :: Lens.Lens' NetworkInterface (Prelude.Maybe [Tag])
networkInterface_tagSet = Lens.lens (\NetworkInterface' {tagSet} -> tagSet) (\s@NetworkInterface' {} a -> s {tagSet = a} :: NetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the VPC.
networkInterface_vpcId :: Lens.Lens' NetworkInterface (Prelude.Maybe Prelude.Text)
networkInterface_vpcId = Lens.lens (\NetworkInterface' {vpcId} -> vpcId) (\s@NetworkInterface' {} a -> s {vpcId = a} :: NetworkInterface)

instance Data.FromXML NetworkInterface where
  parseXML x =
    NetworkInterface'
      Prelude.<$> (x Data..@? "association")
      Prelude.<*> (x Data..@? "attachment")
      Prelude.<*> (x Data..@? "availabilityZone")
      Prelude.<*> (x Data..@? "denyAllIgwTraffic")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> ( x
                      Data..@? "groupSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "interfaceType")
      Prelude.<*> ( x
                      Data..@? "ipv4PrefixSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ipv6Address")
      Prelude.<*> ( x
                      Data..@? "ipv6AddressesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ipv6Native")
      Prelude.<*> ( x
                      Data..@? "ipv6PrefixSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "macAddress")
      Prelude.<*> (x Data..@? "networkInterfaceId")
      Prelude.<*> (x Data..@? "outpostArn")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "privateDnsName")
      Prelude.<*> (x Data..@? "privateIpAddress")
      Prelude.<*> ( x
                      Data..@? "privateIpAddressesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "requesterId")
      Prelude.<*> (x Data..@? "requesterManaged")
      Prelude.<*> (x Data..@? "sourceDestCheck")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "subnetId")
      Prelude.<*> ( x
                      Data..@? "tagSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable NetworkInterface where
  hashWithSalt _salt NetworkInterface' {..} =
    _salt
      `Prelude.hashWithSalt` association
      `Prelude.hashWithSalt` attachment
      `Prelude.hashWithSalt` availabilityZone
      `Prelude.hashWithSalt` denyAllIgwTraffic
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` interfaceType
      `Prelude.hashWithSalt` ipv4Prefixes
      `Prelude.hashWithSalt` ipv6Address
      `Prelude.hashWithSalt` ipv6Addresses
      `Prelude.hashWithSalt` ipv6Native
      `Prelude.hashWithSalt` ipv6Prefixes
      `Prelude.hashWithSalt` macAddress
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` outpostArn
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` privateDnsName
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` privateIpAddresses
      `Prelude.hashWithSalt` requesterId
      `Prelude.hashWithSalt` requesterManaged
      `Prelude.hashWithSalt` sourceDestCheck
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` tagSet
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData NetworkInterface where
  rnf NetworkInterface' {..} =
    Prelude.rnf association
      `Prelude.seq` Prelude.rnf attachment
      `Prelude.seq` Prelude.rnf availabilityZone
      `Prelude.seq` Prelude.rnf denyAllIgwTraffic
      `Prelude.seq` Prelude.rnf description
      `Prelude.seq` Prelude.rnf groups
      `Prelude.seq` Prelude.rnf interfaceType
      `Prelude.seq` Prelude.rnf ipv4Prefixes
      `Prelude.seq` Prelude.rnf ipv6Address
      `Prelude.seq` Prelude.rnf ipv6Addresses
      `Prelude.seq` Prelude.rnf ipv6Native
      `Prelude.seq` Prelude.rnf ipv6Prefixes
      `Prelude.seq` Prelude.rnf macAddress
      `Prelude.seq` Prelude.rnf networkInterfaceId
      `Prelude.seq` Prelude.rnf outpostArn
      `Prelude.seq` Prelude.rnf ownerId
      `Prelude.seq` Prelude.rnf privateDnsName
      `Prelude.seq` Prelude.rnf privateIpAddress
      `Prelude.seq` Prelude.rnf privateIpAddresses
      `Prelude.seq` Prelude.rnf requesterId
      `Prelude.seq` Prelude.rnf requesterManaged
      `Prelude.seq` Prelude.rnf
        sourceDestCheck
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf subnetId
      `Prelude.seq` Prelude.rnf tagSet
      `Prelude.seq` Prelude.rnf vpcId
