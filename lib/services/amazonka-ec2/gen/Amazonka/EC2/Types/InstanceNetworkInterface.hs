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
-- Module      : Amazonka.EC2.Types.InstanceNetworkInterface
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.InstanceNetworkInterface where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.GroupIdentifier
import Amazonka.EC2.Types.InstanceIpv4Prefix
import Amazonka.EC2.Types.InstanceIpv6Address
import Amazonka.EC2.Types.InstanceIpv6Prefix
import Amazonka.EC2.Types.InstanceNetworkInterfaceAssociation
import Amazonka.EC2.Types.InstanceNetworkInterfaceAttachment
import Amazonka.EC2.Types.InstancePrivateIpAddress
import Amazonka.EC2.Types.NetworkInterfaceStatus
import qualified Amazonka.Prelude as Prelude

-- | Describes a network interface.
--
-- /See:/ 'newInstanceNetworkInterface' smart constructor.
data InstanceNetworkInterface = InstanceNetworkInterface'
  { -- | The association information for an Elastic IPv4 associated with the
    -- network interface.
    association :: Prelude.Maybe InstanceNetworkInterfaceAssociation,
    -- | The network interface attachment.
    attachment :: Prelude.Maybe InstanceNetworkInterfaceAttachment,
    -- | The description.
    description :: Prelude.Maybe Prelude.Text,
    -- | The security groups.
    groups :: Prelude.Maybe [GroupIdentifier],
    -- | The type of network interface.
    --
    -- Valid values: @interface@ | @efa@ | @trunk@
    interfaceType :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 delegated prefixes that are assigned to the network interface.
    ipv4Prefixes :: Prelude.Maybe [InstanceIpv4Prefix],
    -- | The IPv6 addresses associated with the network interface.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6Address],
    -- | The IPv6 delegated prefixes that are assigned to the network interface.
    ipv6Prefixes :: Prelude.Maybe [InstanceIpv6Prefix],
    -- | The MAC address.
    macAddress :: Prelude.Maybe Prelude.Text,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Amazon Web Services account that created the network
    -- interface.
    ownerId :: Prelude.Maybe Prelude.Text,
    -- | The private DNS name.
    privateDnsName :: Prelude.Maybe Prelude.Text,
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | The private IPv4 addresses associated with the network interface.
    privateIpAddresses :: Prelude.Maybe [InstancePrivateIpAddress],
    -- | Indicates whether source\/destination checking is enabled.
    sourceDestCheck :: Prelude.Maybe Prelude.Bool,
    -- | The status of the network interface.
    status :: Prelude.Maybe NetworkInterfaceStatus,
    -- | The ID of the subnet.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the VPC.
    vpcId :: Prelude.Maybe Prelude.Text
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
-- 'association', 'instanceNetworkInterface_association' - The association information for an Elastic IPv4 associated with the
-- network interface.
--
-- 'attachment', 'instanceNetworkInterface_attachment' - The network interface attachment.
--
-- 'description', 'instanceNetworkInterface_description' - The description.
--
-- 'groups', 'instanceNetworkInterface_groups' - The security groups.
--
-- 'interfaceType', 'instanceNetworkInterface_interfaceType' - The type of network interface.
--
-- Valid values: @interface@ | @efa@ | @trunk@
--
-- 'ipv4Prefixes', 'instanceNetworkInterface_ipv4Prefixes' - The IPv4 delegated prefixes that are assigned to the network interface.
--
-- 'ipv6Addresses', 'instanceNetworkInterface_ipv6Addresses' - The IPv6 addresses associated with the network interface.
--
-- 'ipv6Prefixes', 'instanceNetworkInterface_ipv6Prefixes' - The IPv6 delegated prefixes that are assigned to the network interface.
--
-- 'macAddress', 'instanceNetworkInterface_macAddress' - The MAC address.
--
-- 'networkInterfaceId', 'instanceNetworkInterface_networkInterfaceId' - The ID of the network interface.
--
-- 'ownerId', 'instanceNetworkInterface_ownerId' - The ID of the Amazon Web Services account that created the network
-- interface.
--
-- 'privateDnsName', 'instanceNetworkInterface_privateDnsName' - The private DNS name.
--
-- 'privateIpAddress', 'instanceNetworkInterface_privateIpAddress' - The IPv4 address of the network interface within the subnet.
--
-- 'privateIpAddresses', 'instanceNetworkInterface_privateIpAddresses' - The private IPv4 addresses associated with the network interface.
--
-- 'sourceDestCheck', 'instanceNetworkInterface_sourceDestCheck' - Indicates whether source\/destination checking is enabled.
--
-- 'status', 'instanceNetworkInterface_status' - The status of the network interface.
--
-- 'subnetId', 'instanceNetworkInterface_subnetId' - The ID of the subnet.
--
-- 'vpcId', 'instanceNetworkInterface_vpcId' - The ID of the VPC.
newInstanceNetworkInterface ::
  InstanceNetworkInterface
newInstanceNetworkInterface =
  InstanceNetworkInterface'
    { association =
        Prelude.Nothing,
      attachment = Prelude.Nothing,
      description = Prelude.Nothing,
      groups = Prelude.Nothing,
      interfaceType = Prelude.Nothing,
      ipv4Prefixes = Prelude.Nothing,
      ipv6Addresses = Prelude.Nothing,
      ipv6Prefixes = Prelude.Nothing,
      macAddress = Prelude.Nothing,
      networkInterfaceId = Prelude.Nothing,
      ownerId = Prelude.Nothing,
      privateDnsName = Prelude.Nothing,
      privateIpAddress = Prelude.Nothing,
      privateIpAddresses = Prelude.Nothing,
      sourceDestCheck = Prelude.Nothing,
      status = Prelude.Nothing,
      subnetId = Prelude.Nothing,
      vpcId = Prelude.Nothing
    }

-- | The association information for an Elastic IPv4 associated with the
-- network interface.
instanceNetworkInterface_association :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe InstanceNetworkInterfaceAssociation)
instanceNetworkInterface_association = Lens.lens (\InstanceNetworkInterface' {association} -> association) (\s@InstanceNetworkInterface' {} a -> s {association = a} :: InstanceNetworkInterface)

-- | The network interface attachment.
instanceNetworkInterface_attachment :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe InstanceNetworkInterfaceAttachment)
instanceNetworkInterface_attachment = Lens.lens (\InstanceNetworkInterface' {attachment} -> attachment) (\s@InstanceNetworkInterface' {} a -> s {attachment = a} :: InstanceNetworkInterface)

-- | The description.
instanceNetworkInterface_description :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_description = Lens.lens (\InstanceNetworkInterface' {description} -> description) (\s@InstanceNetworkInterface' {} a -> s {description = a} :: InstanceNetworkInterface)

-- | The security groups.
instanceNetworkInterface_groups :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe [GroupIdentifier])
instanceNetworkInterface_groups = Lens.lens (\InstanceNetworkInterface' {groups} -> groups) (\s@InstanceNetworkInterface' {} a -> s {groups = a} :: InstanceNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The type of network interface.
--
-- Valid values: @interface@ | @efa@ | @trunk@
instanceNetworkInterface_interfaceType :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_interfaceType = Lens.lens (\InstanceNetworkInterface' {interfaceType} -> interfaceType) (\s@InstanceNetworkInterface' {} a -> s {interfaceType = a} :: InstanceNetworkInterface)

-- | The IPv4 delegated prefixes that are assigned to the network interface.
instanceNetworkInterface_ipv4Prefixes :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe [InstanceIpv4Prefix])
instanceNetworkInterface_ipv4Prefixes = Lens.lens (\InstanceNetworkInterface' {ipv4Prefixes} -> ipv4Prefixes) (\s@InstanceNetworkInterface' {} a -> s {ipv4Prefixes = a} :: InstanceNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 addresses associated with the network interface.
instanceNetworkInterface_ipv6Addresses :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe [InstanceIpv6Address])
instanceNetworkInterface_ipv6Addresses = Lens.lens (\InstanceNetworkInterface' {ipv6Addresses} -> ipv6Addresses) (\s@InstanceNetworkInterface' {} a -> s {ipv6Addresses = a} :: InstanceNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The IPv6 delegated prefixes that are assigned to the network interface.
instanceNetworkInterface_ipv6Prefixes :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe [InstanceIpv6Prefix])
instanceNetworkInterface_ipv6Prefixes = Lens.lens (\InstanceNetworkInterface' {ipv6Prefixes} -> ipv6Prefixes) (\s@InstanceNetworkInterface' {} a -> s {ipv6Prefixes = a} :: InstanceNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | The MAC address.
instanceNetworkInterface_macAddress :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_macAddress = Lens.lens (\InstanceNetworkInterface' {macAddress} -> macAddress) (\s@InstanceNetworkInterface' {} a -> s {macAddress = a} :: InstanceNetworkInterface)

-- | The ID of the network interface.
instanceNetworkInterface_networkInterfaceId :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_networkInterfaceId = Lens.lens (\InstanceNetworkInterface' {networkInterfaceId} -> networkInterfaceId) (\s@InstanceNetworkInterface' {} a -> s {networkInterfaceId = a} :: InstanceNetworkInterface)

-- | The ID of the Amazon Web Services account that created the network
-- interface.
instanceNetworkInterface_ownerId :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_ownerId = Lens.lens (\InstanceNetworkInterface' {ownerId} -> ownerId) (\s@InstanceNetworkInterface' {} a -> s {ownerId = a} :: InstanceNetworkInterface)

-- | The private DNS name.
instanceNetworkInterface_privateDnsName :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_privateDnsName = Lens.lens (\InstanceNetworkInterface' {privateDnsName} -> privateDnsName) (\s@InstanceNetworkInterface' {} a -> s {privateDnsName = a} :: InstanceNetworkInterface)

-- | The IPv4 address of the network interface within the subnet.
instanceNetworkInterface_privateIpAddress :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_privateIpAddress = Lens.lens (\InstanceNetworkInterface' {privateIpAddress} -> privateIpAddress) (\s@InstanceNetworkInterface' {} a -> s {privateIpAddress = a} :: InstanceNetworkInterface)

-- | The private IPv4 addresses associated with the network interface.
instanceNetworkInterface_privateIpAddresses :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe [InstancePrivateIpAddress])
instanceNetworkInterface_privateIpAddresses = Lens.lens (\InstanceNetworkInterface' {privateIpAddresses} -> privateIpAddresses) (\s@InstanceNetworkInterface' {} a -> s {privateIpAddresses = a} :: InstanceNetworkInterface) Prelude.. Lens.mapping Lens.coerced

-- | Indicates whether source\/destination checking is enabled.
instanceNetworkInterface_sourceDestCheck :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Bool)
instanceNetworkInterface_sourceDestCheck = Lens.lens (\InstanceNetworkInterface' {sourceDestCheck} -> sourceDestCheck) (\s@InstanceNetworkInterface' {} a -> s {sourceDestCheck = a} :: InstanceNetworkInterface)

-- | The status of the network interface.
instanceNetworkInterface_status :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe NetworkInterfaceStatus)
instanceNetworkInterface_status = Lens.lens (\InstanceNetworkInterface' {status} -> status) (\s@InstanceNetworkInterface' {} a -> s {status = a} :: InstanceNetworkInterface)

-- | The ID of the subnet.
instanceNetworkInterface_subnetId :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_subnetId = Lens.lens (\InstanceNetworkInterface' {subnetId} -> subnetId) (\s@InstanceNetworkInterface' {} a -> s {subnetId = a} :: InstanceNetworkInterface)

-- | The ID of the VPC.
instanceNetworkInterface_vpcId :: Lens.Lens' InstanceNetworkInterface (Prelude.Maybe Prelude.Text)
instanceNetworkInterface_vpcId = Lens.lens (\InstanceNetworkInterface' {vpcId} -> vpcId) (\s@InstanceNetworkInterface' {} a -> s {vpcId = a} :: InstanceNetworkInterface)

instance Data.FromXML InstanceNetworkInterface where
  parseXML x =
    InstanceNetworkInterface'
      Prelude.<$> (x Data..@? "association")
      Prelude.<*> (x Data..@? "attachment")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> ( x Data..@? "groupSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "interfaceType")
      Prelude.<*> ( x Data..@? "ipv4PrefixSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x
                      Data..@? "ipv6AddressesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> ( x Data..@? "ipv6PrefixSet" Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "macAddress")
      Prelude.<*> (x Data..@? "networkInterfaceId")
      Prelude.<*> (x Data..@? "ownerId")
      Prelude.<*> (x Data..@? "privateDnsName")
      Prelude.<*> (x Data..@? "privateIpAddress")
      Prelude.<*> ( x
                      Data..@? "privateIpAddressesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "sourceDestCheck")
      Prelude.<*> (x Data..@? "status")
      Prelude.<*> (x Data..@? "subnetId")
      Prelude.<*> (x Data..@? "vpcId")

instance Prelude.Hashable InstanceNetworkInterface where
  hashWithSalt _salt InstanceNetworkInterface' {..} =
    _salt
      `Prelude.hashWithSalt` association
      `Prelude.hashWithSalt` attachment
      `Prelude.hashWithSalt` description
      `Prelude.hashWithSalt` groups
      `Prelude.hashWithSalt` interfaceType
      `Prelude.hashWithSalt` ipv4Prefixes
      `Prelude.hashWithSalt` ipv6Addresses
      `Prelude.hashWithSalt` ipv6Prefixes
      `Prelude.hashWithSalt` macAddress
      `Prelude.hashWithSalt` networkInterfaceId
      `Prelude.hashWithSalt` ownerId
      `Prelude.hashWithSalt` privateDnsName
      `Prelude.hashWithSalt` privateIpAddress
      `Prelude.hashWithSalt` privateIpAddresses
      `Prelude.hashWithSalt` sourceDestCheck
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` subnetId
      `Prelude.hashWithSalt` vpcId

instance Prelude.NFData InstanceNetworkInterface where
  rnf InstanceNetworkInterface' {..} =
    Prelude.rnf association `Prelude.seq`
      Prelude.rnf attachment `Prelude.seq`
        Prelude.rnf description `Prelude.seq`
          Prelude.rnf groups `Prelude.seq`
            Prelude.rnf interfaceType `Prelude.seq`
              Prelude.rnf ipv4Prefixes `Prelude.seq`
                Prelude.rnf ipv6Addresses `Prelude.seq`
                  Prelude.rnf ipv6Prefixes `Prelude.seq`
                    Prelude.rnf macAddress `Prelude.seq`
                      Prelude.rnf networkInterfaceId `Prelude.seq`
                        Prelude.rnf ownerId `Prelude.seq`
                          Prelude.rnf privateDnsName `Prelude.seq`
                            Prelude.rnf privateIpAddress `Prelude.seq`
                              Prelude.rnf privateIpAddresses `Prelude.seq`
                                Prelude.rnf sourceDestCheck `Prelude.seq`
                                  Prelude.rnf status `Prelude.seq`
                                    Prelude.rnf subnetId `Prelude.seq`
                                      Prelude.rnf vpcId
