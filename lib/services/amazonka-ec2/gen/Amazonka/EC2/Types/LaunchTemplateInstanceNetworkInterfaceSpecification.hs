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
-- Module      : Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.EC2.Internal
import Amazonka.EC2.Types.InstanceIpv6Address
import Amazonka.EC2.Types.Ipv4PrefixSpecificationResponse
import Amazonka.EC2.Types.Ipv6PrefixSpecificationResponse
import Amazonka.EC2.Types.PrivateIpAddressSpecification
import qualified Amazonka.Prelude as Prelude

-- | Describes a network interface.
--
-- /See:/ 'newLaunchTemplateInstanceNetworkInterfaceSpecification' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecification = LaunchTemplateInstanceNetworkInterfaceSpecification'
  { -- | Indicates whether to associate a Carrier IP address with eth0 for a new
    -- network interface.
    --
    -- Use this option when you launch an instance in a Wavelength Zone and
    -- want to associate a Carrier IP address with the network interface. For
    -- more information about Carrier IP addresses, see
    -- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
    -- in the /Wavelength Developer Guide/.
    associateCarrierIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to associate a public IPv4 address with eth0 for a new
    -- network interface.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | A description for the network interface.
    description :: Prelude.Maybe Prelude.Text,
    -- | The device index for the network interface attachment.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The IDs of one or more security groups.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | The type of network interface.
    interfaceType :: Prelude.Maybe Prelude.Text,
    -- | The number of IPv4 prefixes that Amazon Web Services automatically
    -- assigned to the network interface.
    ipv4PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv4 prefixes assigned to the network interface.
    ipv4Prefixes :: Prelude.Maybe [Ipv4PrefixSpecificationResponse],
    -- | The number of IPv6 addresses for the network interface.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | The IPv6 addresses for the network interface.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6Address],
    -- | The number of IPv6 prefixes that Amazon Web Services automatically
    -- assigned to the network interface.
    ipv6PrefixCount :: Prelude.Maybe Prelude.Int,
    -- | One or more IPv6 prefixes assigned to the network interface.
    ipv6Prefixes :: Prelude.Maybe [Ipv6PrefixSpecificationResponse],
    -- | The index of the network card.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The primary private IPv4 address of the network interface.
    privateIpAddress :: Prelude.Maybe Prelude.Text,
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Prelude.Maybe [PrivateIpAddressSpecification],
    -- | The number of secondary private IPv4 addresses for the network
    -- interface.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The ID of the subnet for the network interface.
    subnetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'LaunchTemplateInstanceNetworkInterfaceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associateCarrierIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress' - Indicates whether to associate a Carrier IP address with eth0 for a new
-- network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /Wavelength Developer Guide/.
--
-- 'associatePublicIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress' - Indicates whether to associate a public IPv4 address with eth0 for a new
-- network interface.
--
-- 'deleteOnTermination', 'launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'description', 'launchTemplateInstanceNetworkInterfaceSpecification_description' - A description for the network interface.
--
-- 'deviceIndex', 'launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex' - The device index for the network interface attachment.
--
-- 'groups', 'launchTemplateInstanceNetworkInterfaceSpecification_groups' - The IDs of one or more security groups.
--
-- 'interfaceType', 'launchTemplateInstanceNetworkInterfaceSpecification_interfaceType' - The type of network interface.
--
-- 'ipv4PrefixCount', 'launchTemplateInstanceNetworkInterfaceSpecification_ipv4PrefixCount' - The number of IPv4 prefixes that Amazon Web Services automatically
-- assigned to the network interface.
--
-- 'ipv4Prefixes', 'launchTemplateInstanceNetworkInterfaceSpecification_ipv4Prefixes' - One or more IPv4 prefixes assigned to the network interface.
--
-- 'ipv6AddressCount', 'launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount' - The number of IPv6 addresses for the network interface.
--
-- 'ipv6Addresses', 'launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses' - The IPv6 addresses for the network interface.
--
-- 'ipv6PrefixCount', 'launchTemplateInstanceNetworkInterfaceSpecification_ipv6PrefixCount' - The number of IPv6 prefixes that Amazon Web Services automatically
-- assigned to the network interface.
--
-- 'ipv6Prefixes', 'launchTemplateInstanceNetworkInterfaceSpecification_ipv6Prefixes' - One or more IPv6 prefixes assigned to the network interface.
--
-- 'networkCardIndex', 'launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex' - The index of the network card.
--
-- 'networkInterfaceId', 'launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId' - The ID of the network interface.
--
-- 'privateIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress' - The primary private IPv4 address of the network interface.
--
-- 'privateIpAddresses', 'launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses' - One or more private IPv4 addresses.
--
-- 'secondaryPrivateIpAddressCount', 'launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount' - The number of secondary private IPv4 addresses for the network
-- interface.
--
-- 'subnetId', 'launchTemplateInstanceNetworkInterfaceSpecification_subnetId' - The ID of the subnet for the network interface.
newLaunchTemplateInstanceNetworkInterfaceSpecification ::
  LaunchTemplateInstanceNetworkInterfaceSpecification
newLaunchTemplateInstanceNetworkInterfaceSpecification =
  LaunchTemplateInstanceNetworkInterfaceSpecification'
    { associateCarrierIpAddress =
        Prelude.Nothing,
      associatePublicIpAddress =
        Prelude.Nothing,
      deleteOnTermination =
        Prelude.Nothing,
      description =
        Prelude.Nothing,
      deviceIndex =
        Prelude.Nothing,
      groups =
        Prelude.Nothing,
      interfaceType =
        Prelude.Nothing,
      ipv4PrefixCount =
        Prelude.Nothing,
      ipv4Prefixes =
        Prelude.Nothing,
      ipv6AddressCount =
        Prelude.Nothing,
      ipv6Addresses =
        Prelude.Nothing,
      ipv6PrefixCount =
        Prelude.Nothing,
      ipv6Prefixes =
        Prelude.Nothing,
      networkCardIndex =
        Prelude.Nothing,
      networkInterfaceId =
        Prelude.Nothing,
      privateIpAddress =
        Prelude.Nothing,
      privateIpAddresses =
        Prelude.Nothing,
      secondaryPrivateIpAddressCount =
        Prelude.Nothing,
      subnetId =
        Prelude.Nothing
    }

-- | Indicates whether to associate a Carrier IP address with eth0 for a new
-- network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /Wavelength Developer Guide/.
launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {associateCarrierIpAddress} -> associateCarrierIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {associateCarrierIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | Indicates whether to associate a public IPv4 address with eth0 for a new
-- network interface.
launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {associatePublicIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {deleteOnTermination} -> deleteOnTermination) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {deleteOnTermination = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | A description for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_description :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecification_description = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {description} -> description) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {description = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The device index for the network interface attachment.
launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {deviceIndex} -> deviceIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {deviceIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The IDs of one or more security groups.
launchTemplateInstanceNetworkInterfaceSpecification_groups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe [Prelude.Text])
launchTemplateInstanceNetworkInterfaceSpecification_groups = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {groups} -> groups) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {groups = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The type of network interface.
launchTemplateInstanceNetworkInterfaceSpecification_interfaceType :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecification_interfaceType = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {interfaceType} -> interfaceType) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {interfaceType = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The number of IPv4 prefixes that Amazon Web Services automatically
-- assigned to the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_ipv4PrefixCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecification_ipv4PrefixCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {ipv4PrefixCount} -> ipv4PrefixCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {ipv4PrefixCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | One or more IPv4 prefixes assigned to the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_ipv4Prefixes :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe [Ipv4PrefixSpecificationResponse])
launchTemplateInstanceNetworkInterfaceSpecification_ipv4Prefixes = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {ipv4Prefixes} -> ipv4Prefixes) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {ipv4Prefixes = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The number of IPv6 addresses for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {ipv6AddressCount} -> ipv6AddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {ipv6AddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The IPv6 addresses for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe [InstanceIpv6Address])
launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {ipv6Addresses} -> ipv6Addresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {ipv6Addresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The number of IPv6 prefixes that Amazon Web Services automatically
-- assigned to the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_ipv6PrefixCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecification_ipv6PrefixCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {ipv6PrefixCount} -> ipv6PrefixCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {ipv6PrefixCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | One or more IPv6 prefixes assigned to the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_ipv6Prefixes :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe [Ipv6PrefixSpecificationResponse])
launchTemplateInstanceNetworkInterfaceSpecification_ipv6Prefixes = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {ipv6Prefixes} -> ipv6Prefixes) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {ipv6Prefixes = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The index of the network card.
launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {networkCardIndex} -> networkCardIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {networkCardIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The ID of the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {networkInterfaceId} -> networkInterfaceId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {networkInterfaceId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The primary private IPv4 address of the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {privateIpAddress} -> privateIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {privateIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | One or more private IPv4 addresses.
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe [PrivateIpAddressSpecification])
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {privateIpAddresses} -> privateIpAddresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {privateIpAddresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Lens.coerced

-- | The number of secondary private IPv4 addresses for the network
-- interface.
launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {secondaryPrivateIpAddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The ID of the subnet for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_subnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecification_subnetId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {subnetId} -> subnetId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {subnetId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

instance
  Data.FromXML
    LaunchTemplateInstanceNetworkInterfaceSpecification
  where
  parseXML x =
    LaunchTemplateInstanceNetworkInterfaceSpecification'
      Prelude.<$> (x Data..@? "associateCarrierIpAddress")
      Prelude.<*> (x Data..@? "associatePublicIpAddress")
      Prelude.<*> (x Data..@? "deleteOnTermination")
      Prelude.<*> (x Data..@? "description")
      Prelude.<*> (x Data..@? "deviceIndex")
      Prelude.<*> ( x
                      Data..@? "groupSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "groupId")
                  )
      Prelude.<*> (x Data..@? "interfaceType")
      Prelude.<*> (x Data..@? "ipv4PrefixCount")
      Prelude.<*> ( x
                      Data..@? "ipv4PrefixSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ipv6AddressCount")
      Prelude.<*> ( x
                      Data..@? "ipv6AddressesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "ipv6PrefixCount")
      Prelude.<*> ( x
                      Data..@? "ipv6PrefixSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "networkCardIndex")
      Prelude.<*> (x Data..@? "networkInterfaceId")
      Prelude.<*> (x Data..@? "privateIpAddress")
      Prelude.<*> ( x
                      Data..@? "privateIpAddressesSet"
                      Core..!@ Prelude.mempty
                      Prelude.>>= Core.may (Data.parseXMLList "item")
                  )
      Prelude.<*> (x Data..@? "secondaryPrivateIpAddressCount")
      Prelude.<*> (x Data..@? "subnetId")

instance
  Prelude.Hashable
    LaunchTemplateInstanceNetworkInterfaceSpecification
  where
  hashWithSalt
    _salt
    LaunchTemplateInstanceNetworkInterfaceSpecification' {..} =
      _salt
        `Prelude.hashWithSalt` associateCarrierIpAddress
        `Prelude.hashWithSalt` associatePublicIpAddress
        `Prelude.hashWithSalt` deleteOnTermination
        `Prelude.hashWithSalt` description
        `Prelude.hashWithSalt` deviceIndex
        `Prelude.hashWithSalt` groups
        `Prelude.hashWithSalt` interfaceType
        `Prelude.hashWithSalt` ipv4PrefixCount
        `Prelude.hashWithSalt` ipv4Prefixes
        `Prelude.hashWithSalt` ipv6AddressCount
        `Prelude.hashWithSalt` ipv6Addresses
        `Prelude.hashWithSalt` ipv6PrefixCount
        `Prelude.hashWithSalt` ipv6Prefixes
        `Prelude.hashWithSalt` networkCardIndex
        `Prelude.hashWithSalt` networkInterfaceId
        `Prelude.hashWithSalt` privateIpAddress
        `Prelude.hashWithSalt` privateIpAddresses
        `Prelude.hashWithSalt` secondaryPrivateIpAddressCount
        `Prelude.hashWithSalt` subnetId

instance
  Prelude.NFData
    LaunchTemplateInstanceNetworkInterfaceSpecification
  where
  rnf
    LaunchTemplateInstanceNetworkInterfaceSpecification' {..} =
      Prelude.rnf associateCarrierIpAddress
        `Prelude.seq` Prelude.rnf associatePublicIpAddress
        `Prelude.seq` Prelude.rnf deleteOnTermination
        `Prelude.seq` Prelude.rnf description
        `Prelude.seq` Prelude.rnf deviceIndex
        `Prelude.seq` Prelude.rnf groups
        `Prelude.seq` Prelude.rnf interfaceType
        `Prelude.seq` Prelude.rnf ipv4PrefixCount
        `Prelude.seq` Prelude.rnf ipv4Prefixes
        `Prelude.seq` Prelude.rnf ipv6AddressCount
        `Prelude.seq` Prelude.rnf ipv6Addresses
        `Prelude.seq` Prelude.rnf ipv6PrefixCount
        `Prelude.seq` Prelude.rnf ipv6Prefixes
        `Prelude.seq` Prelude.rnf networkCardIndex
        `Prelude.seq` Prelude.rnf networkInterfaceId
        `Prelude.seq` Prelude.rnf privateIpAddress
        `Prelude.seq` Prelude.rnf privateIpAddresses
        `Prelude.seq` Prelude.rnf
          secondaryPrivateIpAddressCount
        `Prelude.seq` Prelude.rnf subnetId
