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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceIpv6Address
import Network.AWS.EC2.Types.PrivateIpAddressSpecification
import qualified Network.AWS.Lens as Lens

-- | Describes a network interface.
--
-- /See:/ 'newLaunchTemplateInstanceNetworkInterfaceSpecification' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecification = LaunchTemplateInstanceNetworkInterfaceSpecification'
  { -- | The IDs of one or more security groups.
    groups :: Core.Maybe [Core.Text],
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Core.Maybe [PrivateIpAddressSpecification],
    -- | The IPv6 addresses for the network interface.
    ipv6Addresses :: Core.Maybe [InstanceIpv6Address],
    -- | The type of network interface.
    interfaceType :: Core.Maybe Core.Text,
    -- | Indicates whether to associate a public IPv4 address with eth0 for a new
    -- network interface.
    associatePublicIpAddress :: Core.Maybe Core.Bool,
    -- | Indicates whether to associate a Carrier IP address with eth0 for a new
    -- network interface.
    --
    -- Use this option when you launch an instance in a Wavelength Zone and
    -- want to associate a Carrier IP address with the network interface. For
    -- more information about Carrier IP addresses, see
    -- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
    -- in the /AWS Wavelength Developer Guide/.
    associateCarrierIpAddress :: Core.Maybe Core.Bool,
    -- | The number of IPv6 addresses for the network interface.
    ipv6AddressCount :: Core.Maybe Core.Int,
    -- | The index of the network card.
    networkCardIndex :: Core.Maybe Core.Int,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Core.Text,
    -- | The ID of the subnet for the network interface.
    subnetId :: Core.Maybe Core.Text,
    -- | A description for the network interface.
    description :: Core.Maybe Core.Text,
    -- | The device index for the network interface attachment.
    deviceIndex :: Core.Maybe Core.Int,
    -- | The number of secondary private IPv4 addresses for the network
    -- interface.
    secondaryPrivateIpAddressCount :: Core.Maybe Core.Int,
    -- | The primary private IPv4 address of the network interface.
    privateIpAddress :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'LaunchTemplateInstanceNetworkInterfaceSpecification' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'groups', 'launchTemplateInstanceNetworkInterfaceSpecification_groups' - The IDs of one or more security groups.
--
-- 'privateIpAddresses', 'launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses' - One or more private IPv4 addresses.
--
-- 'ipv6Addresses', 'launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses' - The IPv6 addresses for the network interface.
--
-- 'interfaceType', 'launchTemplateInstanceNetworkInterfaceSpecification_interfaceType' - The type of network interface.
--
-- 'associatePublicIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress' - Indicates whether to associate a public IPv4 address with eth0 for a new
-- network interface.
--
-- 'associateCarrierIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress' - Indicates whether to associate a Carrier IP address with eth0 for a new
-- network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /AWS Wavelength Developer Guide/.
--
-- 'ipv6AddressCount', 'launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount' - The number of IPv6 addresses for the network interface.
--
-- 'networkCardIndex', 'launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex' - The index of the network card.
--
-- 'deleteOnTermination', 'launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination' - Indicates whether the network interface is deleted when the instance is
-- terminated.
--
-- 'networkInterfaceId', 'launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId' - The ID of the network interface.
--
-- 'subnetId', 'launchTemplateInstanceNetworkInterfaceSpecification_subnetId' - The ID of the subnet for the network interface.
--
-- 'description', 'launchTemplateInstanceNetworkInterfaceSpecification_description' - A description for the network interface.
--
-- 'deviceIndex', 'launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex' - The device index for the network interface attachment.
--
-- 'secondaryPrivateIpAddressCount', 'launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount' - The number of secondary private IPv4 addresses for the network
-- interface.
--
-- 'privateIpAddress', 'launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress' - The primary private IPv4 address of the network interface.
newLaunchTemplateInstanceNetworkInterfaceSpecification ::
  LaunchTemplateInstanceNetworkInterfaceSpecification
newLaunchTemplateInstanceNetworkInterfaceSpecification =
  LaunchTemplateInstanceNetworkInterfaceSpecification'
    { groups =
        Core.Nothing,
      privateIpAddresses =
        Core.Nothing,
      ipv6Addresses =
        Core.Nothing,
      interfaceType =
        Core.Nothing,
      associatePublicIpAddress =
        Core.Nothing,
      associateCarrierIpAddress =
        Core.Nothing,
      ipv6AddressCount =
        Core.Nothing,
      networkCardIndex =
        Core.Nothing,
      deleteOnTermination =
        Core.Nothing,
      networkInterfaceId =
        Core.Nothing,
      subnetId =
        Core.Nothing,
      description =
        Core.Nothing,
      deviceIndex =
        Core.Nothing,
      secondaryPrivateIpAddressCount =
        Core.Nothing,
      privateIpAddress =
        Core.Nothing
    }

-- | The IDs of one or more security groups.
launchTemplateInstanceNetworkInterfaceSpecification_groups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe [Core.Text])
launchTemplateInstanceNetworkInterfaceSpecification_groups = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {groups} -> groups) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {groups = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Core.. Lens.mapping Lens._Coerce

-- | One or more private IPv4 addresses.
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe [PrivateIpAddressSpecification])
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {privateIpAddresses} -> privateIpAddresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {privateIpAddresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Core.. Lens.mapping Lens._Coerce

-- | The IPv6 addresses for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe [InstanceIpv6Address])
launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {ipv6Addresses} -> ipv6Addresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {ipv6Addresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Core.. Lens.mapping Lens._Coerce

-- | The type of network interface.
launchTemplateInstanceNetworkInterfaceSpecification_interfaceType :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Text)
launchTemplateInstanceNetworkInterfaceSpecification_interfaceType = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {interfaceType} -> interfaceType) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {interfaceType = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | Indicates whether to associate a public IPv4 address with eth0 for a new
-- network interface.
launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Bool)
launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {associatePublicIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | Indicates whether to associate a Carrier IP address with eth0 for a new
-- network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /AWS Wavelength Developer Guide/.
launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Bool)
launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {associateCarrierIpAddress} -> associateCarrierIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {associateCarrierIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The number of IPv6 addresses for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {ipv6AddressCount} -> ipv6AddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {ipv6AddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The index of the network card.
launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {networkCardIndex} -> networkCardIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {networkCardIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Bool)
launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {deleteOnTermination} -> deleteOnTermination) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {deleteOnTermination = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The ID of the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Text)
launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {networkInterfaceId} -> networkInterfaceId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {networkInterfaceId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The ID of the subnet for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_subnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Text)
launchTemplateInstanceNetworkInterfaceSpecification_subnetId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {subnetId} -> subnetId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {subnetId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | A description for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_description :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Text)
launchTemplateInstanceNetworkInterfaceSpecification_description = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {description} -> description) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {description = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The device index for the network interface attachment.
launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {deviceIndex} -> deviceIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {deviceIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The number of secondary private IPv4 addresses for the network
-- interface.
launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {secondaryPrivateIpAddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The primary private IPv4 address of the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Text)
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {privateIpAddress} -> privateIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {privateIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

instance
  Core.FromXML
    LaunchTemplateInstanceNetworkInterfaceSpecification
  where
  parseXML x =
    LaunchTemplateInstanceNetworkInterfaceSpecification'
      Core.<$> ( x Core..@? "groupSet" Core..!@ Core.mempty
                   Core.>>= Core.may (Core.parseXMLList "groupId")
               )
        Core.<*> ( x Core..@? "privateIpAddressesSet"
                     Core..!@ Core.mempty
                     Core.>>= Core.may (Core.parseXMLList "item")
                 )
        Core.<*> ( x Core..@? "ipv6AddressesSet" Core..!@ Core.mempty
                     Core.>>= Core.may (Core.parseXMLList "item")
                 )
        Core.<*> (x Core..@? "interfaceType")
        Core.<*> (x Core..@? "associatePublicIpAddress")
        Core.<*> (x Core..@? "associateCarrierIpAddress")
        Core.<*> (x Core..@? "ipv6AddressCount")
        Core.<*> (x Core..@? "networkCardIndex")
        Core.<*> (x Core..@? "deleteOnTermination")
        Core.<*> (x Core..@? "networkInterfaceId")
        Core.<*> (x Core..@? "subnetId")
        Core.<*> (x Core..@? "description")
        Core.<*> (x Core..@? "deviceIndex")
        Core.<*> (x Core..@? "secondaryPrivateIpAddressCount")
        Core.<*> (x Core..@? "privateIpAddress")

instance
  Core.Hashable
    LaunchTemplateInstanceNetworkInterfaceSpecification

instance
  Core.NFData
    LaunchTemplateInstanceNetworkInterfaceSpecification
