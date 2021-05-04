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
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification where

import Network.AWS.EC2.Internal
import Network.AWS.EC2.Types.InstanceIpv6Address
import Network.AWS.EC2.Types.PrivateIpAddressSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a network interface.
--
-- /See:/ 'newLaunchTemplateInstanceNetworkInterfaceSpecification' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecification = LaunchTemplateInstanceNetworkInterfaceSpecification'
  { -- | The IDs of one or more security groups.
    groups :: Prelude.Maybe [Prelude.Text],
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Prelude.Maybe [PrivateIpAddressSpecification],
    -- | The IPv6 addresses for the network interface.
    ipv6Addresses :: Prelude.Maybe [InstanceIpv6Address],
    -- | The type of network interface.
    interfaceType :: Prelude.Maybe Prelude.Text,
    -- | Indicates whether to associate a public IPv4 address with eth0 for a new
    -- network interface.
    associatePublicIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether to associate a Carrier IP address with eth0 for a new
    -- network interface.
    --
    -- Use this option when you launch an instance in a Wavelength Zone and
    -- want to associate a Carrier IP address with the network interface. For
    -- more information about Carrier IP addresses, see
    -- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
    -- in the /AWS Wavelength Developer Guide/.
    associateCarrierIpAddress :: Prelude.Maybe Prelude.Bool,
    -- | The number of IPv6 addresses for the network interface.
    ipv6AddressCount :: Prelude.Maybe Prelude.Int,
    -- | The index of the network card.
    networkCardIndex :: Prelude.Maybe Prelude.Int,
    -- | Indicates whether the network interface is deleted when the instance is
    -- terminated.
    deleteOnTermination :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the network interface.
    networkInterfaceId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the subnet for the network interface.
    subnetId :: Prelude.Maybe Prelude.Text,
    -- | A description for the network interface.
    description :: Prelude.Maybe Prelude.Text,
    -- | The device index for the network interface attachment.
    deviceIndex :: Prelude.Maybe Prelude.Int,
    -- | The number of secondary private IPv4 addresses for the network
    -- interface.
    secondaryPrivateIpAddressCount :: Prelude.Maybe Prelude.Int,
    -- | The primary private IPv4 address of the network interface.
    privateIpAddress :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      privateIpAddresses =
        Prelude.Nothing,
      ipv6Addresses =
        Prelude.Nothing,
      interfaceType =
        Prelude.Nothing,
      associatePublicIpAddress =
        Prelude.Nothing,
      associateCarrierIpAddress =
        Prelude.Nothing,
      ipv6AddressCount =
        Prelude.Nothing,
      networkCardIndex =
        Prelude.Nothing,
      deleteOnTermination =
        Prelude.Nothing,
      networkInterfaceId =
        Prelude.Nothing,
      subnetId =
        Prelude.Nothing,
      description =
        Prelude.Nothing,
      deviceIndex =
        Prelude.Nothing,
      secondaryPrivateIpAddressCount =
        Prelude.Nothing,
      privateIpAddress =
        Prelude.Nothing
    }

-- | The IDs of one or more security groups.
launchTemplateInstanceNetworkInterfaceSpecification_groups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe [Prelude.Text])
launchTemplateInstanceNetworkInterfaceSpecification_groups = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {groups} -> groups) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {groups = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | One or more private IPv4 addresses.
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe [PrivateIpAddressSpecification])
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {privateIpAddresses} -> privateIpAddresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {privateIpAddresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The IPv6 addresses for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe [InstanceIpv6Address])
launchTemplateInstanceNetworkInterfaceSpecification_ipv6Addresses = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {ipv6Addresses} -> ipv6Addresses) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {ipv6Addresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification) Prelude.. Lens.mapping Prelude._Coerce

-- | The type of network interface.
launchTemplateInstanceNetworkInterfaceSpecification_interfaceType :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecification_interfaceType = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {interfaceType} -> interfaceType) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {interfaceType = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | Indicates whether to associate a public IPv4 address with eth0 for a new
-- network interface.
launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecification_associatePublicIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {associatePublicIpAddress} -> associatePublicIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {associatePublicIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | Indicates whether to associate a Carrier IP address with eth0 for a new
-- network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and
-- want to associate a Carrier IP address with the network interface. For
-- more information about Carrier IP addresses, see
-- <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses>
-- in the /AWS Wavelength Developer Guide/.
launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecification_associateCarrierIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {associateCarrierIpAddress} -> associateCarrierIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {associateCarrierIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The number of IPv6 addresses for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecification_ipv6AddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {ipv6AddressCount} -> ipv6AddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {ipv6AddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The index of the network card.
launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecification_networkCardIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {networkCardIndex} -> networkCardIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {networkCardIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | Indicates whether the network interface is deleted when the instance is
-- terminated.
launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Bool)
launchTemplateInstanceNetworkInterfaceSpecification_deleteOnTermination = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {deleteOnTermination} -> deleteOnTermination) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {deleteOnTermination = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The ID of the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecification_networkInterfaceId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {networkInterfaceId} -> networkInterfaceId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {networkInterfaceId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The ID of the subnet for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_subnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecification_subnetId = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {subnetId} -> subnetId) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {subnetId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | A description for the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_description :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecification_description = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {description} -> description) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {description = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The device index for the network interface attachment.
launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecification_deviceIndex = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {deviceIndex} -> deviceIndex) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {deviceIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The number of secondary private IPv4 addresses for the network
-- interface.
launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Int)
launchTemplateInstanceNetworkInterfaceSpecification_secondaryPrivateIpAddressCount = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {secondaryPrivateIpAddressCount} -> secondaryPrivateIpAddressCount) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {secondaryPrivateIpAddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

-- | The primary private IPv4 address of the network interface.
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Prelude.Maybe Prelude.Text)
launchTemplateInstanceNetworkInterfaceSpecification_privateIpAddress = Lens.lens (\LaunchTemplateInstanceNetworkInterfaceSpecification' {privateIpAddress} -> privateIpAddress) (\s@LaunchTemplateInstanceNetworkInterfaceSpecification' {} a -> s {privateIpAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)

instance
  Prelude.FromXML
    LaunchTemplateInstanceNetworkInterfaceSpecification
  where
  parseXML x =
    LaunchTemplateInstanceNetworkInterfaceSpecification'
      Prelude.<$> ( x Prelude..@? "groupSet" Prelude..!@ Prelude.mempty
                      Prelude.>>= Prelude.may (Prelude.parseXMLList "groupId")
                  )
        Prelude.<*> ( x Prelude..@? "privateIpAddressesSet"
                        Prelude..!@ Prelude.mempty
                        Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                    )
        Prelude.<*> ( x Prelude..@? "ipv6AddressesSet"
                        Prelude..!@ Prelude.mempty
                        Prelude.>>= Prelude.may (Prelude.parseXMLList "item")
                    )
        Prelude.<*> (x Prelude..@? "interfaceType")
        Prelude.<*> (x Prelude..@? "associatePublicIpAddress")
        Prelude.<*> (x Prelude..@? "associateCarrierIpAddress")
        Prelude.<*> (x Prelude..@? "ipv6AddressCount")
        Prelude.<*> (x Prelude..@? "networkCardIndex")
        Prelude.<*> (x Prelude..@? "deleteOnTermination")
        Prelude.<*> (x Prelude..@? "networkInterfaceId")
        Prelude.<*> (x Prelude..@? "subnetId")
        Prelude.<*> (x Prelude..@? "description")
        Prelude.<*> (x Prelude..@? "deviceIndex")
        Prelude.<*> (x Prelude..@? "secondaryPrivateIpAddressCount")
        Prelude.<*> (x Prelude..@? "privateIpAddress")

instance
  Prelude.Hashable
    LaunchTemplateInstanceNetworkInterfaceSpecification

instance
  Prelude.NFData
    LaunchTemplateInstanceNetworkInterfaceSpecification
