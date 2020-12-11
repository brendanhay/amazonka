-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecification
  ( LaunchTemplateInstanceNetworkInterfaceSpecification (..),

    -- * Smart constructor
    mkLaunchTemplateInstanceNetworkInterfaceSpecification,

    -- * Lenses
    ltinisGroups,
    ltinisPrivateIPAddresses,
    ltinisDeleteOnTermination,
    ltinisAssociateCarrierIPAddress,
    ltinisAssociatePublicIPAddress,
    ltinisInterfaceType,
    ltinisNetworkInterfaceId,
    ltinisSubnetId,
    ltinisIPv6AddressCount,
    ltinisNetworkCardIndex,
    ltinisPrivateIPAddress,
    ltinisSecondaryPrivateIPAddressCount,
    ltinisDescription,
    ltinisDeviceIndex,
    ltinisIPv6Addresses,
  )
where

import Network.AWS.EC2.Types.InstanceIPv6Address
import Network.AWS.EC2.Types.PrivateIPAddressSpecification
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a network interface.
--
-- /See:/ 'mkLaunchTemplateInstanceNetworkInterfaceSpecification' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecification = LaunchTemplateInstanceNetworkInterfaceSpecification'
  { groups ::
      Lude.Maybe
        [Lude.Text],
    privateIPAddresses ::
      Lude.Maybe
        [PrivateIPAddressSpecification],
    deleteOnTermination ::
      Lude.Maybe
        Lude.Bool,
    associateCarrierIPAddress ::
      Lude.Maybe
        Lude.Bool,
    associatePublicIPAddress ::
      Lude.Maybe
        Lude.Bool,
    interfaceType ::
      Lude.Maybe
        Lude.Text,
    networkInterfaceId ::
      Lude.Maybe
        Lude.Text,
    subnetId ::
      Lude.Maybe
        Lude.Text,
    ipv6AddressCount ::
      Lude.Maybe
        Lude.Int,
    networkCardIndex ::
      Lude.Maybe
        Lude.Int,
    privateIPAddress ::
      Lude.Maybe
        Lude.Text,
    secondaryPrivateIPAddressCount ::
      Lude.Maybe
        Lude.Int,
    description ::
      Lude.Maybe
        Lude.Text,
    deviceIndex ::
      Lude.Maybe
        Lude.Int,
    ipv6Addresses ::
      Lude.Maybe
        [InstanceIPv6Address]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'LaunchTemplateInstanceNetworkInterfaceSpecification' with the minimum fields required to make a request.
--
-- * 'associateCarrierIPAddress' - Indicates whether to associate a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
-- * 'associatePublicIPAddress' - Indicates whether to associate a public IPv4 address with eth0 for a new network interface.
-- * 'deleteOnTermination' - Indicates whether the network interface is deleted when the instance is terminated.
-- * 'description' - A description for the network interface.
-- * 'deviceIndex' - The device index for the network interface attachment.
-- * 'groups' - The IDs of one or more security groups.
-- * 'interfaceType' - The type of network interface.
-- * 'ipv6AddressCount' - The number of IPv6 addresses for the network interface.
-- * 'ipv6Addresses' - The IPv6 addresses for the network interface.
-- * 'networkCardIndex' - The index of the network card.
-- * 'networkInterfaceId' - The ID of the network interface.
-- * 'privateIPAddress' - The primary private IPv4 address of the network interface.
-- * 'privateIPAddresses' - One or more private IPv4 addresses.
-- * 'secondaryPrivateIPAddressCount' - The number of secondary private IPv4 addresses for the network interface.
-- * 'subnetId' - The ID of the subnet for the network interface.
mkLaunchTemplateInstanceNetworkInterfaceSpecification ::
  LaunchTemplateInstanceNetworkInterfaceSpecification
mkLaunchTemplateInstanceNetworkInterfaceSpecification =
  LaunchTemplateInstanceNetworkInterfaceSpecification'
    { groups =
        Lude.Nothing,
      privateIPAddresses = Lude.Nothing,
      deleteOnTermination = Lude.Nothing,
      associateCarrierIPAddress = Lude.Nothing,
      associatePublicIPAddress = Lude.Nothing,
      interfaceType = Lude.Nothing,
      networkInterfaceId = Lude.Nothing,
      subnetId = Lude.Nothing,
      ipv6AddressCount = Lude.Nothing,
      networkCardIndex = Lude.Nothing,
      privateIPAddress = Lude.Nothing,
      secondaryPrivateIPAddressCount =
        Lude.Nothing,
      description = Lude.Nothing,
      deviceIndex = Lude.Nothing,
      ipv6Addresses = Lude.Nothing
    }

-- | The IDs of one or more security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisGroups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe [Lude.Text])
ltinisGroups = Lens.lens (groups :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe [Lude.Text]) (\s a -> s {groups = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | One or more private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIPAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisPrivateIPAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe [PrivateIPAddressSpecification])
ltinisPrivateIPAddresses = Lens.lens (privateIPAddresses :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe [PrivateIPAddressSpecification]) (\s a -> s {privateIPAddresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisPrivateIPAddresses "Use generic-lens or generic-optics with 'privateIPAddresses' instead." #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisDeleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Bool)
ltinisDeleteOnTermination = Lens.lens (deleteOnTermination :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {deleteOnTermination = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | Indicates whether to associate a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
--
-- /Note:/ Consider using 'associateCarrierIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisAssociateCarrierIPAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Bool)
ltinisAssociateCarrierIPAddress = Lens.lens (associateCarrierIPAddress :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {associateCarrierIPAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisAssociateCarrierIPAddress "Use generic-lens or generic-optics with 'associateCarrierIPAddress' instead." #-}

-- | Indicates whether to associate a public IPv4 address with eth0 for a new network interface.
--
-- /Note:/ Consider using 'associatePublicIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisAssociatePublicIPAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Bool)
ltinisAssociatePublicIPAddress = Lens.lens (associatePublicIPAddress :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Bool) (\s a -> s {associatePublicIPAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisAssociatePublicIPAddress "Use generic-lens or generic-optics with 'associatePublicIPAddress' instead." #-}

-- | The type of network interface.
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisInterfaceType :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Text)
ltinisInterfaceType = Lens.lens (interfaceType :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {interfaceType = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisInterfaceType "Use generic-lens or generic-optics with 'interfaceType' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisNetworkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Text)
ltinisNetworkInterfaceId = Lens.lens (networkInterfaceId :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {networkInterfaceId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the subnet for the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisSubnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Text)
ltinisSubnetId = Lens.lens (subnetId :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {subnetId = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The number of IPv6 addresses for the network interface.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisIPv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Int)
ltinisIPv6AddressCount = Lens.lens (ipv6AddressCount :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Int) (\s a -> s {ipv6AddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisIPv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead." #-}

-- | The index of the network card.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisNetworkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Int)
ltinisNetworkCardIndex = Lens.lens (networkCardIndex :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Int) (\s a -> s {networkCardIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | The primary private IPv4 address of the network interface.
--
-- /Note:/ Consider using 'privateIPAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisPrivateIPAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Text)
ltinisPrivateIPAddress = Lens.lens (privateIPAddress :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {privateIPAddress = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisPrivateIPAddress "Use generic-lens or generic-optics with 'privateIPAddress' instead." #-}

-- | The number of secondary private IPv4 addresses for the network interface.
--
-- /Note:/ Consider using 'secondaryPrivateIPAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisSecondaryPrivateIPAddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Int)
ltinisSecondaryPrivateIPAddressCount = Lens.lens (secondaryPrivateIPAddressCount :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Int) (\s a -> s {secondaryPrivateIPAddressCount = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisSecondaryPrivateIPAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIPAddressCount' instead." #-}

-- | A description for the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisDescription :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Text)
ltinisDescription = Lens.lens (description :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Text) (\s a -> s {description = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The device index for the network interface attachment.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisDeviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe Lude.Int)
ltinisDeviceIndex = Lens.lens (deviceIndex :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe Lude.Int) (\s a -> s {deviceIndex = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

-- | The IPv6 addresses for the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisIPv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Lude.Maybe [InstanceIPv6Address])
ltinisIPv6Addresses = Lens.lens (ipv6Addresses :: LaunchTemplateInstanceNetworkInterfaceSpecification -> Lude.Maybe [InstanceIPv6Address]) (\s a -> s {ipv6Addresses = a} :: LaunchTemplateInstanceNetworkInterfaceSpecification)
{-# DEPRECATED ltinisIPv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

instance
  Lude.FromXML
    LaunchTemplateInstanceNetworkInterfaceSpecification
  where
  parseXML x =
    LaunchTemplateInstanceNetworkInterfaceSpecification'
      Lude.<$> ( x Lude..@? "groupSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "groupId")
               )
      Lude.<*> ( x Lude..@? "privateIpAddressesSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
      Lude.<*> (x Lude..@? "deleteOnTermination")
      Lude.<*> (x Lude..@? "associateCarrierIpAddress")
      Lude.<*> (x Lude..@? "associatePublicIpAddress")
      Lude.<*> (x Lude..@? "interfaceType")
      Lude.<*> (x Lude..@? "networkInterfaceId")
      Lude.<*> (x Lude..@? "subnetId")
      Lude.<*> (x Lude..@? "ipv6AddressCount")
      Lude.<*> (x Lude..@? "networkCardIndex")
      Lude.<*> (x Lude..@? "privateIpAddress")
      Lude.<*> (x Lude..@? "secondaryPrivateIpAddressCount")
      Lude.<*> (x Lude..@? "description")
      Lude.<*> (x Lude..@? "deviceIndex")
      Lude.<*> ( x Lude..@? "ipv6AddressesSet" Lude..!@ Lude.mempty
                   Lude.>>= Lude.may (Lude.parseXMLList "item")
               )
