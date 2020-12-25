{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    ltinisAssociateCarrierIpAddress,
    ltinisAssociatePublicIpAddress,
    ltinisDeleteOnTermination,
    ltinisDescription,
    ltinisDeviceIndex,
    ltinisGroups,
    ltinisInterfaceType,
    ltinisIpv6AddressCount,
    ltinisIpv6Addresses,
    ltinisNetworkCardIndex,
    ltinisNetworkInterfaceId,
    ltinisPrivateIpAddress,
    ltinisPrivateIpAddresses,
    ltinisSecondaryPrivateIpAddressCount,
    ltinisSubnetId,
  )
where

import qualified Network.AWS.EC2.Types.Description as Types
import qualified Network.AWS.EC2.Types.InstanceIpv6Address as Types
import qualified Network.AWS.EC2.Types.InterfaceType as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceId as Types
import qualified Network.AWS.EC2.Types.PrivateIpAddress as Types
import qualified Network.AWS.EC2.Types.PrivateIpAddressSpecification as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network interface.
--
-- /See:/ 'mkLaunchTemplateInstanceNetworkInterfaceSpecification' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecification = LaunchTemplateInstanceNetworkInterfaceSpecification'
  { -- | Indicates whether to associate a Carrier IP address with eth0 for a new network interface.
    --
    -- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
    associateCarrierIpAddress :: Core.Maybe Core.Bool,
    -- | Indicates whether to associate a public IPv4 address with eth0 for a new network interface.
    associatePublicIpAddress :: Core.Maybe Core.Bool,
    -- | Indicates whether the network interface is deleted when the instance is terminated.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | A description for the network interface.
    description :: Core.Maybe Types.Description,
    -- | The device index for the network interface attachment.
    deviceIndex :: Core.Maybe Core.Int,
    -- | The IDs of one or more security groups.
    groups :: Core.Maybe [Types.String],
    -- | The type of network interface.
    interfaceType :: Core.Maybe Types.InterfaceType,
    -- | The number of IPv6 addresses for the network interface.
    ipv6AddressCount :: Core.Maybe Core.Int,
    -- | The IPv6 addresses for the network interface.
    ipv6Addresses :: Core.Maybe [Types.InstanceIpv6Address],
    -- | The index of the network card.
    networkCardIndex :: Core.Maybe Core.Int,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId,
    -- | The primary private IPv4 address of the network interface.
    privateIpAddress :: Core.Maybe Types.PrivateIpAddress,
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Core.Maybe [Types.PrivateIpAddressSpecification],
    -- | The number of secondary private IPv4 addresses for the network interface.
    secondaryPrivateIpAddressCount :: Core.Maybe Core.Int,
    -- | The ID of the subnet for the network interface.
    subnetId :: Core.Maybe Types.SubnetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateInstanceNetworkInterfaceSpecification' value with any optional fields omitted.
mkLaunchTemplateInstanceNetworkInterfaceSpecification ::
  LaunchTemplateInstanceNetworkInterfaceSpecification
mkLaunchTemplateInstanceNetworkInterfaceSpecification =
  LaunchTemplateInstanceNetworkInterfaceSpecification'
    { associateCarrierIpAddress =
        Core.Nothing,
      associatePublicIpAddress = Core.Nothing,
      deleteOnTermination = Core.Nothing,
      description = Core.Nothing,
      deviceIndex = Core.Nothing,
      groups = Core.Nothing,
      interfaceType = Core.Nothing,
      ipv6AddressCount = Core.Nothing,
      ipv6Addresses = Core.Nothing,
      networkCardIndex = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      privateIpAddress = Core.Nothing,
      privateIpAddresses = Core.Nothing,
      secondaryPrivateIpAddressCount =
        Core.Nothing,
      subnetId = Core.Nothing
    }

-- | Indicates whether to associate a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
--
-- /Note:/ Consider using 'associateCarrierIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisAssociateCarrierIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Bool)
ltinisAssociateCarrierIpAddress = Lens.field @"associateCarrierIpAddress"
{-# DEPRECATED ltinisAssociateCarrierIpAddress "Use generic-lens or generic-optics with 'associateCarrierIpAddress' instead." #-}

-- | Indicates whether to associate a public IPv4 address with eth0 for a new network interface.
--
-- /Note:/ Consider using 'associatePublicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisAssociatePublicIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Bool)
ltinisAssociatePublicIpAddress = Lens.field @"associatePublicIpAddress"
{-# DEPRECATED ltinisAssociatePublicIpAddress "Use generic-lens or generic-optics with 'associatePublicIpAddress' instead." #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisDeleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Bool)
ltinisDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# DEPRECATED ltinisDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | A description for the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisDescription :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Types.Description)
ltinisDescription = Lens.field @"description"
{-# DEPRECATED ltinisDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The device index for the network interface attachment.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisDeviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
ltinisDeviceIndex = Lens.field @"deviceIndex"
{-# DEPRECATED ltinisDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

-- | The IDs of one or more security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisGroups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe [Types.String])
ltinisGroups = Lens.field @"groups"
{-# DEPRECATED ltinisGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The type of network interface.
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisInterfaceType :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Types.InterfaceType)
ltinisInterfaceType = Lens.field @"interfaceType"
{-# DEPRECATED ltinisInterfaceType "Use generic-lens or generic-optics with 'interfaceType' instead." #-}

-- | The number of IPv6 addresses for the network interface.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisIpv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
ltinisIpv6AddressCount = Lens.field @"ipv6AddressCount"
{-# DEPRECATED ltinisIpv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead." #-}

-- | The IPv6 addresses for the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisIpv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe [Types.InstanceIpv6Address])
ltinisIpv6Addresses = Lens.field @"ipv6Addresses"
{-# DEPRECATED ltinisIpv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

-- | The index of the network card.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisNetworkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
ltinisNetworkCardIndex = Lens.field @"networkCardIndex"
{-# DEPRECATED ltinisNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisNetworkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Types.NetworkInterfaceId)
ltinisNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED ltinisNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The primary private IPv4 address of the network interface.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisPrivateIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Types.PrivateIpAddress)
ltinisPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED ltinisPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | One or more private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisPrivateIpAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe [Types.PrivateIpAddressSpecification])
ltinisPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# DEPRECATED ltinisPrivateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead." #-}

-- | The number of secondary private IPv4 addresses for the network interface.
--
-- /Note:/ Consider using 'secondaryPrivateIpAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisSecondaryPrivateIpAddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
ltinisSecondaryPrivateIpAddressCount = Lens.field @"secondaryPrivateIpAddressCount"
{-# DEPRECATED ltinisSecondaryPrivateIpAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIpAddressCount' instead." #-}

-- | The ID of the subnet for the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisSubnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecification (Core.Maybe Types.SubnetId)
ltinisSubnetId = Lens.field @"subnetId"
{-# DEPRECATED ltinisSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

instance
  Core.FromXML
    LaunchTemplateInstanceNetworkInterfaceSpecification
  where
  parseXML x =
    LaunchTemplateInstanceNetworkInterfaceSpecification'
      Core.<$> (x Core..@? "associateCarrierIpAddress")
      Core.<*> (x Core..@? "associatePublicIpAddress")
      Core.<*> (x Core..@? "deleteOnTermination")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "deviceIndex")
      Core.<*> (x Core..@? "groupSet" Core..<@> Core.parseXMLList "groupId")
      Core.<*> (x Core..@? "interfaceType")
      Core.<*> (x Core..@? "ipv6AddressCount")
      Core.<*> (x Core..@? "ipv6AddressesSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "networkCardIndex")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "privateIpAddress")
      Core.<*> ( x Core..@? "privateIpAddressesSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "secondaryPrivateIpAddressCount")
      Core.<*> (x Core..@? "subnetId")
