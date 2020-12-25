{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  ( LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..),

    -- * Smart constructor
    mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest,

    -- * Lenses
    ltinisrAssociateCarrierIpAddress,
    ltinisrAssociatePublicIpAddress,
    ltinisrDeleteOnTermination,
    ltinisrDescription,
    ltinisrDeviceIndex,
    ltinisrGroups,
    ltinisrInterfaceType,
    ltinisrIpv6AddressCount,
    ltinisrIpv6Addresses,
    ltinisrNetworkCardIndex,
    ltinisrNetworkInterfaceId,
    ltinisrPrivateIpAddress,
    ltinisrPrivateIpAddresses,
    ltinisrSecondaryPrivateIpAddressCount,
    ltinisrSubnetId,
  )
where

import qualified Network.AWS.EC2.Types.InstanceIpv6AddressRequest as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceId as Types
import qualified Network.AWS.EC2.Types.PrivateIpAddressSpecification as Types
import qualified Network.AWS.EC2.Types.SecurityGroupId as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The parameters for a network interface.
--
-- /See:/ 'mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecificationRequest = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
  { -- | Associates a Carrier IP address with eth0 for a new network interface.
    --
    -- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
    associateCarrierIpAddress :: Core.Maybe Core.Bool,
    -- | Associates a public IPv4 address with eth0 for a new network interface.
    associatePublicIpAddress :: Core.Maybe Core.Bool,
    -- | Indicates whether the network interface is deleted when the instance is terminated.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | A description for the network interface.
    description :: Core.Maybe Types.String,
    -- | The device index for the network interface attachment.
    deviceIndex :: Core.Maybe Core.Int,
    -- | The IDs of one or more security groups.
    groups :: Core.Maybe [Types.SecurityGroupId],
    -- | The type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
    --
    -- If you are not creating an EFA, specify @interface@ or omit this parameter.
    -- Valid values: @interface@ | @efa@
    interfaceType :: Core.Maybe Types.String,
    -- | The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
    ipv6AddressCount :: Core.Maybe Core.Int,
    -- | One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
    ipv6Addresses :: Core.Maybe [Types.InstanceIpv6AddressRequest],
    -- | The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
    networkCardIndex :: Core.Maybe Core.Int,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId,
    -- | The primary private IPv4 address of the network interface.
    privateIpAddress :: Core.Maybe Types.String,
    -- | One or more private IPv4 addresses.
    privateIpAddresses :: Core.Maybe [Types.PrivateIpAddressSpecification],
    -- | The number of secondary private IPv4 addresses to assign to a network interface.
    secondaryPrivateIpAddressCount :: Core.Maybe Core.Int,
    -- | The ID of the subnet for the network interface.
    subnetId :: Core.Maybe Types.SubnetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' value with any optional fields omitted.
mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest ::
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest =
  LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
    { associateCarrierIpAddress =
        Core.Nothing,
      associatePublicIpAddress =
        Core.Nothing,
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

-- | Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
--
-- /Note:/ Consider using 'associateCarrierIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrAssociateCarrierIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Bool)
ltinisrAssociateCarrierIpAddress = Lens.field @"associateCarrierIpAddress"
{-# DEPRECATED ltinisrAssociateCarrierIpAddress "Use generic-lens or generic-optics with 'associateCarrierIpAddress' instead." #-}

-- | Associates a public IPv4 address with eth0 for a new network interface.
--
-- /Note:/ Consider using 'associatePublicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrAssociatePublicIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Bool)
ltinisrAssociatePublicIpAddress = Lens.field @"associatePublicIpAddress"
{-# DEPRECATED ltinisrAssociatePublicIpAddress "Use generic-lens or generic-optics with 'associatePublicIpAddress' instead." #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrDeleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Bool)
ltinisrDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# DEPRECATED ltinisrDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | A description for the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrDescription :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Types.String)
ltinisrDescription = Lens.field @"description"
{-# DEPRECATED ltinisrDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The device index for the network interface attachment.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrDeviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
ltinisrDeviceIndex = Lens.field @"deviceIndex"
{-# DEPRECATED ltinisrDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

-- | The IDs of one or more security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrGroups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe [Types.SecurityGroupId])
ltinisrGroups = Lens.field @"groups"
{-# DEPRECATED ltinisrGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- If you are not creating an EFA, specify @interface@ or omit this parameter.
-- Valid values: @interface@ | @efa@
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrInterfaceType :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Types.String)
ltinisrInterfaceType = Lens.field @"interfaceType"
{-# DEPRECATED ltinisrInterfaceType "Use generic-lens or generic-optics with 'interfaceType' instead." #-}

-- | The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrIpv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
ltinisrIpv6AddressCount = Lens.field @"ipv6AddressCount"
{-# DEPRECATED ltinisrIpv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead." #-}

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrIpv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe [Types.InstanceIpv6AddressRequest])
ltinisrIpv6Addresses = Lens.field @"ipv6Addresses"
{-# DEPRECATED ltinisrIpv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

-- | The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrNetworkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
ltinisrNetworkCardIndex = Lens.field @"networkCardIndex"
{-# DEPRECATED ltinisrNetworkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrNetworkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Types.NetworkInterfaceId)
ltinisrNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED ltinisrNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The primary private IPv4 address of the network interface.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrPrivateIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Types.String)
ltinisrPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED ltinisrPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | One or more private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrPrivateIpAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe [Types.PrivateIpAddressSpecification])
ltinisrPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# DEPRECATED ltinisrPrivateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead." #-}

-- | The number of secondary private IPv4 addresses to assign to a network interface.
--
-- /Note:/ Consider using 'secondaryPrivateIpAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrSecondaryPrivateIpAddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
ltinisrSecondaryPrivateIpAddressCount = Lens.field @"secondaryPrivateIpAddressCount"
{-# DEPRECATED ltinisrSecondaryPrivateIpAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIpAddressCount' instead." #-}

-- | The ID of the subnet for the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrSubnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Types.SubnetId)
ltinisrSubnetId = Lens.field @"subnetId"
{-# DEPRECATED ltinisrSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}
