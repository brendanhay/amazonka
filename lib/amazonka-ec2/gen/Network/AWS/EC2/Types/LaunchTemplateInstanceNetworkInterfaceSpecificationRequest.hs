{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  ( LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (..)
  -- * Smart constructor
  , mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  -- * Lenses
  , ltinisrAssociateCarrierIpAddress
  , ltinisrAssociatePublicIpAddress
  , ltinisrDeleteOnTermination
  , ltinisrDescription
  , ltinisrDeviceIndex
  , ltinisrGroups
  , ltinisrInterfaceType
  , ltinisrIpv6AddressCount
  , ltinisrIpv6Addresses
  , ltinisrNetworkCardIndex
  , ltinisrNetworkInterfaceId
  , ltinisrPrivateIpAddress
  , ltinisrPrivateIpAddresses
  , ltinisrSecondaryPrivateIpAddressCount
  , ltinisrSubnetId
  ) where

import qualified Network.AWS.EC2.Types.InstanceIpv6AddressRequest as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceId as Types
import qualified Network.AWS.EC2.Types.PrivateIpAddressSpecification as Types
import qualified Network.AWS.EC2.Types.SecurityGroupId as Types
import qualified Network.AWS.EC2.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The parameters for a network interface.
--
-- /See:/ 'mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest' smart constructor.
data LaunchTemplateInstanceNetworkInterfaceSpecificationRequest = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'
  { associateCarrierIpAddress :: Core.Maybe Core.Bool
    -- ^ Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
  , associatePublicIpAddress :: Core.Maybe Core.Bool
    -- ^ Associates a public IPv4 address with eth0 for a new network interface.
  , deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ Indicates whether the network interface is deleted when the instance is terminated.
  , description :: Core.Maybe Core.Text
    -- ^ A description for the network interface.
  , deviceIndex :: Core.Maybe Core.Int
    -- ^ The device index for the network interface attachment.
  , groups :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of one or more security groups.
  , interfaceType :: Core.Maybe Core.Text
    -- ^ The type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- If you are not creating an EFA, specify @interface@ or omit this parameter.
-- Valid values: @interface@ | @efa@ 
  , ipv6AddressCount :: Core.Maybe Core.Int
    -- ^ The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
  , ipv6Addresses :: Core.Maybe [Types.InstanceIpv6AddressRequest]
    -- ^ One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
  , networkCardIndex :: Core.Maybe Core.Int
    -- ^ The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
  , networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId
    -- ^ The ID of the network interface.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The primary private IPv4 address of the network interface.
  , privateIpAddresses :: Core.Maybe [Types.PrivateIpAddressSpecification]
    -- ^ One or more private IPv4 addresses.
  , secondaryPrivateIpAddressCount :: Core.Maybe Core.Int
    -- ^ The number of secondary private IPv4 addresses to assign to a network interface.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The ID of the subnet for the network interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LaunchTemplateInstanceNetworkInterfaceSpecificationRequest' value with any optional fields omitted.
mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest
    :: LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
mkLaunchTemplateInstanceNetworkInterfaceSpecificationRequest
  = LaunchTemplateInstanceNetworkInterfaceSpecificationRequest'{associateCarrierIpAddress
                                                                  = Core.Nothing,
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
                                                                subnetId = Core.Nothing}

-- | Associates a Carrier IP address with eth0 for a new network interface.
--
-- Use this option when you launch an instance in a Wavelength Zone and want to associate a Carrier IP address with the network interface. For more information about Carrier IP addresses, see <https://docs.aws.amazon.com/wavelength/latest/developerguide/how-wavelengths-work.html#provider-owned-ip Carrier IP addresses> in the /AWS Wavelength Developer Guide/ .
--
-- /Note:/ Consider using 'associateCarrierIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrAssociateCarrierIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Bool)
ltinisrAssociateCarrierIpAddress = Lens.field @"associateCarrierIpAddress"
{-# INLINEABLE ltinisrAssociateCarrierIpAddress #-}
{-# DEPRECATED associateCarrierIpAddress "Use generic-lens or generic-optics with 'associateCarrierIpAddress' instead"  #-}

-- | Associates a public IPv4 address with eth0 for a new network interface.
--
-- /Note:/ Consider using 'associatePublicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrAssociatePublicIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Bool)
ltinisrAssociatePublicIpAddress = Lens.field @"associatePublicIpAddress"
{-# INLINEABLE ltinisrAssociatePublicIpAddress #-}
{-# DEPRECATED associatePublicIpAddress "Use generic-lens or generic-optics with 'associatePublicIpAddress' instead"  #-}

-- | Indicates whether the network interface is deleted when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrDeleteOnTermination :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Bool)
ltinisrDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE ltinisrDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | A description for the network interface.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrDescription :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Text)
ltinisrDescription = Lens.field @"description"
{-# INLINEABLE ltinisrDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The device index for the network interface attachment.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrDeviceIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
ltinisrDeviceIndex = Lens.field @"deviceIndex"
{-# INLINEABLE ltinisrDeviceIndex #-}
{-# DEPRECATED deviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead"  #-}

-- | The IDs of one or more security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrGroups :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe [Types.SecurityGroupId])
ltinisrGroups = Lens.field @"groups"
{-# INLINEABLE ltinisrGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | The type of network interface. To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
--
-- If you are not creating an EFA, specify @interface@ or omit this parameter.
-- Valid values: @interface@ | @efa@ 
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrInterfaceType :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Text)
ltinisrInterfaceType = Lens.field @"interfaceType"
{-# INLINEABLE ltinisrInterfaceType #-}
{-# DEPRECATED interfaceType "Use generic-lens or generic-optics with 'interfaceType' instead"  #-}

-- | The number of IPv6 addresses to assign to a network interface. Amazon EC2 automatically selects the IPv6 addresses from the subnet range. You can't use this option if specifying specific IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrIpv6AddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
ltinisrIpv6AddressCount = Lens.field @"ipv6AddressCount"
{-# INLINEABLE ltinisrIpv6AddressCount #-}
{-# DEPRECATED ipv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead"  #-}

-- | One or more specific IPv6 addresses from the IPv6 CIDR block range of your subnet. You can't use this option if you're specifying a number of IPv6 addresses.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrIpv6Addresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe [Types.InstanceIpv6AddressRequest])
ltinisrIpv6Addresses = Lens.field @"ipv6Addresses"
{-# INLINEABLE ltinisrIpv6Addresses #-}
{-# DEPRECATED ipv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead"  #-}

-- | The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrNetworkCardIndex :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
ltinisrNetworkCardIndex = Lens.field @"networkCardIndex"
{-# INLINEABLE ltinisrNetworkCardIndex #-}
{-# DEPRECATED networkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead"  #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrNetworkInterfaceId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Types.NetworkInterfaceId)
ltinisrNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE ltinisrNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The primary private IPv4 address of the network interface.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrPrivateIpAddress :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Text)
ltinisrPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE ltinisrPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

-- | One or more private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrPrivateIpAddresses :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe [Types.PrivateIpAddressSpecification])
ltinisrPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# INLINEABLE ltinisrPrivateIpAddresses #-}
{-# DEPRECATED privateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead"  #-}

-- | The number of secondary private IPv4 addresses to assign to a network interface.
--
-- /Note:/ Consider using 'secondaryPrivateIpAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrSecondaryPrivateIpAddressCount :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Core.Int)
ltinisrSecondaryPrivateIpAddressCount = Lens.field @"secondaryPrivateIpAddressCount"
{-# INLINEABLE ltinisrSecondaryPrivateIpAddressCount #-}
{-# DEPRECATED secondaryPrivateIpAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIpAddressCount' instead"  #-}

-- | The ID of the subnet for the network interface.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ltinisrSubnetId :: Lens.Lens' LaunchTemplateInstanceNetworkInterfaceSpecificationRequest (Core.Maybe Types.SubnetId)
ltinisrSubnetId = Lens.field @"subnetId"
{-# INLINEABLE ltinisrSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

instance Core.ToQuery
           LaunchTemplateInstanceNetworkInterfaceSpecificationRequest
         where
        toQuery
          LaunchTemplateInstanceNetworkInterfaceSpecificationRequest{..}
          = Core.maybe Core.mempty
              (Core.toQueryPair "AssociateCarrierIpAddress")
              associateCarrierIpAddress
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "AssociatePublicIpAddress")
                associatePublicIpAddress
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeleteOnTermination")
                deleteOnTermination
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Description") description
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "DeviceIndex") deviceIndex
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "SecurityGroupId") groups
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "InterfaceType")
                interfaceType
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "Ipv6AddressCount")
                ipv6AddressCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "Ipv6Addresses")
                ipv6Addresses
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NetworkCardIndex")
                networkCardIndex
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "NetworkInterfaceId")
                networkInterfaceId
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "PrivateIpAddress")
                privateIpAddress
              Core.<>
              Core.maybe Core.mempty (Core.toQueryList "PrivateIpAddresses")
                privateIpAddresses
              Core.<>
              Core.maybe Core.mempty
                (Core.toQueryPair "SecondaryPrivateIpAddressCount")
                secondaryPrivateIpAddressCount
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "SubnetId") subnetId
