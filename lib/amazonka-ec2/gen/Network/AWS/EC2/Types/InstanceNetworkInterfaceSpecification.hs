{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceNetworkInterfaceSpecification
  ( InstanceNetworkInterfaceSpecification (..)
  -- * Smart constructor
  , mkInstanceNetworkInterfaceSpecification
  -- * Lenses
  , inisAssociateCarrierIpAddress
  , inisAssociatePublicIpAddress
  , inisDeleteOnTermination
  , inisDescription
  , inisDeviceIndex
  , inisGroups
  , inisInterfaceType
  , inisIpv6AddressCount
  , inisIpv6Addresses
  , inisNetworkCardIndex
  , inisNetworkInterfaceId
  , inisPrivateIpAddress
  , inisPrivateIpAddresses
  , inisSecondaryPrivateIpAddressCount
  , inisSubnetId
  ) where

import qualified Network.AWS.EC2.Types.InstanceIpv6Address as Types
import qualified Network.AWS.EC2.Types.PrivateIpAddressSpecification as Types
import qualified Network.AWS.EC2.Types.SecurityGroupId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network interface.
--
-- /See:/ 'mkInstanceNetworkInterfaceSpecification' smart constructor.
data InstanceNetworkInterfaceSpecification = InstanceNetworkInterfaceSpecification'
  { associateCarrierIpAddress :: Core.Maybe Core.Bool
    -- ^ Indicates whether to assign a carrier IP address to the network interface.
--
-- You can only assign a carrier IP address to a network interface that is in a subnet in a Wavelength Zone. For more information about carrier IP addresses, see Carrier IP addresses in the AWS Wavelength Developer Guide.
  , associatePublicIpAddress :: Core.Maybe Core.Bool
    -- ^ Indicates whether to assign a public IPv4 address to an instance you launch in a VPC. The public IP address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
  , deleteOnTermination :: Core.Maybe Core.Bool
    -- ^ If set to @true@ , the interface is deleted when the instance is terminated. You can specify @true@ only if creating a new network interface when launching an instance.
  , description :: Core.Maybe Core.Text
    -- ^ The description of the network interface. Applies only if creating a network interface when launching an instance.
  , deviceIndex :: Core.Maybe Core.Int
    -- ^ The position of the network interface in the attachment order. A primary network interface has a device index of 0.
--
-- If you specify a network interface when launching an instance, you must specify the device index.
  , groups :: Core.Maybe [Types.SecurityGroupId]
    -- ^ The IDs of the security groups for the network interface. Applies only if creating a network interface when launching an instance.
  , interfaceType :: Core.Maybe Core.Text
    -- ^ The type of network interface.
--
-- To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
-- If you are not creating an EFA, specify @interface@ or omit this parameter.
-- Valid values: @interface@ | @efa@ 
  , ipv6AddressCount :: Core.Maybe Core.Int
    -- ^ A number of IPv6 addresses to assign to the network interface. Amazon EC2 chooses the IPv6 addresses from the range of the subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
  , ipv6Addresses :: Core.Maybe [Types.InstanceIpv6Address]
    -- ^ One or more IPv6 addresses to assign to the network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
  , networkCardIndex :: Core.Maybe Core.Int
    -- ^ The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface.
--
-- If you are creating a Spot Fleet, omit this parameter because you can’t specify a network interface ID in a launch specification.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The private IPv4 address of the network interface. Applies only if creating a network interface when launching an instance. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
  , privateIpAddresses :: Core.Maybe [Types.PrivateIpAddressSpecification]
    -- ^ One or more private IPv4 addresses to assign to the network interface. Only one private IPv4 address can be designated as primary. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
  , secondaryPrivateIpAddressCount :: Core.Maybe Core.Int
    -- ^ The number of secondary private IPv4 addresses. You can't specify this option and specify more than one private IP address using the private IP addresses option. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
  , subnetId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet associated with the network interface. Applies only if creating a network interface when launching an instance.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'InstanceNetworkInterfaceSpecification' value with any optional fields omitted.
mkInstanceNetworkInterfaceSpecification
    :: InstanceNetworkInterfaceSpecification
mkInstanceNetworkInterfaceSpecification
  = InstanceNetworkInterfaceSpecification'{associateCarrierIpAddress
                                             = Core.Nothing,
                                           associatePublicIpAddress = Core.Nothing,
                                           deleteOnTermination = Core.Nothing,
                                           description = Core.Nothing, deviceIndex = Core.Nothing,
                                           groups = Core.Nothing, interfaceType = Core.Nothing,
                                           ipv6AddressCount = Core.Nothing,
                                           ipv6Addresses = Core.Nothing,
                                           networkCardIndex = Core.Nothing,
                                           networkInterfaceId = Core.Nothing,
                                           privateIpAddress = Core.Nothing,
                                           privateIpAddresses = Core.Nothing,
                                           secondaryPrivateIpAddressCount = Core.Nothing,
                                           subnetId = Core.Nothing}

-- | Indicates whether to assign a carrier IP address to the network interface.
--
-- You can only assign a carrier IP address to a network interface that is in a subnet in a Wavelength Zone. For more information about carrier IP addresses, see Carrier IP addresses in the AWS Wavelength Developer Guide.
--
-- /Note:/ Consider using 'associateCarrierIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisAssociateCarrierIpAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Bool)
inisAssociateCarrierIpAddress = Lens.field @"associateCarrierIpAddress"
{-# INLINEABLE inisAssociateCarrierIpAddress #-}
{-# DEPRECATED associateCarrierIpAddress "Use generic-lens or generic-optics with 'associateCarrierIpAddress' instead"  #-}

-- | Indicates whether to assign a public IPv4 address to an instance you launch in a VPC. The public IP address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
--
-- /Note:/ Consider using 'associatePublicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisAssociatePublicIpAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Bool)
inisAssociatePublicIpAddress = Lens.field @"associatePublicIpAddress"
{-# INLINEABLE inisAssociatePublicIpAddress #-}
{-# DEPRECATED associatePublicIpAddress "Use generic-lens or generic-optics with 'associatePublicIpAddress' instead"  #-}

-- | If set to @true@ , the interface is deleted when the instance is terminated. You can specify @true@ only if creating a new network interface when launching an instance.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisDeleteOnTermination :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Bool)
inisDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# INLINEABLE inisDeleteOnTermination #-}
{-# DEPRECATED deleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead"  #-}

-- | The description of the network interface. Applies only if creating a network interface when launching an instance.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisDescription :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Text)
inisDescription = Lens.field @"description"
{-# INLINEABLE inisDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | The position of the network interface in the attachment order. A primary network interface has a device index of 0.
--
-- If you specify a network interface when launching an instance, you must specify the device index.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisDeviceIndex :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
inisDeviceIndex = Lens.field @"deviceIndex"
{-# INLINEABLE inisDeviceIndex #-}
{-# DEPRECATED deviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead"  #-}

-- | The IDs of the security groups for the network interface. Applies only if creating a network interface when launching an instance.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisGroups :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe [Types.SecurityGroupId])
inisGroups = Lens.field @"groups"
{-# INLINEABLE inisGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | The type of network interface.
--
-- To create an Elastic Fabric Adapter (EFA), specify @efa@ . For more information, see <https://docs.aws.amazon.com/AWSEC2/latest/UserGuide/efa.html Elastic Fabric Adapter> in the /Amazon Elastic Compute Cloud User Guide/ .
-- If you are not creating an EFA, specify @interface@ or omit this parameter.
-- Valid values: @interface@ | @efa@ 
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisInterfaceType :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Text)
inisInterfaceType = Lens.field @"interfaceType"
{-# INLINEABLE inisInterfaceType #-}
{-# DEPRECATED interfaceType "Use generic-lens or generic-optics with 'interfaceType' instead"  #-}

-- | A number of IPv6 addresses to assign to the network interface. Amazon EC2 chooses the IPv6 addresses from the range of the subnet. You cannot specify this option and the option to assign specific IPv6 addresses in the same request. You can specify this option if you've specified a minimum number of instances to launch.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisIpv6AddressCount :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
inisIpv6AddressCount = Lens.field @"ipv6AddressCount"
{-# INLINEABLE inisIpv6AddressCount #-}
{-# DEPRECATED ipv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead"  #-}

-- | One or more IPv6 addresses to assign to the network interface. You cannot specify this option and the option to assign a number of IPv6 addresses in the same request. You cannot specify this option if you've specified a minimum number of instances to launch.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisIpv6Addresses :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe [Types.InstanceIpv6Address])
inisIpv6Addresses = Lens.field @"ipv6Addresses"
{-# INLINEABLE inisIpv6Addresses #-}
{-# DEPRECATED ipv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead"  #-}

-- | The index of the network card. Some instance types support multiple network cards. The primary network interface must be assigned to network card index 0. The default is network card index 0.
--
-- /Note:/ Consider using 'networkCardIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisNetworkCardIndex :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
inisNetworkCardIndex = Lens.field @"networkCardIndex"
{-# INLINEABLE inisNetworkCardIndex #-}
{-# DEPRECATED networkCardIndex "Use generic-lens or generic-optics with 'networkCardIndex' instead"  #-}

-- | The ID of the network interface.
--
-- If you are creating a Spot Fleet, omit this parameter because you can’t specify a network interface ID in a launch specification.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisNetworkInterfaceId :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Text)
inisNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE inisNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The private IPv4 address of the network interface. Applies only if creating a network interface when launching an instance. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisPrivateIpAddress :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Text)
inisPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE inisPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

-- | One or more private IPv4 addresses to assign to the network interface. Only one private IPv4 address can be designated as primary. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisPrivateIpAddresses :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe [Types.PrivateIpAddressSpecification])
inisPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# INLINEABLE inisPrivateIpAddresses #-}
{-# DEPRECATED privateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead"  #-}

-- | The number of secondary private IPv4 addresses. You can't specify this option and specify more than one private IP address using the private IP addresses option. You cannot specify this option if you're launching more than one instance in a <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_RunInstances.html RunInstances> request.
--
-- /Note:/ Consider using 'secondaryPrivateIpAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisSecondaryPrivateIpAddressCount :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Int)
inisSecondaryPrivateIpAddressCount = Lens.field @"secondaryPrivateIpAddressCount"
{-# INLINEABLE inisSecondaryPrivateIpAddressCount #-}
{-# DEPRECATED secondaryPrivateIpAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIpAddressCount' instead"  #-}

-- | The ID of the subnet associated with the network interface. Applies only if creating a network interface when launching an instance.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
inisSubnetId :: Lens.Lens' InstanceNetworkInterfaceSpecification (Core.Maybe Core.Text)
inisSubnetId = Lens.field @"subnetId"
{-# INLINEABLE inisSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

instance Core.ToQuery InstanceNetworkInterfaceSpecification where
        toQuery InstanceNetworkInterfaceSpecification{..}
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

instance Core.FromXML InstanceNetworkInterfaceSpecification where
        parseXML x
          = InstanceNetworkInterfaceSpecification' Core.<$>
              (x Core..@? "AssociateCarrierIpAddress") Core.<*>
                x Core..@? "associatePublicIpAddress"
                Core.<*> x Core..@? "deleteOnTermination"
                Core.<*> x Core..@? "description"
                Core.<*> x Core..@? "deviceIndex"
                Core.<*>
                x Core..@? "SecurityGroupId" Core..<@>
                  Core.parseXMLList "SecurityGroupId"
                Core.<*> x Core..@? "InterfaceType"
                Core.<*> x Core..@? "ipv6AddressCount"
                Core.<*>
                x Core..@? "ipv6AddressesSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "NetworkCardIndex"
                Core.<*> x Core..@? "networkInterfaceId"
                Core.<*> x Core..@? "privateIpAddress"
                Core.<*>
                x Core..@? "privateIpAddressesSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "secondaryPrivateIpAddressCount"
                Core.<*> x Core..@? "subnetId"
