{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.ScheduledInstancesNetworkInterface
  ( ScheduledInstancesNetworkInterface (..),

    -- * Smart constructor
    mkScheduledInstancesNetworkInterface,

    -- * Lenses
    siniAssociatePublicIpAddress,
    siniDeleteOnTermination,
    siniDescription,
    siniDeviceIndex,
    siniGroups,
    siniIpv6AddressCount,
    siniIpv6Addresses,
    siniNetworkInterfaceId,
    siniPrivateIpAddress,
    siniPrivateIpAddressConfigs,
    siniSecondaryPrivateIpAddressCount,
    siniSubnetId,
  )
where

import qualified Network.AWS.EC2.Types.NetworkInterfaceId as Types
import qualified Network.AWS.EC2.Types.ScheduledInstancesIpv6Address as Types
import qualified Network.AWS.EC2.Types.ScheduledInstancesPrivateIpAddressConfig as Types
import qualified Network.AWS.EC2.Types.SecurityGroupId as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network interface for a Scheduled Instance.
--
-- /See:/ 'mkScheduledInstancesNetworkInterface' smart constructor.
data ScheduledInstancesNetworkInterface = ScheduledInstancesNetworkInterface'
  { -- | Indicates whether to assign a public IPv4 address to instances launched in a VPC. The public IPv4 address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
    associatePublicIpAddress :: Core.Maybe Core.Bool,
    -- | Indicates whether to delete the interface when the instance is terminated.
    deleteOnTermination :: Core.Maybe Core.Bool,
    -- | The description.
    description :: Core.Maybe Types.String,
    -- | The index of the device for the network interface attachment.
    deviceIndex :: Core.Maybe Core.Int,
    -- | The IDs of the security groups.
    groups :: Core.Maybe [Types.SecurityGroupId],
    -- | The number of IPv6 addresses to assign to the network interface. The IPv6 addresses are automatically selected from the subnet range.
    ipv6AddressCount :: Core.Maybe Core.Int,
    -- | The specific IPv6 addresses from the subnet range.
    ipv6Addresses :: Core.Maybe [Types.ScheduledInstancesIpv6Address],
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Types.NetworkInterfaceId,
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Core.Maybe Types.String,
    -- | The private IPv4 addresses.
    privateIpAddressConfigs :: Core.Maybe [Types.ScheduledInstancesPrivateIpAddressConfig],
    -- | The number of secondary private IPv4 addresses.
    secondaryPrivateIpAddressCount :: Core.Maybe Core.Int,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Types.SubnetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ScheduledInstancesNetworkInterface' value with any optional fields omitted.
mkScheduledInstancesNetworkInterface ::
  ScheduledInstancesNetworkInterface
mkScheduledInstancesNetworkInterface =
  ScheduledInstancesNetworkInterface'
    { associatePublicIpAddress =
        Core.Nothing,
      deleteOnTermination = Core.Nothing,
      description = Core.Nothing,
      deviceIndex = Core.Nothing,
      groups = Core.Nothing,
      ipv6AddressCount = Core.Nothing,
      ipv6Addresses = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      privateIpAddress = Core.Nothing,
      privateIpAddressConfigs = Core.Nothing,
      secondaryPrivateIpAddressCount = Core.Nothing,
      subnetId = Core.Nothing
    }

-- | Indicates whether to assign a public IPv4 address to instances launched in a VPC. The public IPv4 address can only be assigned to a network interface for eth0, and can only be assigned to a new network interface, not an existing one. You cannot specify more than one network interface in the request. If launching into a default subnet, the default value is @true@ .
--
-- /Note:/ Consider using 'associatePublicIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniAssociatePublicIpAddress :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Bool)
siniAssociatePublicIpAddress = Lens.field @"associatePublicIpAddress"
{-# DEPRECATED siniAssociatePublicIpAddress "Use generic-lens or generic-optics with 'associatePublicIpAddress' instead." #-}

-- | Indicates whether to delete the interface when the instance is terminated.
--
-- /Note:/ Consider using 'deleteOnTermination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniDeleteOnTermination :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Bool)
siniDeleteOnTermination = Lens.field @"deleteOnTermination"
{-# DEPRECATED siniDeleteOnTermination "Use generic-lens or generic-optics with 'deleteOnTermination' instead." #-}

-- | The description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniDescription :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Types.String)
siniDescription = Lens.field @"description"
{-# DEPRECATED siniDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | The index of the device for the network interface attachment.
--
-- /Note:/ Consider using 'deviceIndex' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniDeviceIndex :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Int)
siniDeviceIndex = Lens.field @"deviceIndex"
{-# DEPRECATED siniDeviceIndex "Use generic-lens or generic-optics with 'deviceIndex' instead." #-}

-- | The IDs of the security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniGroups :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe [Types.SecurityGroupId])
siniGroups = Lens.field @"groups"
{-# DEPRECATED siniGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The number of IPv6 addresses to assign to the network interface. The IPv6 addresses are automatically selected from the subnet range.
--
-- /Note:/ Consider using 'ipv6AddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniIpv6AddressCount :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Int)
siniIpv6AddressCount = Lens.field @"ipv6AddressCount"
{-# DEPRECATED siniIpv6AddressCount "Use generic-lens or generic-optics with 'ipv6AddressCount' instead." #-}

-- | The specific IPv6 addresses from the subnet range.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniIpv6Addresses :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe [Types.ScheduledInstancesIpv6Address])
siniIpv6Addresses = Lens.field @"ipv6Addresses"
{-# DEPRECATED siniIpv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniNetworkInterfaceId :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Types.NetworkInterfaceId)
siniNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED siniNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The IPv4 address of the network interface within the subnet.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniPrivateIpAddress :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Types.String)
siniPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED siniPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | The private IPv4 addresses.
--
-- /Note:/ Consider using 'privateIpAddressConfigs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniPrivateIpAddressConfigs :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe [Types.ScheduledInstancesPrivateIpAddressConfig])
siniPrivateIpAddressConfigs = Lens.field @"privateIpAddressConfigs"
{-# DEPRECATED siniPrivateIpAddressConfigs "Use generic-lens or generic-optics with 'privateIpAddressConfigs' instead." #-}

-- | The number of secondary private IPv4 addresses.
--
-- /Note:/ Consider using 'secondaryPrivateIpAddressCount' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniSecondaryPrivateIpAddressCount :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Core.Int)
siniSecondaryPrivateIpAddressCount = Lens.field @"secondaryPrivateIpAddressCount"
{-# DEPRECATED siniSecondaryPrivateIpAddressCount "Use generic-lens or generic-optics with 'secondaryPrivateIpAddressCount' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
siniSubnetId :: Lens.Lens' ScheduledInstancesNetworkInterface (Core.Maybe Types.SubnetId)
siniSubnetId = Lens.field @"subnetId"
{-# DEPRECATED siniSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}
