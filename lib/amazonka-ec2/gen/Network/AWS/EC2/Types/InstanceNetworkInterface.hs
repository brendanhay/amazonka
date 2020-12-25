{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.InstanceNetworkInterface
  ( InstanceNetworkInterface (..),

    -- * Smart constructor
    mkInstanceNetworkInterface,

    -- * Lenses
    iniAssociation,
    iniAttachment,
    iniDescription,
    iniGroups,
    iniInterfaceType,
    iniIpv6Addresses,
    iniMacAddress,
    iniNetworkInterfaceId,
    iniOwnerId,
    iniPrivateDnsName,
    iniPrivateIpAddress,
    iniPrivateIpAddresses,
    iniSourceDestCheck,
    iniStatus,
    iniSubnetId,
    iniVpcId,
  )
where

import qualified Network.AWS.EC2.Types.GroupIdentifier as Types
import qualified Network.AWS.EC2.Types.InstanceIpv6Address as Types
import qualified Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation as Types
import qualified Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment as Types
import qualified Network.AWS.EC2.Types.InstancePrivateIpAddress as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceStatus as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network interface.
--
-- /See:/ 'mkInstanceNetworkInterface' smart constructor.
data InstanceNetworkInterface = InstanceNetworkInterface'
  { -- | The association information for an Elastic IPv4 associated with the network interface.
    association :: Core.Maybe Types.InstanceNetworkInterfaceAssociation,
    -- | The network interface attachment.
    attachment :: Core.Maybe Types.InstanceNetworkInterfaceAttachment,
    -- | The description.
    description :: Core.Maybe Types.String,
    -- | One or more security groups.
    groups :: Core.Maybe [Types.GroupIdentifier],
    -- | Describes the type of network interface.
    --
    -- Valid values: @interface@ | @efa@
    interfaceType :: Core.Maybe Types.String,
    -- | One or more IPv6 addresses associated with the network interface.
    ipv6Addresses :: Core.Maybe [Types.InstanceIpv6Address],
    -- | The MAC address.
    macAddress :: Core.Maybe Types.String,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Types.String,
    -- | The ID of the AWS account that created the network interface.
    ownerId :: Core.Maybe Types.String,
    -- | The private DNS name.
    privateDnsName :: Core.Maybe Types.String,
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Core.Maybe Types.String,
    -- | One or more private IPv4 addresses associated with the network interface.
    privateIpAddresses :: Core.Maybe [Types.InstancePrivateIpAddress],
    -- | Indicates whether to validate network traffic to or from this network interface.
    sourceDestCheck :: Core.Maybe Core.Bool,
    -- | The status of the network interface.
    status :: Core.Maybe Types.NetworkInterfaceStatus,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Types.String,
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'InstanceNetworkInterface' value with any optional fields omitted.
mkInstanceNetworkInterface ::
  InstanceNetworkInterface
mkInstanceNetworkInterface =
  InstanceNetworkInterface'
    { association = Core.Nothing,
      attachment = Core.Nothing,
      description = Core.Nothing,
      groups = Core.Nothing,
      interfaceType = Core.Nothing,
      ipv6Addresses = Core.Nothing,
      macAddress = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      ownerId = Core.Nothing,
      privateDnsName = Core.Nothing,
      privateIpAddress = Core.Nothing,
      privateIpAddresses = Core.Nothing,
      sourceDestCheck = Core.Nothing,
      status = Core.Nothing,
      subnetId = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The association information for an Elastic IPv4 associated with the network interface.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniAssociation :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.InstanceNetworkInterfaceAssociation)
iniAssociation = Lens.field @"association"
{-# DEPRECATED iniAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

-- | The network interface attachment.
--
-- /Note:/ Consider using 'attachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniAttachment :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.InstanceNetworkInterfaceAttachment)
iniAttachment = Lens.field @"attachment"
{-# DEPRECATED iniAttachment "Use generic-lens or generic-optics with 'attachment' instead." #-}

-- | The description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniDescription :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.String)
iniDescription = Lens.field @"description"
{-# DEPRECATED iniDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | One or more security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniGroups :: Lens.Lens' InstanceNetworkInterface (Core.Maybe [Types.GroupIdentifier])
iniGroups = Lens.field @"groups"
{-# DEPRECATED iniGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | Describes the type of network interface.
--
-- Valid values: @interface@ | @efa@
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniInterfaceType :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.String)
iniInterfaceType = Lens.field @"interfaceType"
{-# DEPRECATED iniInterfaceType "Use generic-lens or generic-optics with 'interfaceType' instead." #-}

-- | One or more IPv6 addresses associated with the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniIpv6Addresses :: Lens.Lens' InstanceNetworkInterface (Core.Maybe [Types.InstanceIpv6Address])
iniIpv6Addresses = Lens.field @"ipv6Addresses"
{-# DEPRECATED iniIpv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

-- | The MAC address.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniMacAddress :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.String)
iniMacAddress = Lens.field @"macAddress"
{-# DEPRECATED iniMacAddress "Use generic-lens or generic-optics with 'macAddress' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniNetworkInterfaceId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.String)
iniNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED iniNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the AWS account that created the network interface.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniOwnerId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.String)
iniOwnerId = Lens.field @"ownerId"
{-# DEPRECATED iniOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The private DNS name.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniPrivateDnsName :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.String)
iniPrivateDnsName = Lens.field @"privateDnsName"
{-# DEPRECATED iniPrivateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead." #-}

-- | The IPv4 address of the network interface within the subnet.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniPrivateIpAddress :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.String)
iniPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED iniPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | One or more private IPv4 addresses associated with the network interface.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniPrivateIpAddresses :: Lens.Lens' InstanceNetworkInterface (Core.Maybe [Types.InstancePrivateIpAddress])
iniPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# DEPRECATED iniPrivateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead." #-}

-- | Indicates whether to validate network traffic to or from this network interface.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniSourceDestCheck :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Bool)
iniSourceDestCheck = Lens.field @"sourceDestCheck"
{-# DEPRECATED iniSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | The status of the network interface.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniStatus :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.NetworkInterfaceStatus)
iniStatus = Lens.field @"status"
{-# DEPRECATED iniStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniSubnetId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.String)
iniSubnetId = Lens.field @"subnetId"
{-# DEPRECATED iniSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniVpcId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.String)
iniVpcId = Lens.field @"vpcId"
{-# DEPRECATED iniVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML InstanceNetworkInterface where
  parseXML x =
    InstanceNetworkInterface'
      Core.<$> (x Core..@? "association")
      Core.<*> (x Core..@? "attachment")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "groupSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "interfaceType")
      Core.<*> (x Core..@? "ipv6AddressesSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "macAddress")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "privateDnsName")
      Core.<*> (x Core..@? "privateIpAddress")
      Core.<*> ( x Core..@? "privateIpAddressesSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "sourceDestCheck")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "vpcId")
