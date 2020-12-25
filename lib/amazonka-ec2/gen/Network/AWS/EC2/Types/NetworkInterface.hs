{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.NetworkInterface
  ( NetworkInterface (..),

    -- * Smart constructor
    mkNetworkInterface,

    -- * Lenses
    niAssociation,
    niAttachment,
    niAvailabilityZone,
    niDescription,
    niGroups,
    niInterfaceType,
    niIpv6Addresses,
    niMacAddress,
    niNetworkInterfaceId,
    niOutpostArn,
    niOwnerId,
    niPrivateDnsName,
    niPrivateIpAddress,
    niPrivateIpAddresses,
    niRequesterId,
    niRequesterManaged,
    niSourceDestCheck,
    niStatus,
    niSubnetId,
    niTagSet,
    niVpcId,
  )
where

import qualified Network.AWS.EC2.Types.GroupIdentifier as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceAssociation as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceAttachment as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceIpv6Address as Types
import qualified Network.AWS.EC2.Types.NetworkInterfacePrivateIpAddress as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceStatus as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceType as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network interface.
--
-- /See:/ 'mkNetworkInterface' smart constructor.
data NetworkInterface = NetworkInterface'
  { -- | The association information for an Elastic IP address (IPv4) associated with the network interface.
    association :: Core.Maybe Types.NetworkInterfaceAssociation,
    -- | The network interface attachment.
    attachment :: Core.Maybe Types.NetworkInterfaceAttachment,
    -- | The Availability Zone.
    availabilityZone :: Core.Maybe Types.String,
    -- | A description.
    description :: Core.Maybe Types.String,
    -- | Any security groups for the network interface.
    groups :: Core.Maybe [Types.GroupIdentifier],
    -- | The type of network interface.
    interfaceType :: Core.Maybe Types.NetworkInterfaceType,
    -- | The IPv6 addresses associated with the network interface.
    ipv6Addresses :: Core.Maybe [Types.NetworkInterfaceIpv6Address],
    -- | The MAC address.
    macAddress :: Core.Maybe Types.String,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Types.String,
    -- | The Amazon Resource Name (ARN) of the Outpost.
    outpostArn :: Core.Maybe Types.String,
    -- | The AWS account ID of the owner of the network interface.
    ownerId :: Core.Maybe Types.String,
    -- | The private DNS name.
    privateDnsName :: Core.Maybe Types.String,
    -- | The IPv4 address of the network interface within the subnet.
    privateIpAddress :: Core.Maybe Types.String,
    -- | The private IPv4 addresses associated with the network interface.
    privateIpAddresses :: Core.Maybe [Types.NetworkInterfacePrivateIpAddress],
    -- | The ID of the entity that launched the instance on your behalf (for example, AWS Management Console or Auto Scaling).
    requesterId :: Core.Maybe Types.String,
    -- | Indicates whether the network interface is being managed by AWS.
    requesterManaged :: Core.Maybe Core.Bool,
    -- | Indicates whether traffic to or from the instance is validated.
    sourceDestCheck :: Core.Maybe Core.Bool,
    -- | The status of the network interface.
    status :: Core.Maybe Types.NetworkInterfaceStatus,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Types.String,
    -- | Any tags assigned to the network interface.
    tagSet :: Core.Maybe [Types.Tag],
    -- | The ID of the VPC.
    vpcId :: Core.Maybe Types.String
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'NetworkInterface' value with any optional fields omitted.
mkNetworkInterface ::
  NetworkInterface
mkNetworkInterface =
  NetworkInterface'
    { association = Core.Nothing,
      attachment = Core.Nothing,
      availabilityZone = Core.Nothing,
      description = Core.Nothing,
      groups = Core.Nothing,
      interfaceType = Core.Nothing,
      ipv6Addresses = Core.Nothing,
      macAddress = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      outpostArn = Core.Nothing,
      ownerId = Core.Nothing,
      privateDnsName = Core.Nothing,
      privateIpAddress = Core.Nothing,
      privateIpAddresses = Core.Nothing,
      requesterId = Core.Nothing,
      requesterManaged = Core.Nothing,
      sourceDestCheck = Core.Nothing,
      status = Core.Nothing,
      subnetId = Core.Nothing,
      tagSet = Core.Nothing,
      vpcId = Core.Nothing
    }

-- | The association information for an Elastic IP address (IPv4) associated with the network interface.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niAssociation :: Lens.Lens' NetworkInterface (Core.Maybe Types.NetworkInterfaceAssociation)
niAssociation = Lens.field @"association"
{-# DEPRECATED niAssociation "Use generic-lens or generic-optics with 'association' instead." #-}

-- | The network interface attachment.
--
-- /Note:/ Consider using 'attachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niAttachment :: Lens.Lens' NetworkInterface (Core.Maybe Types.NetworkInterfaceAttachment)
niAttachment = Lens.field @"attachment"
{-# DEPRECATED niAttachment "Use generic-lens or generic-optics with 'attachment' instead." #-}

-- | The Availability Zone.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niAvailabilityZone :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niAvailabilityZone = Lens.field @"availabilityZone"
{-# DEPRECATED niAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

-- | A description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niDescription :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niDescription = Lens.field @"description"
{-# DEPRECATED niDescription "Use generic-lens or generic-optics with 'description' instead." #-}

-- | Any security groups for the network interface.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niGroups :: Lens.Lens' NetworkInterface (Core.Maybe [Types.GroupIdentifier])
niGroups = Lens.field @"groups"
{-# DEPRECATED niGroups "Use generic-lens or generic-optics with 'groups' instead." #-}

-- | The type of network interface.
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niInterfaceType :: Lens.Lens' NetworkInterface (Core.Maybe Types.NetworkInterfaceType)
niInterfaceType = Lens.field @"interfaceType"
{-# DEPRECATED niInterfaceType "Use generic-lens or generic-optics with 'interfaceType' instead." #-}

-- | The IPv6 addresses associated with the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niIpv6Addresses :: Lens.Lens' NetworkInterface (Core.Maybe [Types.NetworkInterfaceIpv6Address])
niIpv6Addresses = Lens.field @"ipv6Addresses"
{-# DEPRECATED niIpv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead." #-}

-- | The MAC address.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niMacAddress :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niMacAddress = Lens.field @"macAddress"
{-# DEPRECATED niMacAddress "Use generic-lens or generic-optics with 'macAddress' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niNetworkInterfaceId :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED niNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The Amazon Resource Name (ARN) of the Outpost.
--
-- /Note:/ Consider using 'outpostArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niOutpostArn :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niOutpostArn = Lens.field @"outpostArn"
{-# DEPRECATED niOutpostArn "Use generic-lens or generic-optics with 'outpostArn' instead." #-}

-- | The AWS account ID of the owner of the network interface.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niOwnerId :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niOwnerId = Lens.field @"ownerId"
{-# DEPRECATED niOwnerId "Use generic-lens or generic-optics with 'ownerId' instead." #-}

-- | The private DNS name.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateDnsName :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niPrivateDnsName = Lens.field @"privateDnsName"
{-# DEPRECATED niPrivateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead." #-}

-- | The IPv4 address of the network interface within the subnet.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIpAddress :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED niPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | The private IPv4 addresses associated with the network interface.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niPrivateIpAddresses :: Lens.Lens' NetworkInterface (Core.Maybe [Types.NetworkInterfacePrivateIpAddress])
niPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# DEPRECATED niPrivateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead." #-}

-- | The ID of the entity that launched the instance on your behalf (for example, AWS Management Console or Auto Scaling).
--
-- /Note:/ Consider using 'requesterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niRequesterId :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niRequesterId = Lens.field @"requesterId"
{-# DEPRECATED niRequesterId "Use generic-lens or generic-optics with 'requesterId' instead." #-}

-- | Indicates whether the network interface is being managed by AWS.
--
-- /Note:/ Consider using 'requesterManaged' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niRequesterManaged :: Lens.Lens' NetworkInterface (Core.Maybe Core.Bool)
niRequesterManaged = Lens.field @"requesterManaged"
{-# DEPRECATED niRequesterManaged "Use generic-lens or generic-optics with 'requesterManaged' instead." #-}

-- | Indicates whether traffic to or from the instance is validated.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSourceDestCheck :: Lens.Lens' NetworkInterface (Core.Maybe Core.Bool)
niSourceDestCheck = Lens.field @"sourceDestCheck"
{-# DEPRECATED niSourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead." #-}

-- | The status of the network interface.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niStatus :: Lens.Lens' NetworkInterface (Core.Maybe Types.NetworkInterfaceStatus)
niStatus = Lens.field @"status"
{-# DEPRECATED niStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niSubnetId :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niSubnetId = Lens.field @"subnetId"
{-# DEPRECATED niSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}

-- | Any tags assigned to the network interface.
--
-- /Note:/ Consider using 'tagSet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niTagSet :: Lens.Lens' NetworkInterface (Core.Maybe [Types.Tag])
niTagSet = Lens.field @"tagSet"
{-# DEPRECATED niTagSet "Use generic-lens or generic-optics with 'tagSet' instead." #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niVpcId :: Lens.Lens' NetworkInterface (Core.Maybe Types.String)
niVpcId = Lens.field @"vpcId"
{-# DEPRECATED niVpcId "Use generic-lens or generic-optics with 'vpcId' instead." #-}

instance Core.FromXML NetworkInterface where
  parseXML x =
    NetworkInterface'
      Core.<$> (x Core..@? "association")
      Core.<*> (x Core..@? "attachment")
      Core.<*> (x Core..@? "availabilityZone")
      Core.<*> (x Core..@? "description")
      Core.<*> (x Core..@? "groupSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "interfaceType")
      Core.<*> (x Core..@? "ipv6AddressesSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "macAddress")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "outpostArn")
      Core.<*> (x Core..@? "ownerId")
      Core.<*> (x Core..@? "privateDnsName")
      Core.<*> (x Core..@? "privateIpAddress")
      Core.<*> ( x Core..@? "privateIpAddressesSet"
                   Core..<@> Core.parseXMLList "item"
               )
      Core.<*> (x Core..@? "requesterId")
      Core.<*> (x Core..@? "requesterManaged")
      Core.<*> (x Core..@? "sourceDestCheck")
      Core.<*> (x Core..@? "status")
      Core.<*> (x Core..@? "subnetId")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
      Core.<*> (x Core..@? "vpcId")
