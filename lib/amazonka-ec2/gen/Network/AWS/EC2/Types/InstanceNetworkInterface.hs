{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.InstanceNetworkInterface
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.InstanceNetworkInterface
  ( InstanceNetworkInterface (..)
  -- * Smart constructor
  , mkInstanceNetworkInterface
  -- * Lenses
  , iniAssociation
  , iniAttachment
  , iniDescription
  , iniGroups
  , iniInterfaceType
  , iniIpv6Addresses
  , iniMacAddress
  , iniNetworkInterfaceId
  , iniOwnerId
  , iniPrivateDnsName
  , iniPrivateIpAddress
  , iniPrivateIpAddresses
  , iniSourceDestCheck
  , iniStatus
  , iniSubnetId
  , iniVpcId
  ) where

import qualified Network.AWS.EC2.Types.GroupIdentifier as Types
import qualified Network.AWS.EC2.Types.InstanceIpv6Address as Types
import qualified Network.AWS.EC2.Types.InstanceNetworkInterfaceAssociation as Types
import qualified Network.AWS.EC2.Types.InstanceNetworkInterfaceAttachment as Types
import qualified Network.AWS.EC2.Types.InstancePrivateIpAddress as Types
import qualified Network.AWS.EC2.Types.NetworkInterfaceStatus as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes a network interface.
--
-- /See:/ 'mkInstanceNetworkInterface' smart constructor.
data InstanceNetworkInterface = InstanceNetworkInterface'
  { association :: Core.Maybe Types.InstanceNetworkInterfaceAssociation
    -- ^ The association information for an Elastic IPv4 associated with the network interface.
  , attachment :: Core.Maybe Types.InstanceNetworkInterfaceAttachment
    -- ^ The network interface attachment.
  , description :: Core.Maybe Core.Text
    -- ^ The description.
  , groups :: Core.Maybe [Types.GroupIdentifier]
    -- ^ One or more security groups.
  , interfaceType :: Core.Maybe Core.Text
    -- ^ Describes the type of network interface.
--
-- Valid values: @interface@ | @efa@ 
  , ipv6Addresses :: Core.Maybe [Types.InstanceIpv6Address]
    -- ^ One or more IPv6 addresses associated with the network interface.
  , macAddress :: Core.Maybe Core.Text
    -- ^ The MAC address.
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface.
  , ownerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that created the network interface.
  , privateDnsName :: Core.Maybe Core.Text
    -- ^ The private DNS name.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The IPv4 address of the network interface within the subnet.
  , privateIpAddresses :: Core.Maybe [Types.InstancePrivateIpAddress]
    -- ^ One or more private IPv4 addresses associated with the network interface.
  , sourceDestCheck :: Core.Maybe Core.Bool
    -- ^ Indicates whether to validate network traffic to or from this network interface.
  , status :: Core.Maybe Types.NetworkInterfaceStatus
    -- ^ The status of the network interface.
  , subnetId :: Core.Maybe Core.Text
    -- ^ The ID of the subnet.
  , vpcId :: Core.Maybe Core.Text
    -- ^ The ID of the VPC.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'InstanceNetworkInterface' value with any optional fields omitted.
mkInstanceNetworkInterface
    :: InstanceNetworkInterface
mkInstanceNetworkInterface
  = InstanceNetworkInterface'{association = Core.Nothing,
                              attachment = Core.Nothing, description = Core.Nothing,
                              groups = Core.Nothing, interfaceType = Core.Nothing,
                              ipv6Addresses = Core.Nothing, macAddress = Core.Nothing,
                              networkInterfaceId = Core.Nothing, ownerId = Core.Nothing,
                              privateDnsName = Core.Nothing, privateIpAddress = Core.Nothing,
                              privateIpAddresses = Core.Nothing, sourceDestCheck = Core.Nothing,
                              status = Core.Nothing, subnetId = Core.Nothing,
                              vpcId = Core.Nothing}

-- | The association information for an Elastic IPv4 associated with the network interface.
--
-- /Note:/ Consider using 'association' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniAssociation :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.InstanceNetworkInterfaceAssociation)
iniAssociation = Lens.field @"association"
{-# INLINEABLE iniAssociation #-}
{-# DEPRECATED association "Use generic-lens or generic-optics with 'association' instead"  #-}

-- | The network interface attachment.
--
-- /Note:/ Consider using 'attachment' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniAttachment :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.InstanceNetworkInterfaceAttachment)
iniAttachment = Lens.field @"attachment"
{-# INLINEABLE iniAttachment #-}
{-# DEPRECATED attachment "Use generic-lens or generic-optics with 'attachment' instead"  #-}

-- | The description.
--
-- /Note:/ Consider using 'description' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniDescription :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
iniDescription = Lens.field @"description"
{-# INLINEABLE iniDescription #-}
{-# DEPRECATED description "Use generic-lens or generic-optics with 'description' instead"  #-}

-- | One or more security groups.
--
-- /Note:/ Consider using 'groups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniGroups :: Lens.Lens' InstanceNetworkInterface (Core.Maybe [Types.GroupIdentifier])
iniGroups = Lens.field @"groups"
{-# INLINEABLE iniGroups #-}
{-# DEPRECATED groups "Use generic-lens or generic-optics with 'groups' instead"  #-}

-- | Describes the type of network interface.
--
-- Valid values: @interface@ | @efa@ 
--
-- /Note:/ Consider using 'interfaceType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniInterfaceType :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
iniInterfaceType = Lens.field @"interfaceType"
{-# INLINEABLE iniInterfaceType #-}
{-# DEPRECATED interfaceType "Use generic-lens or generic-optics with 'interfaceType' instead"  #-}

-- | One or more IPv6 addresses associated with the network interface.
--
-- /Note:/ Consider using 'ipv6Addresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniIpv6Addresses :: Lens.Lens' InstanceNetworkInterface (Core.Maybe [Types.InstanceIpv6Address])
iniIpv6Addresses = Lens.field @"ipv6Addresses"
{-# INLINEABLE iniIpv6Addresses #-}
{-# DEPRECATED ipv6Addresses "Use generic-lens or generic-optics with 'ipv6Addresses' instead"  #-}

-- | The MAC address.
--
-- /Note:/ Consider using 'macAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniMacAddress :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
iniMacAddress = Lens.field @"macAddress"
{-# INLINEABLE iniMacAddress #-}
{-# DEPRECATED macAddress "Use generic-lens or generic-optics with 'macAddress' instead"  #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniNetworkInterfaceId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
iniNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE iniNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The ID of the AWS account that created the network interface.
--
-- /Note:/ Consider using 'ownerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniOwnerId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
iniOwnerId = Lens.field @"ownerId"
{-# INLINEABLE iniOwnerId #-}
{-# DEPRECATED ownerId "Use generic-lens or generic-optics with 'ownerId' instead"  #-}

-- | The private DNS name.
--
-- /Note:/ Consider using 'privateDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniPrivateDnsName :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
iniPrivateDnsName = Lens.field @"privateDnsName"
{-# INLINEABLE iniPrivateDnsName #-}
{-# DEPRECATED privateDnsName "Use generic-lens or generic-optics with 'privateDnsName' instead"  #-}

-- | The IPv4 address of the network interface within the subnet.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniPrivateIpAddress :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
iniPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE iniPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

-- | One or more private IPv4 addresses associated with the network interface.
--
-- /Note:/ Consider using 'privateIpAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniPrivateIpAddresses :: Lens.Lens' InstanceNetworkInterface (Core.Maybe [Types.InstancePrivateIpAddress])
iniPrivateIpAddresses = Lens.field @"privateIpAddresses"
{-# INLINEABLE iniPrivateIpAddresses #-}
{-# DEPRECATED privateIpAddresses "Use generic-lens or generic-optics with 'privateIpAddresses' instead"  #-}

-- | Indicates whether to validate network traffic to or from this network interface.
--
-- /Note:/ Consider using 'sourceDestCheck' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniSourceDestCheck :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Bool)
iniSourceDestCheck = Lens.field @"sourceDestCheck"
{-# INLINEABLE iniSourceDestCheck #-}
{-# DEPRECATED sourceDestCheck "Use generic-lens or generic-optics with 'sourceDestCheck' instead"  #-}

-- | The status of the network interface.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniStatus :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Types.NetworkInterfaceStatus)
iniStatus = Lens.field @"status"
{-# INLINEABLE iniStatus #-}
{-# DEPRECATED status "Use generic-lens or generic-optics with 'status' instead"  #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniSubnetId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
iniSubnetId = Lens.field @"subnetId"
{-# INLINEABLE iniSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The ID of the VPC.
--
-- /Note:/ Consider using 'vpcId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
iniVpcId :: Lens.Lens' InstanceNetworkInterface (Core.Maybe Core.Text)
iniVpcId = Lens.field @"vpcId"
{-# INLINEABLE iniVpcId #-}
{-# DEPRECATED vpcId "Use generic-lens or generic-optics with 'vpcId' instead"  #-}

instance Core.FromXML InstanceNetworkInterface where
        parseXML x
          = InstanceNetworkInterface' Core.<$>
              (x Core..@? "association") Core.<*> x Core..@? "attachment"
                Core.<*> x Core..@? "description"
                Core.<*> x Core..@? "groupSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "interfaceType"
                Core.<*>
                x Core..@? "ipv6AddressesSet" Core..<@> Core.parseXMLList "item"
                Core.<*> x Core..@? "macAddress"
                Core.<*> x Core..@? "networkInterfaceId"
                Core.<*> x Core..@? "ownerId"
                Core.<*> x Core..@? "privateDnsName"
                Core.<*> x Core..@? "privateIpAddress"
                Core.<*>
                x Core..@? "privateIpAddressesSet" Core..<@>
                  Core.parseXMLList "item"
                Core.<*> x Core..@? "sourceDestCheck"
                Core.<*> x Core..@? "status"
                Core.<*> x Core..@? "subnetId"
                Core.<*> x Core..@? "vpcId"
