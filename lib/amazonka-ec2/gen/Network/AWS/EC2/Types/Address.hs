{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.Address
  ( Address (..)
  -- * Smart constructor
  , mkAddress
  -- * Lenses
  , aAllocationId
  , aAssociationId
  , aCarrierIp
  , aCustomerOwnedIp
  , aCustomerOwnedIpv4Pool
  , aDomain
  , aInstanceId
  , aNetworkBorderGroup
  , aNetworkInterfaceId
  , aNetworkInterfaceOwnerId
  , aPrivateIpAddress
  , aPublicIp
  , aPublicIpv4Pool
  , aTags
  ) where

import qualified Network.AWS.EC2.Types.DomainType as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Elastic IP address, or a carrier IP address.
--
-- /See:/ 'mkAddress' smart constructor.
data Address = Address'
  { allocationId :: Core.Maybe Core.Text
    -- ^ The ID representing the allocation of the address for use with EC2-VPC.
  , associationId :: Core.Maybe Core.Text
    -- ^ The ID representing the association of the address with an instance in a VPC.
  , carrierIp :: Core.Maybe Core.Text
    -- ^ The carrier IP address associated. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance). 
  , customerOwnedIp :: Core.Maybe Core.Text
    -- ^ The customer-owned IP address.
  , customerOwnedIpv4Pool :: Core.Maybe Core.Text
    -- ^ The ID of the customer-owned address pool.
  , domain :: Core.Maybe Types.DomainType
    -- ^ Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
  , instanceId :: Core.Maybe Core.Text
    -- ^ The ID of the instance that the address is associated with (if any).
  , networkBorderGroup :: Core.Maybe Core.Text
    -- ^ The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses. 
  , networkInterfaceId :: Core.Maybe Core.Text
    -- ^ The ID of the network interface.
  , networkInterfaceOwnerId :: Core.Maybe Core.Text
    -- ^ The ID of the AWS account that owns the network interface.
  , privateIpAddress :: Core.Maybe Core.Text
    -- ^ The private IP address associated with the Elastic IP address.
  , publicIp :: Core.Maybe Core.Text
    -- ^ The Elastic IP address.
  , publicIpv4Pool :: Core.Maybe Core.Text
    -- ^ The ID of an address pool.
  , tags :: Core.Maybe [Types.Tag]
    -- ^ Any tags assigned to the Elastic IP address.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Address' value with any optional fields omitted.
mkAddress
    :: Address
mkAddress
  = Address'{allocationId = Core.Nothing,
             associationId = Core.Nothing, carrierIp = Core.Nothing,
             customerOwnedIp = Core.Nothing,
             customerOwnedIpv4Pool = Core.Nothing, domain = Core.Nothing,
             instanceId = Core.Nothing, networkBorderGroup = Core.Nothing,
             networkInterfaceId = Core.Nothing,
             networkInterfaceOwnerId = Core.Nothing,
             privateIpAddress = Core.Nothing, publicIp = Core.Nothing,
             publicIpv4Pool = Core.Nothing, tags = Core.Nothing}

-- | The ID representing the allocation of the address for use with EC2-VPC.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAllocationId :: Lens.Lens' Address (Core.Maybe Core.Text)
aAllocationId = Lens.field @"allocationId"
{-# INLINEABLE aAllocationId #-}
{-# DEPRECATED allocationId "Use generic-lens or generic-optics with 'allocationId' instead"  #-}

-- | The ID representing the association of the address with an instance in a VPC.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAssociationId :: Lens.Lens' Address (Core.Maybe Core.Text)
aAssociationId = Lens.field @"associationId"
{-# INLINEABLE aAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The carrier IP address associated. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance). 
--
-- /Note:/ Consider using 'carrierIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCarrierIp :: Lens.Lens' Address (Core.Maybe Core.Text)
aCarrierIp = Lens.field @"carrierIp"
{-# INLINEABLE aCarrierIp #-}
{-# DEPRECATED carrierIp "Use generic-lens or generic-optics with 'carrierIp' instead"  #-}

-- | The customer-owned IP address.
--
-- /Note:/ Consider using 'customerOwnedIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCustomerOwnedIp :: Lens.Lens' Address (Core.Maybe Core.Text)
aCustomerOwnedIp = Lens.field @"customerOwnedIp"
{-# INLINEABLE aCustomerOwnedIp #-}
{-# DEPRECATED customerOwnedIp "Use generic-lens or generic-optics with 'customerOwnedIp' instead"  #-}

-- | The ID of the customer-owned address pool.
--
-- /Note:/ Consider using 'customerOwnedIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCustomerOwnedIpv4Pool :: Lens.Lens' Address (Core.Maybe Core.Text)
aCustomerOwnedIpv4Pool = Lens.field @"customerOwnedIpv4Pool"
{-# INLINEABLE aCustomerOwnedIpv4Pool #-}
{-# DEPRECATED customerOwnedIpv4Pool "Use generic-lens or generic-optics with 'customerOwnedIpv4Pool' instead"  #-}

-- | Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDomain :: Lens.Lens' Address (Core.Maybe Types.DomainType)
aDomain = Lens.field @"domain"
{-# INLINEABLE aDomain #-}
{-# DEPRECATED domain "Use generic-lens or generic-optics with 'domain' instead"  #-}

-- | The ID of the instance that the address is associated with (if any).
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aInstanceId :: Lens.Lens' Address (Core.Maybe Core.Text)
aInstanceId = Lens.field @"instanceId"
{-# INLINEABLE aInstanceId #-}
{-# DEPRECATED instanceId "Use generic-lens or generic-optics with 'instanceId' instead"  #-}

-- | The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses. 
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkBorderGroup :: Lens.Lens' Address (Core.Maybe Core.Text)
aNetworkBorderGroup = Lens.field @"networkBorderGroup"
{-# INLINEABLE aNetworkBorderGroup #-}
{-# DEPRECATED networkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead"  #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkInterfaceId :: Lens.Lens' Address (Core.Maybe Core.Text)
aNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# INLINEABLE aNetworkInterfaceId #-}
{-# DEPRECATED networkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead"  #-}

-- | The ID of the AWS account that owns the network interface.
--
-- /Note:/ Consider using 'networkInterfaceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkInterfaceOwnerId :: Lens.Lens' Address (Core.Maybe Core.Text)
aNetworkInterfaceOwnerId = Lens.field @"networkInterfaceOwnerId"
{-# INLINEABLE aNetworkInterfaceOwnerId #-}
{-# DEPRECATED networkInterfaceOwnerId "Use generic-lens or generic-optics with 'networkInterfaceOwnerId' instead"  #-}

-- | The private IP address associated with the Elastic IP address.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrivateIpAddress :: Lens.Lens' Address (Core.Maybe Core.Text)
aPrivateIpAddress = Lens.field @"privateIpAddress"
{-# INLINEABLE aPrivateIpAddress #-}
{-# DEPRECATED privateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead"  #-}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPublicIp :: Lens.Lens' Address (Core.Maybe Core.Text)
aPublicIp = Lens.field @"publicIp"
{-# INLINEABLE aPublicIp #-}
{-# DEPRECATED publicIp "Use generic-lens or generic-optics with 'publicIp' instead"  #-}

-- | The ID of an address pool.
--
-- /Note:/ Consider using 'publicIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPublicIpv4Pool :: Lens.Lens' Address (Core.Maybe Core.Text)
aPublicIpv4Pool = Lens.field @"publicIpv4Pool"
{-# INLINEABLE aPublicIpv4Pool #-}
{-# DEPRECATED publicIpv4Pool "Use generic-lens or generic-optics with 'publicIpv4Pool' instead"  #-}

-- | Any tags assigned to the Elastic IP address.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTags :: Lens.Lens' Address (Core.Maybe [Types.Tag])
aTags = Lens.field @"tags"
{-# INLINEABLE aTags #-}
{-# DEPRECATED tags "Use generic-lens or generic-optics with 'tags' instead"  #-}

instance Core.FromXML Address where
        parseXML x
          = Address' Core.<$>
              (x Core..@? "allocationId") Core.<*> x Core..@? "associationId"
                Core.<*> x Core..@? "carrierIp"
                Core.<*> x Core..@? "customerOwnedIp"
                Core.<*> x Core..@? "customerOwnedIpv4Pool"
                Core.<*> x Core..@? "domain"
                Core.<*> x Core..@? "instanceId"
                Core.<*> x Core..@? "networkBorderGroup"
                Core.<*> x Core..@? "networkInterfaceId"
                Core.<*> x Core..@? "networkInterfaceOwnerId"
                Core.<*> x Core..@? "privateIpAddress"
                Core.<*> x Core..@? "publicIp"
                Core.<*> x Core..@? "publicIpv4Pool"
                Core.<*> x Core..@? "tagSet" Core..<@> Core.parseXMLList "item"
