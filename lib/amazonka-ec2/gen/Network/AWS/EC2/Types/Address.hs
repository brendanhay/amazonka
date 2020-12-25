{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.Address
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EC2.Types.Address
  ( Address (..),

    -- * Smart constructor
    mkAddress,

    -- * Lenses
    aAllocationId,
    aAssociationId,
    aCarrierIp,
    aCustomerOwnedIp,
    aCustomerOwnedIpv4Pool,
    aDomain,
    aInstanceId,
    aNetworkBorderGroup,
    aNetworkInterfaceId,
    aNetworkInterfaceOwnerId,
    aPrivateIpAddress,
    aPublicIp,
    aPublicIpv4Pool,
    aTags,
  )
where

import qualified Network.AWS.EC2.Types.DomainType as Types
import qualified Network.AWS.EC2.Types.String as Types
import qualified Network.AWS.EC2.Types.Tag as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes an Elastic IP address, or a carrier IP address.
--
-- /See:/ 'mkAddress' smart constructor.
data Address = Address'
  { -- | The ID representing the allocation of the address for use with EC2-VPC.
    allocationId :: Core.Maybe Types.String,
    -- | The ID representing the association of the address with an instance in a VPC.
    associationId :: Core.Maybe Types.String,
    -- | The carrier IP address associated. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance).
    carrierIp :: Core.Maybe Types.String,
    -- | The customer-owned IP address.
    customerOwnedIp :: Core.Maybe Types.String,
    -- | The ID of the customer-owned address pool.
    customerOwnedIpv4Pool :: Core.Maybe Types.String,
    -- | Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
    domain :: Core.Maybe Types.DomainType,
    -- | The ID of the instance that the address is associated with (if any).
    instanceId :: Core.Maybe Types.String,
    -- | The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
    networkBorderGroup :: Core.Maybe Types.String,
    -- | The ID of the network interface.
    networkInterfaceId :: Core.Maybe Types.String,
    -- | The ID of the AWS account that owns the network interface.
    networkInterfaceOwnerId :: Core.Maybe Types.String,
    -- | The private IP address associated with the Elastic IP address.
    privateIpAddress :: Core.Maybe Types.String,
    -- | The Elastic IP address.
    publicIp :: Core.Maybe Types.String,
    -- | The ID of an address pool.
    publicIpv4Pool :: Core.Maybe Types.String,
    -- | Any tags assigned to the Elastic IP address.
    tags :: Core.Maybe [Types.Tag]
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Address' value with any optional fields omitted.
mkAddress ::
  Address
mkAddress =
  Address'
    { allocationId = Core.Nothing,
      associationId = Core.Nothing,
      carrierIp = Core.Nothing,
      customerOwnedIp = Core.Nothing,
      customerOwnedIpv4Pool = Core.Nothing,
      domain = Core.Nothing,
      instanceId = Core.Nothing,
      networkBorderGroup = Core.Nothing,
      networkInterfaceId = Core.Nothing,
      networkInterfaceOwnerId = Core.Nothing,
      privateIpAddress = Core.Nothing,
      publicIp = Core.Nothing,
      publicIpv4Pool = Core.Nothing,
      tags = Core.Nothing
    }

-- | The ID representing the allocation of the address for use with EC2-VPC.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAllocationId :: Lens.Lens' Address (Core.Maybe Types.String)
aAllocationId = Lens.field @"allocationId"
{-# DEPRECATED aAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | The ID representing the association of the address with an instance in a VPC.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aAssociationId :: Lens.Lens' Address (Core.Maybe Types.String)
aAssociationId = Lens.field @"associationId"
{-# DEPRECATED aAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The carrier IP address associated. This option is only available for network interfaces which reside in a subnet in a Wavelength Zone (for example an EC2 instance).
--
-- /Note:/ Consider using 'carrierIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCarrierIp :: Lens.Lens' Address (Core.Maybe Types.String)
aCarrierIp = Lens.field @"carrierIp"
{-# DEPRECATED aCarrierIp "Use generic-lens or generic-optics with 'carrierIp' instead." #-}

-- | The customer-owned IP address.
--
-- /Note:/ Consider using 'customerOwnedIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCustomerOwnedIp :: Lens.Lens' Address (Core.Maybe Types.String)
aCustomerOwnedIp = Lens.field @"customerOwnedIp"
{-# DEPRECATED aCustomerOwnedIp "Use generic-lens or generic-optics with 'customerOwnedIp' instead." #-}

-- | The ID of the customer-owned address pool.
--
-- /Note:/ Consider using 'customerOwnedIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aCustomerOwnedIpv4Pool :: Lens.Lens' Address (Core.Maybe Types.String)
aCustomerOwnedIpv4Pool = Lens.field @"customerOwnedIpv4Pool"
{-# DEPRECATED aCustomerOwnedIpv4Pool "Use generic-lens or generic-optics with 'customerOwnedIpv4Pool' instead." #-}

-- | Indicates whether this Elastic IP address is for use with instances in EC2-Classic (@standard@ ) or instances in a VPC (@vpc@ ).
--
-- /Note:/ Consider using 'domain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aDomain :: Lens.Lens' Address (Core.Maybe Types.DomainType)
aDomain = Lens.field @"domain"
{-# DEPRECATED aDomain "Use generic-lens or generic-optics with 'domain' instead." #-}

-- | The ID of the instance that the address is associated with (if any).
--
-- /Note:/ Consider using 'instanceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aInstanceId :: Lens.Lens' Address (Core.Maybe Types.String)
aInstanceId = Lens.field @"instanceId"
{-# DEPRECATED aInstanceId "Use generic-lens or generic-optics with 'instanceId' instead." #-}

-- | The name of the unique set of Availability Zones, Local Zones, or Wavelength Zones from which AWS advertises IP addresses.
--
-- /Note:/ Consider using 'networkBorderGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkBorderGroup :: Lens.Lens' Address (Core.Maybe Types.String)
aNetworkBorderGroup = Lens.field @"networkBorderGroup"
{-# DEPRECATED aNetworkBorderGroup "Use generic-lens or generic-optics with 'networkBorderGroup' instead." #-}

-- | The ID of the network interface.
--
-- /Note:/ Consider using 'networkInterfaceId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkInterfaceId :: Lens.Lens' Address (Core.Maybe Types.String)
aNetworkInterfaceId = Lens.field @"networkInterfaceId"
{-# DEPRECATED aNetworkInterfaceId "Use generic-lens or generic-optics with 'networkInterfaceId' instead." #-}

-- | The ID of the AWS account that owns the network interface.
--
-- /Note:/ Consider using 'networkInterfaceOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aNetworkInterfaceOwnerId :: Lens.Lens' Address (Core.Maybe Types.String)
aNetworkInterfaceOwnerId = Lens.field @"networkInterfaceOwnerId"
{-# DEPRECATED aNetworkInterfaceOwnerId "Use generic-lens or generic-optics with 'networkInterfaceOwnerId' instead." #-}

-- | The private IP address associated with the Elastic IP address.
--
-- /Note:/ Consider using 'privateIpAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPrivateIpAddress :: Lens.Lens' Address (Core.Maybe Types.String)
aPrivateIpAddress = Lens.field @"privateIpAddress"
{-# DEPRECATED aPrivateIpAddress "Use generic-lens or generic-optics with 'privateIpAddress' instead." #-}

-- | The Elastic IP address.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPublicIp :: Lens.Lens' Address (Core.Maybe Types.String)
aPublicIp = Lens.field @"publicIp"
{-# DEPRECATED aPublicIp "Use generic-lens or generic-optics with 'publicIp' instead." #-}

-- | The ID of an address pool.
--
-- /Note:/ Consider using 'publicIpv4Pool' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aPublicIpv4Pool :: Lens.Lens' Address (Core.Maybe Types.String)
aPublicIpv4Pool = Lens.field @"publicIpv4Pool"
{-# DEPRECATED aPublicIpv4Pool "Use generic-lens or generic-optics with 'publicIpv4Pool' instead." #-}

-- | Any tags assigned to the Elastic IP address.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aTags :: Lens.Lens' Address (Core.Maybe [Types.Tag])
aTags = Lens.field @"tags"
{-# DEPRECATED aTags "Use generic-lens or generic-optics with 'tags' instead." #-}

instance Core.FromXML Address where
  parseXML x =
    Address'
      Core.<$> (x Core..@? "allocationId")
      Core.<*> (x Core..@? "associationId")
      Core.<*> (x Core..@? "carrierIp")
      Core.<*> (x Core..@? "customerOwnedIp")
      Core.<*> (x Core..@? "customerOwnedIpv4Pool")
      Core.<*> (x Core..@? "domain")
      Core.<*> (x Core..@? "instanceId")
      Core.<*> (x Core..@? "networkBorderGroup")
      Core.<*> (x Core..@? "networkInterfaceId")
      Core.<*> (x Core..@? "networkInterfaceOwnerId")
      Core.<*> (x Core..@? "privateIpAddress")
      Core.<*> (x Core..@? "publicIp")
      Core.<*> (x Core..@? "publicIpv4Pool")
      Core.<*> (x Core..@? "tagSet" Core..<@> Core.parseXMLList "item")
