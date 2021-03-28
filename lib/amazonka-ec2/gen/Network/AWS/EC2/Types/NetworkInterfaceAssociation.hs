{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.Types.NetworkInterfaceAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EC2.Types.NetworkInterfaceAssociation
  ( NetworkInterfaceAssociation (..)
  -- * Smart constructor
  , mkNetworkInterfaceAssociation
  -- * Lenses
  , niaAllocationId
  , niaAssociationId
  , niaCarrierIp
  , niaCustomerOwnedIp
  , niaIpOwnerId
  , niaPublicDnsName
  , niaPublicIp
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Describes association information for an Elastic IP address (IPv4 only), or a Carrier IP address (for a network interface which resides in a subnet in a Wavelength Zone).
--
-- /See:/ 'mkNetworkInterfaceAssociation' smart constructor.
data NetworkInterfaceAssociation = NetworkInterfaceAssociation'
  { allocationId :: Core.Maybe Core.Text
    -- ^ The allocation ID.
  , associationId :: Core.Maybe Core.Text
    -- ^ The association ID.
  , carrierIp :: Core.Maybe Core.Text
    -- ^ The carrier IP address associated with the network interface.
--
-- This option is only available when the network interface is in a subnet which is associated with a Wavelength Zone.
  , customerOwnedIp :: Core.Maybe Core.Text
    -- ^ The customer-owned IP address associated with the network interface.
  , ipOwnerId :: Core.Maybe Core.Text
    -- ^ The ID of the Elastic IP address owner.
  , publicDnsName :: Core.Maybe Core.Text
    -- ^ The public DNS name.
  , publicIp :: Core.Maybe Core.Text
    -- ^ The address of the Elastic IP address bound to the network interface.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'NetworkInterfaceAssociation' value with any optional fields omitted.
mkNetworkInterfaceAssociation
    :: NetworkInterfaceAssociation
mkNetworkInterfaceAssociation
  = NetworkInterfaceAssociation'{allocationId = Core.Nothing,
                                 associationId = Core.Nothing, carrierIp = Core.Nothing,
                                 customerOwnedIp = Core.Nothing, ipOwnerId = Core.Nothing,
                                 publicDnsName = Core.Nothing, publicIp = Core.Nothing}

-- | The allocation ID.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaAllocationId :: Lens.Lens' NetworkInterfaceAssociation (Core.Maybe Core.Text)
niaAllocationId = Lens.field @"allocationId"
{-# INLINEABLE niaAllocationId #-}
{-# DEPRECATED allocationId "Use generic-lens or generic-optics with 'allocationId' instead"  #-}

-- | The association ID.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaAssociationId :: Lens.Lens' NetworkInterfaceAssociation (Core.Maybe Core.Text)
niaAssociationId = Lens.field @"associationId"
{-# INLINEABLE niaAssociationId #-}
{-# DEPRECATED associationId "Use generic-lens or generic-optics with 'associationId' instead"  #-}

-- | The carrier IP address associated with the network interface.
--
-- This option is only available when the network interface is in a subnet which is associated with a Wavelength Zone.
--
-- /Note:/ Consider using 'carrierIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaCarrierIp :: Lens.Lens' NetworkInterfaceAssociation (Core.Maybe Core.Text)
niaCarrierIp = Lens.field @"carrierIp"
{-# INLINEABLE niaCarrierIp #-}
{-# DEPRECATED carrierIp "Use generic-lens or generic-optics with 'carrierIp' instead"  #-}

-- | The customer-owned IP address associated with the network interface.
--
-- /Note:/ Consider using 'customerOwnedIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaCustomerOwnedIp :: Lens.Lens' NetworkInterfaceAssociation (Core.Maybe Core.Text)
niaCustomerOwnedIp = Lens.field @"customerOwnedIp"
{-# INLINEABLE niaCustomerOwnedIp #-}
{-# DEPRECATED customerOwnedIp "Use generic-lens or generic-optics with 'customerOwnedIp' instead"  #-}

-- | The ID of the Elastic IP address owner.
--
-- /Note:/ Consider using 'ipOwnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaIpOwnerId :: Lens.Lens' NetworkInterfaceAssociation (Core.Maybe Core.Text)
niaIpOwnerId = Lens.field @"ipOwnerId"
{-# INLINEABLE niaIpOwnerId #-}
{-# DEPRECATED ipOwnerId "Use generic-lens or generic-optics with 'ipOwnerId' instead"  #-}

-- | The public DNS name.
--
-- /Note:/ Consider using 'publicDnsName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaPublicDnsName :: Lens.Lens' NetworkInterfaceAssociation (Core.Maybe Core.Text)
niaPublicDnsName = Lens.field @"publicDnsName"
{-# INLINEABLE niaPublicDnsName #-}
{-# DEPRECATED publicDnsName "Use generic-lens or generic-optics with 'publicDnsName' instead"  #-}

-- | The address of the Elastic IP address bound to the network interface.
--
-- /Note:/ Consider using 'publicIp' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
niaPublicIp :: Lens.Lens' NetworkInterfaceAssociation (Core.Maybe Core.Text)
niaPublicIp = Lens.field @"publicIp"
{-# INLINEABLE niaPublicIp #-}
{-# DEPRECATED publicIp "Use generic-lens or generic-optics with 'publicIp' instead"  #-}

instance Core.FromXML NetworkInterfaceAssociation where
        parseXML x
          = NetworkInterfaceAssociation' Core.<$>
              (x Core..@? "allocationId") Core.<*> x Core..@? "associationId"
                Core.<*> x Core..@? "carrierIp"
                Core.<*> x Core..@? "customerOwnedIp"
                Core.<*> x Core..@? "ipOwnerId"
                Core.<*> x Core..@? "publicDnsName"
                Core.<*> x Core..@? "publicIp"
