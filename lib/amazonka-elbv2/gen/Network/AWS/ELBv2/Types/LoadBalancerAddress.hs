{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.LoadBalancerAddress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.LoadBalancerAddress
  ( LoadBalancerAddress (..)
  -- * Smart constructor
  , mkLoadBalancerAddress
  -- * Lenses
  , lbaAllocationId
  , lbaIPv6Address
  , lbaIpAddress
  , lbaPrivateIPv4Address
  ) where

import qualified Network.AWS.ELBv2.Types.AllocationId as Types
import qualified Network.AWS.ELBv2.Types.IPv6Address as Types
import qualified Network.AWS.ELBv2.Types.IpAddress as Types
import qualified Network.AWS.ELBv2.Types.PrivateIPv4Address as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a static IP address for a load balancer.
--
-- /See:/ 'mkLoadBalancerAddress' smart constructor.
data LoadBalancerAddress = LoadBalancerAddress'
  { allocationId :: Core.Maybe Types.AllocationId
    -- ^ [Network Load Balancers] The allocation ID of the Elastic IP address for an internal-facing load balancer.
  , iPv6Address :: Core.Maybe Types.IPv6Address
    -- ^ [Network Load Balancers] The IPv6 address.
  , ipAddress :: Core.Maybe Types.IpAddress
    -- ^ The static IP address.
  , privateIPv4Address :: Core.Maybe Types.PrivateIPv4Address
    -- ^ [Network Load Balancers] The private IPv4 address for an internal load balancer.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LoadBalancerAddress' value with any optional fields omitted.
mkLoadBalancerAddress
    :: LoadBalancerAddress
mkLoadBalancerAddress
  = LoadBalancerAddress'{allocationId = Core.Nothing,
                         iPv6Address = Core.Nothing, ipAddress = Core.Nothing,
                         privateIPv4Address = Core.Nothing}

-- | [Network Load Balancers] The allocation ID of the Elastic IP address for an internal-facing load balancer.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaAllocationId :: Lens.Lens' LoadBalancerAddress (Core.Maybe Types.AllocationId)
lbaAllocationId = Lens.field @"allocationId"
{-# INLINEABLE lbaAllocationId #-}
{-# DEPRECATED allocationId "Use generic-lens or generic-optics with 'allocationId' instead"  #-}

-- | [Network Load Balancers] The IPv6 address.
--
-- /Note:/ Consider using 'iPv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaIPv6Address :: Lens.Lens' LoadBalancerAddress (Core.Maybe Types.IPv6Address)
lbaIPv6Address = Lens.field @"iPv6Address"
{-# INLINEABLE lbaIPv6Address #-}
{-# DEPRECATED iPv6Address "Use generic-lens or generic-optics with 'iPv6Address' instead"  #-}

-- | The static IP address.
--
-- /Note:/ Consider using 'ipAddress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaIpAddress :: Lens.Lens' LoadBalancerAddress (Core.Maybe Types.IpAddress)
lbaIpAddress = Lens.field @"ipAddress"
{-# INLINEABLE lbaIpAddress #-}
{-# DEPRECATED ipAddress "Use generic-lens or generic-optics with 'ipAddress' instead"  #-}

-- | [Network Load Balancers] The private IPv4 address for an internal load balancer.
--
-- /Note:/ Consider using 'privateIPv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaPrivateIPv4Address :: Lens.Lens' LoadBalancerAddress (Core.Maybe Types.PrivateIPv4Address)
lbaPrivateIPv4Address = Lens.field @"privateIPv4Address"
{-# INLINEABLE lbaPrivateIPv4Address #-}
{-# DEPRECATED privateIPv4Address "Use generic-lens or generic-optics with 'privateIPv4Address' instead"  #-}

instance Core.FromXML LoadBalancerAddress where
        parseXML x
          = LoadBalancerAddress' Core.<$>
              (x Core..@? "AllocationId") Core.<*> x Core..@? "IPv6Address"
                Core.<*> x Core..@? "IpAddress"
                Core.<*> x Core..@? "PrivateIPv4Address"
