{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.AvailabilityZone
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.ELBv2.Types.AvailabilityZone
  ( AvailabilityZone (..)
  -- * Smart constructor
  , mkAvailabilityZone
  -- * Lenses
  , azLoadBalancerAddresses
  , azOutpostId
  , azSubnetId
  , azZoneName
  ) where

import qualified Network.AWS.ELBv2.Types.LoadBalancerAddress as Types
import qualified Network.AWS.ELBv2.Types.OutpostId as Types
import qualified Network.AWS.ELBv2.Types.SubnetId as Types
import qualified Network.AWS.ELBv2.Types.ZoneName as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about an Availability Zone.
--
-- /See:/ 'mkAvailabilityZone' smart constructor.
data AvailabilityZone = AvailabilityZone'
  { loadBalancerAddresses :: Core.Maybe [Types.LoadBalancerAddress]
    -- ^ [Network Load Balancers] If you need static IP addresses for your load balancer, you can specify one Elastic IP address per Availability Zone when you create an internal-facing load balancer. For internal load balancers, you can specify a private IP address from the IPv4 range of the subnet.
  , outpostId :: Core.Maybe Types.OutpostId
    -- ^ [Application Load Balancers on Outposts] The ID of the Outpost.
  , subnetId :: Core.Maybe Types.SubnetId
    -- ^ The ID of the subnet. You can specify one subnet per Availability Zone.
  , zoneName :: Core.Maybe Types.ZoneName
    -- ^ The name of the Availability Zone.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AvailabilityZone' value with any optional fields omitted.
mkAvailabilityZone
    :: AvailabilityZone
mkAvailabilityZone
  = AvailabilityZone'{loadBalancerAddresses = Core.Nothing,
                      outpostId = Core.Nothing, subnetId = Core.Nothing,
                      zoneName = Core.Nothing}

-- | [Network Load Balancers] If you need static IP addresses for your load balancer, you can specify one Elastic IP address per Availability Zone when you create an internal-facing load balancer. For internal load balancers, you can specify a private IP address from the IPv4 range of the subnet.
--
-- /Note:/ Consider using 'loadBalancerAddresses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azLoadBalancerAddresses :: Lens.Lens' AvailabilityZone (Core.Maybe [Types.LoadBalancerAddress])
azLoadBalancerAddresses = Lens.field @"loadBalancerAddresses"
{-# INLINEABLE azLoadBalancerAddresses #-}
{-# DEPRECATED loadBalancerAddresses "Use generic-lens or generic-optics with 'loadBalancerAddresses' instead"  #-}

-- | [Application Load Balancers on Outposts] The ID of the Outpost.
--
-- /Note:/ Consider using 'outpostId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azOutpostId :: Lens.Lens' AvailabilityZone (Core.Maybe Types.OutpostId)
azOutpostId = Lens.field @"outpostId"
{-# INLINEABLE azOutpostId #-}
{-# DEPRECATED outpostId "Use generic-lens or generic-optics with 'outpostId' instead"  #-}

-- | The ID of the subnet. You can specify one subnet per Availability Zone.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azSubnetId :: Lens.Lens' AvailabilityZone (Core.Maybe Types.SubnetId)
azSubnetId = Lens.field @"subnetId"
{-# INLINEABLE azSubnetId #-}
{-# DEPRECATED subnetId "Use generic-lens or generic-optics with 'subnetId' instead"  #-}

-- | The name of the Availability Zone.
--
-- /Note:/ Consider using 'zoneName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
azZoneName :: Lens.Lens' AvailabilityZone (Core.Maybe Types.ZoneName)
azZoneName = Lens.field @"zoneName"
{-# INLINEABLE azZoneName #-}
{-# DEPRECATED zoneName "Use generic-lens or generic-optics with 'zoneName' instead"  #-}

instance Core.FromXML AvailabilityZone where
        parseXML x
          = AvailabilityZone' Core.<$>
              (x Core..@? "LoadBalancerAddresses" Core..<@>
                 Core.parseXMLList "member")
                Core.<*> x Core..@? "OutpostId"
                Core.<*> x Core..@? "SubnetId"
                Core.<*> x Core..@? "ZoneName"
