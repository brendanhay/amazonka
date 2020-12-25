{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ELBv2.Types.SubnetMapping
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ELBv2.Types.SubnetMapping
  ( SubnetMapping (..),

    -- * Smart constructor
    mkSubnetMapping,

    -- * Lenses
    smAllocationId,
    smIPv6Address,
    smPrivateIPv4Address,
    smSubnetId,
  )
where

import qualified Network.AWS.ELBv2.Types.AllocationId as Types
import qualified Network.AWS.ELBv2.Types.IPv6Address as Types
import qualified Network.AWS.ELBv2.Types.PrivateIPv4Address as Types
import qualified Network.AWS.ELBv2.Types.SubnetId as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Information about a subnet mapping.
--
-- /See:/ 'mkSubnetMapping' smart constructor.
data SubnetMapping = SubnetMapping'
  { -- | [Network Load Balancers] The allocation ID of the Elastic IP address for an internet-facing load balancer.
    allocationId :: Core.Maybe Types.AllocationId,
    -- | [Network Load Balancers] The IPv6 address.
    iPv6Address :: Core.Maybe Types.IPv6Address,
    -- | [Network Load Balancers] The private IPv4 address for an internal load balancer.
    privateIPv4Address :: Core.Maybe Types.PrivateIPv4Address,
    -- | The ID of the subnet.
    subnetId :: Core.Maybe Types.SubnetId
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'SubnetMapping' value with any optional fields omitted.
mkSubnetMapping ::
  SubnetMapping
mkSubnetMapping =
  SubnetMapping'
    { allocationId = Core.Nothing,
      iPv6Address = Core.Nothing,
      privateIPv4Address = Core.Nothing,
      subnetId = Core.Nothing
    }

-- | [Network Load Balancers] The allocation ID of the Elastic IP address for an internet-facing load balancer.
--
-- /Note:/ Consider using 'allocationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smAllocationId :: Lens.Lens' SubnetMapping (Core.Maybe Types.AllocationId)
smAllocationId = Lens.field @"allocationId"
{-# DEPRECATED smAllocationId "Use generic-lens or generic-optics with 'allocationId' instead." #-}

-- | [Network Load Balancers] The IPv6 address.
--
-- /Note:/ Consider using 'iPv6Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smIPv6Address :: Lens.Lens' SubnetMapping (Core.Maybe Types.IPv6Address)
smIPv6Address = Lens.field @"iPv6Address"
{-# DEPRECATED smIPv6Address "Use generic-lens or generic-optics with 'iPv6Address' instead." #-}

-- | [Network Load Balancers] The private IPv4 address for an internal load balancer.
--
-- /Note:/ Consider using 'privateIPv4Address' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smPrivateIPv4Address :: Lens.Lens' SubnetMapping (Core.Maybe Types.PrivateIPv4Address)
smPrivateIPv4Address = Lens.field @"privateIPv4Address"
{-# DEPRECATED smPrivateIPv4Address "Use generic-lens or generic-optics with 'privateIPv4Address' instead." #-}

-- | The ID of the subnet.
--
-- /Note:/ Consider using 'subnetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
smSubnetId :: Lens.Lens' SubnetMapping (Core.Maybe Types.SubnetId)
smSubnetId = Lens.field @"subnetId"
{-# DEPRECATED smSubnetId "Use generic-lens or generic-optics with 'subnetId' instead." #-}
