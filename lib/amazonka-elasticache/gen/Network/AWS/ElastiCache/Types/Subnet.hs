{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ElastiCache.Types.Subnet
  ( Subnet (..),

    -- * Smart constructor
    mkSubnet,

    -- * Lenses
    sSubnetAvailabilityZone,
    sSubnetIdentifier,
    sSubnetOutpost,
  )
where

import qualified Network.AWS.ElastiCache.Types.AvailabilityZone as Types
import qualified Network.AWS.ElastiCache.Types.String as Types
import qualified Network.AWS.ElastiCache.Types.SubnetOutpost as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the subnet associated with a cluster. This parameter refers to subnets defined in Amazon Virtual Private Cloud (Amazon VPC) and used with ElastiCache.
--
-- /See:/ 'mkSubnet' smart constructor.
data Subnet = Subnet'
  { -- | The Availability Zone associated with the subnet.
    subnetAvailabilityZone :: Core.Maybe Types.AvailabilityZone,
    -- | The unique identifier for the subnet.
    subnetIdentifier :: Core.Maybe Types.String,
    -- | The outpost ARN of the subnet.
    subnetOutpost :: Core.Maybe Types.SubnetOutpost
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subnet' value with any optional fields omitted.
mkSubnet ::
  Subnet
mkSubnet =
  Subnet'
    { subnetAvailabilityZone = Core.Nothing,
      subnetIdentifier = Core.Nothing,
      subnetOutpost = Core.Nothing
    }

-- | The Availability Zone associated with the subnet.
--
-- /Note:/ Consider using 'subnetAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetAvailabilityZone :: Lens.Lens' Subnet (Core.Maybe Types.AvailabilityZone)
sSubnetAvailabilityZone = Lens.field @"subnetAvailabilityZone"
{-# DEPRECATED sSubnetAvailabilityZone "Use generic-lens or generic-optics with 'subnetAvailabilityZone' instead." #-}

-- | The unique identifier for the subnet.
--
-- /Note:/ Consider using 'subnetIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetIdentifier :: Lens.Lens' Subnet (Core.Maybe Types.String)
sSubnetIdentifier = Lens.field @"subnetIdentifier"
{-# DEPRECATED sSubnetIdentifier "Use generic-lens or generic-optics with 'subnetIdentifier' instead." #-}

-- | The outpost ARN of the subnet.
--
-- /Note:/ Consider using 'subnetOutpost' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetOutpost :: Lens.Lens' Subnet (Core.Maybe Types.SubnetOutpost)
sSubnetOutpost = Lens.field @"subnetOutpost"
{-# DEPRECATED sSubnetOutpost "Use generic-lens or generic-optics with 'subnetOutpost' instead." #-}

instance Core.FromXML Subnet where
  parseXML x =
    Subnet'
      Core.<$> (x Core..@? "SubnetAvailabilityZone")
      Core.<*> (x Core..@? "SubnetIdentifier")
      Core.<*> (x Core..@? "SubnetOutpost")
