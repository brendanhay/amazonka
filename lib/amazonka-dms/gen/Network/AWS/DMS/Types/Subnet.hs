{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Subnet
  ( Subnet (..)
  -- * Smart constructor
  , mkSubnet
  -- * Lenses
  , sSubnetAvailabilityZone
  , sSubnetIdentifier
  , sSubnetStatus
  ) where

import qualified Network.AWS.DMS.Types.AvailabilityZone as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | In response to a request by the @DescribeReplicationSubnetGroups@ operation, this object identifies a subnet by its given Availability Zone, subnet identifier, and status.
--
-- /See:/ 'mkSubnet' smart constructor.
data Subnet = Subnet'
  { subnetAvailabilityZone :: Core.Maybe Types.AvailabilityZone
    -- ^ The Availability Zone of the subnet.
  , subnetIdentifier :: Core.Maybe Core.Text
    -- ^ The subnet identifier.
  , subnetStatus :: Core.Maybe Core.Text
    -- ^ The status of the subnet.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Subnet' value with any optional fields omitted.
mkSubnet
    :: Subnet
mkSubnet
  = Subnet'{subnetAvailabilityZone = Core.Nothing,
            subnetIdentifier = Core.Nothing, subnetStatus = Core.Nothing}

-- | The Availability Zone of the subnet.
--
-- /Note:/ Consider using 'subnetAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetAvailabilityZone :: Lens.Lens' Subnet (Core.Maybe Types.AvailabilityZone)
sSubnetAvailabilityZone = Lens.field @"subnetAvailabilityZone"
{-# INLINEABLE sSubnetAvailabilityZone #-}
{-# DEPRECATED subnetAvailabilityZone "Use generic-lens or generic-optics with 'subnetAvailabilityZone' instead"  #-}

-- | The subnet identifier.
--
-- /Note:/ Consider using 'subnetIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetIdentifier :: Lens.Lens' Subnet (Core.Maybe Core.Text)
sSubnetIdentifier = Lens.field @"subnetIdentifier"
{-# INLINEABLE sSubnetIdentifier #-}
{-# DEPRECATED subnetIdentifier "Use generic-lens or generic-optics with 'subnetIdentifier' instead"  #-}

-- | The status of the subnet.
--
-- /Note:/ Consider using 'subnetStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetStatus :: Lens.Lens' Subnet (Core.Maybe Core.Text)
sSubnetStatus = Lens.field @"subnetStatus"
{-# INLINEABLE sSubnetStatus #-}
{-# DEPRECATED subnetStatus "Use generic-lens or generic-optics with 'subnetStatus' instead"  #-}

instance Core.FromJSON Subnet where
        parseJSON
          = Core.withObject "Subnet" Core.$
              \ x ->
                Subnet' Core.<$>
                  (x Core..:? "SubnetAvailabilityZone") Core.<*>
                    x Core..:? "SubnetIdentifier"
                    Core.<*> x Core..:? "SubnetStatus"
