{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Redshift.Types.Subnet
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Redshift.Types.Subnet
  ( Subnet (..)
  -- * Smart constructor
  , mkSubnet
  -- * Lenses
  , sSubnetAvailabilityZone
  , sSubnetIdentifier
  , sSubnetStatus
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Redshift.Internal as Types
import qualified Network.AWS.Redshift.Types.AvailabilityZone as Types

-- | Describes a subnet.
--
-- /See:/ 'mkSubnet' smart constructor.
data Subnet = Subnet'
  { subnetAvailabilityZone :: Core.Maybe Types.AvailabilityZone
    -- ^ 
  , subnetIdentifier :: Core.Maybe Core.Text
    -- ^ The identifier of the subnet.
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

-- | 
--
-- /Note:/ Consider using 'subnetAvailabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sSubnetAvailabilityZone :: Lens.Lens' Subnet (Core.Maybe Types.AvailabilityZone)
sSubnetAvailabilityZone = Lens.field @"subnetAvailabilityZone"
{-# INLINEABLE sSubnetAvailabilityZone #-}
{-# DEPRECATED subnetAvailabilityZone "Use generic-lens or generic-optics with 'subnetAvailabilityZone' instead"  #-}

-- | The identifier of the subnet.
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

instance Core.FromXML Subnet where
        parseXML x
          = Subnet' Core.<$>
              (x Core..@? "SubnetAvailabilityZone") Core.<*>
                x Core..@? "SubnetIdentifier"
                Core.<*> x Core..@? "SubnetStatus"
