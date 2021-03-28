{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.PlacementType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.EMR.Types.PlacementType
  ( PlacementType (..)
  -- * Smart constructor
  , mkPlacementType
  -- * Lenses
  , ptAvailabilityZone
  , ptAvailabilityZones
  ) where

import qualified Network.AWS.EMR.Types.XmlString as Types
import qualified Network.AWS.EMR.Types.XmlStringMaxLen256 as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The Amazon EC2 Availability Zone configuration of the cluster (job flow).
--
-- /See:/ 'mkPlacementType' smart constructor.
data PlacementType = PlacementType'
  { availabilityZone :: Core.Maybe Types.XmlString
    -- ^ The Amazon EC2 Availability Zone for the cluster. @AvailabilityZone@ is used for uniform instance groups, while @AvailabilityZones@ (plural) is used for instance fleets.
  , availabilityZones :: Core.Maybe [Types.XmlStringMaxLen256]
    -- ^ When multiple Availability Zones are specified, Amazon EMR evaluates them and launches instances in the optimal Availability Zone. @AvailabilityZones@ is used for instance fleets, while @AvailabilityZone@ (singular) is used for uniform instance groups.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PlacementType' value with any optional fields omitted.
mkPlacementType
    :: PlacementType
mkPlacementType
  = PlacementType'{availabilityZone = Core.Nothing,
                   availabilityZones = Core.Nothing}

-- | The Amazon EC2 Availability Zone for the cluster. @AvailabilityZone@ is used for uniform instance groups, while @AvailabilityZones@ (plural) is used for instance fleets.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptAvailabilityZone :: Lens.Lens' PlacementType (Core.Maybe Types.XmlString)
ptAvailabilityZone = Lens.field @"availabilityZone"
{-# INLINEABLE ptAvailabilityZone #-}
{-# DEPRECATED availabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead"  #-}

-- | When multiple Availability Zones are specified, Amazon EMR evaluates them and launches instances in the optimal Availability Zone. @AvailabilityZones@ is used for instance fleets, while @AvailabilityZone@ (singular) is used for uniform instance groups.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptAvailabilityZones :: Lens.Lens' PlacementType (Core.Maybe [Types.XmlStringMaxLen256])
ptAvailabilityZones = Lens.field @"availabilityZones"
{-# INLINEABLE ptAvailabilityZones #-}
{-# DEPRECATED availabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead"  #-}

instance Core.FromJSON PlacementType where
        toJSON PlacementType{..}
          = Core.object
              (Core.catMaybes
                 [("AvailabilityZone" Core..=) Core.<$> availabilityZone,
                  ("AvailabilityZones" Core..=) Core.<$> availabilityZones])
