-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.Types.PlacementType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.EMR.Types.PlacementType
  ( PlacementType (..),

    -- * Smart constructor
    mkPlacementType,

    -- * Lenses
    ptAvailabilityZones,
    ptAvailabilityZone,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The Amazon EC2 Availability Zone configuration of the cluster (job flow).
--
-- /See:/ 'mkPlacementType' smart constructor.
data PlacementType = PlacementType'
  { availabilityZones ::
      Lude.Maybe [Lude.Text],
    availabilityZone :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PlacementType' with the minimum fields required to make a request.
--
-- * 'availabilityZone' - The Amazon EC2 Availability Zone for the cluster. @AvailabilityZone@ is used for uniform instance groups, while @AvailabilityZones@ (plural) is used for instance fleets.
-- * 'availabilityZones' - When multiple Availability Zones are specified, Amazon EMR evaluates them and launches instances in the optimal Availability Zone. @AvailabilityZones@ is used for instance fleets, while @AvailabilityZone@ (singular) is used for uniform instance groups.
mkPlacementType ::
  PlacementType
mkPlacementType =
  PlacementType'
    { availabilityZones = Lude.Nothing,
      availabilityZone = Lude.Nothing
    }

-- | When multiple Availability Zones are specified, Amazon EMR evaluates them and launches instances in the optimal Availability Zone. @AvailabilityZones@ is used for instance fleets, while @AvailabilityZone@ (singular) is used for uniform instance groups.
--
-- /Note:/ Consider using 'availabilityZones' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptAvailabilityZones :: Lens.Lens' PlacementType (Lude.Maybe [Lude.Text])
ptAvailabilityZones = Lens.lens (availabilityZones :: PlacementType -> Lude.Maybe [Lude.Text]) (\s a -> s {availabilityZones = a} :: PlacementType)
{-# DEPRECATED ptAvailabilityZones "Use generic-lens or generic-optics with 'availabilityZones' instead." #-}

-- | The Amazon EC2 Availability Zone for the cluster. @AvailabilityZone@ is used for uniform instance groups, while @AvailabilityZones@ (plural) is used for instance fleets.
--
-- /Note:/ Consider using 'availabilityZone' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ptAvailabilityZone :: Lens.Lens' PlacementType (Lude.Maybe Lude.Text)
ptAvailabilityZone = Lens.lens (availabilityZone :: PlacementType -> Lude.Maybe Lude.Text) (\s a -> s {availabilityZone = a} :: PlacementType)
{-# DEPRECATED ptAvailabilityZone "Use generic-lens or generic-optics with 'availabilityZone' instead." #-}

instance Lude.ToJSON PlacementType where
  toJSON PlacementType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("AvailabilityZones" Lude..=) Lude.<$> availabilityZones,
            ("AvailabilityZone" Lude..=) Lude.<$> availabilityZone
          ]
      )
