{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.Types.Scale
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.ECS.Types.Scale
  ( Scale (..),

    -- * Smart constructor
    mkScale,

    -- * Lenses
    sUnit,
    sValue,
  )
where

import qualified Network.AWS.ECS.Types.ScaleUnit as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | A floating-point percentage of the desired number of tasks to place and keep running in the task set.
--
-- /See:/ 'mkScale' smart constructor.
data Scale = Scale'
  { -- | The unit of measure for the scale value.
    unit :: Core.Maybe Types.ScaleUnit,
    -- | The value, specified as a percent total of a service's @desiredCount@ , to scale the task set. Accepted values are numbers between 0 and 100.
    value :: Core.Maybe Core.Double
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Scale' value with any optional fields omitted.
mkScale ::
  Scale
mkScale = Scale' {unit = Core.Nothing, value = Core.Nothing}

-- | The unit of measure for the scale value.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUnit :: Lens.Lens' Scale (Core.Maybe Types.ScaleUnit)
sUnit = Lens.field @"unit"
{-# DEPRECATED sUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

-- | The value, specified as a percent total of a service's @desiredCount@ , to scale the task set. Accepted values are numbers between 0 and 100.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValue :: Lens.Lens' Scale (Core.Maybe Core.Double)
sValue = Lens.field @"value"
{-# DEPRECATED sValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Core.FromJSON Scale where
  toJSON Scale {..} =
    Core.object
      ( Core.catMaybes
          [("unit" Core..=) Core.<$> unit, ("value" Core..=) Core.<$> value]
      )

instance Core.FromJSON Scale where
  parseJSON =
    Core.withObject "Scale" Core.$
      \x ->
        Scale' Core.<$> (x Core..:? "unit") Core.<*> (x Core..:? "value")
