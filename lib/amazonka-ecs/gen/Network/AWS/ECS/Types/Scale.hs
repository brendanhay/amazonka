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
    sValue,
    sUnit,
  )
where

import Network.AWS.ECS.Types.ScaleUnit
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A floating-point percentage of the desired number of tasks to place and keep running in the task set.
--
-- /See:/ 'mkScale' smart constructor.
data Scale = Scale'
  { value :: Lude.Maybe Lude.Double,
    unit :: Lude.Maybe ScaleUnit
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Scale' with the minimum fields required to make a request.
--
-- * 'unit' - The unit of measure for the scale value.
-- * 'value' - The value, specified as a percent total of a service's @desiredCount@ , to scale the task set. Accepted values are numbers between 0 and 100.
mkScale ::
  Scale
mkScale = Scale' {value = Lude.Nothing, unit = Lude.Nothing}

-- | The value, specified as a percent total of a service's @desiredCount@ , to scale the task set. Accepted values are numbers between 0 and 100.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sValue :: Lens.Lens' Scale (Lude.Maybe Lude.Double)
sValue = Lens.lens (value :: Scale -> Lude.Maybe Lude.Double) (\s a -> s {value = a} :: Scale)
{-# DEPRECATED sValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The unit of measure for the scale value.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
sUnit :: Lens.Lens' Scale (Lude.Maybe ScaleUnit)
sUnit = Lens.lens (unit :: Scale -> Lude.Maybe ScaleUnit) (\s a -> s {unit = a} :: Scale)
{-# DEPRECATED sUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Lude.FromJSON Scale where
  parseJSON =
    Lude.withObject
      "Scale"
      ( \x ->
          Scale' Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..:? "unit")
      )

instance Lude.ToJSON Scale where
  toJSON Scale' {..} =
    Lude.object
      ( Lude.catMaybes
          [("value" Lude..=) Lude.<$> value, ("unit" Lude..=) Lude.<$> unit]
      )
