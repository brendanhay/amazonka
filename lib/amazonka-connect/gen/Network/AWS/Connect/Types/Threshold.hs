{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Connect.Types.Threshold
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Connect.Types.Threshold
  ( Threshold (..),

    -- * Smart constructor
    mkThreshold,

    -- * Lenses
    tThresholdValue,
    tComparison,
  )
where

import Network.AWS.Connect.Types.Comparison
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the threshold for service level metrics.
--
-- /See:/ 'mkThreshold' smart constructor.
data Threshold = Threshold'
  { -- | The threshold value to compare.
    thresholdValue :: Lude.Maybe Lude.Double,
    -- | The type of comparison. Only "less than" (LT) comparisons are supported.
    comparison :: Lude.Maybe Comparison
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Threshold' with the minimum fields required to make a request.
--
-- * 'thresholdValue' - The threshold value to compare.
-- * 'comparison' - The type of comparison. Only "less than" (LT) comparisons are supported.
mkThreshold ::
  Threshold
mkThreshold =
  Threshold'
    { thresholdValue = Lude.Nothing,
      comparison = Lude.Nothing
    }

-- | The threshold value to compare.
--
-- /Note:/ Consider using 'thresholdValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tThresholdValue :: Lens.Lens' Threshold (Lude.Maybe Lude.Double)
tThresholdValue = Lens.lens (thresholdValue :: Threshold -> Lude.Maybe Lude.Double) (\s a -> s {thresholdValue = a} :: Threshold)
{-# DEPRECATED tThresholdValue "Use generic-lens or generic-optics with 'thresholdValue' instead." #-}

-- | The type of comparison. Only "less than" (LT) comparisons are supported.
--
-- /Note:/ Consider using 'comparison' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tComparison :: Lens.Lens' Threshold (Lude.Maybe Comparison)
tComparison = Lens.lens (comparison :: Threshold -> Lude.Maybe Comparison) (\s a -> s {comparison = a} :: Threshold)
{-# DEPRECATED tComparison "Use generic-lens or generic-optics with 'comparison' instead." #-}

instance Lude.FromJSON Threshold where
  parseJSON =
    Lude.withObject
      "Threshold"
      ( \x ->
          Threshold'
            Lude.<$> (x Lude..:? "ThresholdValue") Lude.<*> (x Lude..:? "Comparison")
      )

instance Lude.ToJSON Threshold where
  toJSON Threshold' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ThresholdValue" Lude..=) Lude.<$> thresholdValue,
            ("Comparison" Lude..=) Lude.<$> comparison
          ]
      )
