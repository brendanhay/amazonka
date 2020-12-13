{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.PercentPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.PercentPair
  ( PercentPair (..),

    -- * Smart constructor
    mkPercentPair,

    -- * Lenses
    ppValue,
    ppPercent,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the percentile and percentile value.
--
-- /See:/ 'mkPercentPair' smart constructor.
data PercentPair = PercentPair'
  { -- | The value of the percentile.
    value :: Lude.Maybe Lude.Double,
    -- | The percentile.
    percent :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PercentPair' with the minimum fields required to make a request.
--
-- * 'value' - The value of the percentile.
-- * 'percent' - The percentile.
mkPercentPair ::
  PercentPair
mkPercentPair =
  PercentPair' {value = Lude.Nothing, percent = Lude.Nothing}

-- | The value of the percentile.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppValue :: Lens.Lens' PercentPair (Lude.Maybe Lude.Double)
ppValue = Lens.lens (value :: PercentPair -> Lude.Maybe Lude.Double) (\s a -> s {value = a} :: PercentPair)
{-# DEPRECATED ppValue "Use generic-lens or generic-optics with 'value' instead." #-}

-- | The percentile.
--
-- /Note:/ Consider using 'percent' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPercent :: Lens.Lens' PercentPair (Lude.Maybe Lude.Double)
ppPercent = Lens.lens (percent :: PercentPair -> Lude.Maybe Lude.Double) (\s a -> s {percent = a} :: PercentPair)
{-# DEPRECATED ppPercent "Use generic-lens or generic-optics with 'percent' instead." #-}

instance Lude.FromJSON PercentPair where
  parseJSON =
    Lude.withObject
      "PercentPair"
      ( \x ->
          PercentPair'
            Lude.<$> (x Lude..:? "value") Lude.<*> (x Lude..:? "percent")
      )
