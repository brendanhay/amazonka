{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.XRay.Types.HistogramEntry
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.XRay.Types.HistogramEntry
  ( HistogramEntry (..),

    -- * Smart constructor
    mkHistogramEntry,

    -- * Lenses
    heCount,
    heValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An entry in a histogram for a statistic. A histogram maps the range of observed values on the X axis, and the prevalence of each value on the Y axis.
--
-- /See:/ 'mkHistogramEntry' smart constructor.
data HistogramEntry = HistogramEntry'
  { count :: Lude.Maybe Lude.Int,
    value :: Lude.Maybe Lude.Double
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'HistogramEntry' with the minimum fields required to make a request.
--
-- * 'count' - The prevalence of the entry.
-- * 'value' - The value of the entry.
mkHistogramEntry ::
  HistogramEntry
mkHistogramEntry =
  HistogramEntry' {count = Lude.Nothing, value = Lude.Nothing}

-- | The prevalence of the entry.
--
-- /Note:/ Consider using 'count' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heCount :: Lens.Lens' HistogramEntry (Lude.Maybe Lude.Int)
heCount = Lens.lens (count :: HistogramEntry -> Lude.Maybe Lude.Int) (\s a -> s {count = a} :: HistogramEntry)
{-# DEPRECATED heCount "Use generic-lens or generic-optics with 'count' instead." #-}

-- | The value of the entry.
--
-- /Note:/ Consider using 'value' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
heValue :: Lens.Lens' HistogramEntry (Lude.Maybe Lude.Double)
heValue = Lens.lens (value :: HistogramEntry -> Lude.Maybe Lude.Double) (\s a -> s {value = a} :: HistogramEntry)
{-# DEPRECATED heValue "Use generic-lens or generic-optics with 'value' instead." #-}

instance Lude.FromJSON HistogramEntry where
  parseJSON =
    Lude.withObject
      "HistogramEntry"
      ( \x ->
          HistogramEntry'
            Lude.<$> (x Lude..:? "Count") Lude.<*> (x Lude..:? "Value")
      )
