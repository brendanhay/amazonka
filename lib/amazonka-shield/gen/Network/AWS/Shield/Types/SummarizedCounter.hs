{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Shield.Types.SummarizedCounter
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Shield.Types.SummarizedCounter
  ( SummarizedCounter (..),

    -- * Smart constructor
    mkSummarizedCounter,

    -- * Lenses
    scMax,
    scAverage,
    scN,
    scName,
    scSum,
    scUnit,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The counter that describes a DDoS attack.
--
-- /See:/ 'mkSummarizedCounter' smart constructor.
data SummarizedCounter = SummarizedCounter'
  { max ::
      Lude.Maybe Lude.Double,
    average :: Lude.Maybe Lude.Double,
    n :: Lude.Maybe Lude.Int,
    name :: Lude.Maybe Lude.Text,
    sum :: Lude.Maybe Lude.Double,
    unit :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SummarizedCounter' with the minimum fields required to make a request.
--
-- * 'average' - The average value of the counter for a specified time period.
-- * 'max' - The maximum value of the counter for a specified time period.
-- * 'n' - The number of counters for a specified time period.
-- * 'name' - The counter name.
-- * 'sum' - The total of counter values for a specified time period.
-- * 'unit' - The unit of the counters.
mkSummarizedCounter ::
  SummarizedCounter
mkSummarizedCounter =
  SummarizedCounter'
    { max = Lude.Nothing,
      average = Lude.Nothing,
      n = Lude.Nothing,
      name = Lude.Nothing,
      sum = Lude.Nothing,
      unit = Lude.Nothing
    }

-- | The maximum value of the counter for a specified time period.
--
-- /Note:/ Consider using 'max' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scMax :: Lens.Lens' SummarizedCounter (Lude.Maybe Lude.Double)
scMax = Lens.lens (max :: SummarizedCounter -> Lude.Maybe Lude.Double) (\s a -> s {max = a} :: SummarizedCounter)
{-# DEPRECATED scMax "Use generic-lens or generic-optics with 'max' instead." #-}

-- | The average value of the counter for a specified time period.
--
-- /Note:/ Consider using 'average' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scAverage :: Lens.Lens' SummarizedCounter (Lude.Maybe Lude.Double)
scAverage = Lens.lens (average :: SummarizedCounter -> Lude.Maybe Lude.Double) (\s a -> s {average = a} :: SummarizedCounter)
{-# DEPRECATED scAverage "Use generic-lens or generic-optics with 'average' instead." #-}

-- | The number of counters for a specified time period.
--
-- /Note:/ Consider using 'n' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scN :: Lens.Lens' SummarizedCounter (Lude.Maybe Lude.Int)
scN = Lens.lens (n :: SummarizedCounter -> Lude.Maybe Lude.Int) (\s a -> s {n = a} :: SummarizedCounter)
{-# DEPRECATED scN "Use generic-lens or generic-optics with 'n' instead." #-}

-- | The counter name.
--
-- /Note:/ Consider using 'name' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scName :: Lens.Lens' SummarizedCounter (Lude.Maybe Lude.Text)
scName = Lens.lens (name :: SummarizedCounter -> Lude.Maybe Lude.Text) (\s a -> s {name = a} :: SummarizedCounter)
{-# DEPRECATED scName "Use generic-lens or generic-optics with 'name' instead." #-}

-- | The total of counter values for a specified time period.
--
-- /Note:/ Consider using 'sum' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scSum :: Lens.Lens' SummarizedCounter (Lude.Maybe Lude.Double)
scSum = Lens.lens (sum :: SummarizedCounter -> Lude.Maybe Lude.Double) (\s a -> s {sum = a} :: SummarizedCounter)
{-# DEPRECATED scSum "Use generic-lens or generic-optics with 'sum' instead." #-}

-- | The unit of the counters.
--
-- /Note:/ Consider using 'unit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scUnit :: Lens.Lens' SummarizedCounter (Lude.Maybe Lude.Text)
scUnit = Lens.lens (unit :: SummarizedCounter -> Lude.Maybe Lude.Text) (\s a -> s {unit = a} :: SummarizedCounter)
{-# DEPRECATED scUnit "Use generic-lens or generic-optics with 'unit' instead." #-}

instance Lude.FromJSON SummarizedCounter where
  parseJSON =
    Lude.withObject
      "SummarizedCounter"
      ( \x ->
          SummarizedCounter'
            Lude.<$> (x Lude..:? "Max")
            Lude.<*> (x Lude..:? "Average")
            Lude.<*> (x Lude..:? "N")
            Lude.<*> (x Lude..:? "Name")
            Lude.<*> (x Lude..:? "Sum")
            Lude.<*> (x Lude..:? "Unit")
      )
