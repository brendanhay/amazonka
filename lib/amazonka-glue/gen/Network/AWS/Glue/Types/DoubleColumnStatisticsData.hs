{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DoubleColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DoubleColumnStatisticsData
  ( DoubleColumnStatisticsData (..),

    -- * Smart constructor
    mkDoubleColumnStatisticsData,

    -- * Lenses
    douMaximumValue,
    douMinimumValue,
    douNumberOfNulls,
    douNumberOfDistinctValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for floating-point number data columns.
--
-- /See:/ 'mkDoubleColumnStatisticsData' smart constructor.
data DoubleColumnStatisticsData = DoubleColumnStatisticsData'
  { maximumValue ::
      Lude.Maybe Lude.Double,
    minimumValue ::
      Lude.Maybe Lude.Double,
    numberOfNulls :: Lude.Natural,
    numberOfDistinctValues ::
      Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DoubleColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'maximumValue' - The highest value in the column.
-- * 'minimumValue' - The lowest value in the column.
-- * 'numberOfDistinctValues' - The number of distinct values in a column.
-- * 'numberOfNulls' - The number of null values in the column.
mkDoubleColumnStatisticsData ::
  -- | 'numberOfNulls'
  Lude.Natural ->
  -- | 'numberOfDistinctValues'
  Lude.Natural ->
  DoubleColumnStatisticsData
mkDoubleColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    DoubleColumnStatisticsData'
      { maximumValue = Lude.Nothing,
        minimumValue = Lude.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues = pNumberOfDistinctValues_
      }

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
douMaximumValue :: Lens.Lens' DoubleColumnStatisticsData (Lude.Maybe Lude.Double)
douMaximumValue = Lens.lens (maximumValue :: DoubleColumnStatisticsData -> Lude.Maybe Lude.Double) (\s a -> s {maximumValue = a} :: DoubleColumnStatisticsData)
{-# DEPRECATED douMaximumValue "Use generic-lens or generic-optics with 'maximumValue' instead." #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
douMinimumValue :: Lens.Lens' DoubleColumnStatisticsData (Lude.Maybe Lude.Double)
douMinimumValue = Lens.lens (minimumValue :: DoubleColumnStatisticsData -> Lude.Maybe Lude.Double) (\s a -> s {minimumValue = a} :: DoubleColumnStatisticsData)
{-# DEPRECATED douMinimumValue "Use generic-lens or generic-optics with 'minimumValue' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
douNumberOfNulls :: Lens.Lens' DoubleColumnStatisticsData Lude.Natural
douNumberOfNulls = Lens.lens (numberOfNulls :: DoubleColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: DoubleColumnStatisticsData)
{-# DEPRECATED douNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
douNumberOfDistinctValues :: Lens.Lens' DoubleColumnStatisticsData Lude.Natural
douNumberOfDistinctValues = Lens.lens (numberOfDistinctValues :: DoubleColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfDistinctValues = a} :: DoubleColumnStatisticsData)
{-# DEPRECATED douNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

instance Lude.FromJSON DoubleColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "DoubleColumnStatisticsData"
      ( \x ->
          DoubleColumnStatisticsData'
            Lude.<$> (x Lude..:? "MaximumValue")
            Lude.<*> (x Lude..:? "MinimumValue")
            Lude.<*> (x Lude..: "NumberOfNulls")
            Lude.<*> (x Lude..: "NumberOfDistinctValues")
      )

instance Lude.ToJSON DoubleColumnStatisticsData where
  toJSON DoubleColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaximumValue" Lude..=) Lude.<$> maximumValue,
            ("MinimumValue" Lude..=) Lude.<$> minimumValue,
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls),
            Lude.Just
              ("NumberOfDistinctValues" Lude..= numberOfDistinctValues)
          ]
      )
