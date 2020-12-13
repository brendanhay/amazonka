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
    dcsdfNumberOfDistinctValues,
    dcsdfMaximumValue,
    dcsdfNumberOfNulls,
    dcsdfMinimumValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for floating-point number data columns.
--
-- /See:/ 'mkDoubleColumnStatisticsData' smart constructor.
data DoubleColumnStatisticsData = DoubleColumnStatisticsData'
  { -- | The number of distinct values in a column.
    numberOfDistinctValues :: Lude.Natural,
    -- | The highest value in the column.
    maximumValue :: Lude.Maybe Lude.Double,
    -- | The number of null values in the column.
    numberOfNulls :: Lude.Natural,
    -- | The lowest value in the column.
    minimumValue :: Lude.Maybe Lude.Double
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DoubleColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'numberOfDistinctValues' - The number of distinct values in a column.
-- * 'maximumValue' - The highest value in the column.
-- * 'numberOfNulls' - The number of null values in the column.
-- * 'minimumValue' - The lowest value in the column.
mkDoubleColumnStatisticsData ::
  -- | 'numberOfDistinctValues'
  Lude.Natural ->
  -- | 'numberOfNulls'
  Lude.Natural ->
  DoubleColumnStatisticsData
mkDoubleColumnStatisticsData
  pNumberOfDistinctValues_
  pNumberOfNulls_ =
    DoubleColumnStatisticsData'
      { numberOfDistinctValues =
          pNumberOfDistinctValues_,
        maximumValue = Lude.Nothing,
        numberOfNulls = pNumberOfNulls_,
        minimumValue = Lude.Nothing
      }

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdfNumberOfDistinctValues :: Lens.Lens' DoubleColumnStatisticsData Lude.Natural
dcsdfNumberOfDistinctValues = Lens.lens (numberOfDistinctValues :: DoubleColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfDistinctValues = a} :: DoubleColumnStatisticsData)
{-# DEPRECATED dcsdfNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdfMaximumValue :: Lens.Lens' DoubleColumnStatisticsData (Lude.Maybe Lude.Double)
dcsdfMaximumValue = Lens.lens (maximumValue :: DoubleColumnStatisticsData -> Lude.Maybe Lude.Double) (\s a -> s {maximumValue = a} :: DoubleColumnStatisticsData)
{-# DEPRECATED dcsdfMaximumValue "Use generic-lens or generic-optics with 'maximumValue' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdfNumberOfNulls :: Lens.Lens' DoubleColumnStatisticsData Lude.Natural
dcsdfNumberOfNulls = Lens.lens (numberOfNulls :: DoubleColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: DoubleColumnStatisticsData)
{-# DEPRECATED dcsdfNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdfMinimumValue :: Lens.Lens' DoubleColumnStatisticsData (Lude.Maybe Lude.Double)
dcsdfMinimumValue = Lens.lens (minimumValue :: DoubleColumnStatisticsData -> Lude.Maybe Lude.Double) (\s a -> s {minimumValue = a} :: DoubleColumnStatisticsData)
{-# DEPRECATED dcsdfMinimumValue "Use generic-lens or generic-optics with 'minimumValue' instead." #-}

instance Lude.FromJSON DoubleColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "DoubleColumnStatisticsData"
      ( \x ->
          DoubleColumnStatisticsData'
            Lude.<$> (x Lude..: "NumberOfDistinctValues")
            Lude.<*> (x Lude..:? "MaximumValue")
            Lude.<*> (x Lude..: "NumberOfNulls")
            Lude.<*> (x Lude..:? "MinimumValue")
      )

instance Lude.ToJSON DoubleColumnStatisticsData where
  toJSON DoubleColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("NumberOfDistinctValues" Lude..= numberOfDistinctValues),
            ("MaximumValue" Lude..=) Lude.<$> maximumValue,
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls),
            ("MinimumValue" Lude..=) Lude.<$> minimumValue
          ]
      )
