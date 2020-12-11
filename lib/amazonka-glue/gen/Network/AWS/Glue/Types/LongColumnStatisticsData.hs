-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LongColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.LongColumnStatisticsData
  ( LongColumnStatisticsData (..),

    -- * Smart constructor
    mkLongColumnStatisticsData,

    -- * Lenses
    lcsdMaximumValue,
    lcsdMinimumValue,
    lcsdNumberOfNulls,
    lcsdNumberOfDistinctValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for integer data columns.
--
-- /See:/ 'mkLongColumnStatisticsData' smart constructor.
data LongColumnStatisticsData = LongColumnStatisticsData'
  { maximumValue ::
      Lude.Maybe Lude.Integer,
    minimumValue :: Lude.Maybe Lude.Integer,
    numberOfNulls :: Lude.Natural,
    numberOfDistinctValues :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LongColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'maximumValue' - The highest value in the column.
-- * 'minimumValue' - The lowest value in the column.
-- * 'numberOfDistinctValues' - The number of distinct values in a column.
-- * 'numberOfNulls' - The number of null values in the column.
mkLongColumnStatisticsData ::
  -- | 'numberOfNulls'
  Lude.Natural ->
  -- | 'numberOfDistinctValues'
  Lude.Natural ->
  LongColumnStatisticsData
mkLongColumnStatisticsData pNumberOfNulls_ pNumberOfDistinctValues_ =
  LongColumnStatisticsData'
    { maximumValue = Lude.Nothing,
      minimumValue = Lude.Nothing,
      numberOfNulls = pNumberOfNulls_,
      numberOfDistinctValues = pNumberOfDistinctValues_
    }

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdMaximumValue :: Lens.Lens' LongColumnStatisticsData (Lude.Maybe Lude.Integer)
lcsdMaximumValue = Lens.lens (maximumValue :: LongColumnStatisticsData -> Lude.Maybe Lude.Integer) (\s a -> s {maximumValue = a} :: LongColumnStatisticsData)
{-# DEPRECATED lcsdMaximumValue "Use generic-lens or generic-optics with 'maximumValue' instead." #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdMinimumValue :: Lens.Lens' LongColumnStatisticsData (Lude.Maybe Lude.Integer)
lcsdMinimumValue = Lens.lens (minimumValue :: LongColumnStatisticsData -> Lude.Maybe Lude.Integer) (\s a -> s {minimumValue = a} :: LongColumnStatisticsData)
{-# DEPRECATED lcsdMinimumValue "Use generic-lens or generic-optics with 'minimumValue' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdNumberOfNulls :: Lens.Lens' LongColumnStatisticsData Lude.Natural
lcsdNumberOfNulls = Lens.lens (numberOfNulls :: LongColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: LongColumnStatisticsData)
{-# DEPRECATED lcsdNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdNumberOfDistinctValues :: Lens.Lens' LongColumnStatisticsData Lude.Natural
lcsdNumberOfDistinctValues = Lens.lens (numberOfDistinctValues :: LongColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfDistinctValues = a} :: LongColumnStatisticsData)
{-# DEPRECATED lcsdNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

instance Lude.FromJSON LongColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "LongColumnStatisticsData"
      ( \x ->
          LongColumnStatisticsData'
            Lude.<$> (x Lude..:? "MaximumValue")
            Lude.<*> (x Lude..:? "MinimumValue")
            Lude.<*> (x Lude..: "NumberOfNulls")
            Lude.<*> (x Lude..: "NumberOfDistinctValues")
      )

instance Lude.ToJSON LongColumnStatisticsData where
  toJSON LongColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaximumValue" Lude..=) Lude.<$> maximumValue,
            ("MinimumValue" Lude..=) Lude.<$> minimumValue,
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls),
            Lude.Just
              ("NumberOfDistinctValues" Lude..= numberOfDistinctValues)
          ]
      )
