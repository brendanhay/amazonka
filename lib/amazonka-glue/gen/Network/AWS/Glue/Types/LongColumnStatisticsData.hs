{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    lcsdNumberOfDistinctValues,
    lcsdMaximumValue,
    lcsdNumberOfNulls,
    lcsdMinimumValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for integer data columns.
--
-- /See:/ 'mkLongColumnStatisticsData' smart constructor.
data LongColumnStatisticsData = LongColumnStatisticsData'
  { -- | The number of distinct values in a column.
    numberOfDistinctValues :: Lude.Natural,
    -- | The highest value in the column.
    maximumValue :: Lude.Maybe Lude.Integer,
    -- | The number of null values in the column.
    numberOfNulls :: Lude.Natural,
    -- | The lowest value in the column.
    minimumValue :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'LongColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'numberOfDistinctValues' - The number of distinct values in a column.
-- * 'maximumValue' - The highest value in the column.
-- * 'numberOfNulls' - The number of null values in the column.
-- * 'minimumValue' - The lowest value in the column.
mkLongColumnStatisticsData ::
  -- | 'numberOfDistinctValues'
  Lude.Natural ->
  -- | 'numberOfNulls'
  Lude.Natural ->
  LongColumnStatisticsData
mkLongColumnStatisticsData pNumberOfDistinctValues_ pNumberOfNulls_ =
  LongColumnStatisticsData'
    { numberOfDistinctValues =
        pNumberOfDistinctValues_,
      maximumValue = Lude.Nothing,
      numberOfNulls = pNumberOfNulls_,
      minimumValue = Lude.Nothing
    }

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdNumberOfDistinctValues :: Lens.Lens' LongColumnStatisticsData Lude.Natural
lcsdNumberOfDistinctValues = Lens.lens (numberOfDistinctValues :: LongColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfDistinctValues = a} :: LongColumnStatisticsData)
{-# DEPRECATED lcsdNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdMaximumValue :: Lens.Lens' LongColumnStatisticsData (Lude.Maybe Lude.Integer)
lcsdMaximumValue = Lens.lens (maximumValue :: LongColumnStatisticsData -> Lude.Maybe Lude.Integer) (\s a -> s {maximumValue = a} :: LongColumnStatisticsData)
{-# DEPRECATED lcsdMaximumValue "Use generic-lens or generic-optics with 'maximumValue' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdNumberOfNulls :: Lens.Lens' LongColumnStatisticsData Lude.Natural
lcsdNumberOfNulls = Lens.lens (numberOfNulls :: LongColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: LongColumnStatisticsData)
{-# DEPRECATED lcsdNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdMinimumValue :: Lens.Lens' LongColumnStatisticsData (Lude.Maybe Lude.Integer)
lcsdMinimumValue = Lens.lens (minimumValue :: LongColumnStatisticsData -> Lude.Maybe Lude.Integer) (\s a -> s {minimumValue = a} :: LongColumnStatisticsData)
{-# DEPRECATED lcsdMinimumValue "Use generic-lens or generic-optics with 'minimumValue' instead." #-}

instance Lude.FromJSON LongColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "LongColumnStatisticsData"
      ( \x ->
          LongColumnStatisticsData'
            Lude.<$> (x Lude..: "NumberOfDistinctValues")
            Lude.<*> (x Lude..:? "MaximumValue")
            Lude.<*> (x Lude..: "NumberOfNulls")
            Lude.<*> (x Lude..:? "MinimumValue")
      )

instance Lude.ToJSON LongColumnStatisticsData where
  toJSON LongColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("NumberOfDistinctValues" Lude..= numberOfDistinctValues),
            ("MaximumValue" Lude..=) Lude.<$> maximumValue,
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls),
            ("MinimumValue" Lude..=) Lude.<$> minimumValue
          ]
      )
