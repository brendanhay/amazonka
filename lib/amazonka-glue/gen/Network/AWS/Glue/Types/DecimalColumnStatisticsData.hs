{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DecimalColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DecimalColumnStatisticsData
  ( DecimalColumnStatisticsData (..),

    -- * Smart constructor
    mkDecimalColumnStatisticsData,

    -- * Lenses
    dNumberOfDistinctValues,
    dMaximumValue,
    dNumberOfNulls,
    dMinimumValue,
  )
where

import Network.AWS.Glue.Types.DecimalNumber
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for fixed-point number data columns.
--
-- /See:/ 'mkDecimalColumnStatisticsData' smart constructor.
data DecimalColumnStatisticsData = DecimalColumnStatisticsData'
  { -- | The number of distinct values in a column.
    numberOfDistinctValues :: Lude.Natural,
    -- | The highest value in the column.
    maximumValue :: Lude.Maybe DecimalNumber,
    -- | The number of null values in the column.
    numberOfNulls :: Lude.Natural,
    -- | The lowest value in the column.
    minimumValue :: Lude.Maybe DecimalNumber
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DecimalColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'numberOfDistinctValues' - The number of distinct values in a column.
-- * 'maximumValue' - The highest value in the column.
-- * 'numberOfNulls' - The number of null values in the column.
-- * 'minimumValue' - The lowest value in the column.
mkDecimalColumnStatisticsData ::
  -- | 'numberOfDistinctValues'
  Lude.Natural ->
  -- | 'numberOfNulls'
  Lude.Natural ->
  DecimalColumnStatisticsData
mkDecimalColumnStatisticsData
  pNumberOfDistinctValues_
  pNumberOfNulls_ =
    DecimalColumnStatisticsData'
      { numberOfDistinctValues =
          pNumberOfDistinctValues_,
        maximumValue = Lude.Nothing,
        numberOfNulls = pNumberOfNulls_,
        minimumValue = Lude.Nothing
      }

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNumberOfDistinctValues :: Lens.Lens' DecimalColumnStatisticsData Lude.Natural
dNumberOfDistinctValues = Lens.lens (numberOfDistinctValues :: DecimalColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfDistinctValues = a} :: DecimalColumnStatisticsData)
{-# DEPRECATED dNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaximumValue :: Lens.Lens' DecimalColumnStatisticsData (Lude.Maybe DecimalNumber)
dMaximumValue = Lens.lens (maximumValue :: DecimalColumnStatisticsData -> Lude.Maybe DecimalNumber) (\s a -> s {maximumValue = a} :: DecimalColumnStatisticsData)
{-# DEPRECATED dMaximumValue "Use generic-lens or generic-optics with 'maximumValue' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNumberOfNulls :: Lens.Lens' DecimalColumnStatisticsData Lude.Natural
dNumberOfNulls = Lens.lens (numberOfNulls :: DecimalColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: DecimalColumnStatisticsData)
{-# DEPRECATED dNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMinimumValue :: Lens.Lens' DecimalColumnStatisticsData (Lude.Maybe DecimalNumber)
dMinimumValue = Lens.lens (minimumValue :: DecimalColumnStatisticsData -> Lude.Maybe DecimalNumber) (\s a -> s {minimumValue = a} :: DecimalColumnStatisticsData)
{-# DEPRECATED dMinimumValue "Use generic-lens or generic-optics with 'minimumValue' instead." #-}

instance Lude.FromJSON DecimalColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "DecimalColumnStatisticsData"
      ( \x ->
          DecimalColumnStatisticsData'
            Lude.<$> (x Lude..: "NumberOfDistinctValues")
            Lude.<*> (x Lude..:? "MaximumValue")
            Lude.<*> (x Lude..: "NumberOfNulls")
            Lude.<*> (x Lude..:? "MinimumValue")
      )

instance Lude.ToJSON DecimalColumnStatisticsData where
  toJSON DecimalColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("NumberOfDistinctValues" Lude..= numberOfDistinctValues),
            ("MaximumValue" Lude..=) Lude.<$> maximumValue,
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls),
            ("MinimumValue" Lude..=) Lude.<$> minimumValue
          ]
      )
