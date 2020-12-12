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
    dMaximumValue,
    dMinimumValue,
    dNumberOfNulls,
    dNumberOfDistinctValues,
  )
where

import Network.AWS.Glue.Types.DecimalNumber
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for fixed-point number data columns.
--
-- /See:/ 'mkDecimalColumnStatisticsData' smart constructor.
data DecimalColumnStatisticsData = DecimalColumnStatisticsData'
  { maximumValue ::
      Lude.Maybe DecimalNumber,
    minimumValue ::
      Lude.Maybe DecimalNumber,
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

-- | Creates a value of 'DecimalColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'maximumValue' - The highest value in the column.
-- * 'minimumValue' - The lowest value in the column.
-- * 'numberOfDistinctValues' - The number of distinct values in a column.
-- * 'numberOfNulls' - The number of null values in the column.
mkDecimalColumnStatisticsData ::
  -- | 'numberOfNulls'
  Lude.Natural ->
  -- | 'numberOfDistinctValues'
  Lude.Natural ->
  DecimalColumnStatisticsData
mkDecimalColumnStatisticsData
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    DecimalColumnStatisticsData'
      { maximumValue = Lude.Nothing,
        minimumValue = Lude.Nothing,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues = pNumberOfDistinctValues_
      }

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaximumValue :: Lens.Lens' DecimalColumnStatisticsData (Lude.Maybe DecimalNumber)
dMaximumValue = Lens.lens (maximumValue :: DecimalColumnStatisticsData -> Lude.Maybe DecimalNumber) (\s a -> s {maximumValue = a} :: DecimalColumnStatisticsData)
{-# DEPRECATED dMaximumValue "Use generic-lens or generic-optics with 'maximumValue' instead." #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMinimumValue :: Lens.Lens' DecimalColumnStatisticsData (Lude.Maybe DecimalNumber)
dMinimumValue = Lens.lens (minimumValue :: DecimalColumnStatisticsData -> Lude.Maybe DecimalNumber) (\s a -> s {minimumValue = a} :: DecimalColumnStatisticsData)
{-# DEPRECATED dMinimumValue "Use generic-lens or generic-optics with 'minimumValue' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNumberOfNulls :: Lens.Lens' DecimalColumnStatisticsData Lude.Natural
dNumberOfNulls = Lens.lens (numberOfNulls :: DecimalColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: DecimalColumnStatisticsData)
{-# DEPRECATED dNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNumberOfDistinctValues :: Lens.Lens' DecimalColumnStatisticsData Lude.Natural
dNumberOfDistinctValues = Lens.lens (numberOfDistinctValues :: DecimalColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfDistinctValues = a} :: DecimalColumnStatisticsData)
{-# DEPRECATED dNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

instance Lude.FromJSON DecimalColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "DecimalColumnStatisticsData"
      ( \x ->
          DecimalColumnStatisticsData'
            Lude.<$> (x Lude..:? "MaximumValue")
            Lude.<*> (x Lude..:? "MinimumValue")
            Lude.<*> (x Lude..: "NumberOfNulls")
            Lude.<*> (x Lude..: "NumberOfDistinctValues")
      )

instance Lude.ToJSON DecimalColumnStatisticsData where
  toJSON DecimalColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaximumValue" Lude..=) Lude.<$> maximumValue,
            ("MinimumValue" Lude..=) Lude.<$> minimumValue,
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls),
            Lude.Just
              ("NumberOfDistinctValues" Lude..= numberOfDistinctValues)
          ]
      )
