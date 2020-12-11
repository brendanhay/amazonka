-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DateColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.DateColumnStatisticsData
  ( DateColumnStatisticsData (..),

    -- * Smart constructor
    mkDateColumnStatisticsData,

    -- * Lenses
    dcsdMaximumValue,
    dcsdMinimumValue,
    dcsdNumberOfNulls,
    dcsdNumberOfDistinctValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for timestamp data columns.
--
-- /See:/ 'mkDateColumnStatisticsData' smart constructor.
data DateColumnStatisticsData = DateColumnStatisticsData'
  { maximumValue ::
      Lude.Maybe Lude.Timestamp,
    minimumValue :: Lude.Maybe Lude.Timestamp,
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

-- | Creates a value of 'DateColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'maximumValue' - The highest value in the column.
-- * 'minimumValue' - The lowest value in the column.
-- * 'numberOfDistinctValues' - The number of distinct values in a column.
-- * 'numberOfNulls' - The number of null values in the column.
mkDateColumnStatisticsData ::
  -- | 'numberOfNulls'
  Lude.Natural ->
  -- | 'numberOfDistinctValues'
  Lude.Natural ->
  DateColumnStatisticsData
mkDateColumnStatisticsData pNumberOfNulls_ pNumberOfDistinctValues_ =
  DateColumnStatisticsData'
    { maximumValue = Lude.Nothing,
      minimumValue = Lude.Nothing,
      numberOfNulls = pNumberOfNulls_,
      numberOfDistinctValues = pNumberOfDistinctValues_
    }

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdMaximumValue :: Lens.Lens' DateColumnStatisticsData (Lude.Maybe Lude.Timestamp)
dcsdMaximumValue = Lens.lens (maximumValue :: DateColumnStatisticsData -> Lude.Maybe Lude.Timestamp) (\s a -> s {maximumValue = a} :: DateColumnStatisticsData)
{-# DEPRECATED dcsdMaximumValue "Use generic-lens or generic-optics with 'maximumValue' instead." #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdMinimumValue :: Lens.Lens' DateColumnStatisticsData (Lude.Maybe Lude.Timestamp)
dcsdMinimumValue = Lens.lens (minimumValue :: DateColumnStatisticsData -> Lude.Maybe Lude.Timestamp) (\s a -> s {minimumValue = a} :: DateColumnStatisticsData)
{-# DEPRECATED dcsdMinimumValue "Use generic-lens or generic-optics with 'minimumValue' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdNumberOfNulls :: Lens.Lens' DateColumnStatisticsData Lude.Natural
dcsdNumberOfNulls = Lens.lens (numberOfNulls :: DateColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: DateColumnStatisticsData)
{-# DEPRECATED dcsdNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdNumberOfDistinctValues :: Lens.Lens' DateColumnStatisticsData Lude.Natural
dcsdNumberOfDistinctValues = Lens.lens (numberOfDistinctValues :: DateColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfDistinctValues = a} :: DateColumnStatisticsData)
{-# DEPRECATED dcsdNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

instance Lude.FromJSON DateColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "DateColumnStatisticsData"
      ( \x ->
          DateColumnStatisticsData'
            Lude.<$> (x Lude..:? "MaximumValue")
            Lude.<*> (x Lude..:? "MinimumValue")
            Lude.<*> (x Lude..: "NumberOfNulls")
            Lude.<*> (x Lude..: "NumberOfDistinctValues")
      )

instance Lude.ToJSON DateColumnStatisticsData where
  toJSON DateColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("MaximumValue" Lude..=) Lude.<$> maximumValue,
            ("MinimumValue" Lude..=) Lude.<$> minimumValue,
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls),
            Lude.Just
              ("NumberOfDistinctValues" Lude..= numberOfDistinctValues)
          ]
      )
