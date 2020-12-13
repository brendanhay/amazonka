{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dcsdNumberOfDistinctValues,
    dcsdMaximumValue,
    dcsdNumberOfNulls,
    dcsdMinimumValue,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for timestamp data columns.
--
-- /See:/ 'mkDateColumnStatisticsData' smart constructor.
data DateColumnStatisticsData = DateColumnStatisticsData'
  { -- | The number of distinct values in a column.
    numberOfDistinctValues :: Lude.Natural,
    -- | The highest value in the column.
    maximumValue :: Lude.Maybe Lude.Timestamp,
    -- | The number of null values in the column.
    numberOfNulls :: Lude.Natural,
    -- | The lowest value in the column.
    minimumValue :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DateColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'numberOfDistinctValues' - The number of distinct values in a column.
-- * 'maximumValue' - The highest value in the column.
-- * 'numberOfNulls' - The number of null values in the column.
-- * 'minimumValue' - The lowest value in the column.
mkDateColumnStatisticsData ::
  -- | 'numberOfDistinctValues'
  Lude.Natural ->
  -- | 'numberOfNulls'
  Lude.Natural ->
  DateColumnStatisticsData
mkDateColumnStatisticsData pNumberOfDistinctValues_ pNumberOfNulls_ =
  DateColumnStatisticsData'
    { numberOfDistinctValues =
        pNumberOfDistinctValues_,
      maximumValue = Lude.Nothing,
      numberOfNulls = pNumberOfNulls_,
      minimumValue = Lude.Nothing
    }

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdNumberOfDistinctValues :: Lens.Lens' DateColumnStatisticsData Lude.Natural
dcsdNumberOfDistinctValues = Lens.lens (numberOfDistinctValues :: DateColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfDistinctValues = a} :: DateColumnStatisticsData)
{-# DEPRECATED dcsdNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdMaximumValue :: Lens.Lens' DateColumnStatisticsData (Lude.Maybe Lude.Timestamp)
dcsdMaximumValue = Lens.lens (maximumValue :: DateColumnStatisticsData -> Lude.Maybe Lude.Timestamp) (\s a -> s {maximumValue = a} :: DateColumnStatisticsData)
{-# DEPRECATED dcsdMaximumValue "Use generic-lens or generic-optics with 'maximumValue' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdNumberOfNulls :: Lens.Lens' DateColumnStatisticsData Lude.Natural
dcsdNumberOfNulls = Lens.lens (numberOfNulls :: DateColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: DateColumnStatisticsData)
{-# DEPRECATED dcsdNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdMinimumValue :: Lens.Lens' DateColumnStatisticsData (Lude.Maybe Lude.Timestamp)
dcsdMinimumValue = Lens.lens (minimumValue :: DateColumnStatisticsData -> Lude.Maybe Lude.Timestamp) (\s a -> s {minimumValue = a} :: DateColumnStatisticsData)
{-# DEPRECATED dcsdMinimumValue "Use generic-lens or generic-optics with 'minimumValue' instead." #-}

instance Lude.FromJSON DateColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "DateColumnStatisticsData"
      ( \x ->
          DateColumnStatisticsData'
            Lude.<$> (x Lude..: "NumberOfDistinctValues")
            Lude.<*> (x Lude..:? "MaximumValue")
            Lude.<*> (x Lude..: "NumberOfNulls")
            Lude.<*> (x Lude..:? "MinimumValue")
      )

instance Lude.ToJSON DateColumnStatisticsData where
  toJSON DateColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("NumberOfDistinctValues" Lude..= numberOfDistinctValues),
            ("MaximumValue" Lude..=) Lude.<$> maximumValue,
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls),
            ("MinimumValue" Lude..=) Lude.<$> minimumValue
          ]
      )
