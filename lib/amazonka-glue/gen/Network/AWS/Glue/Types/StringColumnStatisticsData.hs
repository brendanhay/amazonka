{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.StringColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.StringColumnStatisticsData
  ( StringColumnStatisticsData (..),

    -- * Smart constructor
    mkStringColumnStatisticsData,

    -- * Lenses
    scsdMaximumLength,
    scsdAverageLength,
    scsdNumberOfNulls,
    scsdNumberOfDistinctValues,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for character sequence data values.
--
-- /See:/ 'mkStringColumnStatisticsData' smart constructor.
data StringColumnStatisticsData = StringColumnStatisticsData'
  { maximumLength ::
      Lude.Natural,
    averageLength :: Lude.Double,
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

-- | Creates a value of 'StringColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'averageLength' - The average string length in the column.
-- * 'maximumLength' - The size of the longest string in the column.
-- * 'numberOfDistinctValues' - The number of distinct values in a column.
-- * 'numberOfNulls' - The number of null values in the column.
mkStringColumnStatisticsData ::
  -- | 'maximumLength'
  Lude.Natural ->
  -- | 'averageLength'
  Lude.Double ->
  -- | 'numberOfNulls'
  Lude.Natural ->
  -- | 'numberOfDistinctValues'
  Lude.Natural ->
  StringColumnStatisticsData
mkStringColumnStatisticsData
  pMaximumLength_
  pAverageLength_
  pNumberOfNulls_
  pNumberOfDistinctValues_ =
    StringColumnStatisticsData'
      { maximumLength = pMaximumLength_,
        averageLength = pAverageLength_,
        numberOfNulls = pNumberOfNulls_,
        numberOfDistinctValues = pNumberOfDistinctValues_
      }

-- | The size of the longest string in the column.
--
-- /Note:/ Consider using 'maximumLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsdMaximumLength :: Lens.Lens' StringColumnStatisticsData Lude.Natural
scsdMaximumLength = Lens.lens (maximumLength :: StringColumnStatisticsData -> Lude.Natural) (\s a -> s {maximumLength = a} :: StringColumnStatisticsData)
{-# DEPRECATED scsdMaximumLength "Use generic-lens or generic-optics with 'maximumLength' instead." #-}

-- | The average string length in the column.
--
-- /Note:/ Consider using 'averageLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsdAverageLength :: Lens.Lens' StringColumnStatisticsData Lude.Double
scsdAverageLength = Lens.lens (averageLength :: StringColumnStatisticsData -> Lude.Double) (\s a -> s {averageLength = a} :: StringColumnStatisticsData)
{-# DEPRECATED scsdAverageLength "Use generic-lens or generic-optics with 'averageLength' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsdNumberOfNulls :: Lens.Lens' StringColumnStatisticsData Lude.Natural
scsdNumberOfNulls = Lens.lens (numberOfNulls :: StringColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: StringColumnStatisticsData)
{-# DEPRECATED scsdNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsdNumberOfDistinctValues :: Lens.Lens' StringColumnStatisticsData Lude.Natural
scsdNumberOfDistinctValues = Lens.lens (numberOfDistinctValues :: StringColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfDistinctValues = a} :: StringColumnStatisticsData)
{-# DEPRECATED scsdNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

instance Lude.FromJSON StringColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "StringColumnStatisticsData"
      ( \x ->
          StringColumnStatisticsData'
            Lude.<$> (x Lude..: "MaximumLength")
            Lude.<*> (x Lude..: "AverageLength")
            Lude.<*> (x Lude..: "NumberOfNulls")
            Lude.<*> (x Lude..: "NumberOfDistinctValues")
      )

instance Lude.ToJSON StringColumnStatisticsData where
  toJSON StringColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MaximumLength" Lude..= maximumLength),
            Lude.Just ("AverageLength" Lude..= averageLength),
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls),
            Lude.Just
              ("NumberOfDistinctValues" Lude..= numberOfDistinctValues)
          ]
      )
