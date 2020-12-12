{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BinaryColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BinaryColumnStatisticsData
  ( BinaryColumnStatisticsData (..),

    -- * Smart constructor
    mkBinaryColumnStatisticsData,

    -- * Lenses
    bcsdMaximumLength,
    bcsdAverageLength,
    bcsdNumberOfNulls,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for bit sequence data values.
--
-- /See:/ 'mkBinaryColumnStatisticsData' smart constructor.
data BinaryColumnStatisticsData = BinaryColumnStatisticsData'
  { maximumLength ::
      Lude.Natural,
    averageLength :: Lude.Double,
    numberOfNulls :: Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BinaryColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'averageLength' - The average bit sequence length in the column.
-- * 'maximumLength' - The size of the longest bit sequence in the column.
-- * 'numberOfNulls' - The number of null values in the column.
mkBinaryColumnStatisticsData ::
  -- | 'maximumLength'
  Lude.Natural ->
  -- | 'averageLength'
  Lude.Double ->
  -- | 'numberOfNulls'
  Lude.Natural ->
  BinaryColumnStatisticsData
mkBinaryColumnStatisticsData
  pMaximumLength_
  pAverageLength_
  pNumberOfNulls_ =
    BinaryColumnStatisticsData'
      { maximumLength = pMaximumLength_,
        averageLength = pAverageLength_,
        numberOfNulls = pNumberOfNulls_
      }

-- | The size of the longest bit sequence in the column.
--
-- /Note:/ Consider using 'maximumLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcsdMaximumLength :: Lens.Lens' BinaryColumnStatisticsData Lude.Natural
bcsdMaximumLength = Lens.lens (maximumLength :: BinaryColumnStatisticsData -> Lude.Natural) (\s a -> s {maximumLength = a} :: BinaryColumnStatisticsData)
{-# DEPRECATED bcsdMaximumLength "Use generic-lens or generic-optics with 'maximumLength' instead." #-}

-- | The average bit sequence length in the column.
--
-- /Note:/ Consider using 'averageLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcsdAverageLength :: Lens.Lens' BinaryColumnStatisticsData Lude.Double
bcsdAverageLength = Lens.lens (averageLength :: BinaryColumnStatisticsData -> Lude.Double) (\s a -> s {averageLength = a} :: BinaryColumnStatisticsData)
{-# DEPRECATED bcsdAverageLength "Use generic-lens or generic-optics with 'averageLength' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bcsdNumberOfNulls :: Lens.Lens' BinaryColumnStatisticsData Lude.Natural
bcsdNumberOfNulls = Lens.lens (numberOfNulls :: BinaryColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: BinaryColumnStatisticsData)
{-# DEPRECATED bcsdNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

instance Lude.FromJSON BinaryColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "BinaryColumnStatisticsData"
      ( \x ->
          BinaryColumnStatisticsData'
            Lude.<$> (x Lude..: "MaximumLength")
            Lude.<*> (x Lude..: "AverageLength")
            Lude.<*> (x Lude..: "NumberOfNulls")
      )

instance Lude.ToJSON BinaryColumnStatisticsData where
  toJSON BinaryColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("MaximumLength" Lude..= maximumLength),
            Lude.Just ("AverageLength" Lude..= averageLength),
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls)
          ]
      )
