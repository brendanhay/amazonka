{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.BooleanColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.BooleanColumnStatisticsData
  ( BooleanColumnStatisticsData (..),

    -- * Smart constructor
    mkBooleanColumnStatisticsData,

    -- * Lenses
    bNumberOfFalses,
    bNumberOfTrues,
    bNumberOfNulls,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Defines column statistics supported for Boolean data columns.
--
-- /See:/ 'mkBooleanColumnStatisticsData' smart constructor.
data BooleanColumnStatisticsData = BooleanColumnStatisticsData'
  { -- | The number of false values in the column.
    numberOfFalses :: Lude.Natural,
    -- | The number of true values in the column.
    numberOfTrues :: Lude.Natural,
    -- | The number of null values in the column.
    numberOfNulls :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'BooleanColumnStatisticsData' with the minimum fields required to make a request.
--
-- * 'numberOfFalses' - The number of false values in the column.
-- * 'numberOfTrues' - The number of true values in the column.
-- * 'numberOfNulls' - The number of null values in the column.
mkBooleanColumnStatisticsData ::
  -- | 'numberOfFalses'
  Lude.Natural ->
  -- | 'numberOfTrues'
  Lude.Natural ->
  -- | 'numberOfNulls'
  Lude.Natural ->
  BooleanColumnStatisticsData
mkBooleanColumnStatisticsData
  pNumberOfFalses_
  pNumberOfTrues_
  pNumberOfNulls_ =
    BooleanColumnStatisticsData'
      { numberOfFalses = pNumberOfFalses_,
        numberOfTrues = pNumberOfTrues_,
        numberOfNulls = pNumberOfNulls_
      }

-- | The number of false values in the column.
--
-- /Note:/ Consider using 'numberOfFalses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNumberOfFalses :: Lens.Lens' BooleanColumnStatisticsData Lude.Natural
bNumberOfFalses = Lens.lens (numberOfFalses :: BooleanColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfFalses = a} :: BooleanColumnStatisticsData)
{-# DEPRECATED bNumberOfFalses "Use generic-lens or generic-optics with 'numberOfFalses' instead." #-}

-- | The number of true values in the column.
--
-- /Note:/ Consider using 'numberOfTrues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNumberOfTrues :: Lens.Lens' BooleanColumnStatisticsData Lude.Natural
bNumberOfTrues = Lens.lens (numberOfTrues :: BooleanColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfTrues = a} :: BooleanColumnStatisticsData)
{-# DEPRECATED bNumberOfTrues "Use generic-lens or generic-optics with 'numberOfTrues' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNumberOfNulls :: Lens.Lens' BooleanColumnStatisticsData Lude.Natural
bNumberOfNulls = Lens.lens (numberOfNulls :: BooleanColumnStatisticsData -> Lude.Natural) (\s a -> s {numberOfNulls = a} :: BooleanColumnStatisticsData)
{-# DEPRECATED bNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

instance Lude.FromJSON BooleanColumnStatisticsData where
  parseJSON =
    Lude.withObject
      "BooleanColumnStatisticsData"
      ( \x ->
          BooleanColumnStatisticsData'
            Lude.<$> (x Lude..: "NumberOfFalses")
            Lude.<*> (x Lude..: "NumberOfTrues")
            Lude.<*> (x Lude..: "NumberOfNulls")
      )

instance Lude.ToJSON BooleanColumnStatisticsData where
  toJSON BooleanColumnStatisticsData' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("NumberOfFalses" Lude..= numberOfFalses),
            Lude.Just ("NumberOfTrues" Lude..= numberOfTrues),
            Lude.Just ("NumberOfNulls" Lude..= numberOfNulls)
          ]
      )
