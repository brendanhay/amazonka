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
    bNumberOfTrues,
    bNumberOfFalses,
    bNumberOfNulls,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines column statistics supported for Boolean data columns.
--
-- /See:/ 'mkBooleanColumnStatisticsData' smart constructor.
data BooleanColumnStatisticsData = BooleanColumnStatisticsData'
  { -- | The number of true values in the column.
    numberOfTrues :: Core.Natural,
    -- | The number of false values in the column.
    numberOfFalses :: Core.Natural,
    -- | The number of null values in the column.
    numberOfNulls :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'BooleanColumnStatisticsData' value with any optional fields omitted.
mkBooleanColumnStatisticsData ::
  -- | 'numberOfTrues'
  Core.Natural ->
  -- | 'numberOfFalses'
  Core.Natural ->
  -- | 'numberOfNulls'
  Core.Natural ->
  BooleanColumnStatisticsData
mkBooleanColumnStatisticsData
  numberOfTrues
  numberOfFalses
  numberOfNulls =
    BooleanColumnStatisticsData'
      { numberOfTrues,
        numberOfFalses,
        numberOfNulls
      }

-- | The number of true values in the column.
--
-- /Note:/ Consider using 'numberOfTrues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNumberOfTrues :: Lens.Lens' BooleanColumnStatisticsData Core.Natural
bNumberOfTrues = Lens.field @"numberOfTrues"
{-# DEPRECATED bNumberOfTrues "Use generic-lens or generic-optics with 'numberOfTrues' instead." #-}

-- | The number of false values in the column.
--
-- /Note:/ Consider using 'numberOfFalses' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNumberOfFalses :: Lens.Lens' BooleanColumnStatisticsData Core.Natural
bNumberOfFalses = Lens.field @"numberOfFalses"
{-# DEPRECATED bNumberOfFalses "Use generic-lens or generic-optics with 'numberOfFalses' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
bNumberOfNulls :: Lens.Lens' BooleanColumnStatisticsData Core.Natural
bNumberOfNulls = Lens.field @"numberOfNulls"
{-# DEPRECATED bNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

instance Core.FromJSON BooleanColumnStatisticsData where
  toJSON BooleanColumnStatisticsData {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("NumberOfTrues" Core..= numberOfTrues),
            Core.Just ("NumberOfFalses" Core..= numberOfFalses),
            Core.Just ("NumberOfNulls" Core..= numberOfNulls)
          ]
      )

instance Core.FromJSON BooleanColumnStatisticsData where
  parseJSON =
    Core.withObject "BooleanColumnStatisticsData" Core.$
      \x ->
        BooleanColumnStatisticsData'
          Core.<$> (x Core..: "NumberOfTrues")
          Core.<*> (x Core..: "NumberOfFalses")
          Core.<*> (x Core..: "NumberOfNulls")
