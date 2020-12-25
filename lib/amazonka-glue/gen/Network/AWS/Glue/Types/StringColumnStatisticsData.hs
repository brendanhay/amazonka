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
import qualified Network.AWS.Prelude as Core

-- | Defines column statistics supported for character sequence data values.
--
-- /See:/ 'mkStringColumnStatisticsData' smart constructor.
data StringColumnStatisticsData = StringColumnStatisticsData'
  { -- | The size of the longest string in the column.
    maximumLength :: Core.Natural,
    -- | The average string length in the column.
    averageLength :: Core.Double,
    -- | The number of null values in the column.
    numberOfNulls :: Core.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'StringColumnStatisticsData' value with any optional fields omitted.
mkStringColumnStatisticsData ::
  -- | 'maximumLength'
  Core.Natural ->
  -- | 'averageLength'
  Core.Double ->
  -- | 'numberOfNulls'
  Core.Natural ->
  -- | 'numberOfDistinctValues'
  Core.Natural ->
  StringColumnStatisticsData
mkStringColumnStatisticsData
  maximumLength
  averageLength
  numberOfNulls
  numberOfDistinctValues =
    StringColumnStatisticsData'
      { maximumLength,
        averageLength,
        numberOfNulls,
        numberOfDistinctValues
      }

-- | The size of the longest string in the column.
--
-- /Note:/ Consider using 'maximumLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsdMaximumLength :: Lens.Lens' StringColumnStatisticsData Core.Natural
scsdMaximumLength = Lens.field @"maximumLength"
{-# DEPRECATED scsdMaximumLength "Use generic-lens or generic-optics with 'maximumLength' instead." #-}

-- | The average string length in the column.
--
-- /Note:/ Consider using 'averageLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsdAverageLength :: Lens.Lens' StringColumnStatisticsData Core.Double
scsdAverageLength = Lens.field @"averageLength"
{-# DEPRECATED scsdAverageLength "Use generic-lens or generic-optics with 'averageLength' instead." #-}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsdNumberOfNulls :: Lens.Lens' StringColumnStatisticsData Core.Natural
scsdNumberOfNulls = Lens.field @"numberOfNulls"
{-# DEPRECATED scsdNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scsdNumberOfDistinctValues :: Lens.Lens' StringColumnStatisticsData Core.Natural
scsdNumberOfDistinctValues = Lens.field @"numberOfDistinctValues"
{-# DEPRECATED scsdNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

instance Core.FromJSON StringColumnStatisticsData where
  toJSON StringColumnStatisticsData {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("MaximumLength" Core..= maximumLength),
            Core.Just ("AverageLength" Core..= averageLength),
            Core.Just ("NumberOfNulls" Core..= numberOfNulls),
            Core.Just
              ("NumberOfDistinctValues" Core..= numberOfDistinctValues)
          ]
      )

instance Core.FromJSON StringColumnStatisticsData where
  parseJSON =
    Core.withObject "StringColumnStatisticsData" Core.$
      \x ->
        StringColumnStatisticsData'
          Core.<$> (x Core..: "MaximumLength")
          Core.<*> (x Core..: "AverageLength")
          Core.<*> (x Core..: "NumberOfNulls")
          Core.<*> (x Core..: "NumberOfDistinctValues")
