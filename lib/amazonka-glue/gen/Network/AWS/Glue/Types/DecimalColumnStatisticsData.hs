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
    dNumberOfNulls,
    dNumberOfDistinctValues,
    dMaximumValue,
    dMinimumValue,
  )
where

import qualified Network.AWS.Glue.Types.DecimalNumber as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines column statistics supported for fixed-point number data columns.
--
-- /See:/ 'mkDecimalColumnStatisticsData' smart constructor.
data DecimalColumnStatisticsData = DecimalColumnStatisticsData'
  { -- | The number of null values in the column.
    numberOfNulls :: Core.Natural,
    -- | The number of distinct values in a column.
    numberOfDistinctValues :: Core.Natural,
    -- | The highest value in the column.
    maximumValue :: Core.Maybe Types.DecimalNumber,
    -- | The lowest value in the column.
    minimumValue :: Core.Maybe Types.DecimalNumber
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DecimalColumnStatisticsData' value with any optional fields omitted.
mkDecimalColumnStatisticsData ::
  -- | 'numberOfNulls'
  Core.Natural ->
  -- | 'numberOfDistinctValues'
  Core.Natural ->
  DecimalColumnStatisticsData
mkDecimalColumnStatisticsData numberOfNulls numberOfDistinctValues =
  DecimalColumnStatisticsData'
    { numberOfNulls,
      numberOfDistinctValues,
      maximumValue = Core.Nothing,
      minimumValue = Core.Nothing
    }

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNumberOfNulls :: Lens.Lens' DecimalColumnStatisticsData Core.Natural
dNumberOfNulls = Lens.field @"numberOfNulls"
{-# DEPRECATED dNumberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead." #-}

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dNumberOfDistinctValues :: Lens.Lens' DecimalColumnStatisticsData Core.Natural
dNumberOfDistinctValues = Lens.field @"numberOfDistinctValues"
{-# DEPRECATED dNumberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead." #-}

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMaximumValue :: Lens.Lens' DecimalColumnStatisticsData (Core.Maybe Types.DecimalNumber)
dMaximumValue = Lens.field @"maximumValue"
{-# DEPRECATED dMaximumValue "Use generic-lens or generic-optics with 'maximumValue' instead." #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMinimumValue :: Lens.Lens' DecimalColumnStatisticsData (Core.Maybe Types.DecimalNumber)
dMinimumValue = Lens.field @"minimumValue"
{-# DEPRECATED dMinimumValue "Use generic-lens or generic-optics with 'minimumValue' instead." #-}

instance Core.FromJSON DecimalColumnStatisticsData where
  toJSON DecimalColumnStatisticsData {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("NumberOfNulls" Core..= numberOfNulls),
            Core.Just
              ("NumberOfDistinctValues" Core..= numberOfDistinctValues),
            ("MaximumValue" Core..=) Core.<$> maximumValue,
            ("MinimumValue" Core..=) Core.<$> minimumValue
          ]
      )

instance Core.FromJSON DecimalColumnStatisticsData where
  parseJSON =
    Core.withObject "DecimalColumnStatisticsData" Core.$
      \x ->
        DecimalColumnStatisticsData'
          Core.<$> (x Core..: "NumberOfNulls")
          Core.<*> (x Core..: "NumberOfDistinctValues")
          Core.<*> (x Core..:? "MaximumValue")
          Core.<*> (x Core..:? "MinimumValue")
