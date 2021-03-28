{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DoubleColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.DoubleColumnStatisticsData
  ( DoubleColumnStatisticsData (..)
  -- * Smart constructor
  , mkDoubleColumnStatisticsData
  -- * Lenses
  , dcsdfNumberOfNulls
  , dcsdfNumberOfDistinctValues
  , dcsdfMaximumValue
  , dcsdfMinimumValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines column statistics supported for floating-point number data columns.
--
-- /See:/ 'mkDoubleColumnStatisticsData' smart constructor.
data DoubleColumnStatisticsData = DoubleColumnStatisticsData'
  { numberOfNulls :: Core.Natural
    -- ^ The number of null values in the column.
  , numberOfDistinctValues :: Core.Natural
    -- ^ The number of distinct values in a column.
  , maximumValue :: Core.Maybe Core.Double
    -- ^ The highest value in the column.
  , minimumValue :: Core.Maybe Core.Double
    -- ^ The lowest value in the column.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DoubleColumnStatisticsData' value with any optional fields omitted.
mkDoubleColumnStatisticsData
    :: Core.Natural -- ^ 'numberOfNulls'
    -> Core.Natural -- ^ 'numberOfDistinctValues'
    -> DoubleColumnStatisticsData
mkDoubleColumnStatisticsData numberOfNulls numberOfDistinctValues
  = DoubleColumnStatisticsData'{numberOfNulls,
                                numberOfDistinctValues, maximumValue = Core.Nothing,
                                minimumValue = Core.Nothing}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdfNumberOfNulls :: Lens.Lens' DoubleColumnStatisticsData Core.Natural
dcsdfNumberOfNulls = Lens.field @"numberOfNulls"
{-# INLINEABLE dcsdfNumberOfNulls #-}
{-# DEPRECATED numberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead"  #-}

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdfNumberOfDistinctValues :: Lens.Lens' DoubleColumnStatisticsData Core.Natural
dcsdfNumberOfDistinctValues = Lens.field @"numberOfDistinctValues"
{-# INLINEABLE dcsdfNumberOfDistinctValues #-}
{-# DEPRECATED numberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead"  #-}

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdfMaximumValue :: Lens.Lens' DoubleColumnStatisticsData (Core.Maybe Core.Double)
dcsdfMaximumValue = Lens.field @"maximumValue"
{-# INLINEABLE dcsdfMaximumValue #-}
{-# DEPRECATED maximumValue "Use generic-lens or generic-optics with 'maximumValue' instead"  #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdfMinimumValue :: Lens.Lens' DoubleColumnStatisticsData (Core.Maybe Core.Double)
dcsdfMinimumValue = Lens.field @"minimumValue"
{-# INLINEABLE dcsdfMinimumValue #-}
{-# DEPRECATED minimumValue "Use generic-lens or generic-optics with 'minimumValue' instead"  #-}

instance Core.FromJSON DoubleColumnStatisticsData where
        toJSON DoubleColumnStatisticsData{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("NumberOfNulls" Core..= numberOfNulls),
                  Core.Just
                    ("NumberOfDistinctValues" Core..= numberOfDistinctValues),
                  ("MaximumValue" Core..=) Core.<$> maximumValue,
                  ("MinimumValue" Core..=) Core.<$> minimumValue])

instance Core.FromJSON DoubleColumnStatisticsData where
        parseJSON
          = Core.withObject "DoubleColumnStatisticsData" Core.$
              \ x ->
                DoubleColumnStatisticsData' Core.<$>
                  (x Core..: "NumberOfNulls") Core.<*>
                    x Core..: "NumberOfDistinctValues"
                    Core.<*> x Core..:? "MaximumValue"
                    Core.<*> x Core..:? "MinimumValue"
