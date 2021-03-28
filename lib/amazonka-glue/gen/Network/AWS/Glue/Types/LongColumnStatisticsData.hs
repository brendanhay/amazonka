{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.LongColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.LongColumnStatisticsData
  ( LongColumnStatisticsData (..)
  -- * Smart constructor
  , mkLongColumnStatisticsData
  -- * Lenses
  , lcsdNumberOfNulls
  , lcsdNumberOfDistinctValues
  , lcsdMaximumValue
  , lcsdMinimumValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines column statistics supported for integer data columns.
--
-- /See:/ 'mkLongColumnStatisticsData' smart constructor.
data LongColumnStatisticsData = LongColumnStatisticsData'
  { numberOfNulls :: Core.Natural
    -- ^ The number of null values in the column.
  , numberOfDistinctValues :: Core.Natural
    -- ^ The number of distinct values in a column.
  , maximumValue :: Core.Maybe Core.Integer
    -- ^ The highest value in the column.
  , minimumValue :: Core.Maybe Core.Integer
    -- ^ The lowest value in the column.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'LongColumnStatisticsData' value with any optional fields omitted.
mkLongColumnStatisticsData
    :: Core.Natural -- ^ 'numberOfNulls'
    -> Core.Natural -- ^ 'numberOfDistinctValues'
    -> LongColumnStatisticsData
mkLongColumnStatisticsData numberOfNulls numberOfDistinctValues
  = LongColumnStatisticsData'{numberOfNulls, numberOfDistinctValues,
                              maximumValue = Core.Nothing, minimumValue = Core.Nothing}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdNumberOfNulls :: Lens.Lens' LongColumnStatisticsData Core.Natural
lcsdNumberOfNulls = Lens.field @"numberOfNulls"
{-# INLINEABLE lcsdNumberOfNulls #-}
{-# DEPRECATED numberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead"  #-}

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdNumberOfDistinctValues :: Lens.Lens' LongColumnStatisticsData Core.Natural
lcsdNumberOfDistinctValues = Lens.field @"numberOfDistinctValues"
{-# INLINEABLE lcsdNumberOfDistinctValues #-}
{-# DEPRECATED numberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead"  #-}

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdMaximumValue :: Lens.Lens' LongColumnStatisticsData (Core.Maybe Core.Integer)
lcsdMaximumValue = Lens.field @"maximumValue"
{-# INLINEABLE lcsdMaximumValue #-}
{-# DEPRECATED maximumValue "Use generic-lens or generic-optics with 'maximumValue' instead"  #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lcsdMinimumValue :: Lens.Lens' LongColumnStatisticsData (Core.Maybe Core.Integer)
lcsdMinimumValue = Lens.field @"minimumValue"
{-# INLINEABLE lcsdMinimumValue #-}
{-# DEPRECATED minimumValue "Use generic-lens or generic-optics with 'minimumValue' instead"  #-}

instance Core.FromJSON LongColumnStatisticsData where
        toJSON LongColumnStatisticsData{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("NumberOfNulls" Core..= numberOfNulls),
                  Core.Just
                    ("NumberOfDistinctValues" Core..= numberOfDistinctValues),
                  ("MaximumValue" Core..=) Core.<$> maximumValue,
                  ("MinimumValue" Core..=) Core.<$> minimumValue])

instance Core.FromJSON LongColumnStatisticsData where
        parseJSON
          = Core.withObject "LongColumnStatisticsData" Core.$
              \ x ->
                LongColumnStatisticsData' Core.<$>
                  (x Core..: "NumberOfNulls") Core.<*>
                    x Core..: "NumberOfDistinctValues"
                    Core.<*> x Core..:? "MaximumValue"
                    Core.<*> x Core..:? "MinimumValue"
