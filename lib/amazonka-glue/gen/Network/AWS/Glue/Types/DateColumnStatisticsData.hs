{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.DateColumnStatisticsData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.DateColumnStatisticsData
  ( DateColumnStatisticsData (..)
  -- * Smart constructor
  , mkDateColumnStatisticsData
  -- * Lenses
  , dcsdNumberOfNulls
  , dcsdNumberOfDistinctValues
  , dcsdMaximumValue
  , dcsdMinimumValue
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Defines column statistics supported for timestamp data columns.
--
-- /See:/ 'mkDateColumnStatisticsData' smart constructor.
data DateColumnStatisticsData = DateColumnStatisticsData'
  { numberOfNulls :: Core.Natural
    -- ^ The number of null values in the column.
  , numberOfDistinctValues :: Core.Natural
    -- ^ The number of distinct values in a column.
  , maximumValue :: Core.Maybe Core.NominalDiffTime
    -- ^ The highest value in the column.
  , minimumValue :: Core.Maybe Core.NominalDiffTime
    -- ^ The lowest value in the column.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'DateColumnStatisticsData' value with any optional fields omitted.
mkDateColumnStatisticsData
    :: Core.Natural -- ^ 'numberOfNulls'
    -> Core.Natural -- ^ 'numberOfDistinctValues'
    -> DateColumnStatisticsData
mkDateColumnStatisticsData numberOfNulls numberOfDistinctValues
  = DateColumnStatisticsData'{numberOfNulls, numberOfDistinctValues,
                              maximumValue = Core.Nothing, minimumValue = Core.Nothing}

-- | The number of null values in the column.
--
-- /Note:/ Consider using 'numberOfNulls' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdNumberOfNulls :: Lens.Lens' DateColumnStatisticsData Core.Natural
dcsdNumberOfNulls = Lens.field @"numberOfNulls"
{-# INLINEABLE dcsdNumberOfNulls #-}
{-# DEPRECATED numberOfNulls "Use generic-lens or generic-optics with 'numberOfNulls' instead"  #-}

-- | The number of distinct values in a column.
--
-- /Note:/ Consider using 'numberOfDistinctValues' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdNumberOfDistinctValues :: Lens.Lens' DateColumnStatisticsData Core.Natural
dcsdNumberOfDistinctValues = Lens.field @"numberOfDistinctValues"
{-# INLINEABLE dcsdNumberOfDistinctValues #-}
{-# DEPRECATED numberOfDistinctValues "Use generic-lens or generic-optics with 'numberOfDistinctValues' instead"  #-}

-- | The highest value in the column.
--
-- /Note:/ Consider using 'maximumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdMaximumValue :: Lens.Lens' DateColumnStatisticsData (Core.Maybe Core.NominalDiffTime)
dcsdMaximumValue = Lens.field @"maximumValue"
{-# INLINEABLE dcsdMaximumValue #-}
{-# DEPRECATED maximumValue "Use generic-lens or generic-optics with 'maximumValue' instead"  #-}

-- | The lowest value in the column.
--
-- /Note:/ Consider using 'minimumValue' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcsdMinimumValue :: Lens.Lens' DateColumnStatisticsData (Core.Maybe Core.NominalDiffTime)
dcsdMinimumValue = Lens.field @"minimumValue"
{-# INLINEABLE dcsdMinimumValue #-}
{-# DEPRECATED minimumValue "Use generic-lens or generic-optics with 'minimumValue' instead"  #-}

instance Core.FromJSON DateColumnStatisticsData where
        toJSON DateColumnStatisticsData{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("NumberOfNulls" Core..= numberOfNulls),
                  Core.Just
                    ("NumberOfDistinctValues" Core..= numberOfDistinctValues),
                  ("MaximumValue" Core..=) Core.<$> maximumValue,
                  ("MinimumValue" Core..=) Core.<$> minimumValue])

instance Core.FromJSON DateColumnStatisticsData where
        parseJSON
          = Core.withObject "DateColumnStatisticsData" Core.$
              \ x ->
                DateColumnStatisticsData' Core.<$>
                  (x Core..: "NumberOfNulls") Core.<*>
                    x Core..: "NumberOfDistinctValues"
                    Core.<*> x Core..:? "MaximumValue"
                    Core.<*> x Core..:? "MinimumValue"
