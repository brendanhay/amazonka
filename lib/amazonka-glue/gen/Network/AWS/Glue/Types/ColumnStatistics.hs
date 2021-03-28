{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Glue.Types.ColumnStatistics
  ( ColumnStatistics (..)
  -- * Smart constructor
  , mkColumnStatistics
  -- * Lenses
  , csColumnName
  , csColumnType
  , csAnalyzedTime
  , csStatisticsData
  ) where

import qualified Network.AWS.Glue.Types.ColumnStatisticsData as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.TypeString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the generated column-level statistics for a table or partition.
--
-- /See:/ 'mkColumnStatistics' smart constructor.
data ColumnStatistics = ColumnStatistics'
  { columnName :: Types.NameString
    -- ^ Name of column which statistics belong to.
  , columnType :: Types.TypeString
    -- ^ The data type of the column.
  , analyzedTime :: Core.NominalDiffTime
    -- ^ The timestamp of when column statistics were generated.
  , statisticsData :: Types.ColumnStatisticsData
    -- ^ A @ColumnStatisticData@ object that contains the statistics data values.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'ColumnStatistics' value with any optional fields omitted.
mkColumnStatistics
    :: Types.NameString -- ^ 'columnName'
    -> Types.TypeString -- ^ 'columnType'
    -> Core.NominalDiffTime -- ^ 'analyzedTime'
    -> Types.ColumnStatisticsData -- ^ 'statisticsData'
    -> ColumnStatistics
mkColumnStatistics columnName columnType analyzedTime
  statisticsData
  = ColumnStatistics'{columnName, columnType, analyzedTime,
                      statisticsData}

-- | Name of column which statistics belong to.
--
-- /Note:/ Consider using 'columnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csColumnName :: Lens.Lens' ColumnStatistics Types.NameString
csColumnName = Lens.field @"columnName"
{-# INLINEABLE csColumnName #-}
{-# DEPRECATED columnName "Use generic-lens or generic-optics with 'columnName' instead"  #-}

-- | The data type of the column.
--
-- /Note:/ Consider using 'columnType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csColumnType :: Lens.Lens' ColumnStatistics Types.TypeString
csColumnType = Lens.field @"columnType"
{-# INLINEABLE csColumnType #-}
{-# DEPRECATED columnType "Use generic-lens or generic-optics with 'columnType' instead"  #-}

-- | The timestamp of when column statistics were generated.
--
-- /Note:/ Consider using 'analyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAnalyzedTime :: Lens.Lens' ColumnStatistics Core.NominalDiffTime
csAnalyzedTime = Lens.field @"analyzedTime"
{-# INLINEABLE csAnalyzedTime #-}
{-# DEPRECATED analyzedTime "Use generic-lens or generic-optics with 'analyzedTime' instead"  #-}

-- | A @ColumnStatisticData@ object that contains the statistics data values.
--
-- /Note:/ Consider using 'statisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStatisticsData :: Lens.Lens' ColumnStatistics Types.ColumnStatisticsData
csStatisticsData = Lens.field @"statisticsData"
{-# INLINEABLE csStatisticsData #-}
{-# DEPRECATED statisticsData "Use generic-lens or generic-optics with 'statisticsData' instead"  #-}

instance Core.FromJSON ColumnStatistics where
        toJSON ColumnStatistics{..}
          = Core.object
              (Core.catMaybes
                 [Core.Just ("ColumnName" Core..= columnName),
                  Core.Just ("ColumnType" Core..= columnType),
                  Core.Just ("AnalyzedTime" Core..= analyzedTime),
                  Core.Just ("StatisticsData" Core..= statisticsData)])

instance Core.FromJSON ColumnStatistics where
        parseJSON
          = Core.withObject "ColumnStatistics" Core.$
              \ x ->
                ColumnStatistics' Core.<$>
                  (x Core..: "ColumnName") Core.<*> x Core..: "ColumnType" Core.<*>
                    x Core..: "AnalyzedTime"
                    Core.<*> x Core..: "StatisticsData"
