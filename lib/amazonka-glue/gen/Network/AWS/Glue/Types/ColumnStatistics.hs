{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatistics
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatistics
  ( ColumnStatistics (..),

    -- * Smart constructor
    mkColumnStatistics,

    -- * Lenses
    csColumnName,
    csColumnType,
    csAnalyzedTime,
    csStatisticsData,
  )
where

import qualified Network.AWS.Glue.Types.ColumnStatisticsData as Types
import qualified Network.AWS.Glue.Types.NameString as Types
import qualified Network.AWS.Glue.Types.TypeString as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Represents the generated column-level statistics for a table or partition.
--
-- /See:/ 'mkColumnStatistics' smart constructor.
data ColumnStatistics = ColumnStatistics'
  { -- | Name of column which statistics belong to.
    columnName :: Types.NameString,
    -- | The data type of the column.
    columnType :: Types.TypeString,
    -- | The timestamp of when column statistics were generated.
    analyzedTime :: Core.NominalDiffTime,
    -- | A @ColumnStatisticData@ object that contains the statistics data values.
    statisticsData :: Types.ColumnStatisticsData
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'ColumnStatistics' value with any optional fields omitted.
mkColumnStatistics ::
  -- | 'columnName'
  Types.NameString ->
  -- | 'columnType'
  Types.TypeString ->
  -- | 'analyzedTime'
  Core.NominalDiffTime ->
  -- | 'statisticsData'
  Types.ColumnStatisticsData ->
  ColumnStatistics
mkColumnStatistics
  columnName
  columnType
  analyzedTime
  statisticsData =
    ColumnStatistics'
      { columnName,
        columnType,
        analyzedTime,
        statisticsData
      }

-- | Name of column which statistics belong to.
--
-- /Note:/ Consider using 'columnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csColumnName :: Lens.Lens' ColumnStatistics Types.NameString
csColumnName = Lens.field @"columnName"
{-# DEPRECATED csColumnName "Use generic-lens or generic-optics with 'columnName' instead." #-}

-- | The data type of the column.
--
-- /Note:/ Consider using 'columnType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csColumnType :: Lens.Lens' ColumnStatistics Types.TypeString
csColumnType = Lens.field @"columnType"
{-# DEPRECATED csColumnType "Use generic-lens or generic-optics with 'columnType' instead." #-}

-- | The timestamp of when column statistics were generated.
--
-- /Note:/ Consider using 'analyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAnalyzedTime :: Lens.Lens' ColumnStatistics Core.NominalDiffTime
csAnalyzedTime = Lens.field @"analyzedTime"
{-# DEPRECATED csAnalyzedTime "Use generic-lens or generic-optics with 'analyzedTime' instead." #-}

-- | A @ColumnStatisticData@ object that contains the statistics data values.
--
-- /Note:/ Consider using 'statisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStatisticsData :: Lens.Lens' ColumnStatistics Types.ColumnStatisticsData
csStatisticsData = Lens.field @"statisticsData"
{-# DEPRECATED csStatisticsData "Use generic-lens or generic-optics with 'statisticsData' instead." #-}

instance Core.FromJSON ColumnStatistics where
  toJSON ColumnStatistics {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("ColumnName" Core..= columnName),
            Core.Just ("ColumnType" Core..= columnType),
            Core.Just ("AnalyzedTime" Core..= analyzedTime),
            Core.Just ("StatisticsData" Core..= statisticsData)
          ]
      )

instance Core.FromJSON ColumnStatistics where
  parseJSON =
    Core.withObject "ColumnStatistics" Core.$
      \x ->
        ColumnStatistics'
          Core.<$> (x Core..: "ColumnName")
          Core.<*> (x Core..: "ColumnType")
          Core.<*> (x Core..: "AnalyzedTime")
          Core.<*> (x Core..: "StatisticsData")
