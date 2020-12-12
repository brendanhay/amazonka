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

import Network.AWS.Glue.Types.ColumnStatisticsData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents the generated column-level statistics for a table or partition.
--
-- /See:/ 'mkColumnStatistics' smart constructor.
data ColumnStatistics = ColumnStatistics'
  { columnName :: Lude.Text,
    columnType :: Lude.Text,
    analyzedTime :: Lude.Timestamp,
    statisticsData :: ColumnStatisticsData
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ColumnStatistics' with the minimum fields required to make a request.
--
-- * 'analyzedTime' - The timestamp of when column statistics were generated.
-- * 'columnName' - Name of column which statistics belong to.
-- * 'columnType' - The data type of the column.
-- * 'statisticsData' - A @ColumnStatisticData@ object that contains the statistics data values.
mkColumnStatistics ::
  -- | 'columnName'
  Lude.Text ->
  -- | 'columnType'
  Lude.Text ->
  -- | 'analyzedTime'
  Lude.Timestamp ->
  -- | 'statisticsData'
  ColumnStatisticsData ->
  ColumnStatistics
mkColumnStatistics
  pColumnName_
  pColumnType_
  pAnalyzedTime_
  pStatisticsData_ =
    ColumnStatistics'
      { columnName = pColumnName_,
        columnType = pColumnType_,
        analyzedTime = pAnalyzedTime_,
        statisticsData = pStatisticsData_
      }

-- | Name of column which statistics belong to.
--
-- /Note:/ Consider using 'columnName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csColumnName :: Lens.Lens' ColumnStatistics Lude.Text
csColumnName = Lens.lens (columnName :: ColumnStatistics -> Lude.Text) (\s a -> s {columnName = a} :: ColumnStatistics)
{-# DEPRECATED csColumnName "Use generic-lens or generic-optics with 'columnName' instead." #-}

-- | The data type of the column.
--
-- /Note:/ Consider using 'columnType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csColumnType :: Lens.Lens' ColumnStatistics Lude.Text
csColumnType = Lens.lens (columnType :: ColumnStatistics -> Lude.Text) (\s a -> s {columnType = a} :: ColumnStatistics)
{-# DEPRECATED csColumnType "Use generic-lens or generic-optics with 'columnType' instead." #-}

-- | The timestamp of when column statistics were generated.
--
-- /Note:/ Consider using 'analyzedTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csAnalyzedTime :: Lens.Lens' ColumnStatistics Lude.Timestamp
csAnalyzedTime = Lens.lens (analyzedTime :: ColumnStatistics -> Lude.Timestamp) (\s a -> s {analyzedTime = a} :: ColumnStatistics)
{-# DEPRECATED csAnalyzedTime "Use generic-lens or generic-optics with 'analyzedTime' instead." #-}

-- | A @ColumnStatisticData@ object that contains the statistics data values.
--
-- /Note:/ Consider using 'statisticsData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csStatisticsData :: Lens.Lens' ColumnStatistics ColumnStatisticsData
csStatisticsData = Lens.lens (statisticsData :: ColumnStatistics -> ColumnStatisticsData) (\s a -> s {statisticsData = a} :: ColumnStatistics)
{-# DEPRECATED csStatisticsData "Use generic-lens or generic-optics with 'statisticsData' instead." #-}

instance Lude.FromJSON ColumnStatistics where
  parseJSON =
    Lude.withObject
      "ColumnStatistics"
      ( \x ->
          ColumnStatistics'
            Lude.<$> (x Lude..: "ColumnName")
            Lude.<*> (x Lude..: "ColumnType")
            Lude.<*> (x Lude..: "AnalyzedTime")
            Lude.<*> (x Lude..: "StatisticsData")
      )

instance Lude.ToJSON ColumnStatistics where
  toJSON ColumnStatistics' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ColumnName" Lude..= columnName),
            Lude.Just ("ColumnType" Lude..= columnType),
            Lude.Just ("AnalyzedTime" Lude..= analyzedTime),
            Lude.Just ("StatisticsData" Lude..= statisticsData)
          ]
      )
