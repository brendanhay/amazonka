{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.Types.ColumnStatistics
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Glue.Types.ColumnStatistics where

import Network.AWS.Glue.Types.ColumnStatisticsData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Represents the generated column-level statistics for a table or
-- partition.
--
-- /See:/ 'newColumnStatistics' smart constructor.
data ColumnStatistics = ColumnStatistics'
  { -- | Name of column which statistics belong to.
    columnName :: Prelude.Text,
    -- | The data type of the column.
    columnType :: Prelude.Text,
    -- | The timestamp of when column statistics were generated.
    analyzedTime :: Prelude.POSIX,
    -- | A @ColumnStatisticData@ object that contains the statistics data values.
    statisticsData :: ColumnStatisticsData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ColumnStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnName', 'columnStatistics_columnName' - Name of column which statistics belong to.
--
-- 'columnType', 'columnStatistics_columnType' - The data type of the column.
--
-- 'analyzedTime', 'columnStatistics_analyzedTime' - The timestamp of when column statistics were generated.
--
-- 'statisticsData', 'columnStatistics_statisticsData' - A @ColumnStatisticData@ object that contains the statistics data values.
newColumnStatistics ::
  -- | 'columnName'
  Prelude.Text ->
  -- | 'columnType'
  Prelude.Text ->
  -- | 'analyzedTime'
  Prelude.UTCTime ->
  -- | 'statisticsData'
  ColumnStatisticsData ->
  ColumnStatistics
newColumnStatistics
  pColumnName_
  pColumnType_
  pAnalyzedTime_
  pStatisticsData_ =
    ColumnStatistics'
      { columnName = pColumnName_,
        columnType = pColumnType_,
        analyzedTime = Prelude._Time Lens.# pAnalyzedTime_,
        statisticsData = pStatisticsData_
      }

-- | Name of column which statistics belong to.
columnStatistics_columnName :: Lens.Lens' ColumnStatistics Prelude.Text
columnStatistics_columnName = Lens.lens (\ColumnStatistics' {columnName} -> columnName) (\s@ColumnStatistics' {} a -> s {columnName = a} :: ColumnStatistics)

-- | The data type of the column.
columnStatistics_columnType :: Lens.Lens' ColumnStatistics Prelude.Text
columnStatistics_columnType = Lens.lens (\ColumnStatistics' {columnType} -> columnType) (\s@ColumnStatistics' {} a -> s {columnType = a} :: ColumnStatistics)

-- | The timestamp of when column statistics were generated.
columnStatistics_analyzedTime :: Lens.Lens' ColumnStatistics Prelude.UTCTime
columnStatistics_analyzedTime = Lens.lens (\ColumnStatistics' {analyzedTime} -> analyzedTime) (\s@ColumnStatistics' {} a -> s {analyzedTime = a} :: ColumnStatistics) Prelude.. Prelude._Time

-- | A @ColumnStatisticData@ object that contains the statistics data values.
columnStatistics_statisticsData :: Lens.Lens' ColumnStatistics ColumnStatisticsData
columnStatistics_statisticsData = Lens.lens (\ColumnStatistics' {statisticsData} -> statisticsData) (\s@ColumnStatistics' {} a -> s {statisticsData = a} :: ColumnStatistics)

instance Prelude.FromJSON ColumnStatistics where
  parseJSON =
    Prelude.withObject
      "ColumnStatistics"
      ( \x ->
          ColumnStatistics'
            Prelude.<$> (x Prelude..: "ColumnName")
            Prelude.<*> (x Prelude..: "ColumnType")
            Prelude.<*> (x Prelude..: "AnalyzedTime")
            Prelude.<*> (x Prelude..: "StatisticsData")
      )

instance Prelude.Hashable ColumnStatistics

instance Prelude.NFData ColumnStatistics

instance Prelude.ToJSON ColumnStatistics where
  toJSON ColumnStatistics' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ColumnName" Prelude..= columnName),
            Prelude.Just ("ColumnType" Prelude..= columnType),
            Prelude.Just
              ("AnalyzedTime" Prelude..= analyzedTime),
            Prelude.Just
              ("StatisticsData" Prelude..= statisticsData)
          ]
      )
