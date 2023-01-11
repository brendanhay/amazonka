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
-- Module      : Amazonka.Glue.Types.ColumnStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Glue.Types.ColumnStatistics where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types.ColumnStatisticsData
import qualified Amazonka.Prelude as Prelude

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
    analyzedTime :: Data.POSIX,
    -- | A @ColumnStatisticData@ object that contains the statistics data values.
    statisticsData :: ColumnStatisticsData
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        analyzedTime = Data._Time Lens.# pAnalyzedTime_,
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
columnStatistics_analyzedTime = Lens.lens (\ColumnStatistics' {analyzedTime} -> analyzedTime) (\s@ColumnStatistics' {} a -> s {analyzedTime = a} :: ColumnStatistics) Prelude.. Data._Time

-- | A @ColumnStatisticData@ object that contains the statistics data values.
columnStatistics_statisticsData :: Lens.Lens' ColumnStatistics ColumnStatisticsData
columnStatistics_statisticsData = Lens.lens (\ColumnStatistics' {statisticsData} -> statisticsData) (\s@ColumnStatistics' {} a -> s {statisticsData = a} :: ColumnStatistics)

instance Data.FromJSON ColumnStatistics where
  parseJSON =
    Data.withObject
      "ColumnStatistics"
      ( \x ->
          ColumnStatistics'
            Prelude.<$> (x Data..: "ColumnName")
            Prelude.<*> (x Data..: "ColumnType")
            Prelude.<*> (x Data..: "AnalyzedTime")
            Prelude.<*> (x Data..: "StatisticsData")
      )

instance Prelude.Hashable ColumnStatistics where
  hashWithSalt _salt ColumnStatistics' {..} =
    _salt `Prelude.hashWithSalt` columnName
      `Prelude.hashWithSalt` columnType
      `Prelude.hashWithSalt` analyzedTime
      `Prelude.hashWithSalt` statisticsData

instance Prelude.NFData ColumnStatistics where
  rnf ColumnStatistics' {..} =
    Prelude.rnf columnName
      `Prelude.seq` Prelude.rnf columnType
      `Prelude.seq` Prelude.rnf analyzedTime
      `Prelude.seq` Prelude.rnf statisticsData

instance Data.ToJSON ColumnStatistics where
  toJSON ColumnStatistics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("ColumnName" Data..= columnName),
            Prelude.Just ("ColumnType" Data..= columnType),
            Prelude.Just ("AnalyzedTime" Data..= analyzedTime),
            Prelude.Just
              ("StatisticsData" Data..= statisticsData)
          ]
      )
