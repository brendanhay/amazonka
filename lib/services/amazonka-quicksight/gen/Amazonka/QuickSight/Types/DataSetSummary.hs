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
-- Module      : Amazonka.QuickSight.Types.DataSetSummary
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSetSummary where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.DataSetImportMode
import Amazonka.QuickSight.Types.RowLevelPermissionDataSet

-- | Dataset summary.
--
-- /See:/ 'newDataSetSummary' smart constructor.
data DataSetSummary = DataSetSummary'
  { -- | The last time that this dataset was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the dataset.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that this dataset was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | Whether or not the row level permission tags are applied.
    rowLevelPermissionTagConfigurationApplied :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates if the dataset has column level permission
    -- configured.
    columnLevelPermissionRulesApplied :: Prelude.Maybe Prelude.Bool,
    -- | A value that indicates whether you want to import the data into SPICE.
    importMode :: Prelude.Maybe DataSetImportMode,
    -- | The ID of the dataset.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | A display name for the dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | The row-level security configuration for the dataset.
    rowLevelPermissionDataSet :: Prelude.Maybe RowLevelPermissionDataSet
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSetSummary' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastUpdatedTime', 'dataSetSummary_lastUpdatedTime' - The last time that this dataset was updated.
--
-- 'arn', 'dataSetSummary_arn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'createdTime', 'dataSetSummary_createdTime' - The time that this dataset was created.
--
-- 'rowLevelPermissionTagConfigurationApplied', 'dataSetSummary_rowLevelPermissionTagConfigurationApplied' - Whether or not the row level permission tags are applied.
--
-- 'columnLevelPermissionRulesApplied', 'dataSetSummary_columnLevelPermissionRulesApplied' - A value that indicates if the dataset has column level permission
-- configured.
--
-- 'importMode', 'dataSetSummary_importMode' - A value that indicates whether you want to import the data into SPICE.
--
-- 'dataSetId', 'dataSetSummary_dataSetId' - The ID of the dataset.
--
-- 'name', 'dataSetSummary_name' - A display name for the dataset.
--
-- 'rowLevelPermissionDataSet', 'dataSetSummary_rowLevelPermissionDataSet' - The row-level security configuration for the dataset.
newDataSetSummary ::
  DataSetSummary
newDataSetSummary =
  DataSetSummary'
    { lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      rowLevelPermissionTagConfigurationApplied =
        Prelude.Nothing,
      columnLevelPermissionRulesApplied = Prelude.Nothing,
      importMode = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      name = Prelude.Nothing,
      rowLevelPermissionDataSet = Prelude.Nothing
    }

-- | The last time that this dataset was updated.
dataSetSummary_lastUpdatedTime :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.UTCTime)
dataSetSummary_lastUpdatedTime = Lens.lens (\DataSetSummary' {lastUpdatedTime} -> lastUpdatedTime) (\s@DataSetSummary' {} a -> s {lastUpdatedTime = a} :: DataSetSummary) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the dataset.
dataSetSummary_arn :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.Text)
dataSetSummary_arn = Lens.lens (\DataSetSummary' {arn} -> arn) (\s@DataSetSummary' {} a -> s {arn = a} :: DataSetSummary)

-- | The time that this dataset was created.
dataSetSummary_createdTime :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.UTCTime)
dataSetSummary_createdTime = Lens.lens (\DataSetSummary' {createdTime} -> createdTime) (\s@DataSetSummary' {} a -> s {createdTime = a} :: DataSetSummary) Prelude.. Lens.mapping Core._Time

-- | Whether or not the row level permission tags are applied.
dataSetSummary_rowLevelPermissionTagConfigurationApplied :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.Bool)
dataSetSummary_rowLevelPermissionTagConfigurationApplied = Lens.lens (\DataSetSummary' {rowLevelPermissionTagConfigurationApplied} -> rowLevelPermissionTagConfigurationApplied) (\s@DataSetSummary' {} a -> s {rowLevelPermissionTagConfigurationApplied = a} :: DataSetSummary)

-- | A value that indicates if the dataset has column level permission
-- configured.
dataSetSummary_columnLevelPermissionRulesApplied :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.Bool)
dataSetSummary_columnLevelPermissionRulesApplied = Lens.lens (\DataSetSummary' {columnLevelPermissionRulesApplied} -> columnLevelPermissionRulesApplied) (\s@DataSetSummary' {} a -> s {columnLevelPermissionRulesApplied = a} :: DataSetSummary)

-- | A value that indicates whether you want to import the data into SPICE.
dataSetSummary_importMode :: Lens.Lens' DataSetSummary (Prelude.Maybe DataSetImportMode)
dataSetSummary_importMode = Lens.lens (\DataSetSummary' {importMode} -> importMode) (\s@DataSetSummary' {} a -> s {importMode = a} :: DataSetSummary)

-- | The ID of the dataset.
dataSetSummary_dataSetId :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.Text)
dataSetSummary_dataSetId = Lens.lens (\DataSetSummary' {dataSetId} -> dataSetId) (\s@DataSetSummary' {} a -> s {dataSetId = a} :: DataSetSummary)

-- | A display name for the dataset.
dataSetSummary_name :: Lens.Lens' DataSetSummary (Prelude.Maybe Prelude.Text)
dataSetSummary_name = Lens.lens (\DataSetSummary' {name} -> name) (\s@DataSetSummary' {} a -> s {name = a} :: DataSetSummary)

-- | The row-level security configuration for the dataset.
dataSetSummary_rowLevelPermissionDataSet :: Lens.Lens' DataSetSummary (Prelude.Maybe RowLevelPermissionDataSet)
dataSetSummary_rowLevelPermissionDataSet = Lens.lens (\DataSetSummary' {rowLevelPermissionDataSet} -> rowLevelPermissionDataSet) (\s@DataSetSummary' {} a -> s {rowLevelPermissionDataSet = a} :: DataSetSummary)

instance Core.FromJSON DataSetSummary where
  parseJSON =
    Core.withObject
      "DataSetSummary"
      ( \x ->
          DataSetSummary'
            Prelude.<$> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> ( x
                            Core..:? "RowLevelPermissionTagConfigurationApplied"
                        )
            Prelude.<*> (x Core..:? "ColumnLevelPermissionRulesApplied")
            Prelude.<*> (x Core..:? "ImportMode")
            Prelude.<*> (x Core..:? "DataSetId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "RowLevelPermissionDataSet")
      )

instance Prelude.Hashable DataSetSummary where
  hashWithSalt _salt DataSetSummary' {..} =
    _salt `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` rowLevelPermissionTagConfigurationApplied
      `Prelude.hashWithSalt` columnLevelPermissionRulesApplied
      `Prelude.hashWithSalt` importMode
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` rowLevelPermissionDataSet

instance Prelude.NFData DataSetSummary where
  rnf DataSetSummary' {..} =
    Prelude.rnf lastUpdatedTime
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf createdTime
      `Prelude.seq` Prelude.rnf rowLevelPermissionTagConfigurationApplied
      `Prelude.seq` Prelude.rnf columnLevelPermissionRulesApplied
      `Prelude.seq` Prelude.rnf importMode
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf rowLevelPermissionDataSet
