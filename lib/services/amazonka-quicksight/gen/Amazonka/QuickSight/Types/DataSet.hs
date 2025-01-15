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
-- Module      : Amazonka.QuickSight.Types.DataSet
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.QuickSight.Types.DataSet where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types.ColumnGroup
import Amazonka.QuickSight.Types.ColumnLevelPermissionRule
import Amazonka.QuickSight.Types.DataSetImportMode
import Amazonka.QuickSight.Types.DataSetUsageConfiguration
import Amazonka.QuickSight.Types.FieldFolder
import Amazonka.QuickSight.Types.LogicalTable
import Amazonka.QuickSight.Types.OutputColumn
import Amazonka.QuickSight.Types.PhysicalTable
import Amazonka.QuickSight.Types.RowLevelPermissionDataSet
import Amazonka.QuickSight.Types.RowLevelPermissionTagConfiguration

-- | Dataset.
--
-- /See:/ 'newDataSet' smart constructor.
data DataSet = DataSet'
  { -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | Groupings of columns that work together in certain Amazon QuickSight
    -- features. Currently, only geospatial hierarchy is supported.
    columnGroups :: Prelude.Maybe (Prelude.NonEmpty ColumnGroup),
    -- | A set of one or more definitions of a
    -- @ @<https://docs.aws.amazon.com/quicksight/latest/APIReference/API_ColumnLevelPermissionRule.html ColumnLevelPermissionRule>@ @.
    columnLevelPermissionRules :: Prelude.Maybe (Prelude.NonEmpty ColumnLevelPermissionRule),
    -- | The amount of SPICE capacity used by this dataset. This is 0 if the
    -- dataset isn\'t imported into SPICE.
    consumedSpiceCapacityInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The time that this dataset was created.
    createdTime :: Prelude.Maybe Data.POSIX,
    -- | The ID of the dataset.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | The usage configuration to apply to child datasets that reference this
    -- dataset as a source.
    dataSetUsageConfiguration :: Prelude.Maybe DataSetUsageConfiguration,
    -- | The folder that contains fields and nested subfolders for your dataset.
    fieldFolders :: Prelude.Maybe (Prelude.HashMap Prelude.Text FieldFolder),
    -- | A value that indicates whether you want to import the data into SPICE.
    importMode :: Prelude.Maybe DataSetImportMode,
    -- | The last time that this dataset was updated.
    lastUpdatedTime :: Prelude.Maybe Data.POSIX,
    -- | Configures the combination and transformation of the data from the
    -- physical tables.
    logicalTableMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text LogicalTable),
    -- | A display name for the dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | The list of columns after all transforms. These columns are available in
    -- templates, analyses, and dashboards.
    outputColumns :: Prelude.Maybe [OutputColumn],
    -- | Declares the physical tables that are available in the underlying data
    -- sources.
    physicalTableMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text PhysicalTable),
    -- | The row-level security configuration for the dataset.
    rowLevelPermissionDataSet :: Prelude.Maybe RowLevelPermissionDataSet,
    -- | The element you can use to define tags for row-level security.
    rowLevelPermissionTagConfiguration :: Prelude.Maybe RowLevelPermissionTagConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DataSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'arn', 'dataSet_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'columnGroups', 'dataSet_columnGroups' - Groupings of columns that work together in certain Amazon QuickSight
-- features. Currently, only geospatial hierarchy is supported.
--
-- 'columnLevelPermissionRules', 'dataSet_columnLevelPermissionRules' - A set of one or more definitions of a
-- @ @<https://docs.aws.amazon.com/quicksight/latest/APIReference/API_ColumnLevelPermissionRule.html ColumnLevelPermissionRule>@ @.
--
-- 'consumedSpiceCapacityInBytes', 'dataSet_consumedSpiceCapacityInBytes' - The amount of SPICE capacity used by this dataset. This is 0 if the
-- dataset isn\'t imported into SPICE.
--
-- 'createdTime', 'dataSet_createdTime' - The time that this dataset was created.
--
-- 'dataSetId', 'dataSet_dataSetId' - The ID of the dataset.
--
-- 'dataSetUsageConfiguration', 'dataSet_dataSetUsageConfiguration' - The usage configuration to apply to child datasets that reference this
-- dataset as a source.
--
-- 'fieldFolders', 'dataSet_fieldFolders' - The folder that contains fields and nested subfolders for your dataset.
--
-- 'importMode', 'dataSet_importMode' - A value that indicates whether you want to import the data into SPICE.
--
-- 'lastUpdatedTime', 'dataSet_lastUpdatedTime' - The last time that this dataset was updated.
--
-- 'logicalTableMap', 'dataSet_logicalTableMap' - Configures the combination and transformation of the data from the
-- physical tables.
--
-- 'name', 'dataSet_name' - A display name for the dataset.
--
-- 'outputColumns', 'dataSet_outputColumns' - The list of columns after all transforms. These columns are available in
-- templates, analyses, and dashboards.
--
-- 'physicalTableMap', 'dataSet_physicalTableMap' - Declares the physical tables that are available in the underlying data
-- sources.
--
-- 'rowLevelPermissionDataSet', 'dataSet_rowLevelPermissionDataSet' - The row-level security configuration for the dataset.
--
-- 'rowLevelPermissionTagConfiguration', 'dataSet_rowLevelPermissionTagConfiguration' - The element you can use to define tags for row-level security.
newDataSet ::
  DataSet
newDataSet =
  DataSet'
    { arn = Prelude.Nothing,
      columnGroups = Prelude.Nothing,
      columnLevelPermissionRules = Prelude.Nothing,
      consumedSpiceCapacityInBytes = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      dataSetUsageConfiguration = Prelude.Nothing,
      fieldFolders = Prelude.Nothing,
      importMode = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      logicalTableMap = Prelude.Nothing,
      name = Prelude.Nothing,
      outputColumns = Prelude.Nothing,
      physicalTableMap = Prelude.Nothing,
      rowLevelPermissionDataSet = Prelude.Nothing,
      rowLevelPermissionTagConfiguration = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the resource.
dataSet_arn :: Lens.Lens' DataSet (Prelude.Maybe Prelude.Text)
dataSet_arn = Lens.lens (\DataSet' {arn} -> arn) (\s@DataSet' {} a -> s {arn = a} :: DataSet)

-- | Groupings of columns that work together in certain Amazon QuickSight
-- features. Currently, only geospatial hierarchy is supported.
dataSet_columnGroups :: Lens.Lens' DataSet (Prelude.Maybe (Prelude.NonEmpty ColumnGroup))
dataSet_columnGroups = Lens.lens (\DataSet' {columnGroups} -> columnGroups) (\s@DataSet' {} a -> s {columnGroups = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | A set of one or more definitions of a
-- @ @<https://docs.aws.amazon.com/quicksight/latest/APIReference/API_ColumnLevelPermissionRule.html ColumnLevelPermissionRule>@ @.
dataSet_columnLevelPermissionRules :: Lens.Lens' DataSet (Prelude.Maybe (Prelude.NonEmpty ColumnLevelPermissionRule))
dataSet_columnLevelPermissionRules = Lens.lens (\DataSet' {columnLevelPermissionRules} -> columnLevelPermissionRules) (\s@DataSet' {} a -> s {columnLevelPermissionRules = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | The amount of SPICE capacity used by this dataset. This is 0 if the
-- dataset isn\'t imported into SPICE.
dataSet_consumedSpiceCapacityInBytes :: Lens.Lens' DataSet (Prelude.Maybe Prelude.Integer)
dataSet_consumedSpiceCapacityInBytes = Lens.lens (\DataSet' {consumedSpiceCapacityInBytes} -> consumedSpiceCapacityInBytes) (\s@DataSet' {} a -> s {consumedSpiceCapacityInBytes = a} :: DataSet)

-- | The time that this dataset was created.
dataSet_createdTime :: Lens.Lens' DataSet (Prelude.Maybe Prelude.UTCTime)
dataSet_createdTime = Lens.lens (\DataSet' {createdTime} -> createdTime) (\s@DataSet' {} a -> s {createdTime = a} :: DataSet) Prelude.. Lens.mapping Data._Time

-- | The ID of the dataset.
dataSet_dataSetId :: Lens.Lens' DataSet (Prelude.Maybe Prelude.Text)
dataSet_dataSetId = Lens.lens (\DataSet' {dataSetId} -> dataSetId) (\s@DataSet' {} a -> s {dataSetId = a} :: DataSet)

-- | The usage configuration to apply to child datasets that reference this
-- dataset as a source.
dataSet_dataSetUsageConfiguration :: Lens.Lens' DataSet (Prelude.Maybe DataSetUsageConfiguration)
dataSet_dataSetUsageConfiguration = Lens.lens (\DataSet' {dataSetUsageConfiguration} -> dataSetUsageConfiguration) (\s@DataSet' {} a -> s {dataSetUsageConfiguration = a} :: DataSet)

-- | The folder that contains fields and nested subfolders for your dataset.
dataSet_fieldFolders :: Lens.Lens' DataSet (Prelude.Maybe (Prelude.HashMap Prelude.Text FieldFolder))
dataSet_fieldFolders = Lens.lens (\DataSet' {fieldFolders} -> fieldFolders) (\s@DataSet' {} a -> s {fieldFolders = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | A value that indicates whether you want to import the data into SPICE.
dataSet_importMode :: Lens.Lens' DataSet (Prelude.Maybe DataSetImportMode)
dataSet_importMode = Lens.lens (\DataSet' {importMode} -> importMode) (\s@DataSet' {} a -> s {importMode = a} :: DataSet)

-- | The last time that this dataset was updated.
dataSet_lastUpdatedTime :: Lens.Lens' DataSet (Prelude.Maybe Prelude.UTCTime)
dataSet_lastUpdatedTime = Lens.lens (\DataSet' {lastUpdatedTime} -> lastUpdatedTime) (\s@DataSet' {} a -> s {lastUpdatedTime = a} :: DataSet) Prelude.. Lens.mapping Data._Time

-- | Configures the combination and transformation of the data from the
-- physical tables.
dataSet_logicalTableMap :: Lens.Lens' DataSet (Prelude.Maybe (Prelude.HashMap Prelude.Text LogicalTable))
dataSet_logicalTableMap = Lens.lens (\DataSet' {logicalTableMap} -> logicalTableMap) (\s@DataSet' {} a -> s {logicalTableMap = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | A display name for the dataset.
dataSet_name :: Lens.Lens' DataSet (Prelude.Maybe Prelude.Text)
dataSet_name = Lens.lens (\DataSet' {name} -> name) (\s@DataSet' {} a -> s {name = a} :: DataSet)

-- | The list of columns after all transforms. These columns are available in
-- templates, analyses, and dashboards.
dataSet_outputColumns :: Lens.Lens' DataSet (Prelude.Maybe [OutputColumn])
dataSet_outputColumns = Lens.lens (\DataSet' {outputColumns} -> outputColumns) (\s@DataSet' {} a -> s {outputColumns = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | Declares the physical tables that are available in the underlying data
-- sources.
dataSet_physicalTableMap :: Lens.Lens' DataSet (Prelude.Maybe (Prelude.HashMap Prelude.Text PhysicalTable))
dataSet_physicalTableMap = Lens.lens (\DataSet' {physicalTableMap} -> physicalTableMap) (\s@DataSet' {} a -> s {physicalTableMap = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | The row-level security configuration for the dataset.
dataSet_rowLevelPermissionDataSet :: Lens.Lens' DataSet (Prelude.Maybe RowLevelPermissionDataSet)
dataSet_rowLevelPermissionDataSet = Lens.lens (\DataSet' {rowLevelPermissionDataSet} -> rowLevelPermissionDataSet) (\s@DataSet' {} a -> s {rowLevelPermissionDataSet = a} :: DataSet)

-- | The element you can use to define tags for row-level security.
dataSet_rowLevelPermissionTagConfiguration :: Lens.Lens' DataSet (Prelude.Maybe RowLevelPermissionTagConfiguration)
dataSet_rowLevelPermissionTagConfiguration = Lens.lens (\DataSet' {rowLevelPermissionTagConfiguration} -> rowLevelPermissionTagConfiguration) (\s@DataSet' {} a -> s {rowLevelPermissionTagConfiguration = a} :: DataSet)

instance Data.FromJSON DataSet where
  parseJSON =
    Data.withObject
      "DataSet"
      ( \x ->
          DataSet'
            Prelude.<$> (x Data..:? "Arn")
            Prelude.<*> (x Data..:? "ColumnGroups")
            Prelude.<*> (x Data..:? "ColumnLevelPermissionRules")
            Prelude.<*> (x Data..:? "ConsumedSpiceCapacityInBytes")
            Prelude.<*> (x Data..:? "CreatedTime")
            Prelude.<*> (x Data..:? "DataSetId")
            Prelude.<*> (x Data..:? "DataSetUsageConfiguration")
            Prelude.<*> (x Data..:? "FieldFolders" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "ImportMode")
            Prelude.<*> (x Data..:? "LastUpdatedTime")
            Prelude.<*> ( x
                            Data..:? "LogicalTableMap"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "Name")
            Prelude.<*> (x Data..:? "OutputColumns" Data..!= Prelude.mempty)
            Prelude.<*> ( x
                            Data..:? "PhysicalTableMap"
                            Data..!= Prelude.mempty
                        )
            Prelude.<*> (x Data..:? "RowLevelPermissionDataSet")
            Prelude.<*> (x Data..:? "RowLevelPermissionTagConfiguration")
      )

instance Prelude.Hashable DataSet where
  hashWithSalt _salt DataSet' {..} =
    _salt
      `Prelude.hashWithSalt` arn
      `Prelude.hashWithSalt` columnGroups
      `Prelude.hashWithSalt` columnLevelPermissionRules
      `Prelude.hashWithSalt` consumedSpiceCapacityInBytes
      `Prelude.hashWithSalt` createdTime
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` dataSetUsageConfiguration
      `Prelude.hashWithSalt` fieldFolders
      `Prelude.hashWithSalt` importMode
      `Prelude.hashWithSalt` lastUpdatedTime
      `Prelude.hashWithSalt` logicalTableMap
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` outputColumns
      `Prelude.hashWithSalt` physicalTableMap
      `Prelude.hashWithSalt` rowLevelPermissionDataSet
      `Prelude.hashWithSalt` rowLevelPermissionTagConfiguration

instance Prelude.NFData DataSet where
  rnf DataSet' {..} =
    Prelude.rnf arn `Prelude.seq`
      Prelude.rnf columnGroups `Prelude.seq`
        Prelude.rnf columnLevelPermissionRules `Prelude.seq`
          Prelude.rnf consumedSpiceCapacityInBytes `Prelude.seq`
            Prelude.rnf createdTime `Prelude.seq`
              Prelude.rnf dataSetId `Prelude.seq`
                Prelude.rnf dataSetUsageConfiguration `Prelude.seq`
                  Prelude.rnf fieldFolders `Prelude.seq`
                    Prelude.rnf importMode `Prelude.seq`
                      Prelude.rnf lastUpdatedTime `Prelude.seq`
                        Prelude.rnf logicalTableMap `Prelude.seq`
                          Prelude.rnf name `Prelude.seq`
                            Prelude.rnf outputColumns `Prelude.seq`
                              Prelude.rnf physicalTableMap `Prelude.seq`
                                Prelude.rnf rowLevelPermissionDataSet `Prelude.seq`
                                  Prelude.rnf
                                    rowLevelPermissionTagConfiguration
