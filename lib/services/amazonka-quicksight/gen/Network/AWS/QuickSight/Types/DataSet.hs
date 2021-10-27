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
-- Module      : Network.AWS.QuickSight.Types.DataSet
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.QuickSight.Types.DataSet where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.QuickSight.Types.ColumnGroup
import Network.AWS.QuickSight.Types.ColumnLevelPermissionRule
import Network.AWS.QuickSight.Types.DataSetImportMode
import Network.AWS.QuickSight.Types.DataSetUsageConfiguration
import Network.AWS.QuickSight.Types.FieldFolder
import Network.AWS.QuickSight.Types.LogicalTable
import Network.AWS.QuickSight.Types.OutputColumn
import Network.AWS.QuickSight.Types.PhysicalTable
import Network.AWS.QuickSight.Types.RowLevelPermissionDataSet
import Network.AWS.QuickSight.Types.RowLevelPermissionTagConfiguration

-- | Dataset.
--
-- /See:/ 'newDataSet' smart constructor.
data DataSet = DataSet'
  { -- | The folder that contains fields and nested subfolders for your dataset.
    fieldFolders :: Prelude.Maybe (Prelude.HashMap Prelude.Text FieldFolder),
    -- | Groupings of columns that work together in certain Amazon QuickSight
    -- features. Currently, only geospatial hierarchy is supported.
    columnGroups :: Prelude.Maybe (Prelude.NonEmpty ColumnGroup),
    -- | The last time that this dataset was updated.
    lastUpdatedTime :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the resource.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The time that this dataset was created.
    createdTime :: Prelude.Maybe Core.POSIX,
    -- | The amount of SPICE capacity used by this dataset. This is 0 if the
    -- dataset isn\'t imported into SPICE.
    consumedSpiceCapacityInBytes :: Prelude.Maybe Prelude.Integer,
    -- | A value that indicates whether you want to import the data into SPICE.
    importMode :: Prelude.Maybe DataSetImportMode,
    -- | Declares the physical tables that are available in the underlying data
    -- sources.
    physicalTableMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text PhysicalTable),
    -- | The ID of the dataset.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | A display name for the dataset.
    name :: Prelude.Maybe Prelude.Text,
    -- | The usage configuration to apply to child datasets that reference this
    -- dataset as a source.
    dataSetUsageConfiguration :: Prelude.Maybe DataSetUsageConfiguration,
    -- | The list of columns after all transforms. These columns are available in
    -- templates, analyses, and dashboards.
    outputColumns :: Prelude.Maybe [OutputColumn],
    -- | The element you can use to define tags for row-level security.
    rowLevelPermissionTagConfiguration :: Prelude.Maybe RowLevelPermissionTagConfiguration,
    -- | The row-level security configuration for the dataset.
    rowLevelPermissionDataSet :: Prelude.Maybe RowLevelPermissionDataSet,
    -- | A set of one or more definitions of a @ ColumnLevelPermissionRule @.
    columnLevelPermissionRules :: Prelude.Maybe (Prelude.NonEmpty ColumnLevelPermissionRule),
    -- | Configures the combination and transformation of the data from the
    -- physical tables.
    logicalTableMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text LogicalTable)
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
-- 'fieldFolders', 'dataSet_fieldFolders' - The folder that contains fields and nested subfolders for your dataset.
--
-- 'columnGroups', 'dataSet_columnGroups' - Groupings of columns that work together in certain Amazon QuickSight
-- features. Currently, only geospatial hierarchy is supported.
--
-- 'lastUpdatedTime', 'dataSet_lastUpdatedTime' - The last time that this dataset was updated.
--
-- 'arn', 'dataSet_arn' - The Amazon Resource Name (ARN) of the resource.
--
-- 'createdTime', 'dataSet_createdTime' - The time that this dataset was created.
--
-- 'consumedSpiceCapacityInBytes', 'dataSet_consumedSpiceCapacityInBytes' - The amount of SPICE capacity used by this dataset. This is 0 if the
-- dataset isn\'t imported into SPICE.
--
-- 'importMode', 'dataSet_importMode' - A value that indicates whether you want to import the data into SPICE.
--
-- 'physicalTableMap', 'dataSet_physicalTableMap' - Declares the physical tables that are available in the underlying data
-- sources.
--
-- 'dataSetId', 'dataSet_dataSetId' - The ID of the dataset.
--
-- 'name', 'dataSet_name' - A display name for the dataset.
--
-- 'dataSetUsageConfiguration', 'dataSet_dataSetUsageConfiguration' - The usage configuration to apply to child datasets that reference this
-- dataset as a source.
--
-- 'outputColumns', 'dataSet_outputColumns' - The list of columns after all transforms. These columns are available in
-- templates, analyses, and dashboards.
--
-- 'rowLevelPermissionTagConfiguration', 'dataSet_rowLevelPermissionTagConfiguration' - The element you can use to define tags for row-level security.
--
-- 'rowLevelPermissionDataSet', 'dataSet_rowLevelPermissionDataSet' - The row-level security configuration for the dataset.
--
-- 'columnLevelPermissionRules', 'dataSet_columnLevelPermissionRules' - A set of one or more definitions of a @ ColumnLevelPermissionRule @.
--
-- 'logicalTableMap', 'dataSet_logicalTableMap' - Configures the combination and transformation of the data from the
-- physical tables.
newDataSet ::
  DataSet
newDataSet =
  DataSet'
    { fieldFolders = Prelude.Nothing,
      columnGroups = Prelude.Nothing,
      lastUpdatedTime = Prelude.Nothing,
      arn = Prelude.Nothing,
      createdTime = Prelude.Nothing,
      consumedSpiceCapacityInBytes = Prelude.Nothing,
      importMode = Prelude.Nothing,
      physicalTableMap = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      name = Prelude.Nothing,
      dataSetUsageConfiguration = Prelude.Nothing,
      outputColumns = Prelude.Nothing,
      rowLevelPermissionTagConfiguration = Prelude.Nothing,
      rowLevelPermissionDataSet = Prelude.Nothing,
      columnLevelPermissionRules = Prelude.Nothing,
      logicalTableMap = Prelude.Nothing
    }

-- | The folder that contains fields and nested subfolders for your dataset.
dataSet_fieldFolders :: Lens.Lens' DataSet (Prelude.Maybe (Prelude.HashMap Prelude.Text FieldFolder))
dataSet_fieldFolders = Lens.lens (\DataSet' {fieldFolders} -> fieldFolders) (\s@DataSet' {} a -> s {fieldFolders = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | Groupings of columns that work together in certain Amazon QuickSight
-- features. Currently, only geospatial hierarchy is supported.
dataSet_columnGroups :: Lens.Lens' DataSet (Prelude.Maybe (Prelude.NonEmpty ColumnGroup))
dataSet_columnGroups = Lens.lens (\DataSet' {columnGroups} -> columnGroups) (\s@DataSet' {} a -> s {columnGroups = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | The last time that this dataset was updated.
dataSet_lastUpdatedTime :: Lens.Lens' DataSet (Prelude.Maybe Prelude.UTCTime)
dataSet_lastUpdatedTime = Lens.lens (\DataSet' {lastUpdatedTime} -> lastUpdatedTime) (\s@DataSet' {} a -> s {lastUpdatedTime = a} :: DataSet) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the resource.
dataSet_arn :: Lens.Lens' DataSet (Prelude.Maybe Prelude.Text)
dataSet_arn = Lens.lens (\DataSet' {arn} -> arn) (\s@DataSet' {} a -> s {arn = a} :: DataSet)

-- | The time that this dataset was created.
dataSet_createdTime :: Lens.Lens' DataSet (Prelude.Maybe Prelude.UTCTime)
dataSet_createdTime = Lens.lens (\DataSet' {createdTime} -> createdTime) (\s@DataSet' {} a -> s {createdTime = a} :: DataSet) Prelude.. Lens.mapping Core._Time

-- | The amount of SPICE capacity used by this dataset. This is 0 if the
-- dataset isn\'t imported into SPICE.
dataSet_consumedSpiceCapacityInBytes :: Lens.Lens' DataSet (Prelude.Maybe Prelude.Integer)
dataSet_consumedSpiceCapacityInBytes = Lens.lens (\DataSet' {consumedSpiceCapacityInBytes} -> consumedSpiceCapacityInBytes) (\s@DataSet' {} a -> s {consumedSpiceCapacityInBytes = a} :: DataSet)

-- | A value that indicates whether you want to import the data into SPICE.
dataSet_importMode :: Lens.Lens' DataSet (Prelude.Maybe DataSetImportMode)
dataSet_importMode = Lens.lens (\DataSet' {importMode} -> importMode) (\s@DataSet' {} a -> s {importMode = a} :: DataSet)

-- | Declares the physical tables that are available in the underlying data
-- sources.
dataSet_physicalTableMap :: Lens.Lens' DataSet (Prelude.Maybe (Prelude.HashMap Prelude.Text PhysicalTable))
dataSet_physicalTableMap = Lens.lens (\DataSet' {physicalTableMap} -> physicalTableMap) (\s@DataSet' {} a -> s {physicalTableMap = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the dataset.
dataSet_dataSetId :: Lens.Lens' DataSet (Prelude.Maybe Prelude.Text)
dataSet_dataSetId = Lens.lens (\DataSet' {dataSetId} -> dataSetId) (\s@DataSet' {} a -> s {dataSetId = a} :: DataSet)

-- | A display name for the dataset.
dataSet_name :: Lens.Lens' DataSet (Prelude.Maybe Prelude.Text)
dataSet_name = Lens.lens (\DataSet' {name} -> name) (\s@DataSet' {} a -> s {name = a} :: DataSet)

-- | The usage configuration to apply to child datasets that reference this
-- dataset as a source.
dataSet_dataSetUsageConfiguration :: Lens.Lens' DataSet (Prelude.Maybe DataSetUsageConfiguration)
dataSet_dataSetUsageConfiguration = Lens.lens (\DataSet' {dataSetUsageConfiguration} -> dataSetUsageConfiguration) (\s@DataSet' {} a -> s {dataSetUsageConfiguration = a} :: DataSet)

-- | The list of columns after all transforms. These columns are available in
-- templates, analyses, and dashboards.
dataSet_outputColumns :: Lens.Lens' DataSet (Prelude.Maybe [OutputColumn])
dataSet_outputColumns = Lens.lens (\DataSet' {outputColumns} -> outputColumns) (\s@DataSet' {} a -> s {outputColumns = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | The element you can use to define tags for row-level security.
dataSet_rowLevelPermissionTagConfiguration :: Lens.Lens' DataSet (Prelude.Maybe RowLevelPermissionTagConfiguration)
dataSet_rowLevelPermissionTagConfiguration = Lens.lens (\DataSet' {rowLevelPermissionTagConfiguration} -> rowLevelPermissionTagConfiguration) (\s@DataSet' {} a -> s {rowLevelPermissionTagConfiguration = a} :: DataSet)

-- | The row-level security configuration for the dataset.
dataSet_rowLevelPermissionDataSet :: Lens.Lens' DataSet (Prelude.Maybe RowLevelPermissionDataSet)
dataSet_rowLevelPermissionDataSet = Lens.lens (\DataSet' {rowLevelPermissionDataSet} -> rowLevelPermissionDataSet) (\s@DataSet' {} a -> s {rowLevelPermissionDataSet = a} :: DataSet)

-- | A set of one or more definitions of a @ ColumnLevelPermissionRule @.
dataSet_columnLevelPermissionRules :: Lens.Lens' DataSet (Prelude.Maybe (Prelude.NonEmpty ColumnLevelPermissionRule))
dataSet_columnLevelPermissionRules = Lens.lens (\DataSet' {columnLevelPermissionRules} -> columnLevelPermissionRules) (\s@DataSet' {} a -> s {columnLevelPermissionRules = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

-- | Configures the combination and transformation of the data from the
-- physical tables.
dataSet_logicalTableMap :: Lens.Lens' DataSet (Prelude.Maybe (Prelude.HashMap Prelude.Text LogicalTable))
dataSet_logicalTableMap = Lens.lens (\DataSet' {logicalTableMap} -> logicalTableMap) (\s@DataSet' {} a -> s {logicalTableMap = a} :: DataSet) Prelude.. Lens.mapping Lens.coerced

instance Core.FromJSON DataSet where
  parseJSON =
    Core.withObject
      "DataSet"
      ( \x ->
          DataSet'
            Prelude.<$> (x Core..:? "FieldFolders" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "ColumnGroups")
            Prelude.<*> (x Core..:? "LastUpdatedTime")
            Prelude.<*> (x Core..:? "Arn")
            Prelude.<*> (x Core..:? "CreatedTime")
            Prelude.<*> (x Core..:? "ConsumedSpiceCapacityInBytes")
            Prelude.<*> (x Core..:? "ImportMode")
            Prelude.<*> ( x Core..:? "PhysicalTableMap"
                            Core..!= Prelude.mempty
                        )
            Prelude.<*> (x Core..:? "DataSetId")
            Prelude.<*> (x Core..:? "Name")
            Prelude.<*> (x Core..:? "DataSetUsageConfiguration")
            Prelude.<*> (x Core..:? "OutputColumns" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "RowLevelPermissionTagConfiguration")
            Prelude.<*> (x Core..:? "RowLevelPermissionDataSet")
            Prelude.<*> (x Core..:? "ColumnLevelPermissionRules")
            Prelude.<*> ( x Core..:? "LogicalTableMap"
                            Core..!= Prelude.mempty
                        )
      )

instance Prelude.Hashable DataSet

instance Prelude.NFData DataSet
