{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.QuickSight.UpdateDataSet
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates a dataset. This operation doesn\'t support datasets that include
-- uploaded files as a source. Partial updates are not supported by this
-- operation.
module Amazonka.QuickSight.UpdateDataSet
  ( -- * Creating a Request
    UpdateDataSet (..),
    newUpdateDataSet,

    -- * Request Lenses
    updateDataSet_columnLevelPermissionRules,
    updateDataSet_dataSetUsageConfiguration,
    updateDataSet_rowLevelPermissionTagConfiguration,
    updateDataSet_columnGroups,
    updateDataSet_fieldFolders,
    updateDataSet_rowLevelPermissionDataSet,
    updateDataSet_logicalTableMap,
    updateDataSet_awsAccountId,
    updateDataSet_dataSetId,
    updateDataSet_name,
    updateDataSet_physicalTableMap,
    updateDataSet_importMode,

    -- * Destructuring the Response
    UpdateDataSetResponse (..),
    newUpdateDataSetResponse,

    -- * Response Lenses
    updateDataSetResponse_ingestionArn,
    updateDataSetResponse_requestId,
    updateDataSetResponse_arn,
    updateDataSetResponse_dataSetId,
    updateDataSetResponse_ingestionId,
    updateDataSetResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateDataSet' smart constructor.
data UpdateDataSet = UpdateDataSet'
  { -- | A set of one or more definitions of a @ ColumnLevelPermissionRule @.
    columnLevelPermissionRules :: Prelude.Maybe (Prelude.NonEmpty ColumnLevelPermissionRule),
    dataSetUsageConfiguration :: Prelude.Maybe DataSetUsageConfiguration,
    -- | The configuration of tags on a dataset to set row-level security.
    -- Row-level security tags are currently supported for anonymous embedding
    -- only.
    rowLevelPermissionTagConfiguration :: Prelude.Maybe RowLevelPermissionTagConfiguration,
    -- | Groupings of columns that work together in certain Amazon QuickSight
    -- features. Currently, only geospatial hierarchy is supported.
    columnGroups :: Prelude.Maybe (Prelude.NonEmpty ColumnGroup),
    -- | The folder that contains fields and nested subfolders for your dataset.
    fieldFolders :: Prelude.Maybe (Prelude.HashMap Prelude.Text FieldFolder),
    -- | The row-level security configuration for the data you want to create.
    rowLevelPermissionDataSet :: Prelude.Maybe RowLevelPermissionDataSet,
    -- | Configures the combination and transformation of the data from the
    -- physical tables.
    logicalTableMap :: Prelude.Maybe (Prelude.HashMap Prelude.Text LogicalTable),
    -- | The Amazon Web Services account ID.
    awsAccountId :: Prelude.Text,
    -- | The ID for the dataset that you want to update. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    dataSetId :: Prelude.Text,
    -- | The display name for the dataset.
    name :: Prelude.Text,
    -- | Declares the physical tables that are available in the underlying data
    -- sources.
    physicalTableMap :: Prelude.HashMap Prelude.Text PhysicalTable,
    -- | Indicates whether you want to import the data into SPICE.
    importMode :: DataSetImportMode
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSet' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'columnLevelPermissionRules', 'updateDataSet_columnLevelPermissionRules' - A set of one or more definitions of a @ ColumnLevelPermissionRule @.
--
-- 'dataSetUsageConfiguration', 'updateDataSet_dataSetUsageConfiguration' - Undocumented member.
--
-- 'rowLevelPermissionTagConfiguration', 'updateDataSet_rowLevelPermissionTagConfiguration' - The configuration of tags on a dataset to set row-level security.
-- Row-level security tags are currently supported for anonymous embedding
-- only.
--
-- 'columnGroups', 'updateDataSet_columnGroups' - Groupings of columns that work together in certain Amazon QuickSight
-- features. Currently, only geospatial hierarchy is supported.
--
-- 'fieldFolders', 'updateDataSet_fieldFolders' - The folder that contains fields and nested subfolders for your dataset.
--
-- 'rowLevelPermissionDataSet', 'updateDataSet_rowLevelPermissionDataSet' - The row-level security configuration for the data you want to create.
--
-- 'logicalTableMap', 'updateDataSet_logicalTableMap' - Configures the combination and transformation of the data from the
-- physical tables.
--
-- 'awsAccountId', 'updateDataSet_awsAccountId' - The Amazon Web Services account ID.
--
-- 'dataSetId', 'updateDataSet_dataSetId' - The ID for the dataset that you want to update. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'name', 'updateDataSet_name' - The display name for the dataset.
--
-- 'physicalTableMap', 'updateDataSet_physicalTableMap' - Declares the physical tables that are available in the underlying data
-- sources.
--
-- 'importMode', 'updateDataSet_importMode' - Indicates whether you want to import the data into SPICE.
newUpdateDataSet ::
  -- | 'awsAccountId'
  Prelude.Text ->
  -- | 'dataSetId'
  Prelude.Text ->
  -- | 'name'
  Prelude.Text ->
  -- | 'importMode'
  DataSetImportMode ->
  UpdateDataSet
newUpdateDataSet
  pAwsAccountId_
  pDataSetId_
  pName_
  pImportMode_ =
    UpdateDataSet'
      { columnLevelPermissionRules =
          Prelude.Nothing,
        dataSetUsageConfiguration = Prelude.Nothing,
        rowLevelPermissionTagConfiguration = Prelude.Nothing,
        columnGroups = Prelude.Nothing,
        fieldFolders = Prelude.Nothing,
        rowLevelPermissionDataSet = Prelude.Nothing,
        logicalTableMap = Prelude.Nothing,
        awsAccountId = pAwsAccountId_,
        dataSetId = pDataSetId_,
        name = pName_,
        physicalTableMap = Prelude.mempty,
        importMode = pImportMode_
      }

-- | A set of one or more definitions of a @ ColumnLevelPermissionRule @.
updateDataSet_columnLevelPermissionRules :: Lens.Lens' UpdateDataSet (Prelude.Maybe (Prelude.NonEmpty ColumnLevelPermissionRule))
updateDataSet_columnLevelPermissionRules = Lens.lens (\UpdateDataSet' {columnLevelPermissionRules} -> columnLevelPermissionRules) (\s@UpdateDataSet' {} a -> s {columnLevelPermissionRules = a} :: UpdateDataSet) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
updateDataSet_dataSetUsageConfiguration :: Lens.Lens' UpdateDataSet (Prelude.Maybe DataSetUsageConfiguration)
updateDataSet_dataSetUsageConfiguration = Lens.lens (\UpdateDataSet' {dataSetUsageConfiguration} -> dataSetUsageConfiguration) (\s@UpdateDataSet' {} a -> s {dataSetUsageConfiguration = a} :: UpdateDataSet)

-- | The configuration of tags on a dataset to set row-level security.
-- Row-level security tags are currently supported for anonymous embedding
-- only.
updateDataSet_rowLevelPermissionTagConfiguration :: Lens.Lens' UpdateDataSet (Prelude.Maybe RowLevelPermissionTagConfiguration)
updateDataSet_rowLevelPermissionTagConfiguration = Lens.lens (\UpdateDataSet' {rowLevelPermissionTagConfiguration} -> rowLevelPermissionTagConfiguration) (\s@UpdateDataSet' {} a -> s {rowLevelPermissionTagConfiguration = a} :: UpdateDataSet)

-- | Groupings of columns that work together in certain Amazon QuickSight
-- features. Currently, only geospatial hierarchy is supported.
updateDataSet_columnGroups :: Lens.Lens' UpdateDataSet (Prelude.Maybe (Prelude.NonEmpty ColumnGroup))
updateDataSet_columnGroups = Lens.lens (\UpdateDataSet' {columnGroups} -> columnGroups) (\s@UpdateDataSet' {} a -> s {columnGroups = a} :: UpdateDataSet) Prelude.. Lens.mapping Lens.coerced

-- | The folder that contains fields and nested subfolders for your dataset.
updateDataSet_fieldFolders :: Lens.Lens' UpdateDataSet (Prelude.Maybe (Prelude.HashMap Prelude.Text FieldFolder))
updateDataSet_fieldFolders = Lens.lens (\UpdateDataSet' {fieldFolders} -> fieldFolders) (\s@UpdateDataSet' {} a -> s {fieldFolders = a} :: UpdateDataSet) Prelude.. Lens.mapping Lens.coerced

-- | The row-level security configuration for the data you want to create.
updateDataSet_rowLevelPermissionDataSet :: Lens.Lens' UpdateDataSet (Prelude.Maybe RowLevelPermissionDataSet)
updateDataSet_rowLevelPermissionDataSet = Lens.lens (\UpdateDataSet' {rowLevelPermissionDataSet} -> rowLevelPermissionDataSet) (\s@UpdateDataSet' {} a -> s {rowLevelPermissionDataSet = a} :: UpdateDataSet)

-- | Configures the combination and transformation of the data from the
-- physical tables.
updateDataSet_logicalTableMap :: Lens.Lens' UpdateDataSet (Prelude.Maybe (Prelude.HashMap Prelude.Text LogicalTable))
updateDataSet_logicalTableMap = Lens.lens (\UpdateDataSet' {logicalTableMap} -> logicalTableMap) (\s@UpdateDataSet' {} a -> s {logicalTableMap = a} :: UpdateDataSet) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Web Services account ID.
updateDataSet_awsAccountId :: Lens.Lens' UpdateDataSet Prelude.Text
updateDataSet_awsAccountId = Lens.lens (\UpdateDataSet' {awsAccountId} -> awsAccountId) (\s@UpdateDataSet' {} a -> s {awsAccountId = a} :: UpdateDataSet)

-- | The ID for the dataset that you want to update. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
updateDataSet_dataSetId :: Lens.Lens' UpdateDataSet Prelude.Text
updateDataSet_dataSetId = Lens.lens (\UpdateDataSet' {dataSetId} -> dataSetId) (\s@UpdateDataSet' {} a -> s {dataSetId = a} :: UpdateDataSet)

-- | The display name for the dataset.
updateDataSet_name :: Lens.Lens' UpdateDataSet Prelude.Text
updateDataSet_name = Lens.lens (\UpdateDataSet' {name} -> name) (\s@UpdateDataSet' {} a -> s {name = a} :: UpdateDataSet)

-- | Declares the physical tables that are available in the underlying data
-- sources.
updateDataSet_physicalTableMap :: Lens.Lens' UpdateDataSet (Prelude.HashMap Prelude.Text PhysicalTable)
updateDataSet_physicalTableMap = Lens.lens (\UpdateDataSet' {physicalTableMap} -> physicalTableMap) (\s@UpdateDataSet' {} a -> s {physicalTableMap = a} :: UpdateDataSet) Prelude.. Lens.coerced

-- | Indicates whether you want to import the data into SPICE.
updateDataSet_importMode :: Lens.Lens' UpdateDataSet DataSetImportMode
updateDataSet_importMode = Lens.lens (\UpdateDataSet' {importMode} -> importMode) (\s@UpdateDataSet' {} a -> s {importMode = a} :: UpdateDataSet)

instance Core.AWSRequest UpdateDataSet where
  type
    AWSResponse UpdateDataSet =
      UpdateDataSetResponse
  request overrides =
    Request.putJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDataSetResponse'
            Prelude.<$> (x Core..?> "IngestionArn")
            Prelude.<*> (x Core..?> "RequestId")
            Prelude.<*> (x Core..?> "Arn")
            Prelude.<*> (x Core..?> "DataSetId")
            Prelude.<*> (x Core..?> "IngestionId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateDataSet where
  hashWithSalt _salt UpdateDataSet' {..} =
    _salt
      `Prelude.hashWithSalt` columnLevelPermissionRules
      `Prelude.hashWithSalt` dataSetUsageConfiguration
      `Prelude.hashWithSalt` rowLevelPermissionTagConfiguration
      `Prelude.hashWithSalt` columnGroups
      `Prelude.hashWithSalt` fieldFolders
      `Prelude.hashWithSalt` rowLevelPermissionDataSet
      `Prelude.hashWithSalt` logicalTableMap
      `Prelude.hashWithSalt` awsAccountId
      `Prelude.hashWithSalt` dataSetId
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` physicalTableMap
      `Prelude.hashWithSalt` importMode

instance Prelude.NFData UpdateDataSet where
  rnf UpdateDataSet' {..} =
    Prelude.rnf columnLevelPermissionRules
      `Prelude.seq` Prelude.rnf dataSetUsageConfiguration
      `Prelude.seq` Prelude.rnf rowLevelPermissionTagConfiguration
      `Prelude.seq` Prelude.rnf columnGroups
      `Prelude.seq` Prelude.rnf fieldFolders
      `Prelude.seq` Prelude.rnf rowLevelPermissionDataSet
      `Prelude.seq` Prelude.rnf logicalTableMap
      `Prelude.seq` Prelude.rnf awsAccountId
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf name
      `Prelude.seq` Prelude.rnf physicalTableMap
      `Prelude.seq` Prelude.rnf importMode

instance Core.ToHeaders UpdateDataSet where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON UpdateDataSet where
  toJSON UpdateDataSet' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ColumnLevelPermissionRules" Core..=)
              Prelude.<$> columnLevelPermissionRules,
            ("DataSetUsageConfiguration" Core..=)
              Prelude.<$> dataSetUsageConfiguration,
            ("RowLevelPermissionTagConfiguration" Core..=)
              Prelude.<$> rowLevelPermissionTagConfiguration,
            ("ColumnGroups" Core..=) Prelude.<$> columnGroups,
            ("FieldFolders" Core..=) Prelude.<$> fieldFolders,
            ("RowLevelPermissionDataSet" Core..=)
              Prelude.<$> rowLevelPermissionDataSet,
            ("LogicalTableMap" Core..=)
              Prelude.<$> logicalTableMap,
            Prelude.Just ("Name" Core..= name),
            Prelude.Just
              ("PhysicalTableMap" Core..= physicalTableMap),
            Prelude.Just ("ImportMode" Core..= importMode)
          ]
      )

instance Core.ToPath UpdateDataSet where
  toPath UpdateDataSet' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Core.toBS awsAccountId,
        "/data-sets/",
        Core.toBS dataSetId
      ]

instance Core.ToQuery UpdateDataSet where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateDataSetResponse' smart constructor.
data UpdateDataSetResponse = UpdateDataSetResponse'
  { -- | The ARN for the ingestion, which is triggered as a result of dataset
    -- creation if the import mode is SPICE.
    ingestionArn :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the dataset.
    arn :: Prelude.Maybe Prelude.Text,
    -- | The ID for the dataset that you want to create. This ID is unique per
    -- Amazon Web Services Region for each Amazon Web Services account.
    dataSetId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the ingestion, which is triggered as a result of dataset
    -- creation if the import mode is SPICE.
    ingestionId :: Prelude.Maybe Prelude.Text,
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateDataSetResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'ingestionArn', 'updateDataSetResponse_ingestionArn' - The ARN for the ingestion, which is triggered as a result of dataset
-- creation if the import mode is SPICE.
--
-- 'requestId', 'updateDataSetResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'arn', 'updateDataSetResponse_arn' - The Amazon Resource Name (ARN) of the dataset.
--
-- 'dataSetId', 'updateDataSetResponse_dataSetId' - The ID for the dataset that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
--
-- 'ingestionId', 'updateDataSetResponse_ingestionId' - The ID of the ingestion, which is triggered as a result of dataset
-- creation if the import mode is SPICE.
--
-- 'status', 'updateDataSetResponse_status' - The HTTP status of the request.
newUpdateDataSetResponse ::
  -- | 'status'
  Prelude.Int ->
  UpdateDataSetResponse
newUpdateDataSetResponse pStatus_ =
  UpdateDataSetResponse'
    { ingestionArn =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      arn = Prelude.Nothing,
      dataSetId = Prelude.Nothing,
      ingestionId = Prelude.Nothing,
      status = pStatus_
    }

-- | The ARN for the ingestion, which is triggered as a result of dataset
-- creation if the import mode is SPICE.
updateDataSetResponse_ingestionArn :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.Text)
updateDataSetResponse_ingestionArn = Lens.lens (\UpdateDataSetResponse' {ingestionArn} -> ingestionArn) (\s@UpdateDataSetResponse' {} a -> s {ingestionArn = a} :: UpdateDataSetResponse)

-- | The Amazon Web Services request ID for this operation.
updateDataSetResponse_requestId :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.Text)
updateDataSetResponse_requestId = Lens.lens (\UpdateDataSetResponse' {requestId} -> requestId) (\s@UpdateDataSetResponse' {} a -> s {requestId = a} :: UpdateDataSetResponse)

-- | The Amazon Resource Name (ARN) of the dataset.
updateDataSetResponse_arn :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.Text)
updateDataSetResponse_arn = Lens.lens (\UpdateDataSetResponse' {arn} -> arn) (\s@UpdateDataSetResponse' {} a -> s {arn = a} :: UpdateDataSetResponse)

-- | The ID for the dataset that you want to create. This ID is unique per
-- Amazon Web Services Region for each Amazon Web Services account.
updateDataSetResponse_dataSetId :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.Text)
updateDataSetResponse_dataSetId = Lens.lens (\UpdateDataSetResponse' {dataSetId} -> dataSetId) (\s@UpdateDataSetResponse' {} a -> s {dataSetId = a} :: UpdateDataSetResponse)

-- | The ID of the ingestion, which is triggered as a result of dataset
-- creation if the import mode is SPICE.
updateDataSetResponse_ingestionId :: Lens.Lens' UpdateDataSetResponse (Prelude.Maybe Prelude.Text)
updateDataSetResponse_ingestionId = Lens.lens (\UpdateDataSetResponse' {ingestionId} -> ingestionId) (\s@UpdateDataSetResponse' {} a -> s {ingestionId = a} :: UpdateDataSetResponse)

-- | The HTTP status of the request.
updateDataSetResponse_status :: Lens.Lens' UpdateDataSetResponse Prelude.Int
updateDataSetResponse_status = Lens.lens (\UpdateDataSetResponse' {status} -> status) (\s@UpdateDataSetResponse' {} a -> s {status = a} :: UpdateDataSetResponse)

instance Prelude.NFData UpdateDataSetResponse where
  rnf UpdateDataSetResponse' {..} =
    Prelude.rnf ingestionArn
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf arn
      `Prelude.seq` Prelude.rnf dataSetId
      `Prelude.seq` Prelude.rnf ingestionId
      `Prelude.seq` Prelude.rnf status
