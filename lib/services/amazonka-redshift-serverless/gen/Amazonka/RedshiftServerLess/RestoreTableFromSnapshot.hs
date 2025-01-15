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
-- Module      : Amazonka.RedshiftServerLess.RestoreTableFromSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Restores a table from a snapshot to your Amazon Redshift Serverless
-- instance.
module Amazonka.RedshiftServerLess.RestoreTableFromSnapshot
  ( -- * Creating a Request
    RestoreTableFromSnapshot (..),
    newRestoreTableFromSnapshot,

    -- * Request Lenses
    restoreTableFromSnapshot_activateCaseSensitiveIdentifier,
    restoreTableFromSnapshot_sourceSchemaName,
    restoreTableFromSnapshot_targetDatabaseName,
    restoreTableFromSnapshot_targetSchemaName,
    restoreTableFromSnapshot_namespaceName,
    restoreTableFromSnapshot_newTableName,
    restoreTableFromSnapshot_snapshotName,
    restoreTableFromSnapshot_sourceDatabaseName,
    restoreTableFromSnapshot_sourceTableName,
    restoreTableFromSnapshot_workgroupName,

    -- * Destructuring the Response
    RestoreTableFromSnapshotResponse (..),
    newRestoreTableFromSnapshotResponse,

    -- * Response Lenses
    restoreTableFromSnapshotResponse_tableRestoreStatus,
    restoreTableFromSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.RedshiftServerLess.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newRestoreTableFromSnapshot' smart constructor.
data RestoreTableFromSnapshot = RestoreTableFromSnapshot'
  { -- | Indicates whether name identifiers for database, schema, and table are
    -- case sensitive. If true, the names are case sensitive. If false, the
    -- names are not case sensitive. The default is false.
    activateCaseSensitiveIdentifier :: Prelude.Maybe Prelude.Bool,
    -- | The name of the source schema that contains the table being restored.
    sourceSchemaName :: Prelude.Maybe Prelude.Text,
    -- | The name of the database to restore the table to.
    targetDatabaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema to restore the table to.
    targetSchemaName :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the snapshot to restore from.
    namespaceName :: Prelude.Text,
    -- | The name of the table to create from the restore operation.
    newTableName' :: Prelude.Text,
    -- | The name of the snapshot to restore the table from.
    snapshotName :: Prelude.Text,
    -- | The name of the source database that contains the table being restored.
    sourceDatabaseName :: Prelude.Text,
    -- | The name of the source table being restored.
    sourceTableName :: Prelude.Text,
    -- | The workgroup to restore the table to.
    workgroupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreTableFromSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'activateCaseSensitiveIdentifier', 'restoreTableFromSnapshot_activateCaseSensitiveIdentifier' - Indicates whether name identifiers for database, schema, and table are
-- case sensitive. If true, the names are case sensitive. If false, the
-- names are not case sensitive. The default is false.
--
-- 'sourceSchemaName', 'restoreTableFromSnapshot_sourceSchemaName' - The name of the source schema that contains the table being restored.
--
-- 'targetDatabaseName', 'restoreTableFromSnapshot_targetDatabaseName' - The name of the database to restore the table to.
--
-- 'targetSchemaName', 'restoreTableFromSnapshot_targetSchemaName' - The name of the schema to restore the table to.
--
-- 'namespaceName', 'restoreTableFromSnapshot_namespaceName' - The namespace of the snapshot to restore from.
--
-- 'newTableName'', 'restoreTableFromSnapshot_newTableName' - The name of the table to create from the restore operation.
--
-- 'snapshotName', 'restoreTableFromSnapshot_snapshotName' - The name of the snapshot to restore the table from.
--
-- 'sourceDatabaseName', 'restoreTableFromSnapshot_sourceDatabaseName' - The name of the source database that contains the table being restored.
--
-- 'sourceTableName', 'restoreTableFromSnapshot_sourceTableName' - The name of the source table being restored.
--
-- 'workgroupName', 'restoreTableFromSnapshot_workgroupName' - The workgroup to restore the table to.
newRestoreTableFromSnapshot ::
  -- | 'namespaceName'
  Prelude.Text ->
  -- | 'newTableName''
  Prelude.Text ->
  -- | 'snapshotName'
  Prelude.Text ->
  -- | 'sourceDatabaseName'
  Prelude.Text ->
  -- | 'sourceTableName'
  Prelude.Text ->
  -- | 'workgroupName'
  Prelude.Text ->
  RestoreTableFromSnapshot
newRestoreTableFromSnapshot
  pNamespaceName_
  pNewTableName_
  pSnapshotName_
  pSourceDatabaseName_
  pSourceTableName_
  pWorkgroupName_ =
    RestoreTableFromSnapshot'
      { activateCaseSensitiveIdentifier =
          Prelude.Nothing,
        sourceSchemaName = Prelude.Nothing,
        targetDatabaseName = Prelude.Nothing,
        targetSchemaName = Prelude.Nothing,
        namespaceName = pNamespaceName_,
        newTableName' = pNewTableName_,
        snapshotName = pSnapshotName_,
        sourceDatabaseName = pSourceDatabaseName_,
        sourceTableName = pSourceTableName_,
        workgroupName = pWorkgroupName_
      }

-- | Indicates whether name identifiers for database, schema, and table are
-- case sensitive. If true, the names are case sensitive. If false, the
-- names are not case sensitive. The default is false.
restoreTableFromSnapshot_activateCaseSensitiveIdentifier :: Lens.Lens' RestoreTableFromSnapshot (Prelude.Maybe Prelude.Bool)
restoreTableFromSnapshot_activateCaseSensitiveIdentifier = Lens.lens (\RestoreTableFromSnapshot' {activateCaseSensitiveIdentifier} -> activateCaseSensitiveIdentifier) (\s@RestoreTableFromSnapshot' {} a -> s {activateCaseSensitiveIdentifier = a} :: RestoreTableFromSnapshot)

-- | The name of the source schema that contains the table being restored.
restoreTableFromSnapshot_sourceSchemaName :: Lens.Lens' RestoreTableFromSnapshot (Prelude.Maybe Prelude.Text)
restoreTableFromSnapshot_sourceSchemaName = Lens.lens (\RestoreTableFromSnapshot' {sourceSchemaName} -> sourceSchemaName) (\s@RestoreTableFromSnapshot' {} a -> s {sourceSchemaName = a} :: RestoreTableFromSnapshot)

-- | The name of the database to restore the table to.
restoreTableFromSnapshot_targetDatabaseName :: Lens.Lens' RestoreTableFromSnapshot (Prelude.Maybe Prelude.Text)
restoreTableFromSnapshot_targetDatabaseName = Lens.lens (\RestoreTableFromSnapshot' {targetDatabaseName} -> targetDatabaseName) (\s@RestoreTableFromSnapshot' {} a -> s {targetDatabaseName = a} :: RestoreTableFromSnapshot)

-- | The name of the schema to restore the table to.
restoreTableFromSnapshot_targetSchemaName :: Lens.Lens' RestoreTableFromSnapshot (Prelude.Maybe Prelude.Text)
restoreTableFromSnapshot_targetSchemaName = Lens.lens (\RestoreTableFromSnapshot' {targetSchemaName} -> targetSchemaName) (\s@RestoreTableFromSnapshot' {} a -> s {targetSchemaName = a} :: RestoreTableFromSnapshot)

-- | The namespace of the snapshot to restore from.
restoreTableFromSnapshot_namespaceName :: Lens.Lens' RestoreTableFromSnapshot Prelude.Text
restoreTableFromSnapshot_namespaceName = Lens.lens (\RestoreTableFromSnapshot' {namespaceName} -> namespaceName) (\s@RestoreTableFromSnapshot' {} a -> s {namespaceName = a} :: RestoreTableFromSnapshot)

-- | The name of the table to create from the restore operation.
restoreTableFromSnapshot_newTableName :: Lens.Lens' RestoreTableFromSnapshot Prelude.Text
restoreTableFromSnapshot_newTableName = Lens.lens (\RestoreTableFromSnapshot' {newTableName'} -> newTableName') (\s@RestoreTableFromSnapshot' {} a -> s {newTableName' = a} :: RestoreTableFromSnapshot)

-- | The name of the snapshot to restore the table from.
restoreTableFromSnapshot_snapshotName :: Lens.Lens' RestoreTableFromSnapshot Prelude.Text
restoreTableFromSnapshot_snapshotName = Lens.lens (\RestoreTableFromSnapshot' {snapshotName} -> snapshotName) (\s@RestoreTableFromSnapshot' {} a -> s {snapshotName = a} :: RestoreTableFromSnapshot)

-- | The name of the source database that contains the table being restored.
restoreTableFromSnapshot_sourceDatabaseName :: Lens.Lens' RestoreTableFromSnapshot Prelude.Text
restoreTableFromSnapshot_sourceDatabaseName = Lens.lens (\RestoreTableFromSnapshot' {sourceDatabaseName} -> sourceDatabaseName) (\s@RestoreTableFromSnapshot' {} a -> s {sourceDatabaseName = a} :: RestoreTableFromSnapshot)

-- | The name of the source table being restored.
restoreTableFromSnapshot_sourceTableName :: Lens.Lens' RestoreTableFromSnapshot Prelude.Text
restoreTableFromSnapshot_sourceTableName = Lens.lens (\RestoreTableFromSnapshot' {sourceTableName} -> sourceTableName) (\s@RestoreTableFromSnapshot' {} a -> s {sourceTableName = a} :: RestoreTableFromSnapshot)

-- | The workgroup to restore the table to.
restoreTableFromSnapshot_workgroupName :: Lens.Lens' RestoreTableFromSnapshot Prelude.Text
restoreTableFromSnapshot_workgroupName = Lens.lens (\RestoreTableFromSnapshot' {workgroupName} -> workgroupName) (\s@RestoreTableFromSnapshot' {} a -> s {workgroupName = a} :: RestoreTableFromSnapshot)

instance Core.AWSRequest RestoreTableFromSnapshot where
  type
    AWSResponse RestoreTableFromSnapshot =
      RestoreTableFromSnapshotResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          RestoreTableFromSnapshotResponse'
            Prelude.<$> (x Data..?> "tableRestoreStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable RestoreTableFromSnapshot where
  hashWithSalt _salt RestoreTableFromSnapshot' {..} =
    _salt
      `Prelude.hashWithSalt` activateCaseSensitiveIdentifier
      `Prelude.hashWithSalt` sourceSchemaName
      `Prelude.hashWithSalt` targetDatabaseName
      `Prelude.hashWithSalt` targetSchemaName
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` newTableName'
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` sourceDatabaseName
      `Prelude.hashWithSalt` sourceTableName
      `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData RestoreTableFromSnapshot where
  rnf RestoreTableFromSnapshot' {..} =
    Prelude.rnf activateCaseSensitiveIdentifier `Prelude.seq`
      Prelude.rnf sourceSchemaName `Prelude.seq`
        Prelude.rnf targetDatabaseName `Prelude.seq`
          Prelude.rnf targetSchemaName `Prelude.seq`
            Prelude.rnf namespaceName `Prelude.seq`
              Prelude.rnf newTableName' `Prelude.seq`
                Prelude.rnf snapshotName `Prelude.seq`
                  Prelude.rnf sourceDatabaseName `Prelude.seq`
                    Prelude.rnf sourceTableName `Prelude.seq`
                      Prelude.rnf workgroupName

instance Data.ToHeaders RestoreTableFromSnapshot where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "RedshiftServerless.RestoreTableFromSnapshot" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON RestoreTableFromSnapshot where
  toJSON RestoreTableFromSnapshot' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("activateCaseSensitiveIdentifier" Data..=)
              Prelude.<$> activateCaseSensitiveIdentifier,
            ("sourceSchemaName" Data..=)
              Prelude.<$> sourceSchemaName,
            ("targetDatabaseName" Data..=)
              Prelude.<$> targetDatabaseName,
            ("targetSchemaName" Data..=)
              Prelude.<$> targetSchemaName,
            Prelude.Just ("namespaceName" Data..= namespaceName),
            Prelude.Just ("newTableName" Data..= newTableName'),
            Prelude.Just ("snapshotName" Data..= snapshotName),
            Prelude.Just
              ("sourceDatabaseName" Data..= sourceDatabaseName),
            Prelude.Just
              ("sourceTableName" Data..= sourceTableName),
            Prelude.Just
              ("workgroupName" Data..= workgroupName)
          ]
      )

instance Data.ToPath RestoreTableFromSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreTableFromSnapshot where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newRestoreTableFromSnapshotResponse' smart constructor.
data RestoreTableFromSnapshotResponse = RestoreTableFromSnapshotResponse'
  { -- | The TableRestoreStatus object that contains the status of the restore
    -- operation.
    tableRestoreStatus :: Prelude.Maybe TableRestoreStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreTableFromSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableRestoreStatus', 'restoreTableFromSnapshotResponse_tableRestoreStatus' - The TableRestoreStatus object that contains the status of the restore
-- operation.
--
-- 'httpStatus', 'restoreTableFromSnapshotResponse_httpStatus' - The response's http status code.
newRestoreTableFromSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreTableFromSnapshotResponse
newRestoreTableFromSnapshotResponse pHttpStatus_ =
  RestoreTableFromSnapshotResponse'
    { tableRestoreStatus =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The TableRestoreStatus object that contains the status of the restore
-- operation.
restoreTableFromSnapshotResponse_tableRestoreStatus :: Lens.Lens' RestoreTableFromSnapshotResponse (Prelude.Maybe TableRestoreStatus)
restoreTableFromSnapshotResponse_tableRestoreStatus = Lens.lens (\RestoreTableFromSnapshotResponse' {tableRestoreStatus} -> tableRestoreStatus) (\s@RestoreTableFromSnapshotResponse' {} a -> s {tableRestoreStatus = a} :: RestoreTableFromSnapshotResponse)

-- | The response's http status code.
restoreTableFromSnapshotResponse_httpStatus :: Lens.Lens' RestoreTableFromSnapshotResponse Prelude.Int
restoreTableFromSnapshotResponse_httpStatus = Lens.lens (\RestoreTableFromSnapshotResponse' {httpStatus} -> httpStatus) (\s@RestoreTableFromSnapshotResponse' {} a -> s {httpStatus = a} :: RestoreTableFromSnapshotResponse)

instance
  Prelude.NFData
    RestoreTableFromSnapshotResponse
  where
  rnf RestoreTableFromSnapshotResponse' {..} =
    Prelude.rnf tableRestoreStatus `Prelude.seq`
      Prelude.rnf httpStatus
