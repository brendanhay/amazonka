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
-- Module      : Amazonka.Redshift.RestoreTableFromClusterSnapshot
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new table from a table in an Amazon Redshift cluster snapshot.
-- You must create the new table within the Amazon Redshift cluster that
-- the snapshot was taken from.
--
-- You cannot use @RestoreTableFromClusterSnapshot@ to restore a table with
-- the same name as an existing table in an Amazon Redshift cluster. That
-- is, you cannot overwrite an existing table in a cluster with a restored
-- table. If you want to replace your original table with a new, restored
-- table, then rename or drop your original table before you call
-- @RestoreTableFromClusterSnapshot@. When you have renamed your original
-- table, then you can pass the original name of the table as the
-- @NewTableName@ parameter value in the call to
-- @RestoreTableFromClusterSnapshot@. This way, you can replace the
-- original table with the table created from the snapshot.
module Amazonka.Redshift.RestoreTableFromClusterSnapshot
  ( -- * Creating a Request
    RestoreTableFromClusterSnapshot (..),
    newRestoreTableFromClusterSnapshot,

    -- * Request Lenses
    restoreTableFromClusterSnapshot_enableCaseSensitiveIdentifier,
    restoreTableFromClusterSnapshot_sourceSchemaName,
    restoreTableFromClusterSnapshot_targetDatabaseName,
    restoreTableFromClusterSnapshot_targetSchemaName,
    restoreTableFromClusterSnapshot_clusterIdentifier,
    restoreTableFromClusterSnapshot_snapshotIdentifier,
    restoreTableFromClusterSnapshot_sourceDatabaseName,
    restoreTableFromClusterSnapshot_sourceTableName,
    restoreTableFromClusterSnapshot_newTableName,

    -- * Destructuring the Response
    RestoreTableFromClusterSnapshotResponse (..),
    newRestoreTableFromClusterSnapshotResponse,

    -- * Response Lenses
    restoreTableFromClusterSnapshotResponse_tableRestoreStatus,
    restoreTableFromClusterSnapshotResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newRestoreTableFromClusterSnapshot' smart constructor.
data RestoreTableFromClusterSnapshot = RestoreTableFromClusterSnapshot'
  { -- | Indicates whether name identifiers for database, schema, and table are
    -- case sensitive. If @true@, the names are case sensitive. If @false@
    -- (default), the names are not case sensitive.
    enableCaseSensitiveIdentifier :: Prelude.Maybe Prelude.Bool,
    -- | The name of the source schema that contains the table to restore from.
    -- If you do not specify a @SourceSchemaName@ value, the default is
    -- @public@.
    sourceSchemaName :: Prelude.Maybe Prelude.Text,
    -- | The name of the database to restore the table to.
    targetDatabaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema to restore the table to.
    targetSchemaName :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the Amazon Redshift cluster to restore the table to.
    clusterIdentifier :: Prelude.Text,
    -- | The identifier of the snapshot to restore the table from. This snapshot
    -- must have been created from the Amazon Redshift cluster specified by the
    -- @ClusterIdentifier@ parameter.
    snapshotIdentifier :: Prelude.Text,
    -- | The name of the source database that contains the table to restore from.
    sourceDatabaseName :: Prelude.Text,
    -- | The name of the source table to restore from.
    sourceTableName :: Prelude.Text,
    -- | The name of the table to create as a result of the current request.
    newTableName' :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreTableFromClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableCaseSensitiveIdentifier', 'restoreTableFromClusterSnapshot_enableCaseSensitiveIdentifier' - Indicates whether name identifiers for database, schema, and table are
-- case sensitive. If @true@, the names are case sensitive. If @false@
-- (default), the names are not case sensitive.
--
-- 'sourceSchemaName', 'restoreTableFromClusterSnapshot_sourceSchemaName' - The name of the source schema that contains the table to restore from.
-- If you do not specify a @SourceSchemaName@ value, the default is
-- @public@.
--
-- 'targetDatabaseName', 'restoreTableFromClusterSnapshot_targetDatabaseName' - The name of the database to restore the table to.
--
-- 'targetSchemaName', 'restoreTableFromClusterSnapshot_targetSchemaName' - The name of the schema to restore the table to.
--
-- 'clusterIdentifier', 'restoreTableFromClusterSnapshot_clusterIdentifier' - The identifier of the Amazon Redshift cluster to restore the table to.
--
-- 'snapshotIdentifier', 'restoreTableFromClusterSnapshot_snapshotIdentifier' - The identifier of the snapshot to restore the table from. This snapshot
-- must have been created from the Amazon Redshift cluster specified by the
-- @ClusterIdentifier@ parameter.
--
-- 'sourceDatabaseName', 'restoreTableFromClusterSnapshot_sourceDatabaseName' - The name of the source database that contains the table to restore from.
--
-- 'sourceTableName', 'restoreTableFromClusterSnapshot_sourceTableName' - The name of the source table to restore from.
--
-- 'newTableName'', 'restoreTableFromClusterSnapshot_newTableName' - The name of the table to create as a result of the current request.
newRestoreTableFromClusterSnapshot ::
  -- | 'clusterIdentifier'
  Prelude.Text ->
  -- | 'snapshotIdentifier'
  Prelude.Text ->
  -- | 'sourceDatabaseName'
  Prelude.Text ->
  -- | 'sourceTableName'
  Prelude.Text ->
  -- | 'newTableName''
  Prelude.Text ->
  RestoreTableFromClusterSnapshot
newRestoreTableFromClusterSnapshot
  pClusterIdentifier_
  pSnapshotIdentifier_
  pSourceDatabaseName_
  pSourceTableName_
  pNewTableName_ =
    RestoreTableFromClusterSnapshot'
      { enableCaseSensitiveIdentifier =
          Prelude.Nothing,
        sourceSchemaName = Prelude.Nothing,
        targetDatabaseName = Prelude.Nothing,
        targetSchemaName = Prelude.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        snapshotIdentifier = pSnapshotIdentifier_,
        sourceDatabaseName = pSourceDatabaseName_,
        sourceTableName = pSourceTableName_,
        newTableName' = pNewTableName_
      }

-- | Indicates whether name identifiers for database, schema, and table are
-- case sensitive. If @true@, the names are case sensitive. If @false@
-- (default), the names are not case sensitive.
restoreTableFromClusterSnapshot_enableCaseSensitiveIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot (Prelude.Maybe Prelude.Bool)
restoreTableFromClusterSnapshot_enableCaseSensitiveIdentifier = Lens.lens (\RestoreTableFromClusterSnapshot' {enableCaseSensitiveIdentifier} -> enableCaseSensitiveIdentifier) (\s@RestoreTableFromClusterSnapshot' {} a -> s {enableCaseSensitiveIdentifier = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the source schema that contains the table to restore from.
-- If you do not specify a @SourceSchemaName@ value, the default is
-- @public@.
restoreTableFromClusterSnapshot_sourceSchemaName :: Lens.Lens' RestoreTableFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreTableFromClusterSnapshot_sourceSchemaName = Lens.lens (\RestoreTableFromClusterSnapshot' {sourceSchemaName} -> sourceSchemaName) (\s@RestoreTableFromClusterSnapshot' {} a -> s {sourceSchemaName = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the database to restore the table to.
restoreTableFromClusterSnapshot_targetDatabaseName :: Lens.Lens' RestoreTableFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreTableFromClusterSnapshot_targetDatabaseName = Lens.lens (\RestoreTableFromClusterSnapshot' {targetDatabaseName} -> targetDatabaseName) (\s@RestoreTableFromClusterSnapshot' {} a -> s {targetDatabaseName = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the schema to restore the table to.
restoreTableFromClusterSnapshot_targetSchemaName :: Lens.Lens' RestoreTableFromClusterSnapshot (Prelude.Maybe Prelude.Text)
restoreTableFromClusterSnapshot_targetSchemaName = Lens.lens (\RestoreTableFromClusterSnapshot' {targetSchemaName} -> targetSchemaName) (\s@RestoreTableFromClusterSnapshot' {} a -> s {targetSchemaName = a} :: RestoreTableFromClusterSnapshot)

-- | The identifier of the Amazon Redshift cluster to restore the table to.
restoreTableFromClusterSnapshot_clusterIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot Prelude.Text
restoreTableFromClusterSnapshot_clusterIdentifier = Lens.lens (\RestoreTableFromClusterSnapshot' {clusterIdentifier} -> clusterIdentifier) (\s@RestoreTableFromClusterSnapshot' {} a -> s {clusterIdentifier = a} :: RestoreTableFromClusterSnapshot)

-- | The identifier of the snapshot to restore the table from. This snapshot
-- must have been created from the Amazon Redshift cluster specified by the
-- @ClusterIdentifier@ parameter.
restoreTableFromClusterSnapshot_snapshotIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot Prelude.Text
restoreTableFromClusterSnapshot_snapshotIdentifier = Lens.lens (\RestoreTableFromClusterSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@RestoreTableFromClusterSnapshot' {} a -> s {snapshotIdentifier = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the source database that contains the table to restore from.
restoreTableFromClusterSnapshot_sourceDatabaseName :: Lens.Lens' RestoreTableFromClusterSnapshot Prelude.Text
restoreTableFromClusterSnapshot_sourceDatabaseName = Lens.lens (\RestoreTableFromClusterSnapshot' {sourceDatabaseName} -> sourceDatabaseName) (\s@RestoreTableFromClusterSnapshot' {} a -> s {sourceDatabaseName = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the source table to restore from.
restoreTableFromClusterSnapshot_sourceTableName :: Lens.Lens' RestoreTableFromClusterSnapshot Prelude.Text
restoreTableFromClusterSnapshot_sourceTableName = Lens.lens (\RestoreTableFromClusterSnapshot' {sourceTableName} -> sourceTableName) (\s@RestoreTableFromClusterSnapshot' {} a -> s {sourceTableName = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the table to create as a result of the current request.
restoreTableFromClusterSnapshot_newTableName :: Lens.Lens' RestoreTableFromClusterSnapshot Prelude.Text
restoreTableFromClusterSnapshot_newTableName = Lens.lens (\RestoreTableFromClusterSnapshot' {newTableName'} -> newTableName') (\s@RestoreTableFromClusterSnapshot' {} a -> s {newTableName' = a} :: RestoreTableFromClusterSnapshot)

instance
  Core.AWSRequest
    RestoreTableFromClusterSnapshot
  where
  type
    AWSResponse RestoreTableFromClusterSnapshot =
      RestoreTableFromClusterSnapshotResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "RestoreTableFromClusterSnapshotResult"
      ( \s h x ->
          RestoreTableFromClusterSnapshotResponse'
            Prelude.<$> (x Data..@? "TableRestoreStatus")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RestoreTableFromClusterSnapshot
  where
  hashWithSalt
    _salt
    RestoreTableFromClusterSnapshot' {..} =
      _salt
        `Prelude.hashWithSalt` enableCaseSensitiveIdentifier
        `Prelude.hashWithSalt` sourceSchemaName
        `Prelude.hashWithSalt` targetDatabaseName
        `Prelude.hashWithSalt` targetSchemaName
        `Prelude.hashWithSalt` clusterIdentifier
        `Prelude.hashWithSalt` snapshotIdentifier
        `Prelude.hashWithSalt` sourceDatabaseName
        `Prelude.hashWithSalt` sourceTableName
        `Prelude.hashWithSalt` newTableName'

instance
  Prelude.NFData
    RestoreTableFromClusterSnapshot
  where
  rnf RestoreTableFromClusterSnapshot' {..} =
    Prelude.rnf enableCaseSensitiveIdentifier `Prelude.seq`
      Prelude.rnf sourceSchemaName `Prelude.seq`
        Prelude.rnf targetDatabaseName `Prelude.seq`
          Prelude.rnf targetSchemaName `Prelude.seq`
            Prelude.rnf clusterIdentifier `Prelude.seq`
              Prelude.rnf snapshotIdentifier `Prelude.seq`
                Prelude.rnf sourceDatabaseName `Prelude.seq`
                  Prelude.rnf sourceTableName `Prelude.seq`
                    Prelude.rnf newTableName'

instance
  Data.ToHeaders
    RestoreTableFromClusterSnapshot
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath RestoreTableFromClusterSnapshot where
  toPath = Prelude.const "/"

instance Data.ToQuery RestoreTableFromClusterSnapshot where
  toQuery RestoreTableFromClusterSnapshot' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ( "RestoreTableFromClusterSnapshot" ::
                      Prelude.ByteString
                  ),
        "Version"
          Data.=: ("2012-12-01" :: Prelude.ByteString),
        "EnableCaseSensitiveIdentifier"
          Data.=: enableCaseSensitiveIdentifier,
        "SourceSchemaName" Data.=: sourceSchemaName,
        "TargetDatabaseName" Data.=: targetDatabaseName,
        "TargetSchemaName" Data.=: targetSchemaName,
        "ClusterIdentifier" Data.=: clusterIdentifier,
        "SnapshotIdentifier" Data.=: snapshotIdentifier,
        "SourceDatabaseName" Data.=: sourceDatabaseName,
        "SourceTableName" Data.=: sourceTableName,
        "NewTableName" Data.=: newTableName'
      ]

-- | /See:/ 'newRestoreTableFromClusterSnapshotResponse' smart constructor.
data RestoreTableFromClusterSnapshotResponse = RestoreTableFromClusterSnapshotResponse'
  { tableRestoreStatus :: Prelude.Maybe TableRestoreStatus,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RestoreTableFromClusterSnapshotResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tableRestoreStatus', 'restoreTableFromClusterSnapshotResponse_tableRestoreStatus' - Undocumented member.
--
-- 'httpStatus', 'restoreTableFromClusterSnapshotResponse_httpStatus' - The response's http status code.
newRestoreTableFromClusterSnapshotResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RestoreTableFromClusterSnapshotResponse
newRestoreTableFromClusterSnapshotResponse
  pHttpStatus_ =
    RestoreTableFromClusterSnapshotResponse'
      { tableRestoreStatus =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
restoreTableFromClusterSnapshotResponse_tableRestoreStatus :: Lens.Lens' RestoreTableFromClusterSnapshotResponse (Prelude.Maybe TableRestoreStatus)
restoreTableFromClusterSnapshotResponse_tableRestoreStatus = Lens.lens (\RestoreTableFromClusterSnapshotResponse' {tableRestoreStatus} -> tableRestoreStatus) (\s@RestoreTableFromClusterSnapshotResponse' {} a -> s {tableRestoreStatus = a} :: RestoreTableFromClusterSnapshotResponse)

-- | The response's http status code.
restoreTableFromClusterSnapshotResponse_httpStatus :: Lens.Lens' RestoreTableFromClusterSnapshotResponse Prelude.Int
restoreTableFromClusterSnapshotResponse_httpStatus = Lens.lens (\RestoreTableFromClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@RestoreTableFromClusterSnapshotResponse' {} a -> s {httpStatus = a} :: RestoreTableFromClusterSnapshotResponse)

instance
  Prelude.NFData
    RestoreTableFromClusterSnapshotResponse
  where
  rnf RestoreTableFromClusterSnapshotResponse' {..} =
    Prelude.rnf tableRestoreStatus `Prelude.seq`
      Prelude.rnf httpStatus
