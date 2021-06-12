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
-- Module      : Network.AWS.Redshift.RestoreTableFromClusterSnapshot
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
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
module Network.AWS.Redshift.RestoreTableFromClusterSnapshot
  ( -- * Creating a Request
    RestoreTableFromClusterSnapshot (..),
    newRestoreTableFromClusterSnapshot,

    -- * Request Lenses
    restoreTableFromClusterSnapshot_targetSchemaName,
    restoreTableFromClusterSnapshot_targetDatabaseName,
    restoreTableFromClusterSnapshot_sourceSchemaName,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newRestoreTableFromClusterSnapshot' smart constructor.
data RestoreTableFromClusterSnapshot = RestoreTableFromClusterSnapshot'
  { -- | The name of the schema to restore the table to.
    targetSchemaName :: Core.Maybe Core.Text,
    -- | The name of the database to restore the table to.
    targetDatabaseName :: Core.Maybe Core.Text,
    -- | The name of the source schema that contains the table to restore from.
    -- If you do not specify a @SourceSchemaName@ value, the default is
    -- @public@.
    sourceSchemaName :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon Redshift cluster to restore the table to.
    clusterIdentifier :: Core.Text,
    -- | The identifier of the snapshot to restore the table from. This snapshot
    -- must have been created from the Amazon Redshift cluster specified by the
    -- @ClusterIdentifier@ parameter.
    snapshotIdentifier :: Core.Text,
    -- | The name of the source database that contains the table to restore from.
    sourceDatabaseName :: Core.Text,
    -- | The name of the source table to restore from.
    sourceTableName :: Core.Text,
    -- | The name of the table to create as a result of the current request.
    newTableName' :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'RestoreTableFromClusterSnapshot' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'targetSchemaName', 'restoreTableFromClusterSnapshot_targetSchemaName' - The name of the schema to restore the table to.
--
-- 'targetDatabaseName', 'restoreTableFromClusterSnapshot_targetDatabaseName' - The name of the database to restore the table to.
--
-- 'sourceSchemaName', 'restoreTableFromClusterSnapshot_sourceSchemaName' - The name of the source schema that contains the table to restore from.
-- If you do not specify a @SourceSchemaName@ value, the default is
-- @public@.
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
  Core.Text ->
  -- | 'snapshotIdentifier'
  Core.Text ->
  -- | 'sourceDatabaseName'
  Core.Text ->
  -- | 'sourceTableName'
  Core.Text ->
  -- | 'newTableName''
  Core.Text ->
  RestoreTableFromClusterSnapshot
newRestoreTableFromClusterSnapshot
  pClusterIdentifier_
  pSnapshotIdentifier_
  pSourceDatabaseName_
  pSourceTableName_
  pNewTableName_ =
    RestoreTableFromClusterSnapshot'
      { targetSchemaName =
          Core.Nothing,
        targetDatabaseName = Core.Nothing,
        sourceSchemaName = Core.Nothing,
        clusterIdentifier = pClusterIdentifier_,
        snapshotIdentifier = pSnapshotIdentifier_,
        sourceDatabaseName = pSourceDatabaseName_,
        sourceTableName = pSourceTableName_,
        newTableName' = pNewTableName_
      }

-- | The name of the schema to restore the table to.
restoreTableFromClusterSnapshot_targetSchemaName :: Lens.Lens' RestoreTableFromClusterSnapshot (Core.Maybe Core.Text)
restoreTableFromClusterSnapshot_targetSchemaName = Lens.lens (\RestoreTableFromClusterSnapshot' {targetSchemaName} -> targetSchemaName) (\s@RestoreTableFromClusterSnapshot' {} a -> s {targetSchemaName = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the database to restore the table to.
restoreTableFromClusterSnapshot_targetDatabaseName :: Lens.Lens' RestoreTableFromClusterSnapshot (Core.Maybe Core.Text)
restoreTableFromClusterSnapshot_targetDatabaseName = Lens.lens (\RestoreTableFromClusterSnapshot' {targetDatabaseName} -> targetDatabaseName) (\s@RestoreTableFromClusterSnapshot' {} a -> s {targetDatabaseName = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the source schema that contains the table to restore from.
-- If you do not specify a @SourceSchemaName@ value, the default is
-- @public@.
restoreTableFromClusterSnapshot_sourceSchemaName :: Lens.Lens' RestoreTableFromClusterSnapshot (Core.Maybe Core.Text)
restoreTableFromClusterSnapshot_sourceSchemaName = Lens.lens (\RestoreTableFromClusterSnapshot' {sourceSchemaName} -> sourceSchemaName) (\s@RestoreTableFromClusterSnapshot' {} a -> s {sourceSchemaName = a} :: RestoreTableFromClusterSnapshot)

-- | The identifier of the Amazon Redshift cluster to restore the table to.
restoreTableFromClusterSnapshot_clusterIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot Core.Text
restoreTableFromClusterSnapshot_clusterIdentifier = Lens.lens (\RestoreTableFromClusterSnapshot' {clusterIdentifier} -> clusterIdentifier) (\s@RestoreTableFromClusterSnapshot' {} a -> s {clusterIdentifier = a} :: RestoreTableFromClusterSnapshot)

-- | The identifier of the snapshot to restore the table from. This snapshot
-- must have been created from the Amazon Redshift cluster specified by the
-- @ClusterIdentifier@ parameter.
restoreTableFromClusterSnapshot_snapshotIdentifier :: Lens.Lens' RestoreTableFromClusterSnapshot Core.Text
restoreTableFromClusterSnapshot_snapshotIdentifier = Lens.lens (\RestoreTableFromClusterSnapshot' {snapshotIdentifier} -> snapshotIdentifier) (\s@RestoreTableFromClusterSnapshot' {} a -> s {snapshotIdentifier = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the source database that contains the table to restore from.
restoreTableFromClusterSnapshot_sourceDatabaseName :: Lens.Lens' RestoreTableFromClusterSnapshot Core.Text
restoreTableFromClusterSnapshot_sourceDatabaseName = Lens.lens (\RestoreTableFromClusterSnapshot' {sourceDatabaseName} -> sourceDatabaseName) (\s@RestoreTableFromClusterSnapshot' {} a -> s {sourceDatabaseName = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the source table to restore from.
restoreTableFromClusterSnapshot_sourceTableName :: Lens.Lens' RestoreTableFromClusterSnapshot Core.Text
restoreTableFromClusterSnapshot_sourceTableName = Lens.lens (\RestoreTableFromClusterSnapshot' {sourceTableName} -> sourceTableName) (\s@RestoreTableFromClusterSnapshot' {} a -> s {sourceTableName = a} :: RestoreTableFromClusterSnapshot)

-- | The name of the table to create as a result of the current request.
restoreTableFromClusterSnapshot_newTableName :: Lens.Lens' RestoreTableFromClusterSnapshot Core.Text
restoreTableFromClusterSnapshot_newTableName = Lens.lens (\RestoreTableFromClusterSnapshot' {newTableName'} -> newTableName') (\s@RestoreTableFromClusterSnapshot' {} a -> s {newTableName' = a} :: RestoreTableFromClusterSnapshot)

instance
  Core.AWSRequest
    RestoreTableFromClusterSnapshot
  where
  type
    AWSResponse RestoreTableFromClusterSnapshot =
      RestoreTableFromClusterSnapshotResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "RestoreTableFromClusterSnapshotResult"
      ( \s h x ->
          RestoreTableFromClusterSnapshotResponse'
            Core.<$> (x Core..@? "TableRestoreStatus")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    RestoreTableFromClusterSnapshot

instance Core.NFData RestoreTableFromClusterSnapshot

instance
  Core.ToHeaders
    RestoreTableFromClusterSnapshot
  where
  toHeaders = Core.const Core.mempty

instance Core.ToPath RestoreTableFromClusterSnapshot where
  toPath = Core.const "/"

instance Core.ToQuery RestoreTableFromClusterSnapshot where
  toQuery RestoreTableFromClusterSnapshot' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ( "RestoreTableFromClusterSnapshot" ::
                      Core.ByteString
                  ),
        "Version" Core.=: ("2012-12-01" :: Core.ByteString),
        "TargetSchemaName" Core.=: targetSchemaName,
        "TargetDatabaseName" Core.=: targetDatabaseName,
        "SourceSchemaName" Core.=: sourceSchemaName,
        "ClusterIdentifier" Core.=: clusterIdentifier,
        "SnapshotIdentifier" Core.=: snapshotIdentifier,
        "SourceDatabaseName" Core.=: sourceDatabaseName,
        "SourceTableName" Core.=: sourceTableName,
        "NewTableName" Core.=: newTableName'
      ]

-- | /See:/ 'newRestoreTableFromClusterSnapshotResponse' smart constructor.
data RestoreTableFromClusterSnapshotResponse = RestoreTableFromClusterSnapshotResponse'
  { tableRestoreStatus :: Core.Maybe TableRestoreStatus,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  RestoreTableFromClusterSnapshotResponse
newRestoreTableFromClusterSnapshotResponse
  pHttpStatus_ =
    RestoreTableFromClusterSnapshotResponse'
      { tableRestoreStatus =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
restoreTableFromClusterSnapshotResponse_tableRestoreStatus :: Lens.Lens' RestoreTableFromClusterSnapshotResponse (Core.Maybe TableRestoreStatus)
restoreTableFromClusterSnapshotResponse_tableRestoreStatus = Lens.lens (\RestoreTableFromClusterSnapshotResponse' {tableRestoreStatus} -> tableRestoreStatus) (\s@RestoreTableFromClusterSnapshotResponse' {} a -> s {tableRestoreStatus = a} :: RestoreTableFromClusterSnapshotResponse)

-- | The response's http status code.
restoreTableFromClusterSnapshotResponse_httpStatus :: Lens.Lens' RestoreTableFromClusterSnapshotResponse Core.Int
restoreTableFromClusterSnapshotResponse_httpStatus = Lens.lens (\RestoreTableFromClusterSnapshotResponse' {httpStatus} -> httpStatus) (\s@RestoreTableFromClusterSnapshotResponse' {} a -> s {httpStatus = a} :: RestoreTableFromClusterSnapshotResponse)

instance
  Core.NFData
    RestoreTableFromClusterSnapshotResponse
