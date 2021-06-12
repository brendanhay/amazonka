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
-- Module      : Network.AWS.Redshift.Types.TableRestoreStatus
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Redshift.Types.TableRestoreStatus where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Redshift.Internal
import Network.AWS.Redshift.Types.TableRestoreStatusType

-- | Describes the status of a RestoreTableFromClusterSnapshot operation.
--
-- /See:/ 'newTableRestoreStatus' smart constructor.
data TableRestoreStatus = TableRestoreStatus'
  { -- | The name of the source database that contains the table being restored.
    sourceDatabaseName :: Core.Maybe Core.Text,
    -- | The identifier of the snapshot that the table is being restored from.
    snapshotIdentifier :: Core.Maybe Core.Text,
    -- | The name of the schema to restore the table to.
    targetSchemaName :: Core.Maybe Core.Text,
    -- | A value that describes the current state of the table restore request.
    --
    -- Valid Values: @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@,
    -- @IN_PROGRESS@
    status :: Core.Maybe TableRestoreStatusType,
    -- | The time that the table restore request was made, in Universal
    -- Coordinated Time (UTC).
    requestTime :: Core.Maybe Core.ISO8601,
    -- | The name of the source table being restored.
    sourceTableName :: Core.Maybe Core.Text,
    -- | The name of the database to restore the table to.
    targetDatabaseName :: Core.Maybe Core.Text,
    -- | A description of the status of the table restore request. Status values
    -- include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@, @IN_PROGRESS@.
    message :: Core.Maybe Core.Text,
    -- | The name of the source schema that contains the table being restored.
    sourceSchemaName :: Core.Maybe Core.Text,
    -- | The identifier of the Amazon Redshift cluster that the table is being
    -- restored to.
    clusterIdentifier :: Core.Maybe Core.Text,
    -- | The amount of data restored to the new table so far, in megabytes (MB).
    progressInMegaBytes :: Core.Maybe Core.Integer,
    -- | The name of the table to create as a result of the table restore
    -- request.
    newTableName' :: Core.Maybe Core.Text,
    -- | The total amount of data to restore to the new table, in megabytes (MB).
    totalDataInMegaBytes :: Core.Maybe Core.Integer,
    -- | The unique identifier for the table restore request.
    tableRestoreRequestId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TableRestoreStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sourceDatabaseName', 'tableRestoreStatus_sourceDatabaseName' - The name of the source database that contains the table being restored.
--
-- 'snapshotIdentifier', 'tableRestoreStatus_snapshotIdentifier' - The identifier of the snapshot that the table is being restored from.
--
-- 'targetSchemaName', 'tableRestoreStatus_targetSchemaName' - The name of the schema to restore the table to.
--
-- 'status', 'tableRestoreStatus_status' - A value that describes the current state of the table restore request.
--
-- Valid Values: @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@,
-- @IN_PROGRESS@
--
-- 'requestTime', 'tableRestoreStatus_requestTime' - The time that the table restore request was made, in Universal
-- Coordinated Time (UTC).
--
-- 'sourceTableName', 'tableRestoreStatus_sourceTableName' - The name of the source table being restored.
--
-- 'targetDatabaseName', 'tableRestoreStatus_targetDatabaseName' - The name of the database to restore the table to.
--
-- 'message', 'tableRestoreStatus_message' - A description of the status of the table restore request. Status values
-- include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@, @IN_PROGRESS@.
--
-- 'sourceSchemaName', 'tableRestoreStatus_sourceSchemaName' - The name of the source schema that contains the table being restored.
--
-- 'clusterIdentifier', 'tableRestoreStatus_clusterIdentifier' - The identifier of the Amazon Redshift cluster that the table is being
-- restored to.
--
-- 'progressInMegaBytes', 'tableRestoreStatus_progressInMegaBytes' - The amount of data restored to the new table so far, in megabytes (MB).
--
-- 'newTableName'', 'tableRestoreStatus_newTableName' - The name of the table to create as a result of the table restore
-- request.
--
-- 'totalDataInMegaBytes', 'tableRestoreStatus_totalDataInMegaBytes' - The total amount of data to restore to the new table, in megabytes (MB).
--
-- 'tableRestoreRequestId', 'tableRestoreStatus_tableRestoreRequestId' - The unique identifier for the table restore request.
newTableRestoreStatus ::
  TableRestoreStatus
newTableRestoreStatus =
  TableRestoreStatus'
    { sourceDatabaseName =
        Core.Nothing,
      snapshotIdentifier = Core.Nothing,
      targetSchemaName = Core.Nothing,
      status = Core.Nothing,
      requestTime = Core.Nothing,
      sourceTableName = Core.Nothing,
      targetDatabaseName = Core.Nothing,
      message = Core.Nothing,
      sourceSchemaName = Core.Nothing,
      clusterIdentifier = Core.Nothing,
      progressInMegaBytes = Core.Nothing,
      newTableName' = Core.Nothing,
      totalDataInMegaBytes = Core.Nothing,
      tableRestoreRequestId = Core.Nothing
    }

-- | The name of the source database that contains the table being restored.
tableRestoreStatus_sourceDatabaseName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
tableRestoreStatus_sourceDatabaseName = Lens.lens (\TableRestoreStatus' {sourceDatabaseName} -> sourceDatabaseName) (\s@TableRestoreStatus' {} a -> s {sourceDatabaseName = a} :: TableRestoreStatus)

-- | The identifier of the snapshot that the table is being restored from.
tableRestoreStatus_snapshotIdentifier :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
tableRestoreStatus_snapshotIdentifier = Lens.lens (\TableRestoreStatus' {snapshotIdentifier} -> snapshotIdentifier) (\s@TableRestoreStatus' {} a -> s {snapshotIdentifier = a} :: TableRestoreStatus)

-- | The name of the schema to restore the table to.
tableRestoreStatus_targetSchemaName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
tableRestoreStatus_targetSchemaName = Lens.lens (\TableRestoreStatus' {targetSchemaName} -> targetSchemaName) (\s@TableRestoreStatus' {} a -> s {targetSchemaName = a} :: TableRestoreStatus)

-- | A value that describes the current state of the table restore request.
--
-- Valid Values: @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@,
-- @IN_PROGRESS@
tableRestoreStatus_status :: Lens.Lens' TableRestoreStatus (Core.Maybe TableRestoreStatusType)
tableRestoreStatus_status = Lens.lens (\TableRestoreStatus' {status} -> status) (\s@TableRestoreStatus' {} a -> s {status = a} :: TableRestoreStatus)

-- | The time that the table restore request was made, in Universal
-- Coordinated Time (UTC).
tableRestoreStatus_requestTime :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.UTCTime)
tableRestoreStatus_requestTime = Lens.lens (\TableRestoreStatus' {requestTime} -> requestTime) (\s@TableRestoreStatus' {} a -> s {requestTime = a} :: TableRestoreStatus) Core.. Lens.mapping Core._Time

-- | The name of the source table being restored.
tableRestoreStatus_sourceTableName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
tableRestoreStatus_sourceTableName = Lens.lens (\TableRestoreStatus' {sourceTableName} -> sourceTableName) (\s@TableRestoreStatus' {} a -> s {sourceTableName = a} :: TableRestoreStatus)

-- | The name of the database to restore the table to.
tableRestoreStatus_targetDatabaseName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
tableRestoreStatus_targetDatabaseName = Lens.lens (\TableRestoreStatus' {targetDatabaseName} -> targetDatabaseName) (\s@TableRestoreStatus' {} a -> s {targetDatabaseName = a} :: TableRestoreStatus)

-- | A description of the status of the table restore request. Status values
-- include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@, @IN_PROGRESS@.
tableRestoreStatus_message :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
tableRestoreStatus_message = Lens.lens (\TableRestoreStatus' {message} -> message) (\s@TableRestoreStatus' {} a -> s {message = a} :: TableRestoreStatus)

-- | The name of the source schema that contains the table being restored.
tableRestoreStatus_sourceSchemaName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
tableRestoreStatus_sourceSchemaName = Lens.lens (\TableRestoreStatus' {sourceSchemaName} -> sourceSchemaName) (\s@TableRestoreStatus' {} a -> s {sourceSchemaName = a} :: TableRestoreStatus)

-- | The identifier of the Amazon Redshift cluster that the table is being
-- restored to.
tableRestoreStatus_clusterIdentifier :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
tableRestoreStatus_clusterIdentifier = Lens.lens (\TableRestoreStatus' {clusterIdentifier} -> clusterIdentifier) (\s@TableRestoreStatus' {} a -> s {clusterIdentifier = a} :: TableRestoreStatus)

-- | The amount of data restored to the new table so far, in megabytes (MB).
tableRestoreStatus_progressInMegaBytes :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Integer)
tableRestoreStatus_progressInMegaBytes = Lens.lens (\TableRestoreStatus' {progressInMegaBytes} -> progressInMegaBytes) (\s@TableRestoreStatus' {} a -> s {progressInMegaBytes = a} :: TableRestoreStatus)

-- | The name of the table to create as a result of the table restore
-- request.
tableRestoreStatus_newTableName :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
tableRestoreStatus_newTableName = Lens.lens (\TableRestoreStatus' {newTableName'} -> newTableName') (\s@TableRestoreStatus' {} a -> s {newTableName' = a} :: TableRestoreStatus)

-- | The total amount of data to restore to the new table, in megabytes (MB).
tableRestoreStatus_totalDataInMegaBytes :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Integer)
tableRestoreStatus_totalDataInMegaBytes = Lens.lens (\TableRestoreStatus' {totalDataInMegaBytes} -> totalDataInMegaBytes) (\s@TableRestoreStatus' {} a -> s {totalDataInMegaBytes = a} :: TableRestoreStatus)

-- | The unique identifier for the table restore request.
tableRestoreStatus_tableRestoreRequestId :: Lens.Lens' TableRestoreStatus (Core.Maybe Core.Text)
tableRestoreStatus_tableRestoreRequestId = Lens.lens (\TableRestoreStatus' {tableRestoreRequestId} -> tableRestoreRequestId) (\s@TableRestoreStatus' {} a -> s {tableRestoreRequestId = a} :: TableRestoreStatus)

instance Core.FromXML TableRestoreStatus where
  parseXML x =
    TableRestoreStatus'
      Core.<$> (x Core..@? "SourceDatabaseName")
      Core.<*> (x Core..@? "SnapshotIdentifier")
      Core.<*> (x Core..@? "TargetSchemaName")
      Core.<*> (x Core..@? "Status")
      Core.<*> (x Core..@? "RequestTime")
      Core.<*> (x Core..@? "SourceTableName")
      Core.<*> (x Core..@? "TargetDatabaseName")
      Core.<*> (x Core..@? "Message")
      Core.<*> (x Core..@? "SourceSchemaName")
      Core.<*> (x Core..@? "ClusterIdentifier")
      Core.<*> (x Core..@? "ProgressInMegaBytes")
      Core.<*> (x Core..@? "NewTableName")
      Core.<*> (x Core..@? "TotalDataInMegaBytes")
      Core.<*> (x Core..@? "TableRestoreRequestId")

instance Core.Hashable TableRestoreStatus

instance Core.NFData TableRestoreStatus
