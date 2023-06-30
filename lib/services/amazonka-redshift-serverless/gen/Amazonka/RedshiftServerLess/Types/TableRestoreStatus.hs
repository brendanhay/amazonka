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
-- Module      : Amazonka.RedshiftServerLess.Types.TableRestoreStatus
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.RedshiftServerLess.Types.TableRestoreStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about a table restore request.
--
-- /See:/ 'newTableRestoreStatus' smart constructor.
data TableRestoreStatus = TableRestoreStatus'
  { -- | A description of the status of the table restore request. Status values
    -- include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@, @IN_PROGRESS@.
    message :: Prelude.Maybe Prelude.Text,
    -- | The namespace of the table being restored from.
    namespaceName :: Prelude.Maybe Prelude.Text,
    -- | The name of the table to create from the restore operation.
    newTableName' :: Prelude.Maybe Prelude.Text,
    -- | The amount of data restored to the new table so far, in megabytes (MB).
    progressInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | The time that the table restore request was made, in Universal
    -- Coordinated Time (UTC).
    requestTime :: Prelude.Maybe Data.POSIX,
    -- | The name of the snapshot being restored from.
    snapshotName :: Prelude.Maybe Prelude.Text,
    -- | The name of the source database being restored from.
    sourceDatabaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the source schema being restored from.
    sourceSchemaName :: Prelude.Maybe Prelude.Text,
    -- | The name of the source table being restored from.
    sourceTableName :: Prelude.Maybe Prelude.Text,
    -- | A value that describes the current state of the table restore request.
    -- Possible values include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@,
    -- @IN_PROGRESS@.
    status :: Prelude.Maybe Prelude.Text,
    -- | The ID of the RestoreTableFromSnapshot request.
    tableRestoreRequestId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database to restore to.
    targetDatabaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema to restore to.
    targetSchemaName :: Prelude.Maybe Prelude.Text,
    -- | The total amount of data to restore to the new table, in megabytes (MB).
    totalDataInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | The name of the workgroup being restored from.
    workgroupName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TableRestoreStatus' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'message', 'tableRestoreStatus_message' - A description of the status of the table restore request. Status values
-- include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@, @IN_PROGRESS@.
--
-- 'namespaceName', 'tableRestoreStatus_namespaceName' - The namespace of the table being restored from.
--
-- 'newTableName'', 'tableRestoreStatus_newTableName' - The name of the table to create from the restore operation.
--
-- 'progressInMegaBytes', 'tableRestoreStatus_progressInMegaBytes' - The amount of data restored to the new table so far, in megabytes (MB).
--
-- 'requestTime', 'tableRestoreStatus_requestTime' - The time that the table restore request was made, in Universal
-- Coordinated Time (UTC).
--
-- 'snapshotName', 'tableRestoreStatus_snapshotName' - The name of the snapshot being restored from.
--
-- 'sourceDatabaseName', 'tableRestoreStatus_sourceDatabaseName' - The name of the source database being restored from.
--
-- 'sourceSchemaName', 'tableRestoreStatus_sourceSchemaName' - The name of the source schema being restored from.
--
-- 'sourceTableName', 'tableRestoreStatus_sourceTableName' - The name of the source table being restored from.
--
-- 'status', 'tableRestoreStatus_status' - A value that describes the current state of the table restore request.
-- Possible values include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@,
-- @IN_PROGRESS@.
--
-- 'tableRestoreRequestId', 'tableRestoreStatus_tableRestoreRequestId' - The ID of the RestoreTableFromSnapshot request.
--
-- 'targetDatabaseName', 'tableRestoreStatus_targetDatabaseName' - The name of the database to restore to.
--
-- 'targetSchemaName', 'tableRestoreStatus_targetSchemaName' - The name of the schema to restore to.
--
-- 'totalDataInMegaBytes', 'tableRestoreStatus_totalDataInMegaBytes' - The total amount of data to restore to the new table, in megabytes (MB).
--
-- 'workgroupName', 'tableRestoreStatus_workgroupName' - The name of the workgroup being restored from.
newTableRestoreStatus ::
  TableRestoreStatus
newTableRestoreStatus =
  TableRestoreStatus'
    { message = Prelude.Nothing,
      namespaceName = Prelude.Nothing,
      newTableName' = Prelude.Nothing,
      progressInMegaBytes = Prelude.Nothing,
      requestTime = Prelude.Nothing,
      snapshotName = Prelude.Nothing,
      sourceDatabaseName = Prelude.Nothing,
      sourceSchemaName = Prelude.Nothing,
      sourceTableName = Prelude.Nothing,
      status = Prelude.Nothing,
      tableRestoreRequestId = Prelude.Nothing,
      targetDatabaseName = Prelude.Nothing,
      targetSchemaName = Prelude.Nothing,
      totalDataInMegaBytes = Prelude.Nothing,
      workgroupName = Prelude.Nothing
    }

-- | A description of the status of the table restore request. Status values
-- include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@, @IN_PROGRESS@.
tableRestoreStatus_message :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_message = Lens.lens (\TableRestoreStatus' {message} -> message) (\s@TableRestoreStatus' {} a -> s {message = a} :: TableRestoreStatus)

-- | The namespace of the table being restored from.
tableRestoreStatus_namespaceName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_namespaceName = Lens.lens (\TableRestoreStatus' {namespaceName} -> namespaceName) (\s@TableRestoreStatus' {} a -> s {namespaceName = a} :: TableRestoreStatus)

-- | The name of the table to create from the restore operation.
tableRestoreStatus_newTableName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_newTableName = Lens.lens (\TableRestoreStatus' {newTableName'} -> newTableName') (\s@TableRestoreStatus' {} a -> s {newTableName' = a} :: TableRestoreStatus)

-- | The amount of data restored to the new table so far, in megabytes (MB).
tableRestoreStatus_progressInMegaBytes :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Integer)
tableRestoreStatus_progressInMegaBytes = Lens.lens (\TableRestoreStatus' {progressInMegaBytes} -> progressInMegaBytes) (\s@TableRestoreStatus' {} a -> s {progressInMegaBytes = a} :: TableRestoreStatus)

-- | The time that the table restore request was made, in Universal
-- Coordinated Time (UTC).
tableRestoreStatus_requestTime :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.UTCTime)
tableRestoreStatus_requestTime = Lens.lens (\TableRestoreStatus' {requestTime} -> requestTime) (\s@TableRestoreStatus' {} a -> s {requestTime = a} :: TableRestoreStatus) Prelude.. Lens.mapping Data._Time

-- | The name of the snapshot being restored from.
tableRestoreStatus_snapshotName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_snapshotName = Lens.lens (\TableRestoreStatus' {snapshotName} -> snapshotName) (\s@TableRestoreStatus' {} a -> s {snapshotName = a} :: TableRestoreStatus)

-- | The name of the source database being restored from.
tableRestoreStatus_sourceDatabaseName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_sourceDatabaseName = Lens.lens (\TableRestoreStatus' {sourceDatabaseName} -> sourceDatabaseName) (\s@TableRestoreStatus' {} a -> s {sourceDatabaseName = a} :: TableRestoreStatus)

-- | The name of the source schema being restored from.
tableRestoreStatus_sourceSchemaName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_sourceSchemaName = Lens.lens (\TableRestoreStatus' {sourceSchemaName} -> sourceSchemaName) (\s@TableRestoreStatus' {} a -> s {sourceSchemaName = a} :: TableRestoreStatus)

-- | The name of the source table being restored from.
tableRestoreStatus_sourceTableName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_sourceTableName = Lens.lens (\TableRestoreStatus' {sourceTableName} -> sourceTableName) (\s@TableRestoreStatus' {} a -> s {sourceTableName = a} :: TableRestoreStatus)

-- | A value that describes the current state of the table restore request.
-- Possible values include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@,
-- @IN_PROGRESS@.
tableRestoreStatus_status :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_status = Lens.lens (\TableRestoreStatus' {status} -> status) (\s@TableRestoreStatus' {} a -> s {status = a} :: TableRestoreStatus)

-- | The ID of the RestoreTableFromSnapshot request.
tableRestoreStatus_tableRestoreRequestId :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_tableRestoreRequestId = Lens.lens (\TableRestoreStatus' {tableRestoreRequestId} -> tableRestoreRequestId) (\s@TableRestoreStatus' {} a -> s {tableRestoreRequestId = a} :: TableRestoreStatus)

-- | The name of the database to restore to.
tableRestoreStatus_targetDatabaseName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_targetDatabaseName = Lens.lens (\TableRestoreStatus' {targetDatabaseName} -> targetDatabaseName) (\s@TableRestoreStatus' {} a -> s {targetDatabaseName = a} :: TableRestoreStatus)

-- | The name of the schema to restore to.
tableRestoreStatus_targetSchemaName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_targetSchemaName = Lens.lens (\TableRestoreStatus' {targetSchemaName} -> targetSchemaName) (\s@TableRestoreStatus' {} a -> s {targetSchemaName = a} :: TableRestoreStatus)

-- | The total amount of data to restore to the new table, in megabytes (MB).
tableRestoreStatus_totalDataInMegaBytes :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Integer)
tableRestoreStatus_totalDataInMegaBytes = Lens.lens (\TableRestoreStatus' {totalDataInMegaBytes} -> totalDataInMegaBytes) (\s@TableRestoreStatus' {} a -> s {totalDataInMegaBytes = a} :: TableRestoreStatus)

-- | The name of the workgroup being restored from.
tableRestoreStatus_workgroupName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_workgroupName = Lens.lens (\TableRestoreStatus' {workgroupName} -> workgroupName) (\s@TableRestoreStatus' {} a -> s {workgroupName = a} :: TableRestoreStatus)

instance Data.FromJSON TableRestoreStatus where
  parseJSON =
    Data.withObject
      "TableRestoreStatus"
      ( \x ->
          TableRestoreStatus'
            Prelude.<$> (x Data..:? "message")
            Prelude.<*> (x Data..:? "namespaceName")
            Prelude.<*> (x Data..:? "newTableName")
            Prelude.<*> (x Data..:? "progressInMegaBytes")
            Prelude.<*> (x Data..:? "requestTime")
            Prelude.<*> (x Data..:? "snapshotName")
            Prelude.<*> (x Data..:? "sourceDatabaseName")
            Prelude.<*> (x Data..:? "sourceSchemaName")
            Prelude.<*> (x Data..:? "sourceTableName")
            Prelude.<*> (x Data..:? "status")
            Prelude.<*> (x Data..:? "tableRestoreRequestId")
            Prelude.<*> (x Data..:? "targetDatabaseName")
            Prelude.<*> (x Data..:? "targetSchemaName")
            Prelude.<*> (x Data..:? "totalDataInMegaBytes")
            Prelude.<*> (x Data..:? "workgroupName")
      )

instance Prelude.Hashable TableRestoreStatus where
  hashWithSalt _salt TableRestoreStatus' {..} =
    _salt
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` namespaceName
      `Prelude.hashWithSalt` newTableName'
      `Prelude.hashWithSalt` progressInMegaBytes
      `Prelude.hashWithSalt` requestTime
      `Prelude.hashWithSalt` snapshotName
      `Prelude.hashWithSalt` sourceDatabaseName
      `Prelude.hashWithSalt` sourceSchemaName
      `Prelude.hashWithSalt` sourceTableName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` tableRestoreRequestId
      `Prelude.hashWithSalt` targetDatabaseName
      `Prelude.hashWithSalt` targetSchemaName
      `Prelude.hashWithSalt` totalDataInMegaBytes
      `Prelude.hashWithSalt` workgroupName

instance Prelude.NFData TableRestoreStatus where
  rnf TableRestoreStatus' {..} =
    Prelude.rnf message
      `Prelude.seq` Prelude.rnf namespaceName
      `Prelude.seq` Prelude.rnf newTableName'
      `Prelude.seq` Prelude.rnf progressInMegaBytes
      `Prelude.seq` Prelude.rnf requestTime
      `Prelude.seq` Prelude.rnf snapshotName
      `Prelude.seq` Prelude.rnf sourceDatabaseName
      `Prelude.seq` Prelude.rnf sourceSchemaName
      `Prelude.seq` Prelude.rnf sourceTableName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf tableRestoreRequestId
      `Prelude.seq` Prelude.rnf targetDatabaseName
      `Prelude.seq` Prelude.rnf targetSchemaName
      `Prelude.seq` Prelude.rnf totalDataInMegaBytes
      `Prelude.seq` Prelude.rnf workgroupName
