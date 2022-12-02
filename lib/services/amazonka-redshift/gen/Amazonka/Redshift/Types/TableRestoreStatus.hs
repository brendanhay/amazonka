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
-- Module      : Amazonka.Redshift.Types.TableRestoreStatus
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Redshift.Types.TableRestoreStatus where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Redshift.Internal
import Amazonka.Redshift.Types.TableRestoreStatusType

-- | Describes the status of a RestoreTableFromClusterSnapshot operation.
--
-- /See:/ 'newTableRestoreStatus' smart constructor.
data TableRestoreStatus = TableRestoreStatus'
  { -- | The total amount of data to restore to the new table, in megabytes (MB).
    totalDataInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | The identifier of the Amazon Redshift cluster that the table is being
    -- restored to.
    clusterIdentifier :: Prelude.Maybe Prelude.Text,
    -- | A description of the status of the table restore request. Status values
    -- include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@, @IN_PROGRESS@.
    message :: Prelude.Maybe Prelude.Text,
    -- | The name of the table to create as a result of the table restore
    -- request.
    newTableName' :: Prelude.Maybe Prelude.Text,
    -- | The name of the schema to restore the table to.
    targetSchemaName :: Prelude.Maybe Prelude.Text,
    -- | The time that the table restore request was made, in Universal
    -- Coordinated Time (UTC).
    requestTime :: Prelude.Maybe Data.ISO8601,
    -- | The identifier of the snapshot that the table is being restored from.
    snapshotIdentifier :: Prelude.Maybe Prelude.Text,
    -- | The name of the source database that contains the table being restored.
    sourceDatabaseName :: Prelude.Maybe Prelude.Text,
    -- | The name of the source schema that contains the table being restored.
    sourceSchemaName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the table restore request.
    tableRestoreRequestId :: Prelude.Maybe Prelude.Text,
    -- | The name of the database to restore the table to.
    targetDatabaseName :: Prelude.Maybe Prelude.Text,
    -- | A value that describes the current state of the table restore request.
    --
    -- Valid Values: @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@,
    -- @IN_PROGRESS@
    status :: Prelude.Maybe TableRestoreStatusType,
    -- | The amount of data restored to the new table so far, in megabytes (MB).
    progressInMegaBytes :: Prelude.Maybe Prelude.Integer,
    -- | The name of the source table being restored.
    sourceTableName :: Prelude.Maybe Prelude.Text
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
-- 'totalDataInMegaBytes', 'tableRestoreStatus_totalDataInMegaBytes' - The total amount of data to restore to the new table, in megabytes (MB).
--
-- 'clusterIdentifier', 'tableRestoreStatus_clusterIdentifier' - The identifier of the Amazon Redshift cluster that the table is being
-- restored to.
--
-- 'message', 'tableRestoreStatus_message' - A description of the status of the table restore request. Status values
-- include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@, @IN_PROGRESS@.
--
-- 'newTableName'', 'tableRestoreStatus_newTableName' - The name of the table to create as a result of the table restore
-- request.
--
-- 'targetSchemaName', 'tableRestoreStatus_targetSchemaName' - The name of the schema to restore the table to.
--
-- 'requestTime', 'tableRestoreStatus_requestTime' - The time that the table restore request was made, in Universal
-- Coordinated Time (UTC).
--
-- 'snapshotIdentifier', 'tableRestoreStatus_snapshotIdentifier' - The identifier of the snapshot that the table is being restored from.
--
-- 'sourceDatabaseName', 'tableRestoreStatus_sourceDatabaseName' - The name of the source database that contains the table being restored.
--
-- 'sourceSchemaName', 'tableRestoreStatus_sourceSchemaName' - The name of the source schema that contains the table being restored.
--
-- 'tableRestoreRequestId', 'tableRestoreStatus_tableRestoreRequestId' - The unique identifier for the table restore request.
--
-- 'targetDatabaseName', 'tableRestoreStatus_targetDatabaseName' - The name of the database to restore the table to.
--
-- 'status', 'tableRestoreStatus_status' - A value that describes the current state of the table restore request.
--
-- Valid Values: @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@,
-- @IN_PROGRESS@
--
-- 'progressInMegaBytes', 'tableRestoreStatus_progressInMegaBytes' - The amount of data restored to the new table so far, in megabytes (MB).
--
-- 'sourceTableName', 'tableRestoreStatus_sourceTableName' - The name of the source table being restored.
newTableRestoreStatus ::
  TableRestoreStatus
newTableRestoreStatus =
  TableRestoreStatus'
    { totalDataInMegaBytes =
        Prelude.Nothing,
      clusterIdentifier = Prelude.Nothing,
      message = Prelude.Nothing,
      newTableName' = Prelude.Nothing,
      targetSchemaName = Prelude.Nothing,
      requestTime = Prelude.Nothing,
      snapshotIdentifier = Prelude.Nothing,
      sourceDatabaseName = Prelude.Nothing,
      sourceSchemaName = Prelude.Nothing,
      tableRestoreRequestId = Prelude.Nothing,
      targetDatabaseName = Prelude.Nothing,
      status = Prelude.Nothing,
      progressInMegaBytes = Prelude.Nothing,
      sourceTableName = Prelude.Nothing
    }

-- | The total amount of data to restore to the new table, in megabytes (MB).
tableRestoreStatus_totalDataInMegaBytes :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Integer)
tableRestoreStatus_totalDataInMegaBytes = Lens.lens (\TableRestoreStatus' {totalDataInMegaBytes} -> totalDataInMegaBytes) (\s@TableRestoreStatus' {} a -> s {totalDataInMegaBytes = a} :: TableRestoreStatus)

-- | The identifier of the Amazon Redshift cluster that the table is being
-- restored to.
tableRestoreStatus_clusterIdentifier :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_clusterIdentifier = Lens.lens (\TableRestoreStatus' {clusterIdentifier} -> clusterIdentifier) (\s@TableRestoreStatus' {} a -> s {clusterIdentifier = a} :: TableRestoreStatus)

-- | A description of the status of the table restore request. Status values
-- include @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@, @IN_PROGRESS@.
tableRestoreStatus_message :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_message = Lens.lens (\TableRestoreStatus' {message} -> message) (\s@TableRestoreStatus' {} a -> s {message = a} :: TableRestoreStatus)

-- | The name of the table to create as a result of the table restore
-- request.
tableRestoreStatus_newTableName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_newTableName = Lens.lens (\TableRestoreStatus' {newTableName'} -> newTableName') (\s@TableRestoreStatus' {} a -> s {newTableName' = a} :: TableRestoreStatus)

-- | The name of the schema to restore the table to.
tableRestoreStatus_targetSchemaName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_targetSchemaName = Lens.lens (\TableRestoreStatus' {targetSchemaName} -> targetSchemaName) (\s@TableRestoreStatus' {} a -> s {targetSchemaName = a} :: TableRestoreStatus)

-- | The time that the table restore request was made, in Universal
-- Coordinated Time (UTC).
tableRestoreStatus_requestTime :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.UTCTime)
tableRestoreStatus_requestTime = Lens.lens (\TableRestoreStatus' {requestTime} -> requestTime) (\s@TableRestoreStatus' {} a -> s {requestTime = a} :: TableRestoreStatus) Prelude.. Lens.mapping Data._Time

-- | The identifier of the snapshot that the table is being restored from.
tableRestoreStatus_snapshotIdentifier :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_snapshotIdentifier = Lens.lens (\TableRestoreStatus' {snapshotIdentifier} -> snapshotIdentifier) (\s@TableRestoreStatus' {} a -> s {snapshotIdentifier = a} :: TableRestoreStatus)

-- | The name of the source database that contains the table being restored.
tableRestoreStatus_sourceDatabaseName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_sourceDatabaseName = Lens.lens (\TableRestoreStatus' {sourceDatabaseName} -> sourceDatabaseName) (\s@TableRestoreStatus' {} a -> s {sourceDatabaseName = a} :: TableRestoreStatus)

-- | The name of the source schema that contains the table being restored.
tableRestoreStatus_sourceSchemaName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_sourceSchemaName = Lens.lens (\TableRestoreStatus' {sourceSchemaName} -> sourceSchemaName) (\s@TableRestoreStatus' {} a -> s {sourceSchemaName = a} :: TableRestoreStatus)

-- | The unique identifier for the table restore request.
tableRestoreStatus_tableRestoreRequestId :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_tableRestoreRequestId = Lens.lens (\TableRestoreStatus' {tableRestoreRequestId} -> tableRestoreRequestId) (\s@TableRestoreStatus' {} a -> s {tableRestoreRequestId = a} :: TableRestoreStatus)

-- | The name of the database to restore the table to.
tableRestoreStatus_targetDatabaseName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_targetDatabaseName = Lens.lens (\TableRestoreStatus' {targetDatabaseName} -> targetDatabaseName) (\s@TableRestoreStatus' {} a -> s {targetDatabaseName = a} :: TableRestoreStatus)

-- | A value that describes the current state of the table restore request.
--
-- Valid Values: @SUCCEEDED@, @FAILED@, @CANCELED@, @PENDING@,
-- @IN_PROGRESS@
tableRestoreStatus_status :: Lens.Lens' TableRestoreStatus (Prelude.Maybe TableRestoreStatusType)
tableRestoreStatus_status = Lens.lens (\TableRestoreStatus' {status} -> status) (\s@TableRestoreStatus' {} a -> s {status = a} :: TableRestoreStatus)

-- | The amount of data restored to the new table so far, in megabytes (MB).
tableRestoreStatus_progressInMegaBytes :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Integer)
tableRestoreStatus_progressInMegaBytes = Lens.lens (\TableRestoreStatus' {progressInMegaBytes} -> progressInMegaBytes) (\s@TableRestoreStatus' {} a -> s {progressInMegaBytes = a} :: TableRestoreStatus)

-- | The name of the source table being restored.
tableRestoreStatus_sourceTableName :: Lens.Lens' TableRestoreStatus (Prelude.Maybe Prelude.Text)
tableRestoreStatus_sourceTableName = Lens.lens (\TableRestoreStatus' {sourceTableName} -> sourceTableName) (\s@TableRestoreStatus' {} a -> s {sourceTableName = a} :: TableRestoreStatus)

instance Data.FromXML TableRestoreStatus where
  parseXML x =
    TableRestoreStatus'
      Prelude.<$> (x Data..@? "TotalDataInMegaBytes")
      Prelude.<*> (x Data..@? "ClusterIdentifier")
      Prelude.<*> (x Data..@? "Message")
      Prelude.<*> (x Data..@? "NewTableName")
      Prelude.<*> (x Data..@? "TargetSchemaName")
      Prelude.<*> (x Data..@? "RequestTime")
      Prelude.<*> (x Data..@? "SnapshotIdentifier")
      Prelude.<*> (x Data..@? "SourceDatabaseName")
      Prelude.<*> (x Data..@? "SourceSchemaName")
      Prelude.<*> (x Data..@? "TableRestoreRequestId")
      Prelude.<*> (x Data..@? "TargetDatabaseName")
      Prelude.<*> (x Data..@? "Status")
      Prelude.<*> (x Data..@? "ProgressInMegaBytes")
      Prelude.<*> (x Data..@? "SourceTableName")

instance Prelude.Hashable TableRestoreStatus where
  hashWithSalt _salt TableRestoreStatus' {..} =
    _salt `Prelude.hashWithSalt` totalDataInMegaBytes
      `Prelude.hashWithSalt` clusterIdentifier
      `Prelude.hashWithSalt` message
      `Prelude.hashWithSalt` newTableName'
      `Prelude.hashWithSalt` targetSchemaName
      `Prelude.hashWithSalt` requestTime
      `Prelude.hashWithSalt` snapshotIdentifier
      `Prelude.hashWithSalt` sourceDatabaseName
      `Prelude.hashWithSalt` sourceSchemaName
      `Prelude.hashWithSalt` tableRestoreRequestId
      `Prelude.hashWithSalt` targetDatabaseName
      `Prelude.hashWithSalt` status
      `Prelude.hashWithSalt` progressInMegaBytes
      `Prelude.hashWithSalt` sourceTableName

instance Prelude.NFData TableRestoreStatus where
  rnf TableRestoreStatus' {..} =
    Prelude.rnf totalDataInMegaBytes
      `Prelude.seq` Prelude.rnf clusterIdentifier
      `Prelude.seq` Prelude.rnf message
      `Prelude.seq` Prelude.rnf newTableName'
      `Prelude.seq` Prelude.rnf targetSchemaName
      `Prelude.seq` Prelude.rnf requestTime
      `Prelude.seq` Prelude.rnf snapshotIdentifier
      `Prelude.seq` Prelude.rnf sourceDatabaseName
      `Prelude.seq` Prelude.rnf sourceSchemaName
      `Prelude.seq` Prelude.rnf tableRestoreRequestId
      `Prelude.seq` Prelude.rnf targetDatabaseName
      `Prelude.seq` Prelude.rnf status
      `Prelude.seq` Prelude.rnf progressInMegaBytes
      `Prelude.seq` Prelude.rnf sourceTableName
