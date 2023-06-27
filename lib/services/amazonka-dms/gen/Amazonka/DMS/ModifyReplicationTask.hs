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
-- Module      : Amazonka.DMS.ModifyReplicationTask
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified replication task.
--
-- You can\'t modify the task endpoints. The task must be stopped before
-- you can modify it.
--
-- For more information about DMS tasks, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks>
-- in the /Database Migration Service User Guide/.
module Amazonka.DMS.ModifyReplicationTask
  ( -- * Creating a Request
    ModifyReplicationTask (..),
    newModifyReplicationTask,

    -- * Request Lenses
    modifyReplicationTask_cdcStartPosition,
    modifyReplicationTask_cdcStartTime,
    modifyReplicationTask_cdcStopPosition,
    modifyReplicationTask_migrationType,
    modifyReplicationTask_replicationTaskIdentifier,
    modifyReplicationTask_replicationTaskSettings,
    modifyReplicationTask_tableMappings,
    modifyReplicationTask_taskData,
    modifyReplicationTask_replicationTaskArn,

    -- * Destructuring the Response
    ModifyReplicationTaskResponse (..),
    newModifyReplicationTaskResponse,

    -- * Response Lenses
    modifyReplicationTaskResponse_replicationTask,
    modifyReplicationTaskResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.DMS.Types
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- |
--
-- /See:/ 'newModifyReplicationTask' smart constructor.
data ModifyReplicationTask = ModifyReplicationTask'
  { -- | Indicates when you want a change data capture (CDC) operation to start.
    -- Use either CdcStartPosition or CdcStartTime to specify when you want a
    -- CDC operation to start. Specifying both values results in an error.
    --
    -- The value can be in date, checkpoint, or LSN\/SCN format.
    --
    -- Date Example: --cdc-start-position “2018-03-08T12:12:12”
    --
    -- Checkpoint Example: --cdc-start-position
    -- \"checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93\"
    --
    -- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
    --
    -- When you use this task setting with a source PostgreSQL database, a
    -- logical replication slot should already be created and associated with
    -- the source endpoint. You can verify this by setting the @slotName@ extra
    -- connection attribute to the name of this logical replication slot. For
    -- more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra Connection Attributes When Using PostgreSQL as a Source for DMS>.
    cdcStartPosition :: Prelude.Maybe Prelude.Text,
    -- | Indicates the start time for a change data capture (CDC) operation. Use
    -- either CdcStartTime or CdcStartPosition to specify when you want a CDC
    -- operation to start. Specifying both values results in an error.
    --
    -- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
    cdcStartTime :: Prelude.Maybe Data.POSIX,
    -- | Indicates when you want a change data capture (CDC) operation to stop.
    -- The value can be either server time or commit time.
    --
    -- Server time example: --cdc-stop-position
    -- “server_time:2018-02-09T12:12:12”
    --
    -- Commit time example: --cdc-stop-position “commit_time:
    -- 2018-02-09T12:12:12“
    cdcStopPosition :: Prelude.Maybe Prelude.Text,
    -- | The migration type. Valid values: @full-load@ | @cdc@ |
    -- @full-load-and-cdc@
    migrationType :: Prelude.Maybe MigrationTypeValue,
    -- | The replication task identifier.
    --
    -- Constraints:
    --
    -- -   Must contain 1-255 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    replicationTaskIdentifier :: Prelude.Maybe Prelude.Text,
    -- | JSON file that contains settings for the task, such as task metadata
    -- settings.
    replicationTaskSettings :: Prelude.Maybe Prelude.Text,
    -- | When using the CLI or boto3, provide the path of the JSON file that
    -- contains the table mappings. Precede the path with @file:\/\/@. For
    -- example, @--table-mappings file:\/\/mappingfile.json@. When working with
    -- the DMS API, provide the JSON as the parameter value.
    tableMappings :: Prelude.Maybe Prelude.Text,
    -- | Supplemental information that the task requires to migrate the data for
    -- certain source and target endpoints. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
    -- in the /Database Migration Service User Guide./
    taskData :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the replication task.
    replicationTaskArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyReplicationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cdcStartPosition', 'modifyReplicationTask_cdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start.
-- Use either CdcStartPosition or CdcStartTime to specify when you want a
-- CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN\/SCN format.
--
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
--
-- Checkpoint Example: --cdc-start-position
-- \"checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93\"
--
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- When you use this task setting with a source PostgreSQL database, a
-- logical replication slot should already be created and associated with
-- the source endpoint. You can verify this by setting the @slotName@ extra
-- connection attribute to the name of this logical replication slot. For
-- more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra Connection Attributes When Using PostgreSQL as a Source for DMS>.
--
-- 'cdcStartTime', 'modifyReplicationTask_cdcStartTime' - Indicates the start time for a change data capture (CDC) operation. Use
-- either CdcStartTime or CdcStartPosition to specify when you want a CDC
-- operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- 'cdcStopPosition', 'modifyReplicationTask_cdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop.
-- The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position
-- “server_time:2018-02-09T12:12:12”
--
-- Commit time example: --cdc-stop-position “commit_time:
-- 2018-02-09T12:12:12“
--
-- 'migrationType', 'modifyReplicationTask_migrationType' - The migration type. Valid values: @full-load@ | @cdc@ |
-- @full-load-and-cdc@
--
-- 'replicationTaskIdentifier', 'modifyReplicationTask_replicationTaskIdentifier' - The replication task identifier.
--
-- Constraints:
--
-- -   Must contain 1-255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- 'replicationTaskSettings', 'modifyReplicationTask_replicationTaskSettings' - JSON file that contains settings for the task, such as task metadata
-- settings.
--
-- 'tableMappings', 'modifyReplicationTask_tableMappings' - When using the CLI or boto3, provide the path of the JSON file that
-- contains the table mappings. Precede the path with @file:\/\/@. For
-- example, @--table-mappings file:\/\/mappingfile.json@. When working with
-- the DMS API, provide the JSON as the parameter value.
--
-- 'taskData', 'modifyReplicationTask_taskData' - Supplemental information that the task requires to migrate the data for
-- certain source and target endpoints. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
-- in the /Database Migration Service User Guide./
--
-- 'replicationTaskArn', 'modifyReplicationTask_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
newModifyReplicationTask ::
  -- | 'replicationTaskArn'
  Prelude.Text ->
  ModifyReplicationTask
newModifyReplicationTask pReplicationTaskArn_ =
  ModifyReplicationTask'
    { cdcStartPosition =
        Prelude.Nothing,
      cdcStartTime = Prelude.Nothing,
      cdcStopPosition = Prelude.Nothing,
      migrationType = Prelude.Nothing,
      replicationTaskIdentifier = Prelude.Nothing,
      replicationTaskSettings = Prelude.Nothing,
      tableMappings = Prelude.Nothing,
      taskData = Prelude.Nothing,
      replicationTaskArn = pReplicationTaskArn_
    }

-- | Indicates when you want a change data capture (CDC) operation to start.
-- Use either CdcStartPosition or CdcStartTime to specify when you want a
-- CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN\/SCN format.
--
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
--
-- Checkpoint Example: --cdc-start-position
-- \"checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93\"
--
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- When you use this task setting with a source PostgreSQL database, a
-- logical replication slot should already be created and associated with
-- the source endpoint. You can verify this by setting the @slotName@ extra
-- connection attribute to the name of this logical replication slot. For
-- more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra Connection Attributes When Using PostgreSQL as a Source for DMS>.
modifyReplicationTask_cdcStartPosition :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_cdcStartPosition = Lens.lens (\ModifyReplicationTask' {cdcStartPosition} -> cdcStartPosition) (\s@ModifyReplicationTask' {} a -> s {cdcStartPosition = a} :: ModifyReplicationTask)

-- | Indicates the start time for a change data capture (CDC) operation. Use
-- either CdcStartTime or CdcStartPosition to specify when you want a CDC
-- operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
modifyReplicationTask_cdcStartTime :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.UTCTime)
modifyReplicationTask_cdcStartTime = Lens.lens (\ModifyReplicationTask' {cdcStartTime} -> cdcStartTime) (\s@ModifyReplicationTask' {} a -> s {cdcStartTime = a} :: ModifyReplicationTask) Prelude.. Lens.mapping Data._Time

-- | Indicates when you want a change data capture (CDC) operation to stop.
-- The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position
-- “server_time:2018-02-09T12:12:12”
--
-- Commit time example: --cdc-stop-position “commit_time:
-- 2018-02-09T12:12:12“
modifyReplicationTask_cdcStopPosition :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_cdcStopPosition = Lens.lens (\ModifyReplicationTask' {cdcStopPosition} -> cdcStopPosition) (\s@ModifyReplicationTask' {} a -> s {cdcStopPosition = a} :: ModifyReplicationTask)

-- | The migration type. Valid values: @full-load@ | @cdc@ |
-- @full-load-and-cdc@
modifyReplicationTask_migrationType :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe MigrationTypeValue)
modifyReplicationTask_migrationType = Lens.lens (\ModifyReplicationTask' {migrationType} -> migrationType) (\s@ModifyReplicationTask' {} a -> s {migrationType = a} :: ModifyReplicationTask)

-- | The replication task identifier.
--
-- Constraints:
--
-- -   Must contain 1-255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
modifyReplicationTask_replicationTaskIdentifier :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_replicationTaskIdentifier = Lens.lens (\ModifyReplicationTask' {replicationTaskIdentifier} -> replicationTaskIdentifier) (\s@ModifyReplicationTask' {} a -> s {replicationTaskIdentifier = a} :: ModifyReplicationTask)

-- | JSON file that contains settings for the task, such as task metadata
-- settings.
modifyReplicationTask_replicationTaskSettings :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_replicationTaskSettings = Lens.lens (\ModifyReplicationTask' {replicationTaskSettings} -> replicationTaskSettings) (\s@ModifyReplicationTask' {} a -> s {replicationTaskSettings = a} :: ModifyReplicationTask)

-- | When using the CLI or boto3, provide the path of the JSON file that
-- contains the table mappings. Precede the path with @file:\/\/@. For
-- example, @--table-mappings file:\/\/mappingfile.json@. When working with
-- the DMS API, provide the JSON as the parameter value.
modifyReplicationTask_tableMappings :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_tableMappings = Lens.lens (\ModifyReplicationTask' {tableMappings} -> tableMappings) (\s@ModifyReplicationTask' {} a -> s {tableMappings = a} :: ModifyReplicationTask)

-- | Supplemental information that the task requires to migrate the data for
-- certain source and target endpoints. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
-- in the /Database Migration Service User Guide./
modifyReplicationTask_taskData :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_taskData = Lens.lens (\ModifyReplicationTask' {taskData} -> taskData) (\s@ModifyReplicationTask' {} a -> s {taskData = a} :: ModifyReplicationTask)

-- | The Amazon Resource Name (ARN) of the replication task.
modifyReplicationTask_replicationTaskArn :: Lens.Lens' ModifyReplicationTask Prelude.Text
modifyReplicationTask_replicationTaskArn = Lens.lens (\ModifyReplicationTask' {replicationTaskArn} -> replicationTaskArn) (\s@ModifyReplicationTask' {} a -> s {replicationTaskArn = a} :: ModifyReplicationTask)

instance Core.AWSRequest ModifyReplicationTask where
  type
    AWSResponse ModifyReplicationTask =
      ModifyReplicationTaskResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyReplicationTaskResponse'
            Prelude.<$> (x Data..?> "ReplicationTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyReplicationTask where
  hashWithSalt _salt ModifyReplicationTask' {..} =
    _salt
      `Prelude.hashWithSalt` cdcStartPosition
      `Prelude.hashWithSalt` cdcStartTime
      `Prelude.hashWithSalt` cdcStopPosition
      `Prelude.hashWithSalt` migrationType
      `Prelude.hashWithSalt` replicationTaskIdentifier
      `Prelude.hashWithSalt` replicationTaskSettings
      `Prelude.hashWithSalt` tableMappings
      `Prelude.hashWithSalt` taskData
      `Prelude.hashWithSalt` replicationTaskArn

instance Prelude.NFData ModifyReplicationTask where
  rnf ModifyReplicationTask' {..} =
    Prelude.rnf cdcStartPosition
      `Prelude.seq` Prelude.rnf cdcStartTime
      `Prelude.seq` Prelude.rnf cdcStopPosition
      `Prelude.seq` Prelude.rnf migrationType
      `Prelude.seq` Prelude.rnf replicationTaskIdentifier
      `Prelude.seq` Prelude.rnf replicationTaskSettings
      `Prelude.seq` Prelude.rnf tableMappings
      `Prelude.seq` Prelude.rnf taskData
      `Prelude.seq` Prelude.rnf replicationTaskArn

instance Data.ToHeaders ModifyReplicationTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AmazonDMSv20160101.ModifyReplicationTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ModifyReplicationTask where
  toJSON ModifyReplicationTask' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CdcStartPosition" Data..=)
              Prelude.<$> cdcStartPosition,
            ("CdcStartTime" Data..=) Prelude.<$> cdcStartTime,
            ("CdcStopPosition" Data..=)
              Prelude.<$> cdcStopPosition,
            ("MigrationType" Data..=) Prelude.<$> migrationType,
            ("ReplicationTaskIdentifier" Data..=)
              Prelude.<$> replicationTaskIdentifier,
            ("ReplicationTaskSettings" Data..=)
              Prelude.<$> replicationTaskSettings,
            ("TableMappings" Data..=) Prelude.<$> tableMappings,
            ("TaskData" Data..=) Prelude.<$> taskData,
            Prelude.Just
              ("ReplicationTaskArn" Data..= replicationTaskArn)
          ]
      )

instance Data.ToPath ModifyReplicationTask where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyReplicationTask where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newModifyReplicationTaskResponse' smart constructor.
data ModifyReplicationTaskResponse = ModifyReplicationTaskResponse'
  { -- | The replication task that was modified.
    replicationTask :: Prelude.Maybe ReplicationTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyReplicationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTask', 'modifyReplicationTaskResponse_replicationTask' - The replication task that was modified.
--
-- 'httpStatus', 'modifyReplicationTaskResponse_httpStatus' - The response's http status code.
newModifyReplicationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ModifyReplicationTaskResponse
newModifyReplicationTaskResponse pHttpStatus_ =
  ModifyReplicationTaskResponse'
    { replicationTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication task that was modified.
modifyReplicationTaskResponse_replicationTask :: Lens.Lens' ModifyReplicationTaskResponse (Prelude.Maybe ReplicationTask)
modifyReplicationTaskResponse_replicationTask = Lens.lens (\ModifyReplicationTaskResponse' {replicationTask} -> replicationTask) (\s@ModifyReplicationTaskResponse' {} a -> s {replicationTask = a} :: ModifyReplicationTaskResponse)

-- | The response's http status code.
modifyReplicationTaskResponse_httpStatus :: Lens.Lens' ModifyReplicationTaskResponse Prelude.Int
modifyReplicationTaskResponse_httpStatus = Lens.lens (\ModifyReplicationTaskResponse' {httpStatus} -> httpStatus) (\s@ModifyReplicationTaskResponse' {} a -> s {httpStatus = a} :: ModifyReplicationTaskResponse)

instance Prelude.NFData ModifyReplicationTaskResponse where
  rnf ModifyReplicationTaskResponse' {..} =
    Prelude.rnf replicationTask
      `Prelude.seq` Prelude.rnf httpStatus
