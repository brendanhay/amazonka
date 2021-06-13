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
-- Module      : Network.AWS.DMS.ModifyReplicationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
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
-- For more information about AWS DMS tasks, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks>
-- in the /AWS Database Migration Service User Guide/.
module Network.AWS.DMS.ModifyReplicationTask
  ( -- * Creating a Request
    ModifyReplicationTask (..),
    newModifyReplicationTask,

    -- * Request Lenses
    modifyReplicationTask_migrationType,
    modifyReplicationTask_taskData,
    modifyReplicationTask_replicationTaskSettings,
    modifyReplicationTask_tableMappings,
    modifyReplicationTask_cdcStartTime,
    modifyReplicationTask_cdcStopPosition,
    modifyReplicationTask_cdcStartPosition,
    modifyReplicationTask_replicationTaskIdentifier,
    modifyReplicationTask_replicationTaskArn,

    -- * Destructuring the Response
    ModifyReplicationTaskResponse (..),
    newModifyReplicationTaskResponse,

    -- * Response Lenses
    modifyReplicationTaskResponse_replicationTask,
    modifyReplicationTaskResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- |
--
-- /See:/ 'newModifyReplicationTask' smart constructor.
data ModifyReplicationTask = ModifyReplicationTask'
  { -- | The migration type. Valid values: @full-load@ | @cdc@ |
    -- @full-load-and-cdc@
    migrationType :: Prelude.Maybe MigrationTypeValue,
    -- | Supplemental information that the task requires to migrate the data for
    -- certain source and target endpoints. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
    -- in the /AWS Database Migration Service User Guide./
    taskData :: Prelude.Maybe Prelude.Text,
    -- | JSON file that contains settings for the task, such as task metadata
    -- settings.
    replicationTaskSettings :: Prelude.Maybe Prelude.Text,
    -- | When using the AWS CLI or boto3, provide the path of the JSON file that
    -- contains the table mappings. Precede the path with @file:\/\/@. When
    -- working with the DMS API, provide the JSON as the parameter value, for
    -- example: @--table-mappings file:\/\/mappingfile.json@
    tableMappings :: Prelude.Maybe Prelude.Text,
    -- | Indicates the start time for a change data capture (CDC) operation. Use
    -- either CdcStartTime or CdcStartPosition to specify when you want a CDC
    -- operation to start. Specifying both values results in an error.
    --
    -- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
    cdcStartTime :: Prelude.Maybe Core.POSIX,
    -- | Indicates when you want a change data capture (CDC) operation to stop.
    -- The value can be either server time or commit time.
    --
    -- Server time example: --cdc-stop-position
    -- “server_time:2018-02-09T12:12:12”
    --
    -- Commit time example: --cdc-stop-position “commit_time:
    -- 2018-02-09T12:12:12 “
    cdcStopPosition :: Prelude.Maybe Prelude.Text,
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
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra Connection Attributes When Using PostgreSQL as a Source for AWS DMS>.
    cdcStartPosition :: Prelude.Maybe Prelude.Text,
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
-- 'migrationType', 'modifyReplicationTask_migrationType' - The migration type. Valid values: @full-load@ | @cdc@ |
-- @full-load-and-cdc@
--
-- 'taskData', 'modifyReplicationTask_taskData' - Supplemental information that the task requires to migrate the data for
-- certain source and target endpoints. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
-- in the /AWS Database Migration Service User Guide./
--
-- 'replicationTaskSettings', 'modifyReplicationTask_replicationTaskSettings' - JSON file that contains settings for the task, such as task metadata
-- settings.
--
-- 'tableMappings', 'modifyReplicationTask_tableMappings' - When using the AWS CLI or boto3, provide the path of the JSON file that
-- contains the table mappings. Precede the path with @file:\/\/@. When
-- working with the DMS API, provide the JSON as the parameter value, for
-- example: @--table-mappings file:\/\/mappingfile.json@
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
-- 2018-02-09T12:12:12 “
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
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra Connection Attributes When Using PostgreSQL as a Source for AWS DMS>.
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
-- 'replicationTaskArn', 'modifyReplicationTask_replicationTaskArn' - The Amazon Resource Name (ARN) of the replication task.
newModifyReplicationTask ::
  -- | 'replicationTaskArn'
  Prelude.Text ->
  ModifyReplicationTask
newModifyReplicationTask pReplicationTaskArn_ =
  ModifyReplicationTask'
    { migrationType =
        Prelude.Nothing,
      taskData = Prelude.Nothing,
      replicationTaskSettings = Prelude.Nothing,
      tableMappings = Prelude.Nothing,
      cdcStartTime = Prelude.Nothing,
      cdcStopPosition = Prelude.Nothing,
      cdcStartPosition = Prelude.Nothing,
      replicationTaskIdentifier = Prelude.Nothing,
      replicationTaskArn = pReplicationTaskArn_
    }

-- | The migration type. Valid values: @full-load@ | @cdc@ |
-- @full-load-and-cdc@
modifyReplicationTask_migrationType :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe MigrationTypeValue)
modifyReplicationTask_migrationType = Lens.lens (\ModifyReplicationTask' {migrationType} -> migrationType) (\s@ModifyReplicationTask' {} a -> s {migrationType = a} :: ModifyReplicationTask)

-- | Supplemental information that the task requires to migrate the data for
-- certain source and target endpoints. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
-- in the /AWS Database Migration Service User Guide./
modifyReplicationTask_taskData :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_taskData = Lens.lens (\ModifyReplicationTask' {taskData} -> taskData) (\s@ModifyReplicationTask' {} a -> s {taskData = a} :: ModifyReplicationTask)

-- | JSON file that contains settings for the task, such as task metadata
-- settings.
modifyReplicationTask_replicationTaskSettings :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_replicationTaskSettings = Lens.lens (\ModifyReplicationTask' {replicationTaskSettings} -> replicationTaskSettings) (\s@ModifyReplicationTask' {} a -> s {replicationTaskSettings = a} :: ModifyReplicationTask)

-- | When using the AWS CLI or boto3, provide the path of the JSON file that
-- contains the table mappings. Precede the path with @file:\/\/@. When
-- working with the DMS API, provide the JSON as the parameter value, for
-- example: @--table-mappings file:\/\/mappingfile.json@
modifyReplicationTask_tableMappings :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_tableMappings = Lens.lens (\ModifyReplicationTask' {tableMappings} -> tableMappings) (\s@ModifyReplicationTask' {} a -> s {tableMappings = a} :: ModifyReplicationTask)

-- | Indicates the start time for a change data capture (CDC) operation. Use
-- either CdcStartTime or CdcStartPosition to specify when you want a CDC
-- operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
modifyReplicationTask_cdcStartTime :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.UTCTime)
modifyReplicationTask_cdcStartTime = Lens.lens (\ModifyReplicationTask' {cdcStartTime} -> cdcStartTime) (\s@ModifyReplicationTask' {} a -> s {cdcStartTime = a} :: ModifyReplicationTask) Prelude.. Lens.mapping Core._Time

-- | Indicates when you want a change data capture (CDC) operation to stop.
-- The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position
-- “server_time:2018-02-09T12:12:12”
--
-- Commit time example: --cdc-stop-position “commit_time:
-- 2018-02-09T12:12:12 “
modifyReplicationTask_cdcStopPosition :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_cdcStopPosition = Lens.lens (\ModifyReplicationTask' {cdcStopPosition} -> cdcStopPosition) (\s@ModifyReplicationTask' {} a -> s {cdcStopPosition = a} :: ModifyReplicationTask)

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
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Source.PostgreSQL.html#CHAP_Source.PostgreSQL.ConnectionAttrib Extra Connection Attributes When Using PostgreSQL as a Source for AWS DMS>.
modifyReplicationTask_cdcStartPosition :: Lens.Lens' ModifyReplicationTask (Prelude.Maybe Prelude.Text)
modifyReplicationTask_cdcStartPosition = Lens.lens (\ModifyReplicationTask' {cdcStartPosition} -> cdcStartPosition) (\s@ModifyReplicationTask' {} a -> s {cdcStartPosition = a} :: ModifyReplicationTask)

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

-- | The Amazon Resource Name (ARN) of the replication task.
modifyReplicationTask_replicationTaskArn :: Lens.Lens' ModifyReplicationTask Prelude.Text
modifyReplicationTask_replicationTaskArn = Lens.lens (\ModifyReplicationTask' {replicationTaskArn} -> replicationTaskArn) (\s@ModifyReplicationTask' {} a -> s {replicationTaskArn = a} :: ModifyReplicationTask)

instance Core.AWSRequest ModifyReplicationTask where
  type
    AWSResponse ModifyReplicationTask =
      ModifyReplicationTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ModifyReplicationTaskResponse'
            Prelude.<$> (x Core..?> "ReplicationTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ModifyReplicationTask

instance Prelude.NFData ModifyReplicationTask

instance Core.ToHeaders ModifyReplicationTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.ModifyReplicationTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ModifyReplicationTask where
  toJSON ModifyReplicationTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("MigrationType" Core..=) Prelude.<$> migrationType,
            ("TaskData" Core..=) Prelude.<$> taskData,
            ("ReplicationTaskSettings" Core..=)
              Prelude.<$> replicationTaskSettings,
            ("TableMappings" Core..=) Prelude.<$> tableMappings,
            ("CdcStartTime" Core..=) Prelude.<$> cdcStartTime,
            ("CdcStopPosition" Core..=)
              Prelude.<$> cdcStopPosition,
            ("CdcStartPosition" Core..=)
              Prelude.<$> cdcStartPosition,
            ("ReplicationTaskIdentifier" Core..=)
              Prelude.<$> replicationTaskIdentifier,
            Prelude.Just
              ("ReplicationTaskArn" Core..= replicationTaskArn)
          ]
      )

instance Core.ToPath ModifyReplicationTask where
  toPath = Prelude.const "/"

instance Core.ToQuery ModifyReplicationTask where
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

instance Prelude.NFData ModifyReplicationTaskResponse
