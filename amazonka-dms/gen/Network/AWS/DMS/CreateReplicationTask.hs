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
-- Module      : Network.AWS.DMS.CreateReplicationTask
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication task using the specified parameters.
module Network.AWS.DMS.CreateReplicationTask
  ( -- * Creating a Request
    CreateReplicationTask (..),
    newCreateReplicationTask,

    -- * Request Lenses
    createReplicationTask_taskData,
    createReplicationTask_replicationTaskSettings,
    createReplicationTask_tags,
    createReplicationTask_cdcStartTime,
    createReplicationTask_resourceIdentifier,
    createReplicationTask_cdcStopPosition,
    createReplicationTask_cdcStartPosition,
    createReplicationTask_replicationTaskIdentifier,
    createReplicationTask_sourceEndpointArn,
    createReplicationTask_targetEndpointArn,
    createReplicationTask_replicationInstanceArn,
    createReplicationTask_migrationType,
    createReplicationTask_tableMappings,

    -- * Destructuring the Response
    CreateReplicationTaskResponse (..),
    newCreateReplicationTaskResponse,

    -- * Response Lenses
    createReplicationTaskResponse_replicationTask,
    createReplicationTaskResponse_httpStatus,
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
-- /See:/ 'newCreateReplicationTask' smart constructor.
data CreateReplicationTask = CreateReplicationTask'
  { -- | Supplemental information that the task requires to migrate the data for
    -- certain source and target endpoints. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
    -- in the /AWS Database Migration Service User Guide./
    taskData :: Prelude.Maybe Prelude.Text,
    -- | Overall settings for the task, in JSON format. For more information, see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Specifying Task Settings for AWS Database Migration Service Tasks>
    -- in the /AWS Database Migration User Guide./
    replicationTaskSettings :: Prelude.Maybe Prelude.Text,
    -- | One or more tags to be assigned to the replication task.
    tags :: Prelude.Maybe [Tag],
    -- | Indicates the start time for a change data capture (CDC) operation. Use
    -- either CdcStartTime or CdcStartPosition to specify when you want a CDC
    -- operation to start. Specifying both values results in an error.
    --
    -- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
    cdcStartTime :: Prelude.Maybe Core.POSIX,
    -- | A friendly name for the resource identifier at the end of the
    -- @EndpointArn@ response parameter that is returned in the created
    -- @Endpoint@ object. The value for this parameter can have up to 31
    -- characters. It can contain only ASCII letters, digits, and hyphen
    -- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
    -- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
    -- For example, this value might result in the @EndpointArn@ value
    -- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
    -- specify a @ResourceIdentifier@ value, AWS DMS generates a default
    -- identifier value for the end of @EndpointArn@.
    resourceIdentifier :: Prelude.Maybe Prelude.Text,
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
    -- | An identifier for the replication task.
    --
    -- Constraints:
    --
    -- -   Must contain 1-255 alphanumeric characters or hyphens.
    --
    -- -   First character must be a letter.
    --
    -- -   Cannot end with a hyphen or contain two consecutive hyphens.
    replicationTaskIdentifier :: Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies the source
    -- endpoint.
    sourceEndpointArn :: Prelude.Text,
    -- | An Amazon Resource Name (ARN) that uniquely identifies the target
    -- endpoint.
    targetEndpointArn :: Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a replication instance.
    replicationInstanceArn :: Prelude.Text,
    -- | The migration type. Valid values: @full-load@ | @cdc@ |
    -- @full-load-and-cdc@
    migrationType :: MigrationTypeValue,
    -- | The table mappings for the task, in JSON format. For more information,
    -- see
    -- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TableMapping.html Using Table Mapping to Specify Task Settings>
    -- in the /AWS Database Migration Service User Guide./
    tableMappings :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationTask' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'taskData', 'createReplicationTask_taskData' - Supplemental information that the task requires to migrate the data for
-- certain source and target endpoints. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
-- in the /AWS Database Migration Service User Guide./
--
-- 'replicationTaskSettings', 'createReplicationTask_replicationTaskSettings' - Overall settings for the task, in JSON format. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Specifying Task Settings for AWS Database Migration Service Tasks>
-- in the /AWS Database Migration User Guide./
--
-- 'tags', 'createReplicationTask_tags' - One or more tags to be assigned to the replication task.
--
-- 'cdcStartTime', 'createReplicationTask_cdcStartTime' - Indicates the start time for a change data capture (CDC) operation. Use
-- either CdcStartTime or CdcStartPosition to specify when you want a CDC
-- operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- 'resourceIdentifier', 'createReplicationTask_resourceIdentifier' - A friendly name for the resource identifier at the end of the
-- @EndpointArn@ response parameter that is returned in the created
-- @Endpoint@ object. The value for this parameter can have up to 31
-- characters. It can contain only ASCII letters, digits, and hyphen
-- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
-- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
-- For example, this value might result in the @EndpointArn@ value
-- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
-- specify a @ResourceIdentifier@ value, AWS DMS generates a default
-- identifier value for the end of @EndpointArn@.
--
-- 'cdcStopPosition', 'createReplicationTask_cdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop.
-- The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position
-- “server_time:2018-02-09T12:12:12”
--
-- Commit time example: --cdc-stop-position “commit_time:
-- 2018-02-09T12:12:12 “
--
-- 'cdcStartPosition', 'createReplicationTask_cdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start.
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
-- 'replicationTaskIdentifier', 'createReplicationTask_replicationTaskIdentifier' - An identifier for the replication task.
--
-- Constraints:
--
-- -   Must contain 1-255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
--
-- 'sourceEndpointArn', 'createReplicationTask_sourceEndpointArn' - An Amazon Resource Name (ARN) that uniquely identifies the source
-- endpoint.
--
-- 'targetEndpointArn', 'createReplicationTask_targetEndpointArn' - An Amazon Resource Name (ARN) that uniquely identifies the target
-- endpoint.
--
-- 'replicationInstanceArn', 'createReplicationTask_replicationInstanceArn' - The Amazon Resource Name (ARN) of a replication instance.
--
-- 'migrationType', 'createReplicationTask_migrationType' - The migration type. Valid values: @full-load@ | @cdc@ |
-- @full-load-and-cdc@
--
-- 'tableMappings', 'createReplicationTask_tableMappings' - The table mappings for the task, in JSON format. For more information,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TableMapping.html Using Table Mapping to Specify Task Settings>
-- in the /AWS Database Migration Service User Guide./
newCreateReplicationTask ::
  -- | 'replicationTaskIdentifier'
  Prelude.Text ->
  -- | 'sourceEndpointArn'
  Prelude.Text ->
  -- | 'targetEndpointArn'
  Prelude.Text ->
  -- | 'replicationInstanceArn'
  Prelude.Text ->
  -- | 'migrationType'
  MigrationTypeValue ->
  -- | 'tableMappings'
  Prelude.Text ->
  CreateReplicationTask
newCreateReplicationTask
  pReplicationTaskIdentifier_
  pSourceEndpointArn_
  pTargetEndpointArn_
  pReplicationInstanceArn_
  pMigrationType_
  pTableMappings_ =
    CreateReplicationTask'
      { taskData = Prelude.Nothing,
        replicationTaskSettings = Prelude.Nothing,
        tags = Prelude.Nothing,
        cdcStartTime = Prelude.Nothing,
        resourceIdentifier = Prelude.Nothing,
        cdcStopPosition = Prelude.Nothing,
        cdcStartPosition = Prelude.Nothing,
        replicationTaskIdentifier =
          pReplicationTaskIdentifier_,
        sourceEndpointArn = pSourceEndpointArn_,
        targetEndpointArn = pTargetEndpointArn_,
        replicationInstanceArn = pReplicationInstanceArn_,
        migrationType = pMigrationType_,
        tableMappings = pTableMappings_
      }

-- | Supplemental information that the task requires to migrate the data for
-- certain source and target endpoints. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings>
-- in the /AWS Database Migration Service User Guide./
createReplicationTask_taskData :: Lens.Lens' CreateReplicationTask (Prelude.Maybe Prelude.Text)
createReplicationTask_taskData = Lens.lens (\CreateReplicationTask' {taskData} -> taskData) (\s@CreateReplicationTask' {} a -> s {taskData = a} :: CreateReplicationTask)

-- | Overall settings for the task, in JSON format. For more information, see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Specifying Task Settings for AWS Database Migration Service Tasks>
-- in the /AWS Database Migration User Guide./
createReplicationTask_replicationTaskSettings :: Lens.Lens' CreateReplicationTask (Prelude.Maybe Prelude.Text)
createReplicationTask_replicationTaskSettings = Lens.lens (\CreateReplicationTask' {replicationTaskSettings} -> replicationTaskSettings) (\s@CreateReplicationTask' {} a -> s {replicationTaskSettings = a} :: CreateReplicationTask)

-- | One or more tags to be assigned to the replication task.
createReplicationTask_tags :: Lens.Lens' CreateReplicationTask (Prelude.Maybe [Tag])
createReplicationTask_tags = Lens.lens (\CreateReplicationTask' {tags} -> tags) (\s@CreateReplicationTask' {} a -> s {tags = a} :: CreateReplicationTask) Prelude.. Lens.mapping Lens._Coerce

-- | Indicates the start time for a change data capture (CDC) operation. Use
-- either CdcStartTime or CdcStartPosition to specify when you want a CDC
-- operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
createReplicationTask_cdcStartTime :: Lens.Lens' CreateReplicationTask (Prelude.Maybe Prelude.UTCTime)
createReplicationTask_cdcStartTime = Lens.lens (\CreateReplicationTask' {cdcStartTime} -> cdcStartTime) (\s@CreateReplicationTask' {} a -> s {cdcStartTime = a} :: CreateReplicationTask) Prelude.. Lens.mapping Core._Time

-- | A friendly name for the resource identifier at the end of the
-- @EndpointArn@ response parameter that is returned in the created
-- @Endpoint@ object. The value for this parameter can have up to 31
-- characters. It can contain only ASCII letters, digits, and hyphen
-- (\'-\'). Also, it can\'t end with a hyphen or contain two consecutive
-- hyphens, and can only begin with a letter, such as @Example-App-ARN1@.
-- For example, this value might result in the @EndpointArn@ value
-- @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@. If you don\'t
-- specify a @ResourceIdentifier@ value, AWS DMS generates a default
-- identifier value for the end of @EndpointArn@.
createReplicationTask_resourceIdentifier :: Lens.Lens' CreateReplicationTask (Prelude.Maybe Prelude.Text)
createReplicationTask_resourceIdentifier = Lens.lens (\CreateReplicationTask' {resourceIdentifier} -> resourceIdentifier) (\s@CreateReplicationTask' {} a -> s {resourceIdentifier = a} :: CreateReplicationTask)

-- | Indicates when you want a change data capture (CDC) operation to stop.
-- The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position
-- “server_time:2018-02-09T12:12:12”
--
-- Commit time example: --cdc-stop-position “commit_time:
-- 2018-02-09T12:12:12 “
createReplicationTask_cdcStopPosition :: Lens.Lens' CreateReplicationTask (Prelude.Maybe Prelude.Text)
createReplicationTask_cdcStopPosition = Lens.lens (\CreateReplicationTask' {cdcStopPosition} -> cdcStopPosition) (\s@CreateReplicationTask' {} a -> s {cdcStopPosition = a} :: CreateReplicationTask)

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
createReplicationTask_cdcStartPosition :: Lens.Lens' CreateReplicationTask (Prelude.Maybe Prelude.Text)
createReplicationTask_cdcStartPosition = Lens.lens (\CreateReplicationTask' {cdcStartPosition} -> cdcStartPosition) (\s@CreateReplicationTask' {} a -> s {cdcStartPosition = a} :: CreateReplicationTask)

-- | An identifier for the replication task.
--
-- Constraints:
--
-- -   Must contain 1-255 alphanumeric characters or hyphens.
--
-- -   First character must be a letter.
--
-- -   Cannot end with a hyphen or contain two consecutive hyphens.
createReplicationTask_replicationTaskIdentifier :: Lens.Lens' CreateReplicationTask Prelude.Text
createReplicationTask_replicationTaskIdentifier = Lens.lens (\CreateReplicationTask' {replicationTaskIdentifier} -> replicationTaskIdentifier) (\s@CreateReplicationTask' {} a -> s {replicationTaskIdentifier = a} :: CreateReplicationTask)

-- | An Amazon Resource Name (ARN) that uniquely identifies the source
-- endpoint.
createReplicationTask_sourceEndpointArn :: Lens.Lens' CreateReplicationTask Prelude.Text
createReplicationTask_sourceEndpointArn = Lens.lens (\CreateReplicationTask' {sourceEndpointArn} -> sourceEndpointArn) (\s@CreateReplicationTask' {} a -> s {sourceEndpointArn = a} :: CreateReplicationTask)

-- | An Amazon Resource Name (ARN) that uniquely identifies the target
-- endpoint.
createReplicationTask_targetEndpointArn :: Lens.Lens' CreateReplicationTask Prelude.Text
createReplicationTask_targetEndpointArn = Lens.lens (\CreateReplicationTask' {targetEndpointArn} -> targetEndpointArn) (\s@CreateReplicationTask' {} a -> s {targetEndpointArn = a} :: CreateReplicationTask)

-- | The Amazon Resource Name (ARN) of a replication instance.
createReplicationTask_replicationInstanceArn :: Lens.Lens' CreateReplicationTask Prelude.Text
createReplicationTask_replicationInstanceArn = Lens.lens (\CreateReplicationTask' {replicationInstanceArn} -> replicationInstanceArn) (\s@CreateReplicationTask' {} a -> s {replicationInstanceArn = a} :: CreateReplicationTask)

-- | The migration type. Valid values: @full-load@ | @cdc@ |
-- @full-load-and-cdc@
createReplicationTask_migrationType :: Lens.Lens' CreateReplicationTask MigrationTypeValue
createReplicationTask_migrationType = Lens.lens (\CreateReplicationTask' {migrationType} -> migrationType) (\s@CreateReplicationTask' {} a -> s {migrationType = a} :: CreateReplicationTask)

-- | The table mappings for the task, in JSON format. For more information,
-- see
-- <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TableMapping.html Using Table Mapping to Specify Task Settings>
-- in the /AWS Database Migration Service User Guide./
createReplicationTask_tableMappings :: Lens.Lens' CreateReplicationTask Prelude.Text
createReplicationTask_tableMappings = Lens.lens (\CreateReplicationTask' {tableMappings} -> tableMappings) (\s@CreateReplicationTask' {} a -> s {tableMappings = a} :: CreateReplicationTask)

instance Core.AWSRequest CreateReplicationTask where
  type
    AWSResponse CreateReplicationTask =
      CreateReplicationTaskResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateReplicationTaskResponse'
            Prelude.<$> (x Core..?> "ReplicationTask")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateReplicationTask

instance Prelude.NFData CreateReplicationTask

instance Core.ToHeaders CreateReplicationTask where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AmazonDMSv20160101.CreateReplicationTask" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateReplicationTask where
  toJSON CreateReplicationTask' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("TaskData" Core..=) Prelude.<$> taskData,
            ("ReplicationTaskSettings" Core..=)
              Prelude.<$> replicationTaskSettings,
            ("Tags" Core..=) Prelude.<$> tags,
            ("CdcStartTime" Core..=) Prelude.<$> cdcStartTime,
            ("ResourceIdentifier" Core..=)
              Prelude.<$> resourceIdentifier,
            ("CdcStopPosition" Core..=)
              Prelude.<$> cdcStopPosition,
            ("CdcStartPosition" Core..=)
              Prelude.<$> cdcStartPosition,
            Prelude.Just
              ( "ReplicationTaskIdentifier"
                  Core..= replicationTaskIdentifier
              ),
            Prelude.Just
              ("SourceEndpointArn" Core..= sourceEndpointArn),
            Prelude.Just
              ("TargetEndpointArn" Core..= targetEndpointArn),
            Prelude.Just
              ( "ReplicationInstanceArn"
                  Core..= replicationInstanceArn
              ),
            Prelude.Just ("MigrationType" Core..= migrationType),
            Prelude.Just
              ("TableMappings" Core..= tableMappings)
          ]
      )

instance Core.ToPath CreateReplicationTask where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateReplicationTask where
  toQuery = Prelude.const Prelude.mempty

-- |
--
-- /See:/ 'newCreateReplicationTaskResponse' smart constructor.
data CreateReplicationTaskResponse = CreateReplicationTaskResponse'
  { -- | The replication task that was created.
    replicationTask :: Prelude.Maybe ReplicationTask,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateReplicationTaskResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'replicationTask', 'createReplicationTaskResponse_replicationTask' - The replication task that was created.
--
-- 'httpStatus', 'createReplicationTaskResponse_httpStatus' - The response's http status code.
newCreateReplicationTaskResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateReplicationTaskResponse
newCreateReplicationTaskResponse pHttpStatus_ =
  CreateReplicationTaskResponse'
    { replicationTask =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The replication task that was created.
createReplicationTaskResponse_replicationTask :: Lens.Lens' CreateReplicationTaskResponse (Prelude.Maybe ReplicationTask)
createReplicationTaskResponse_replicationTask = Lens.lens (\CreateReplicationTaskResponse' {replicationTask} -> replicationTask) (\s@CreateReplicationTaskResponse' {} a -> s {replicationTask = a} :: CreateReplicationTaskResponse)

-- | The response's http status code.
createReplicationTaskResponse_httpStatus :: Lens.Lens' CreateReplicationTaskResponse Prelude.Int
createReplicationTaskResponse_httpStatus = Lens.lens (\CreateReplicationTaskResponse' {httpStatus} -> httpStatus) (\s@CreateReplicationTaskResponse' {} a -> s {httpStatus = a} :: CreateReplicationTaskResponse)

instance Prelude.NFData CreateReplicationTaskResponse
