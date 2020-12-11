{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.ModifyReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Modifies the specified replication task.
--
-- You can't modify the task endpoints. The task must be stopped before you can modify it.
-- For more information about AWS DMS tasks, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks> in the /AWS Database Migration Service User Guide/ .
module Network.AWS.DMS.ModifyReplicationTask
  ( -- * Creating a request
    ModifyReplicationTask (..),
    mkModifyReplicationTask,

    -- ** Request lenses
    mReplicationTaskSettings,
    mReplicationTaskIdentifier,
    mCdcStartPosition,
    mTableMappings,
    mMigrationType,
    mTaskData,
    mCdcStopPosition,
    mCdcStartTime,
    mReplicationTaskARN,

    -- * Destructuring the response
    ModifyReplicationTaskResponse (..),
    mkModifyReplicationTaskResponse,

    -- ** Response lenses
    mrsReplicationTask,
    mrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkModifyReplicationTask' smart constructor.
data ModifyReplicationTask = ModifyReplicationTask'
  { replicationTaskSettings ::
      Lude.Maybe Lude.Text,
    replicationTaskIdentifier ::
      Lude.Maybe Lude.Text,
    cdcStartPosition :: Lude.Maybe Lude.Text,
    tableMappings :: Lude.Maybe Lude.Text,
    migrationType :: Lude.Maybe MigrationTypeValue,
    taskData :: Lude.Maybe Lude.Text,
    cdcStopPosition :: Lude.Maybe Lude.Text,
    cdcStartTime :: Lude.Maybe Lude.Timestamp,
    replicationTaskARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReplicationTask' with the minimum fields required to make a request.
--
-- * 'cdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
-- * 'cdcStartTime' - Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
-- * 'cdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
-- * 'migrationType' - The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task.
-- * 'replicationTaskIdentifier' - The replication task identifier.
--
-- Constraints:
--
--     * Must contain 1-255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
-- * 'replicationTaskSettings' - JSON file that contains settings for the task, such as task metadata settings.
-- * 'tableMappings' - When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with @file://@ . When working with the DMS API, provide the JSON as the parameter value, for example: @--table-mappings file://mappingfile.json@
-- * 'taskData' - Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
mkModifyReplicationTask ::
  -- | 'replicationTaskARN'
  Lude.Text ->
  ModifyReplicationTask
mkModifyReplicationTask pReplicationTaskARN_ =
  ModifyReplicationTask'
    { replicationTaskSettings = Lude.Nothing,
      replicationTaskIdentifier = Lude.Nothing,
      cdcStartPosition = Lude.Nothing,
      tableMappings = Lude.Nothing,
      migrationType = Lude.Nothing,
      taskData = Lude.Nothing,
      cdcStopPosition = Lude.Nothing,
      cdcStartTime = Lude.Nothing,
      replicationTaskARN = pReplicationTaskARN_
    }

-- | JSON file that contains settings for the task, such as task metadata settings.
--
-- /Note:/ Consider using 'replicationTaskSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mReplicationTaskSettings :: Lens.Lens' ModifyReplicationTask (Lude.Maybe Lude.Text)
mReplicationTaskSettings = Lens.lens (replicationTaskSettings :: ModifyReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskSettings = a} :: ModifyReplicationTask)
{-# DEPRECATED mReplicationTaskSettings "Use generic-lens or generic-optics with 'replicationTaskSettings' instead." #-}

-- | The replication task identifier.
--
-- Constraints:
--
--     * Must contain 1-255 alphanumeric characters or hyphens.
--
--
--     * First character must be a letter.
--
--
--     * Cannot end with a hyphen or contain two consecutive hyphens.
--
--
--
-- /Note:/ Consider using 'replicationTaskIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mReplicationTaskIdentifier :: Lens.Lens' ModifyReplicationTask (Lude.Maybe Lude.Text)
mReplicationTaskIdentifier = Lens.lens (replicationTaskIdentifier :: ModifyReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskIdentifier = a} :: ModifyReplicationTask)
{-# DEPRECATED mReplicationTaskIdentifier "Use generic-lens or generic-optics with 'replicationTaskIdentifier' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- /Note:/ Consider using 'cdcStartPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCdcStartPosition :: Lens.Lens' ModifyReplicationTask (Lude.Maybe Lude.Text)
mCdcStartPosition = Lens.lens (cdcStartPosition :: ModifyReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {cdcStartPosition = a} :: ModifyReplicationTask)
{-# DEPRECATED mCdcStartPosition "Use generic-lens or generic-optics with 'cdcStartPosition' instead." #-}

-- | When using the AWS CLI or boto3, provide the path of the JSON file that contains the table mappings. Precede the path with @file://@ . When working with the DMS API, provide the JSON as the parameter value, for example: @--table-mappings file://mappingfile.json@
--
-- /Note:/ Consider using 'tableMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTableMappings :: Lens.Lens' ModifyReplicationTask (Lude.Maybe Lude.Text)
mTableMappings = Lens.lens (tableMappings :: ModifyReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {tableMappings = a} :: ModifyReplicationTask)
{-# DEPRECATED mTableMappings "Use generic-lens or generic-optics with 'tableMappings' instead." #-}

-- | The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@
--
-- /Note:/ Consider using 'migrationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mMigrationType :: Lens.Lens' ModifyReplicationTask (Lude.Maybe MigrationTypeValue)
mMigrationType = Lens.lens (migrationType :: ModifyReplicationTask -> Lude.Maybe MigrationTypeValue) (\s a -> s {migrationType = a} :: ModifyReplicationTask)
{-# DEPRECATED mMigrationType "Use generic-lens or generic-optics with 'migrationType' instead." #-}

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'taskData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mTaskData :: Lens.Lens' ModifyReplicationTask (Lude.Maybe Lude.Text)
mTaskData = Lens.lens (taskData :: ModifyReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {taskData = a} :: ModifyReplicationTask)
{-# DEPRECATED mTaskData "Use generic-lens or generic-optics with 'taskData' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- /Note:/ Consider using 'cdcStopPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCdcStopPosition :: Lens.Lens' ModifyReplicationTask (Lude.Maybe Lude.Text)
mCdcStopPosition = Lens.lens (cdcStopPosition :: ModifyReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {cdcStopPosition = a} :: ModifyReplicationTask)
{-# DEPRECATED mCdcStopPosition "Use generic-lens or generic-optics with 'cdcStopPosition' instead." #-}

-- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- /Note:/ Consider using 'cdcStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mCdcStartTime :: Lens.Lens' ModifyReplicationTask (Lude.Maybe Lude.Timestamp)
mCdcStartTime = Lens.lens (cdcStartTime :: ModifyReplicationTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {cdcStartTime = a} :: ModifyReplicationTask)
{-# DEPRECATED mCdcStartTime "Use generic-lens or generic-optics with 'cdcStartTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication task.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mReplicationTaskARN :: Lens.Lens' ModifyReplicationTask Lude.Text
mReplicationTaskARN = Lens.lens (replicationTaskARN :: ModifyReplicationTask -> Lude.Text) (\s a -> s {replicationTaskARN = a} :: ModifyReplicationTask)
{-# DEPRECATED mReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

instance Lude.AWSRequest ModifyReplicationTask where
  type Rs ModifyReplicationTask = ModifyReplicationTaskResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          ModifyReplicationTaskResponse'
            Lude.<$> (x Lude..?> "ReplicationTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ModifyReplicationTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.ModifyReplicationTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ModifyReplicationTask where
  toJSON ModifyReplicationTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ReplicationTaskSettings" Lude..=)
              Lude.<$> replicationTaskSettings,
            ("ReplicationTaskIdentifier" Lude..=)
              Lude.<$> replicationTaskIdentifier,
            ("CdcStartPosition" Lude..=) Lude.<$> cdcStartPosition,
            ("TableMappings" Lude..=) Lude.<$> tableMappings,
            ("MigrationType" Lude..=) Lude.<$> migrationType,
            ("TaskData" Lude..=) Lude.<$> taskData,
            ("CdcStopPosition" Lude..=) Lude.<$> cdcStopPosition,
            ("CdcStartTime" Lude..=) Lude.<$> cdcStartTime,
            Lude.Just ("ReplicationTaskArn" Lude..= replicationTaskARN)
          ]
      )

instance Lude.ToPath ModifyReplicationTask where
  toPath = Lude.const "/"

instance Lude.ToQuery ModifyReplicationTask where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkModifyReplicationTaskResponse' smart constructor.
data ModifyReplicationTaskResponse = ModifyReplicationTaskResponse'
  { replicationTask ::
      Lude.Maybe ReplicationTask,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ModifyReplicationTaskResponse' with the minimum fields required to make a request.
--
-- * 'replicationTask' - The replication task that was modified.
-- * 'responseStatus' - The response status code.
mkModifyReplicationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ModifyReplicationTaskResponse
mkModifyReplicationTaskResponse pResponseStatus_ =
  ModifyReplicationTaskResponse'
    { replicationTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The replication task that was modified.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsReplicationTask :: Lens.Lens' ModifyReplicationTaskResponse (Lude.Maybe ReplicationTask)
mrsReplicationTask = Lens.lens (replicationTask :: ModifyReplicationTaskResponse -> Lude.Maybe ReplicationTask) (\s a -> s {replicationTask = a} :: ModifyReplicationTaskResponse)
{-# DEPRECATED mrsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
mrsResponseStatus :: Lens.Lens' ModifyReplicationTaskResponse Lude.Int
mrsResponseStatus = Lens.lens (responseStatus :: ModifyReplicationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ModifyReplicationTaskResponse)
{-# DEPRECATED mrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
