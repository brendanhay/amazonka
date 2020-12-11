{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.CreateReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication task using the specified parameters.
module Network.AWS.DMS.CreateReplicationTask
  ( -- * Creating a request
    CreateReplicationTask (..),
    mkCreateReplicationTask,

    -- ** Request lenses
    crtReplicationTaskSettings,
    crtCdcStartPosition,
    crtTaskData,
    crtCdcStopPosition,
    crtResourceIdentifier,
    crtTags,
    crtCdcStartTime,
    crtReplicationTaskIdentifier,
    crtSourceEndpointARN,
    crtTargetEndpointARN,
    crtReplicationInstanceARN,
    crtMigrationType,
    crtTableMappings,

    -- * Destructuring the response
    CreateReplicationTaskResponse (..),
    mkCreateReplicationTaskResponse,

    -- ** Response lenses
    crtrsReplicationTask,
    crtrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkCreateReplicationTask' smart constructor.
data CreateReplicationTask = CreateReplicationTask'
  { replicationTaskSettings ::
      Lude.Maybe Lude.Text,
    cdcStartPosition :: Lude.Maybe Lude.Text,
    taskData :: Lude.Maybe Lude.Text,
    cdcStopPosition :: Lude.Maybe Lude.Text,
    resourceIdentifier :: Lude.Maybe Lude.Text,
    tags :: Lude.Maybe [Tag],
    cdcStartTime :: Lude.Maybe Lude.Timestamp,
    replicationTaskIdentifier :: Lude.Text,
    sourceEndpointARN :: Lude.Text,
    targetEndpointARN :: Lude.Text,
    replicationInstanceARN :: Lude.Text,
    migrationType :: MigrationTypeValue,
    tableMappings :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateReplicationTask' with the minimum fields required to make a request.
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
-- * 'replicationInstanceARN' - The Amazon Resource Name (ARN) of a replication instance.
-- * 'replicationTaskIdentifier' - An identifier for the replication task.
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
-- * 'replicationTaskSettings' - Overall settings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Specifying Task Settings for AWS Database Migration Service Tasks> in the /AWS Database Migration User Guide./
-- * 'resourceIdentifier' - A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
-- * 'sourceEndpointARN' - An Amazon Resource Name (ARN) that uniquely identifies the source endpoint.
-- * 'tableMappings' - The table mappings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TableMapping.html Using Table Mapping to Specify Task Settings> in the /AWS Database Migration Service User Guide./
-- * 'tags' - One or more tags to be assigned to the replication task.
-- * 'targetEndpointARN' - An Amazon Resource Name (ARN) that uniquely identifies the target endpoint.
-- * 'taskData' - Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
mkCreateReplicationTask ::
  -- | 'replicationTaskIdentifier'
  Lude.Text ->
  -- | 'sourceEndpointARN'
  Lude.Text ->
  -- | 'targetEndpointARN'
  Lude.Text ->
  -- | 'replicationInstanceARN'
  Lude.Text ->
  -- | 'migrationType'
  MigrationTypeValue ->
  -- | 'tableMappings'
  Lude.Text ->
  CreateReplicationTask
mkCreateReplicationTask
  pReplicationTaskIdentifier_
  pSourceEndpointARN_
  pTargetEndpointARN_
  pReplicationInstanceARN_
  pMigrationType_
  pTableMappings_ =
    CreateReplicationTask'
      { replicationTaskSettings = Lude.Nothing,
        cdcStartPosition = Lude.Nothing,
        taskData = Lude.Nothing,
        cdcStopPosition = Lude.Nothing,
        resourceIdentifier = Lude.Nothing,
        tags = Lude.Nothing,
        cdcStartTime = Lude.Nothing,
        replicationTaskIdentifier = pReplicationTaskIdentifier_,
        sourceEndpointARN = pSourceEndpointARN_,
        targetEndpointARN = pTargetEndpointARN_,
        replicationInstanceARN = pReplicationInstanceARN_,
        migrationType = pMigrationType_,
        tableMappings = pTableMappings_
      }

-- | Overall settings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TaskSettings.html Specifying Task Settings for AWS Database Migration Service Tasks> in the /AWS Database Migration User Guide./
--
-- /Note:/ Consider using 'replicationTaskSettings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtReplicationTaskSettings :: Lens.Lens' CreateReplicationTask (Lude.Maybe Lude.Text)
crtReplicationTaskSettings = Lens.lens (replicationTaskSettings :: CreateReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {replicationTaskSettings = a} :: CreateReplicationTask)
{-# DEPRECATED crtReplicationTaskSettings "Use generic-lens or generic-optics with 'replicationTaskSettings' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- /Note:/ Consider using 'cdcStartPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCdcStartPosition :: Lens.Lens' CreateReplicationTask (Lude.Maybe Lude.Text)
crtCdcStartPosition = Lens.lens (cdcStartPosition :: CreateReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {cdcStartPosition = a} :: CreateReplicationTask)
{-# DEPRECATED crtCdcStartPosition "Use generic-lens or generic-optics with 'cdcStartPosition' instead." #-}

-- | Supplemental information that the task requires to migrate the data for certain source and target endpoints. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.TaskData.html Specifying Supplemental Data for Task Settings> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'taskData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTaskData :: Lens.Lens' CreateReplicationTask (Lude.Maybe Lude.Text)
crtTaskData = Lens.lens (taskData :: CreateReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {taskData = a} :: CreateReplicationTask)
{-# DEPRECATED crtTaskData "Use generic-lens or generic-optics with 'taskData' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- /Note:/ Consider using 'cdcStopPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCdcStopPosition :: Lens.Lens' CreateReplicationTask (Lude.Maybe Lude.Text)
crtCdcStopPosition = Lens.lens (cdcStopPosition :: CreateReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {cdcStopPosition = a} :: CreateReplicationTask)
{-# DEPRECATED crtCdcStopPosition "Use generic-lens or generic-optics with 'cdcStopPosition' instead." #-}

-- | A friendly name for the resource identifier at the end of the @EndpointArn@ response parameter that is returned in the created @Endpoint@ object. The value for this parameter can have up to 31 characters. It can contain only ASCII letters, digits, and hyphen ('-'). Also, it can't end with a hyphen or contain two consecutive hyphens, and can only begin with a letter, such as @Example-App-ARN1@ . For example, this value might result in the @EndpointArn@ value @arn:aws:dms:eu-west-1:012345678901:rep:Example-App-ARN1@ . If you don't specify a @ResourceIdentifier@ value, AWS DMS generates a default identifier value for the end of @EndpointArn@ .
--
-- /Note:/ Consider using 'resourceIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtResourceIdentifier :: Lens.Lens' CreateReplicationTask (Lude.Maybe Lude.Text)
crtResourceIdentifier = Lens.lens (resourceIdentifier :: CreateReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {resourceIdentifier = a} :: CreateReplicationTask)
{-# DEPRECATED crtResourceIdentifier "Use generic-lens or generic-optics with 'resourceIdentifier' instead." #-}

-- | One or more tags to be assigned to the replication task.
--
-- /Note:/ Consider using 'tags' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTags :: Lens.Lens' CreateReplicationTask (Lude.Maybe [Tag])
crtTags = Lens.lens (tags :: CreateReplicationTask -> Lude.Maybe [Tag]) (\s a -> s {tags = a} :: CreateReplicationTask)
{-# DEPRECATED crtTags "Use generic-lens or generic-optics with 'tags' instead." #-}

-- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- /Note:/ Consider using 'cdcStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtCdcStartTime :: Lens.Lens' CreateReplicationTask (Lude.Maybe Lude.Timestamp)
crtCdcStartTime = Lens.lens (cdcStartTime :: CreateReplicationTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {cdcStartTime = a} :: CreateReplicationTask)
{-# DEPRECATED crtCdcStartTime "Use generic-lens or generic-optics with 'cdcStartTime' instead." #-}

-- | An identifier for the replication task.
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
crtReplicationTaskIdentifier :: Lens.Lens' CreateReplicationTask Lude.Text
crtReplicationTaskIdentifier = Lens.lens (replicationTaskIdentifier :: CreateReplicationTask -> Lude.Text) (\s a -> s {replicationTaskIdentifier = a} :: CreateReplicationTask)
{-# DEPRECATED crtReplicationTaskIdentifier "Use generic-lens or generic-optics with 'replicationTaskIdentifier' instead." #-}

-- | An Amazon Resource Name (ARN) that uniquely identifies the source endpoint.
--
-- /Note:/ Consider using 'sourceEndpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtSourceEndpointARN :: Lens.Lens' CreateReplicationTask Lude.Text
crtSourceEndpointARN = Lens.lens (sourceEndpointARN :: CreateReplicationTask -> Lude.Text) (\s a -> s {sourceEndpointARN = a} :: CreateReplicationTask)
{-# DEPRECATED crtSourceEndpointARN "Use generic-lens or generic-optics with 'sourceEndpointARN' instead." #-}

-- | An Amazon Resource Name (ARN) that uniquely identifies the target endpoint.
--
-- /Note:/ Consider using 'targetEndpointARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTargetEndpointARN :: Lens.Lens' CreateReplicationTask Lude.Text
crtTargetEndpointARN = Lens.lens (targetEndpointARN :: CreateReplicationTask -> Lude.Text) (\s a -> s {targetEndpointARN = a} :: CreateReplicationTask)
{-# DEPRECATED crtTargetEndpointARN "Use generic-lens or generic-optics with 'targetEndpointARN' instead." #-}

-- | The Amazon Resource Name (ARN) of a replication instance.
--
-- /Note:/ Consider using 'replicationInstanceARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtReplicationInstanceARN :: Lens.Lens' CreateReplicationTask Lude.Text
crtReplicationInstanceARN = Lens.lens (replicationInstanceARN :: CreateReplicationTask -> Lude.Text) (\s a -> s {replicationInstanceARN = a} :: CreateReplicationTask)
{-# DEPRECATED crtReplicationInstanceARN "Use generic-lens or generic-optics with 'replicationInstanceARN' instead." #-}

-- | The migration type. Valid values: @full-load@ | @cdc@ | @full-load-and-cdc@
--
-- /Note:/ Consider using 'migrationType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtMigrationType :: Lens.Lens' CreateReplicationTask MigrationTypeValue
crtMigrationType = Lens.lens (migrationType :: CreateReplicationTask -> MigrationTypeValue) (\s a -> s {migrationType = a} :: CreateReplicationTask)
{-# DEPRECATED crtMigrationType "Use generic-lens or generic-optics with 'migrationType' instead." #-}

-- | The table mappings for the task, in JSON format. For more information, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.CustomizingTasks.TableMapping.html Using Table Mapping to Specify Task Settings> in the /AWS Database Migration Service User Guide./
--
-- /Note:/ Consider using 'tableMappings' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtTableMappings :: Lens.Lens' CreateReplicationTask Lude.Text
crtTableMappings = Lens.lens (tableMappings :: CreateReplicationTask -> Lude.Text) (\s a -> s {tableMappings = a} :: CreateReplicationTask)
{-# DEPRECATED crtTableMappings "Use generic-lens or generic-optics with 'tableMappings' instead." #-}

instance Lude.AWSRequest CreateReplicationTask where
  type Rs CreateReplicationTask = CreateReplicationTaskResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateReplicationTaskResponse'
            Lude.<$> (x Lude..?> "ReplicationTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateReplicationTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.CreateReplicationTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateReplicationTask where
  toJSON CreateReplicationTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("ReplicationTaskSettings" Lude..=)
              Lude.<$> replicationTaskSettings,
            ("CdcStartPosition" Lude..=) Lude.<$> cdcStartPosition,
            ("TaskData" Lude..=) Lude.<$> taskData,
            ("CdcStopPosition" Lude..=) Lude.<$> cdcStopPosition,
            ("ResourceIdentifier" Lude..=) Lude.<$> resourceIdentifier,
            ("Tags" Lude..=) Lude.<$> tags,
            ("CdcStartTime" Lude..=) Lude.<$> cdcStartTime,
            Lude.Just
              ("ReplicationTaskIdentifier" Lude..= replicationTaskIdentifier),
            Lude.Just ("SourceEndpointArn" Lude..= sourceEndpointARN),
            Lude.Just ("TargetEndpointArn" Lude..= targetEndpointARN),
            Lude.Just
              ("ReplicationInstanceArn" Lude..= replicationInstanceARN),
            Lude.Just ("MigrationType" Lude..= migrationType),
            Lude.Just ("TableMappings" Lude..= tableMappings)
          ]
      )

instance Lude.ToPath CreateReplicationTask where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateReplicationTask where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkCreateReplicationTaskResponse' smart constructor.
data CreateReplicationTaskResponse = CreateReplicationTaskResponse'
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

-- | Creates a value of 'CreateReplicationTaskResponse' with the minimum fields required to make a request.
--
-- * 'replicationTask' - The replication task that was created.
-- * 'responseStatus' - The response status code.
mkCreateReplicationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateReplicationTaskResponse
mkCreateReplicationTaskResponse pResponseStatus_ =
  CreateReplicationTaskResponse'
    { replicationTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The replication task that was created.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrsReplicationTask :: Lens.Lens' CreateReplicationTaskResponse (Lude.Maybe ReplicationTask)
crtrsReplicationTask = Lens.lens (replicationTask :: CreateReplicationTaskResponse -> Lude.Maybe ReplicationTask) (\s a -> s {replicationTask = a} :: CreateReplicationTaskResponse)
{-# DEPRECATED crtrsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
crtrsResponseStatus :: Lens.Lens' CreateReplicationTaskResponse Lude.Int
crtrsResponseStatus = Lens.lens (responseStatus :: CreateReplicationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateReplicationTaskResponse)
{-# DEPRECATED crtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
