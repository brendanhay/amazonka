{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.StartReplicationTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Starts the replication task.
--
-- For more information about AWS DMS tasks, see <https://docs.aws.amazon.com/dms/latest/userguide/CHAP_Tasks.html Working with Migration Tasks > in the /AWS Database Migration Service User Guide./
module Network.AWS.DMS.StartReplicationTask
  ( -- * Creating a request
    StartReplicationTask (..),
    mkStartReplicationTask,

    -- ** Request lenses
    srtCdcStartPosition,
    srtReplicationTaskARN,
    srtCdcStopPosition,
    srtStartReplicationTaskType,
    srtCdcStartTime,

    -- * Destructuring the response
    StartReplicationTaskResponse (..),
    mkStartReplicationTaskResponse,

    -- ** Response lenses
    srtrsReplicationTask,
    srtrsResponseStatus,
  )
where

import Network.AWS.DMS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- |
--
-- /See:/ 'mkStartReplicationTask' smart constructor.
data StartReplicationTask = StartReplicationTask'
  { -- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
    --
    -- The value can be in date, checkpoint, or LSN/SCN format.
    -- Date Example: --cdc-start-position “2018-03-08T12:12:12”
    -- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
    -- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
    cdcStartPosition :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the replication task to be started.
    replicationTaskARN :: Lude.Text,
    -- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
    --
    -- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
    -- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
    cdcStopPosition :: Lude.Maybe Lude.Text,
    -- | A type of replication task.
    startReplicationTaskType :: StartReplicationTaskTypeValue,
    -- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
    --
    -- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
    cdcStartTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartReplicationTask' with the minimum fields required to make a request.
--
-- * 'cdcStartPosition' - Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
-- * 'replicationTaskARN' - The Amazon Resource Name (ARN) of the replication task to be started.
-- * 'cdcStopPosition' - Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
-- * 'startReplicationTaskType' - A type of replication task.
-- * 'cdcStartTime' - Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
mkStartReplicationTask ::
  -- | 'replicationTaskARN'
  Lude.Text ->
  -- | 'startReplicationTaskType'
  StartReplicationTaskTypeValue ->
  StartReplicationTask
mkStartReplicationTask
  pReplicationTaskARN_
  pStartReplicationTaskType_ =
    StartReplicationTask'
      { cdcStartPosition = Lude.Nothing,
        replicationTaskARN = pReplicationTaskARN_,
        cdcStopPosition = Lude.Nothing,
        startReplicationTaskType = pStartReplicationTaskType_,
        cdcStartTime = Lude.Nothing
      }

-- | Indicates when you want a change data capture (CDC) operation to start. Use either CdcStartPosition or CdcStartTime to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- The value can be in date, checkpoint, or LSN/SCN format.
-- Date Example: --cdc-start-position “2018-03-08T12:12:12”
-- Checkpoint Example: --cdc-start-position "checkpoint:V1#27#mysql-bin-changelog.157832:1975:-1:2002:677883278264080:mysql-bin-changelog.157832:1876#0#0#*#0#93"
-- LSN Example: --cdc-start-position “mysql-bin-changelog.000024:373”
--
-- /Note:/ Consider using 'cdcStartPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtCdcStartPosition :: Lens.Lens' StartReplicationTask (Lude.Maybe Lude.Text)
srtCdcStartPosition = Lens.lens (cdcStartPosition :: StartReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {cdcStartPosition = a} :: StartReplicationTask)
{-# DEPRECATED srtCdcStartPosition "Use generic-lens or generic-optics with 'cdcStartPosition' instead." #-}

-- | The Amazon Resource Name (ARN) of the replication task to be started.
--
-- /Note:/ Consider using 'replicationTaskARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtReplicationTaskARN :: Lens.Lens' StartReplicationTask Lude.Text
srtReplicationTaskARN = Lens.lens (replicationTaskARN :: StartReplicationTask -> Lude.Text) (\s a -> s {replicationTaskARN = a} :: StartReplicationTask)
{-# DEPRECATED srtReplicationTaskARN "Use generic-lens or generic-optics with 'replicationTaskARN' instead." #-}

-- | Indicates when you want a change data capture (CDC) operation to stop. The value can be either server time or commit time.
--
-- Server time example: --cdc-stop-position “server_time:2018-02-09T12:12:12”
-- Commit time example: --cdc-stop-position “commit_time: 2018-02-09T12:12:12 “
--
-- /Note:/ Consider using 'cdcStopPosition' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtCdcStopPosition :: Lens.Lens' StartReplicationTask (Lude.Maybe Lude.Text)
srtCdcStopPosition = Lens.lens (cdcStopPosition :: StartReplicationTask -> Lude.Maybe Lude.Text) (\s a -> s {cdcStopPosition = a} :: StartReplicationTask)
{-# DEPRECATED srtCdcStopPosition "Use generic-lens or generic-optics with 'cdcStopPosition' instead." #-}

-- | A type of replication task.
--
-- /Note:/ Consider using 'startReplicationTaskType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtStartReplicationTaskType :: Lens.Lens' StartReplicationTask StartReplicationTaskTypeValue
srtStartReplicationTaskType = Lens.lens (startReplicationTaskType :: StartReplicationTask -> StartReplicationTaskTypeValue) (\s a -> s {startReplicationTaskType = a} :: StartReplicationTask)
{-# DEPRECATED srtStartReplicationTaskType "Use generic-lens or generic-optics with 'startReplicationTaskType' instead." #-}

-- | Indicates the start time for a change data capture (CDC) operation. Use either CdcStartTime or CdcStartPosition to specify when you want a CDC operation to start. Specifying both values results in an error.
--
-- Timestamp Example: --cdc-start-time “2018-03-08T12:12:12”
--
-- /Note:/ Consider using 'cdcStartTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtCdcStartTime :: Lens.Lens' StartReplicationTask (Lude.Maybe Lude.Timestamp)
srtCdcStartTime = Lens.lens (cdcStartTime :: StartReplicationTask -> Lude.Maybe Lude.Timestamp) (\s a -> s {cdcStartTime = a} :: StartReplicationTask)
{-# DEPRECATED srtCdcStartTime "Use generic-lens or generic-optics with 'cdcStartTime' instead." #-}

instance Lude.AWSRequest StartReplicationTask where
  type Rs StartReplicationTask = StartReplicationTaskResponse
  request = Req.postJSON dmsService
  response =
    Res.receiveJSON
      ( \s h x ->
          StartReplicationTaskResponse'
            Lude.<$> (x Lude..?> "ReplicationTask")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders StartReplicationTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AmazonDMSv20160101.StartReplicationTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON StartReplicationTask where
  toJSON StartReplicationTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("CdcStartPosition" Lude..=) Lude.<$> cdcStartPosition,
            Lude.Just ("ReplicationTaskArn" Lude..= replicationTaskARN),
            ("CdcStopPosition" Lude..=) Lude.<$> cdcStopPosition,
            Lude.Just
              ("StartReplicationTaskType" Lude..= startReplicationTaskType),
            ("CdcStartTime" Lude..=) Lude.<$> cdcStartTime
          ]
      )

instance Lude.ToPath StartReplicationTask where
  toPath = Lude.const "/"

instance Lude.ToQuery StartReplicationTask where
  toQuery = Lude.const Lude.mempty

-- |
--
-- /See:/ 'mkStartReplicationTaskResponse' smart constructor.
data StartReplicationTaskResponse = StartReplicationTaskResponse'
  { -- | The replication task started.
    replicationTask :: Lude.Maybe ReplicationTask,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'StartReplicationTaskResponse' with the minimum fields required to make a request.
--
-- * 'replicationTask' - The replication task started.
-- * 'responseStatus' - The response status code.
mkStartReplicationTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  StartReplicationTaskResponse
mkStartReplicationTaskResponse pResponseStatus_ =
  StartReplicationTaskResponse'
    { replicationTask = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The replication task started.
--
-- /Note:/ Consider using 'replicationTask' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtrsReplicationTask :: Lens.Lens' StartReplicationTaskResponse (Lude.Maybe ReplicationTask)
srtrsReplicationTask = Lens.lens (replicationTask :: StartReplicationTaskResponse -> Lude.Maybe ReplicationTask) (\s a -> s {replicationTask = a} :: StartReplicationTaskResponse)
{-# DEPRECATED srtrsReplicationTask "Use generic-lens or generic-optics with 'replicationTask' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
srtrsResponseStatus :: Lens.Lens' StartReplicationTaskResponse Lude.Int
srtrsResponseStatus = Lens.lens (responseStatus :: StartReplicationTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: StartReplicationTaskResponse)
{-# DEPRECATED srtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
