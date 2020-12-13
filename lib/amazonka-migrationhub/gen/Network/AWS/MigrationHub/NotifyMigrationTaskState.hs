{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.MigrationHub.NotifyMigrationTaskState
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Notifies Migration Hub of the current status, progress, or other detail regarding a migration task. This API has the following traits:
--
--
--     * Migration tools will call the @NotifyMigrationTaskState@ API to share the latest progress and status.
--
--
--     * @MigrationTaskName@ is used for addressing updates to the correct target.
--
--
--     * @ProgressUpdateStream@ is used for access control and to provide a namespace for each migration tool.
module Network.AWS.MigrationHub.NotifyMigrationTaskState
  ( -- * Creating a request
    NotifyMigrationTaskState (..),
    mkNotifyMigrationTaskState,

    -- ** Request lenses
    nmtsNextUpdateSeconds,
    nmtsUpdateDateTime,
    nmtsTask,
    nmtsProgressUpdateStream,
    nmtsMigrationTaskName,
    nmtsDryRun,

    -- * Destructuring the response
    NotifyMigrationTaskStateResponse (..),
    mkNotifyMigrationTaskStateResponse,

    -- ** Response lenses
    nmtsrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.MigrationHub.Types
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkNotifyMigrationTaskState' smart constructor.
data NotifyMigrationTaskState = NotifyMigrationTaskState'
  { -- | Number of seconds after the UpdateDateTime within which the Migration Hub can expect an update. If Migration Hub does not receive an update within the specified interval, then the migration task will be considered stale.
    nextUpdateSeconds :: Lude.Natural,
    -- | The timestamp when the task was gathered.
    updateDateTime :: Lude.Timestamp,
    -- | Information about the task's progress and status.
    task :: Task,
    -- | The name of the ProgressUpdateStream.
    progressUpdateStream :: Lude.Text,
    -- | Unique identifier that references the migration task. /Do not store personal data in this field./
    migrationTaskName :: Lude.Text,
    -- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyMigrationTaskState' with the minimum fields required to make a request.
--
-- * 'nextUpdateSeconds' - Number of seconds after the UpdateDateTime within which the Migration Hub can expect an update. If Migration Hub does not receive an update within the specified interval, then the migration task will be considered stale.
-- * 'updateDateTime' - The timestamp when the task was gathered.
-- * 'task' - Information about the task's progress and status.
-- * 'progressUpdateStream' - The name of the ProgressUpdateStream.
-- * 'migrationTaskName' - Unique identifier that references the migration task. /Do not store personal data in this field./
-- * 'dryRun' - Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
mkNotifyMigrationTaskState ::
  -- | 'nextUpdateSeconds'
  Lude.Natural ->
  -- | 'updateDateTime'
  Lude.Timestamp ->
  -- | 'task'
  Task ->
  -- | 'progressUpdateStream'
  Lude.Text ->
  -- | 'migrationTaskName'
  Lude.Text ->
  NotifyMigrationTaskState
mkNotifyMigrationTaskState
  pNextUpdateSeconds_
  pUpdateDateTime_
  pTask_
  pProgressUpdateStream_
  pMigrationTaskName_ =
    NotifyMigrationTaskState'
      { nextUpdateSeconds =
          pNextUpdateSeconds_,
        updateDateTime = pUpdateDateTime_,
        task = pTask_,
        progressUpdateStream = pProgressUpdateStream_,
        migrationTaskName = pMigrationTaskName_,
        dryRun = Lude.Nothing
      }

-- | Number of seconds after the UpdateDateTime within which the Migration Hub can expect an update. If Migration Hub does not receive an update within the specified interval, then the migration task will be considered stale.
--
-- /Note:/ Consider using 'nextUpdateSeconds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsNextUpdateSeconds :: Lens.Lens' NotifyMigrationTaskState Lude.Natural
nmtsNextUpdateSeconds = Lens.lens (nextUpdateSeconds :: NotifyMigrationTaskState -> Lude.Natural) (\s a -> s {nextUpdateSeconds = a} :: NotifyMigrationTaskState)
{-# DEPRECATED nmtsNextUpdateSeconds "Use generic-lens or generic-optics with 'nextUpdateSeconds' instead." #-}

-- | The timestamp when the task was gathered.
--
-- /Note:/ Consider using 'updateDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsUpdateDateTime :: Lens.Lens' NotifyMigrationTaskState Lude.Timestamp
nmtsUpdateDateTime = Lens.lens (updateDateTime :: NotifyMigrationTaskState -> Lude.Timestamp) (\s a -> s {updateDateTime = a} :: NotifyMigrationTaskState)
{-# DEPRECATED nmtsUpdateDateTime "Use generic-lens or generic-optics with 'updateDateTime' instead." #-}

-- | Information about the task's progress and status.
--
-- /Note:/ Consider using 'task' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsTask :: Lens.Lens' NotifyMigrationTaskState Task
nmtsTask = Lens.lens (task :: NotifyMigrationTaskState -> Task) (\s a -> s {task = a} :: NotifyMigrationTaskState)
{-# DEPRECATED nmtsTask "Use generic-lens or generic-optics with 'task' instead." #-}

-- | The name of the ProgressUpdateStream.
--
-- /Note:/ Consider using 'progressUpdateStream' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsProgressUpdateStream :: Lens.Lens' NotifyMigrationTaskState Lude.Text
nmtsProgressUpdateStream = Lens.lens (progressUpdateStream :: NotifyMigrationTaskState -> Lude.Text) (\s a -> s {progressUpdateStream = a} :: NotifyMigrationTaskState)
{-# DEPRECATED nmtsProgressUpdateStream "Use generic-lens or generic-optics with 'progressUpdateStream' instead." #-}

-- | Unique identifier that references the migration task. /Do not store personal data in this field./
--
-- /Note:/ Consider using 'migrationTaskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsMigrationTaskName :: Lens.Lens' NotifyMigrationTaskState Lude.Text
nmtsMigrationTaskName = Lens.lens (migrationTaskName :: NotifyMigrationTaskState -> Lude.Text) (\s a -> s {migrationTaskName = a} :: NotifyMigrationTaskState)
{-# DEPRECATED nmtsMigrationTaskName "Use generic-lens or generic-optics with 'migrationTaskName' instead." #-}

-- | Optional boolean flag to indicate whether any effect should take place. Used to test if the caller has permission to make the call.
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsDryRun :: Lens.Lens' NotifyMigrationTaskState (Lude.Maybe Lude.Bool)
nmtsDryRun = Lens.lens (dryRun :: NotifyMigrationTaskState -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: NotifyMigrationTaskState)
{-# DEPRECATED nmtsDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest NotifyMigrationTaskState where
  type Rs NotifyMigrationTaskState = NotifyMigrationTaskStateResponse
  request = Req.postJSON migrationHubService
  response =
    Res.receiveEmpty
      ( \s h x ->
          NotifyMigrationTaskStateResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders NotifyMigrationTaskState where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSMigrationHub.NotifyMigrationTaskState" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON NotifyMigrationTaskState where
  toJSON NotifyMigrationTaskState' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("NextUpdateSeconds" Lude..= nextUpdateSeconds),
            Lude.Just ("UpdateDateTime" Lude..= updateDateTime),
            Lude.Just ("Task" Lude..= task),
            Lude.Just ("ProgressUpdateStream" Lude..= progressUpdateStream),
            Lude.Just ("MigrationTaskName" Lude..= migrationTaskName),
            ("DryRun" Lude..=) Lude.<$> dryRun
          ]
      )

instance Lude.ToPath NotifyMigrationTaskState where
  toPath = Lude.const "/"

instance Lude.ToQuery NotifyMigrationTaskState where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkNotifyMigrationTaskStateResponse' smart constructor.
newtype NotifyMigrationTaskStateResponse = NotifyMigrationTaskStateResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'NotifyMigrationTaskStateResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkNotifyMigrationTaskStateResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  NotifyMigrationTaskStateResponse
mkNotifyMigrationTaskStateResponse pResponseStatus_ =
  NotifyMigrationTaskStateResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
nmtsrsResponseStatus :: Lens.Lens' NotifyMigrationTaskStateResponse Lude.Int
nmtsrsResponseStatus = Lens.lens (responseStatus :: NotifyMigrationTaskStateResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: NotifyMigrationTaskStateResponse)
{-# DEPRECATED nmtsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
