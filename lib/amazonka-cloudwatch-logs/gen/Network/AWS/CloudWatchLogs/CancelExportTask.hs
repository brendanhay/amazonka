{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.CancelExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Cancels the specified export task.
--
-- The task must be in the @PENDING@ or @RUNNING@ state.
module Network.AWS.CloudWatchLogs.CancelExportTask
  ( -- * Creating a request
    CancelExportTask (..),
    mkCancelExportTask,

    -- ** Request lenses
    cetTaskId,

    -- * Destructuring the response
    CancelExportTaskResponse (..),
    mkCancelExportTaskResponse,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCancelExportTask' smart constructor.
newtype CancelExportTask = CancelExportTask' {taskId :: Lude.Text}
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelExportTask' with the minimum fields required to make a request.
--
-- * 'taskId' - The ID of the export task.
mkCancelExportTask ::
  -- | 'taskId'
  Lude.Text ->
  CancelExportTask
mkCancelExportTask pTaskId_ = CancelExportTask' {taskId = pTaskId_}

-- | The ID of the export task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetTaskId :: Lens.Lens' CancelExportTask Lude.Text
cetTaskId = Lens.lens (taskId :: CancelExportTask -> Lude.Text) (\s a -> s {taskId = a} :: CancelExportTask)
{-# DEPRECATED cetTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Lude.AWSRequest CancelExportTask where
  type Rs CancelExportTask = CancelExportTaskResponse
  request = Req.postJSON cloudWatchLogsService
  response = Res.receiveNull CancelExportTaskResponse'

instance Lude.ToHeaders CancelExportTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.CancelExportTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CancelExportTask where
  toJSON CancelExportTask' {..} =
    Lude.object
      (Lude.catMaybes [Lude.Just ("taskId" Lude..= taskId)])

instance Lude.ToPath CancelExportTask where
  toPath = Lude.const "/"

instance Lude.ToQuery CancelExportTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCancelExportTaskResponse' smart constructor.
data CancelExportTaskResponse = CancelExportTaskResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CancelExportTaskResponse' with the minimum fields required to make a request.
mkCancelExportTaskResponse ::
  CancelExportTaskResponse
mkCancelExportTaskResponse = CancelExportTaskResponse'
