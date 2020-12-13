{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.SetTaskStatus
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @SetTaskStatus@ to notify AWS Data Pipeline that a task is completed and provide information about the final status. A task runner makes this call regardless of whether the task was sucessful. A task runner does not need to call @SetTaskStatus@ for tasks that are canceled by the web service during a call to 'ReportTaskProgress' .
module Network.AWS.DataPipeline.SetTaskStatus
  ( -- * Creating a request
    SetTaskStatus (..),
    mkSetTaskStatus,

    -- ** Request lenses
    stsTaskId,
    stsErrorStackTrace,
    stsErrorId,
    stsErrorMessage,
    stsTaskStatus,

    -- * Destructuring the response
    SetTaskStatusResponse (..),
    mkSetTaskStatusResponse,

    -- ** Response lenses
    stsrsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for SetTaskStatus.
--
-- /See:/ 'mkSetTaskStatus' smart constructor.
data SetTaskStatus = SetTaskStatus'
  { -- | The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
    taskId :: Lude.Text,
    -- | If an error occurred during the task, this value specifies the stack trace associated with the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
    errorStackTrace :: Lude.Maybe Lude.Text,
    -- | If an error occurred during the task, this value specifies the error code. This value is set on the physical attempt object. It is used to display error information to the user. It should not start with string "Service_" which is reserved by the system.
    errorId :: Lude.Maybe Lude.Text,
    -- | If an error occurred during the task, this value specifies a text description of the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
    errorMessage :: Lude.Maybe Lude.Text,
    -- | If @FINISHED@ , the task successfully completed. If @FAILED@ , the task ended unsuccessfully. Preconditions use false.
    taskStatus :: TaskStatus
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetTaskStatus' with the minimum fields required to make a request.
--
-- * 'taskId' - The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
-- * 'errorStackTrace' - If an error occurred during the task, this value specifies the stack trace associated with the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
-- * 'errorId' - If an error occurred during the task, this value specifies the error code. This value is set on the physical attempt object. It is used to display error information to the user. It should not start with string "Service_" which is reserved by the system.
-- * 'errorMessage' - If an error occurred during the task, this value specifies a text description of the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
-- * 'taskStatus' - If @FINISHED@ , the task successfully completed. If @FAILED@ , the task ended unsuccessfully. Preconditions use false.
mkSetTaskStatus ::
  -- | 'taskId'
  Lude.Text ->
  -- | 'taskStatus'
  TaskStatus ->
  SetTaskStatus
mkSetTaskStatus pTaskId_ pTaskStatus_ =
  SetTaskStatus'
    { taskId = pTaskId_,
      errorStackTrace = Lude.Nothing,
      errorId = Lude.Nothing,
      errorMessage = Lude.Nothing,
      taskStatus = pTaskStatus_
    }

-- | The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsTaskId :: Lens.Lens' SetTaskStatus Lude.Text
stsTaskId = Lens.lens (taskId :: SetTaskStatus -> Lude.Text) (\s a -> s {taskId = a} :: SetTaskStatus)
{-# DEPRECATED stsTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | If an error occurred during the task, this value specifies the stack trace associated with the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
--
-- /Note:/ Consider using 'errorStackTrace' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsErrorStackTrace :: Lens.Lens' SetTaskStatus (Lude.Maybe Lude.Text)
stsErrorStackTrace = Lens.lens (errorStackTrace :: SetTaskStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorStackTrace = a} :: SetTaskStatus)
{-# DEPRECATED stsErrorStackTrace "Use generic-lens or generic-optics with 'errorStackTrace' instead." #-}

-- | If an error occurred during the task, this value specifies the error code. This value is set on the physical attempt object. It is used to display error information to the user. It should not start with string "Service_" which is reserved by the system.
--
-- /Note:/ Consider using 'errorId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsErrorId :: Lens.Lens' SetTaskStatus (Lude.Maybe Lude.Text)
stsErrorId = Lens.lens (errorId :: SetTaskStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorId = a} :: SetTaskStatus)
{-# DEPRECATED stsErrorId "Use generic-lens or generic-optics with 'errorId' instead." #-}

-- | If an error occurred during the task, this value specifies a text description of the error. This value is set on the physical attempt object. It is used to display error information to the user. The web service does not parse this value.
--
-- /Note:/ Consider using 'errorMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsErrorMessage :: Lens.Lens' SetTaskStatus (Lude.Maybe Lude.Text)
stsErrorMessage = Lens.lens (errorMessage :: SetTaskStatus -> Lude.Maybe Lude.Text) (\s a -> s {errorMessage = a} :: SetTaskStatus)
{-# DEPRECATED stsErrorMessage "Use generic-lens or generic-optics with 'errorMessage' instead." #-}

-- | If @FINISHED@ , the task successfully completed. If @FAILED@ , the task ended unsuccessfully. Preconditions use false.
--
-- /Note:/ Consider using 'taskStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsTaskStatus :: Lens.Lens' SetTaskStatus TaskStatus
stsTaskStatus = Lens.lens (taskStatus :: SetTaskStatus -> TaskStatus) (\s a -> s {taskStatus = a} :: SetTaskStatus)
{-# DEPRECATED stsTaskStatus "Use generic-lens or generic-optics with 'taskStatus' instead." #-}

instance Lude.AWSRequest SetTaskStatus where
  type Rs SetTaskStatus = SetTaskStatusResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveEmpty
      ( \s h x ->
          SetTaskStatusResponse' Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders SetTaskStatus where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.SetTaskStatus" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON SetTaskStatus where
  toJSON SetTaskStatus' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("taskId" Lude..= taskId),
            ("errorStackTrace" Lude..=) Lude.<$> errorStackTrace,
            ("errorId" Lude..=) Lude.<$> errorId,
            ("errorMessage" Lude..=) Lude.<$> errorMessage,
            Lude.Just ("taskStatus" Lude..= taskStatus)
          ]
      )

instance Lude.ToPath SetTaskStatus where
  toPath = Lude.const "/"

instance Lude.ToQuery SetTaskStatus where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of SetTaskStatus.
--
-- /See:/ 'mkSetTaskStatusResponse' smart constructor.
newtype SetTaskStatusResponse = SetTaskStatusResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SetTaskStatusResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkSetTaskStatusResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  SetTaskStatusResponse
mkSetTaskStatusResponse pResponseStatus_ =
  SetTaskStatusResponse' {responseStatus = pResponseStatus_}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
stsrsResponseStatus :: Lens.Lens' SetTaskStatusResponse Lude.Int
stsrsResponseStatus = Lens.lens (responseStatus :: SetTaskStatusResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: SetTaskStatusResponse)
{-# DEPRECATED stsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
