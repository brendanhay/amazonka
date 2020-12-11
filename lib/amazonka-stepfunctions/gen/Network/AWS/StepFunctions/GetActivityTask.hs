{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StepFunctions.GetActivityTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Used by workers to retrieve a task (with the specified activity ARN) which has been scheduled for execution by a running state machine. This initiates a long poll, where the service holds the HTTP connection open and responds as soon as a task becomes available (i.e. an execution of a task of this type is needed.) The maximum time the service holds on to the request before responding is 60 seconds. If no task is available within 60 seconds, the poll returns a @taskToken@ with a null string.
--
-- /Important:/ Workers should set their client side socket timeout to at least 65 seconds (5 seconds higher than the maximum time the service may hold the poll request).
-- Polling with @GetActivityTask@ can cause latency in some implementations. See <https://docs.aws.amazon.com/step-functions/latest/dg/bp-activity-pollers.html Avoid Latency When Polling for Activity Tasks> in the Step Functions Developer Guide.
module Network.AWS.StepFunctions.GetActivityTask
  ( -- * Creating a request
    GetActivityTask (..),
    mkGetActivityTask,

    -- ** Request lenses
    gatWorkerName,
    gatActivityARN,

    -- * Destructuring the response
    GetActivityTaskResponse (..),
    mkGetActivityTaskResponse,

    -- ** Response lenses
    gatrsInput,
    gatrsTaskToken,
    gatrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StepFunctions.Types

-- | /See:/ 'mkGetActivityTask' smart constructor.
data GetActivityTask = GetActivityTask'
  { workerName ::
      Lude.Maybe Lude.Text,
    activityARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetActivityTask' with the minimum fields required to make a request.
--
-- * 'activityARN' - The Amazon Resource Name (ARN) of the activity to retrieve tasks from (assigned when you create the task using 'CreateActivity' .)
-- * 'workerName' - You can provide an arbitrary name in order to identify the worker that the task is assigned to. This name is used when it is logged in the execution history.
mkGetActivityTask ::
  -- | 'activityARN'
  Lude.Text ->
  GetActivityTask
mkGetActivityTask pActivityARN_ =
  GetActivityTask'
    { workerName = Lude.Nothing,
      activityARN = pActivityARN_
    }

-- | You can provide an arbitrary name in order to identify the worker that the task is assigned to. This name is used when it is logged in the execution history.
--
-- /Note:/ Consider using 'workerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatWorkerName :: Lens.Lens' GetActivityTask (Lude.Maybe Lude.Text)
gatWorkerName = Lens.lens (workerName :: GetActivityTask -> Lude.Maybe Lude.Text) (\s a -> s {workerName = a} :: GetActivityTask)
{-# DEPRECATED gatWorkerName "Use generic-lens or generic-optics with 'workerName' instead." #-}

-- | The Amazon Resource Name (ARN) of the activity to retrieve tasks from (assigned when you create the task using 'CreateActivity' .)
--
-- /Note:/ Consider using 'activityARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatActivityARN :: Lens.Lens' GetActivityTask Lude.Text
gatActivityARN = Lens.lens (activityARN :: GetActivityTask -> Lude.Text) (\s a -> s {activityARN = a} :: GetActivityTask)
{-# DEPRECATED gatActivityARN "Use generic-lens or generic-optics with 'activityARN' instead." #-}

instance Lude.AWSRequest GetActivityTask where
  type Rs GetActivityTask = GetActivityTaskResponse
  request = Req.postJSON stepFunctionsService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetActivityTaskResponse'
            Lude.<$> (x Lude..?> "input")
            Lude.<*> (x Lude..?> "taskToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetActivityTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSStepFunctions.GetActivityTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.0" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetActivityTask where
  toJSON GetActivityTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("workerName" Lude..=) Lude.<$> workerName,
            Lude.Just ("activityArn" Lude..= activityARN)
          ]
      )

instance Lude.ToPath GetActivityTask where
  toPath = Lude.const "/"

instance Lude.ToQuery GetActivityTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetActivityTaskResponse' smart constructor.
data GetActivityTaskResponse = GetActivityTaskResponse'
  { input ::
      Lude.Maybe (Lude.Sensitive Lude.Text),
    taskToken :: Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetActivityTaskResponse' with the minimum fields required to make a request.
--
-- * 'input' - The string that contains the JSON input data for the task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
-- * 'responseStatus' - The response status code.
-- * 'taskToken' - A token that identifies the scheduled task. This token must be copied and included in subsequent calls to 'SendTaskHeartbeat' , 'SendTaskSuccess' or 'SendTaskFailure' in order to report the progress or completion of the task.
mkGetActivityTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetActivityTaskResponse
mkGetActivityTaskResponse pResponseStatus_ =
  GetActivityTaskResponse'
    { input = Lude.Nothing,
      taskToken = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The string that contains the JSON input data for the task. Length constraints apply to the payload size, and are expressed as bytes in UTF-8 encoding.
--
-- /Note:/ Consider using 'input' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatrsInput :: Lens.Lens' GetActivityTaskResponse (Lude.Maybe (Lude.Sensitive Lude.Text))
gatrsInput = Lens.lens (input :: GetActivityTaskResponse -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {input = a} :: GetActivityTaskResponse)
{-# DEPRECATED gatrsInput "Use generic-lens or generic-optics with 'input' instead." #-}

-- | A token that identifies the scheduled task. This token must be copied and included in subsequent calls to 'SendTaskHeartbeat' , 'SendTaskSuccess' or 'SendTaskFailure' in order to report the progress or completion of the task.
--
-- /Note:/ Consider using 'taskToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatrsTaskToken :: Lens.Lens' GetActivityTaskResponse (Lude.Maybe Lude.Text)
gatrsTaskToken = Lens.lens (taskToken :: GetActivityTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskToken = a} :: GetActivityTaskResponse)
{-# DEPRECATED gatrsTaskToken "Use generic-lens or generic-optics with 'taskToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gatrsResponseStatus :: Lens.Lens' GetActivityTaskResponse Lude.Int
gatrsResponseStatus = Lens.lens (responseStatus :: GetActivityTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetActivityTaskResponse)
{-# DEPRECATED gatrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
