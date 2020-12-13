{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.PollForTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @PollForTask@ to receive a task to perform from AWS Data Pipeline. The task runner specifies which tasks it can perform by setting a value for the @workerGroup@ parameter. The task returned can come from any of the pipelines that match the @workerGroup@ value passed in by the task runner and that was launched using the IAM user credentials specified by the task runner.
--
-- If tasks are ready in the work queue, @PollForTask@ returns a response immediately. If no tasks are available in the queue, @PollForTask@ uses long-polling and holds on to a poll connection for up to a 90 seconds, during which time the first newly scheduled task is handed to the task runner. To accomodate this, set the socket timeout in your task runner to 90 seconds. The task runner should not call @PollForTask@ again on the same @workerGroup@ until it receives a response, and this can take up to 90 seconds.
module Network.AWS.DataPipeline.PollForTask
  ( -- * Creating a request
    PollForTask (..),
    mkPollForTask,

    -- ** Request lenses
    pftHostname,
    pftWorkerGroup,
    pftInstanceIdentity,

    -- * Destructuring the response
    PollForTaskResponse (..),
    mkPollForTaskResponse,

    -- ** Response lenses
    pftrsTaskObject,
    pftrsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for PollForTask.
--
-- /See:/ 'mkPollForTask' smart constructor.
data PollForTask = PollForTask'
  { -- | The public DNS name of the calling task runner.
    hostname :: Lude.Maybe Lude.Text,
    -- | The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ in the call to @PollForTask@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
    workerGroup :: Lude.Text,
    -- | Identity information for the EC2 instance that is hosting the task runner. You can get this value from the instance using @http://169.254.169.254/latest/meta-data/instance-id@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
    instanceIdentity :: Lude.Maybe InstanceIdentity
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PollForTask' with the minimum fields required to make a request.
--
-- * 'hostname' - The public DNS name of the calling task runner.
-- * 'workerGroup' - The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ in the call to @PollForTask@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
-- * 'instanceIdentity' - Identity information for the EC2 instance that is hosting the task runner. You can get this value from the instance using @http://169.254.169.254/latest/meta-data/instance-id@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
mkPollForTask ::
  -- | 'workerGroup'
  Lude.Text ->
  PollForTask
mkPollForTask pWorkerGroup_ =
  PollForTask'
    { hostname = Lude.Nothing,
      workerGroup = pWorkerGroup_,
      instanceIdentity = Lude.Nothing
    }

-- | The public DNS name of the calling task runner.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftHostname :: Lens.Lens' PollForTask (Lude.Maybe Lude.Text)
pftHostname = Lens.lens (hostname :: PollForTask -> Lude.Maybe Lude.Text) (\s a -> s {hostname = a} :: PollForTask)
{-# DEPRECATED pftHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ in the call to @PollForTask@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
--
-- /Note:/ Consider using 'workerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftWorkerGroup :: Lens.Lens' PollForTask Lude.Text
pftWorkerGroup = Lens.lens (workerGroup :: PollForTask -> Lude.Text) (\s a -> s {workerGroup = a} :: PollForTask)
{-# DEPRECATED pftWorkerGroup "Use generic-lens or generic-optics with 'workerGroup' instead." #-}

-- | Identity information for the EC2 instance that is hosting the task runner. You can get this value from the instance using @http://169.254.169.254/latest/meta-data/instance-id@ . For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/AESDG-chapter-instancedata.html Instance Metadata> in the /Amazon Elastic Compute Cloud User Guide./ Passing in this value proves that your task runner is running on an EC2 instance, and ensures the proper AWS Data Pipeline service charges are applied to your pipeline.
--
-- /Note:/ Consider using 'instanceIdentity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftInstanceIdentity :: Lens.Lens' PollForTask (Lude.Maybe InstanceIdentity)
pftInstanceIdentity = Lens.lens (instanceIdentity :: PollForTask -> Lude.Maybe InstanceIdentity) (\s a -> s {instanceIdentity = a} :: PollForTask)
{-# DEPRECATED pftInstanceIdentity "Use generic-lens or generic-optics with 'instanceIdentity' instead." #-}

instance Lude.AWSRequest PollForTask where
  type Rs PollForTask = PollForTaskResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          PollForTaskResponse'
            Lude.<$> (x Lude..?> "taskObject") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders PollForTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.PollForTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON PollForTask where
  toJSON PollForTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("hostname" Lude..=) Lude.<$> hostname,
            Lude.Just ("workerGroup" Lude..= workerGroup),
            ("instanceIdentity" Lude..=) Lude.<$> instanceIdentity
          ]
      )

instance Lude.ToPath PollForTask where
  toPath = Lude.const "/"

instance Lude.ToQuery PollForTask where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of PollForTask.
--
-- /See:/ 'mkPollForTaskResponse' smart constructor.
data PollForTaskResponse = PollForTaskResponse'
  { -- | The information needed to complete the task that is being assigned to the task runner. One of the fields returned in this object is @taskId@ , which contains an identifier for the task being assigned. The calling task runner uses @taskId@ in subsequent calls to 'ReportTaskProgress' and 'SetTaskStatus' .
    taskObject :: Lude.Maybe TaskObject,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PollForTaskResponse' with the minimum fields required to make a request.
--
-- * 'taskObject' - The information needed to complete the task that is being assigned to the task runner. One of the fields returned in this object is @taskId@ , which contains an identifier for the task being assigned. The calling task runner uses @taskId@ in subsequent calls to 'ReportTaskProgress' and 'SetTaskStatus' .
-- * 'responseStatus' - The response status code.
mkPollForTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  PollForTaskResponse
mkPollForTaskResponse pResponseStatus_ =
  PollForTaskResponse'
    { taskObject = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The information needed to complete the task that is being assigned to the task runner. One of the fields returned in this object is @taskId@ , which contains an identifier for the task being assigned. The calling task runner uses @taskId@ in subsequent calls to 'ReportTaskProgress' and 'SetTaskStatus' .
--
-- /Note:/ Consider using 'taskObject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftrsTaskObject :: Lens.Lens' PollForTaskResponse (Lude.Maybe TaskObject)
pftrsTaskObject = Lens.lens (taskObject :: PollForTaskResponse -> Lude.Maybe TaskObject) (\s a -> s {taskObject = a} :: PollForTaskResponse)
{-# DEPRECATED pftrsTaskObject "Use generic-lens or generic-optics with 'taskObject' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pftrsResponseStatus :: Lens.Lens' PollForTaskResponse Lude.Int
pftrsResponseStatus = Lens.lens (responseStatus :: PollForTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: PollForTaskResponse)
{-# DEPRECATED pftrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
