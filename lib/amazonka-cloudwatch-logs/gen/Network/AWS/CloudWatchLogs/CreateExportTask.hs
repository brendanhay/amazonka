{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.CreateExportTask
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an export task, which allows you to efficiently export data from a log group to an Amazon S3 bucket. When you perform a @CreateExportTask@ operation, you must use credentials that have permission to write to the S3 bucket that you specify as the destination.
--
-- This is an asynchronous call. If all the required information is provided, this operation initiates an export task and responds with the ID of the task. After the task has started, you can use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_DescribeExportTasks.html DescribeExportTasks> to get the status of the export task. Each account can only have one active (@RUNNING@ or @PENDING@ ) export task at a time. To cancel an export task, use <https://docs.aws.amazon.com/AmazonCloudWatchLogs/latest/APIReference/API_CancelExportTask.html CancelExportTask> .
-- You can export logs from multiple log groups or multiple time ranges to the same S3 bucket. To separate out log data for each export task, you can specify a prefix to be used as the Amazon S3 key prefix for all exported objects.
-- Exporting to S3 buckets that are encrypted with AES-256 is supported. Exporting to S3 buckets encrypted with SSE-KMS is not supported.
module Network.AWS.CloudWatchLogs.CreateExportTask
  ( -- * Creating a request
    CreateExportTask (..),
    mkCreateExportTask,

    -- ** Request lenses
    cetDestinationPrefix,
    cetTaskName,
    cetLogStreamNamePrefix,
    cetLogGroupName,
    cetFrom,
    cetTo,
    cetDestination,

    -- * Destructuring the response
    CreateExportTaskResponse (..),
    mkCreateExportTaskResponse,

    -- ** Response lenses
    cetrsTaskId,
    cetrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateExportTask' smart constructor.
data CreateExportTask = CreateExportTask'
  { destinationPrefix ::
      Lude.Maybe Lude.Text,
    taskName :: Lude.Maybe Lude.Text,
    logStreamNamePrefix :: Lude.Maybe Lude.Text,
    logGroupName :: Lude.Text,
    from :: Lude.Natural,
    to :: Lude.Natural,
    destination :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateExportTask' with the minimum fields required to make a request.
--
-- * 'destination' - The name of S3 bucket for the exported log data. The bucket must be in the same AWS region.
-- * 'destinationPrefix' - The prefix used as the start of the key for every object exported. If you don't specify a value, the default is @exportedlogs@ .
-- * 'from' - The start time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp earlier than this time are not exported.
-- * 'logGroupName' - The name of the log group.
-- * 'logStreamNamePrefix' - Export only log streams that match the provided prefix. If you don't specify a value, no prefix filter is applied.
-- * 'taskName' - The name of the export task.
-- * 'to' - The end time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
mkCreateExportTask ::
  -- | 'logGroupName'
  Lude.Text ->
  -- | 'from'
  Lude.Natural ->
  -- | 'to'
  Lude.Natural ->
  -- | 'destination'
  Lude.Text ->
  CreateExportTask
mkCreateExportTask pLogGroupName_ pFrom_ pTo_ pDestination_ =
  CreateExportTask'
    { destinationPrefix = Lude.Nothing,
      taskName = Lude.Nothing,
      logStreamNamePrefix = Lude.Nothing,
      logGroupName = pLogGroupName_,
      from = pFrom_,
      to = pTo_,
      destination = pDestination_
    }

-- | The prefix used as the start of the key for every object exported. If you don't specify a value, the default is @exportedlogs@ .
--
-- /Note:/ Consider using 'destinationPrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetDestinationPrefix :: Lens.Lens' CreateExportTask (Lude.Maybe Lude.Text)
cetDestinationPrefix = Lens.lens (destinationPrefix :: CreateExportTask -> Lude.Maybe Lude.Text) (\s a -> s {destinationPrefix = a} :: CreateExportTask)
{-# DEPRECATED cetDestinationPrefix "Use generic-lens or generic-optics with 'destinationPrefix' instead." #-}

-- | The name of the export task.
--
-- /Note:/ Consider using 'taskName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetTaskName :: Lens.Lens' CreateExportTask (Lude.Maybe Lude.Text)
cetTaskName = Lens.lens (taskName :: CreateExportTask -> Lude.Maybe Lude.Text) (\s a -> s {taskName = a} :: CreateExportTask)
{-# DEPRECATED cetTaskName "Use generic-lens or generic-optics with 'taskName' instead." #-}

-- | Export only log streams that match the provided prefix. If you don't specify a value, no prefix filter is applied.
--
-- /Note:/ Consider using 'logStreamNamePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetLogStreamNamePrefix :: Lens.Lens' CreateExportTask (Lude.Maybe Lude.Text)
cetLogStreamNamePrefix = Lens.lens (logStreamNamePrefix :: CreateExportTask -> Lude.Maybe Lude.Text) (\s a -> s {logStreamNamePrefix = a} :: CreateExportTask)
{-# DEPRECATED cetLogStreamNamePrefix "Use generic-lens or generic-optics with 'logStreamNamePrefix' instead." #-}

-- | The name of the log group.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetLogGroupName :: Lens.Lens' CreateExportTask Lude.Text
cetLogGroupName = Lens.lens (logGroupName :: CreateExportTask -> Lude.Text) (\s a -> s {logGroupName = a} :: CreateExportTask)
{-# DEPRECATED cetLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The start time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp earlier than this time are not exported.
--
-- /Note:/ Consider using 'from' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetFrom :: Lens.Lens' CreateExportTask Lude.Natural
cetFrom = Lens.lens (from :: CreateExportTask -> Lude.Natural) (\s a -> s {from = a} :: CreateExportTask)
{-# DEPRECATED cetFrom "Use generic-lens or generic-optics with 'from' instead." #-}

-- | The end time of the range for the request, expressed as the number of milliseconds after Jan 1, 1970 00:00:00 UTC. Events with a timestamp later than this time are not exported.
--
-- /Note:/ Consider using 'to' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetTo :: Lens.Lens' CreateExportTask Lude.Natural
cetTo = Lens.lens (to :: CreateExportTask -> Lude.Natural) (\s a -> s {to = a} :: CreateExportTask)
{-# DEPRECATED cetTo "Use generic-lens or generic-optics with 'to' instead." #-}

-- | The name of S3 bucket for the exported log data. The bucket must be in the same AWS region.
--
-- /Note:/ Consider using 'destination' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetDestination :: Lens.Lens' CreateExportTask Lude.Text
cetDestination = Lens.lens (destination :: CreateExportTask -> Lude.Text) (\s a -> s {destination = a} :: CreateExportTask)
{-# DEPRECATED cetDestination "Use generic-lens or generic-optics with 'destination' instead." #-}

instance Lude.AWSRequest CreateExportTask where
  type Rs CreateExportTask = CreateExportTaskResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateExportTaskResponse'
            Lude.<$> (x Lude..?> "taskId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateExportTask where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.CreateExportTask" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateExportTask where
  toJSON CreateExportTask' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("destinationPrefix" Lude..=) Lude.<$> destinationPrefix,
            ("taskName" Lude..=) Lude.<$> taskName,
            ("logStreamNamePrefix" Lude..=) Lude.<$> logStreamNamePrefix,
            Lude.Just ("logGroupName" Lude..= logGroupName),
            Lude.Just ("from" Lude..= from),
            Lude.Just ("to" Lude..= to),
            Lude.Just ("destination" Lude..= destination)
          ]
      )

instance Lude.ToPath CreateExportTask where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateExportTask where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateExportTaskResponse' smart constructor.
data CreateExportTaskResponse = CreateExportTaskResponse'
  { taskId ::
      Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CreateExportTaskResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'taskId' - The ID of the export task.
mkCreateExportTaskResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateExportTaskResponse
mkCreateExportTaskResponse pResponseStatus_ =
  CreateExportTaskResponse'
    { taskId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the export task.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetrsTaskId :: Lens.Lens' CreateExportTaskResponse (Lude.Maybe Lude.Text)
cetrsTaskId = Lens.lens (taskId :: CreateExportTaskResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: CreateExportTaskResponse)
{-# DEPRECATED cetrsTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cetrsResponseStatus :: Lens.Lens' CreateExportTaskResponse Lude.Int
cetrsResponseStatus = Lens.lens (responseStatus :: CreateExportTaskResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateExportTaskResponse)
{-# DEPRECATED cetrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
