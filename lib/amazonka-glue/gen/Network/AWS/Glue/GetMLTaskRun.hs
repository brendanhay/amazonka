{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetMLTaskRun
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets details for a specific task run on a machine learning transform. Machine learning task runs are asynchronous tasks that AWS Glue runs on your behalf as part of various machine learning workflows. You can check the stats of any task run by calling @GetMLTaskRun@ with the @TaskRunID@ and its parent transform's @TransformID@ .
module Network.AWS.Glue.GetMLTaskRun
  ( -- * Creating a request
    GetMLTaskRun (..),
    mkGetMLTaskRun,

    -- ** Request lenses
    gTransformId,
    gTaskRunId,

    -- * Destructuring the response
    GetMLTaskRunResponse (..),
    mkGetMLTaskRunResponse,

    -- ** Response lenses
    gmltrrsCompletedOn,
    gmltrrsStatus,
    gmltrrsLastModifiedOn,
    gmltrrsErrorString,
    gmltrrsStartedOn,
    gmltrrsLogGroupName,
    gmltrrsExecutionTime,
    gmltrrsProperties,
    gmltrrsTransformId,
    gmltrrsTaskRunId,
    gmltrrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMLTaskRun' smart constructor.
data GetMLTaskRun = GetMLTaskRun'
  { transformId :: Lude.Text,
    taskRunId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMLTaskRun' with the minimum fields required to make a request.
--
-- * 'taskRunId' - The unique identifier of the task run.
-- * 'transformId' - The unique identifier of the machine learning transform.
mkGetMLTaskRun ::
  -- | 'transformId'
  Lude.Text ->
  -- | 'taskRunId'
  Lude.Text ->
  GetMLTaskRun
mkGetMLTaskRun pTransformId_ pTaskRunId_ =
  GetMLTaskRun'
    { transformId = pTransformId_,
      taskRunId = pTaskRunId_
    }

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gTransformId :: Lens.Lens' GetMLTaskRun Lude.Text
gTransformId = Lens.lens (transformId :: GetMLTaskRun -> Lude.Text) (\s a -> s {transformId = a} :: GetMLTaskRun)
{-# DEPRECATED gTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The unique identifier of the task run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gTaskRunId :: Lens.Lens' GetMLTaskRun Lude.Text
gTaskRunId = Lens.lens (taskRunId :: GetMLTaskRun -> Lude.Text) (\s a -> s {taskRunId = a} :: GetMLTaskRun)
{-# DEPRECATED gTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

instance Lude.AWSRequest GetMLTaskRun where
  type Rs GetMLTaskRun = GetMLTaskRunResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMLTaskRunResponse'
            Lude.<$> (x Lude..?> "CompletedOn")
            Lude.<*> (x Lude..?> "Status")
            Lude.<*> (x Lude..?> "LastModifiedOn")
            Lude.<*> (x Lude..?> "ErrorString")
            Lude.<*> (x Lude..?> "StartedOn")
            Lude.<*> (x Lude..?> "LogGroupName")
            Lude.<*> (x Lude..?> "ExecutionTime")
            Lude.<*> (x Lude..?> "Properties")
            Lude.<*> (x Lude..?> "TransformId")
            Lude.<*> (x Lude..?> "TaskRunId")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMLTaskRun where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetMLTaskRun" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMLTaskRun where
  toJSON GetMLTaskRun' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TransformId" Lude..= transformId),
            Lude.Just ("TaskRunId" Lude..= taskRunId)
          ]
      )

instance Lude.ToPath GetMLTaskRun where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMLTaskRun where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMLTaskRunResponse' smart constructor.
data GetMLTaskRunResponse = GetMLTaskRunResponse'
  { completedOn ::
      Lude.Maybe Lude.Timestamp,
    status :: Lude.Maybe TaskStatusType,
    lastModifiedOn :: Lude.Maybe Lude.Timestamp,
    errorString :: Lude.Maybe Lude.Text,
    startedOn :: Lude.Maybe Lude.Timestamp,
    logGroupName :: Lude.Maybe Lude.Text,
    executionTime :: Lude.Maybe Lude.Int,
    properties :: Lude.Maybe TaskRunProperties,
    transformId :: Lude.Maybe Lude.Text,
    taskRunId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'GetMLTaskRunResponse' with the minimum fields required to make a request.
--
-- * 'completedOn' - The date and time when this task run was completed.
-- * 'errorString' - The error strings that are associated with the task run.
-- * 'executionTime' - The amount of time (in seconds) that the task run consumed resources.
-- * 'lastModifiedOn' - The date and time when this task run was last modified.
-- * 'logGroupName' - The names of the log groups that are associated with the task run.
-- * 'properties' - The list of properties that are associated with the task run.
-- * 'responseStatus' - The response status code.
-- * 'startedOn' - The date and time when this task run started.
-- * 'status' - The status for this task run.
-- * 'taskRunId' - The unique run identifier associated with this run.
-- * 'transformId' - The unique identifier of the task run.
mkGetMLTaskRunResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMLTaskRunResponse
mkGetMLTaskRunResponse pResponseStatus_ =
  GetMLTaskRunResponse'
    { completedOn = Lude.Nothing,
      status = Lude.Nothing,
      lastModifiedOn = Lude.Nothing,
      errorString = Lude.Nothing,
      startedOn = Lude.Nothing,
      logGroupName = Lude.Nothing,
      executionTime = Lude.Nothing,
      properties = Lude.Nothing,
      transformId = Lude.Nothing,
      taskRunId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The date and time when this task run was completed.
--
-- /Note:/ Consider using 'completedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsCompletedOn :: Lens.Lens' GetMLTaskRunResponse (Lude.Maybe Lude.Timestamp)
gmltrrsCompletedOn = Lens.lens (completedOn :: GetMLTaskRunResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {completedOn = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsCompletedOn "Use generic-lens or generic-optics with 'completedOn' instead." #-}

-- | The status for this task run.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsStatus :: Lens.Lens' GetMLTaskRunResponse (Lude.Maybe TaskStatusType)
gmltrrsStatus = Lens.lens (status :: GetMLTaskRunResponse -> Lude.Maybe TaskStatusType) (\s a -> s {status = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The date and time when this task run was last modified.
--
-- /Note:/ Consider using 'lastModifiedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsLastModifiedOn :: Lens.Lens' GetMLTaskRunResponse (Lude.Maybe Lude.Timestamp)
gmltrrsLastModifiedOn = Lens.lens (lastModifiedOn :: GetMLTaskRunResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedOn = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsLastModifiedOn "Use generic-lens or generic-optics with 'lastModifiedOn' instead." #-}

-- | The error strings that are associated with the task run.
--
-- /Note:/ Consider using 'errorString' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsErrorString :: Lens.Lens' GetMLTaskRunResponse (Lude.Maybe Lude.Text)
gmltrrsErrorString = Lens.lens (errorString :: GetMLTaskRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {errorString = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsErrorString "Use generic-lens or generic-optics with 'errorString' instead." #-}

-- | The date and time when this task run started.
--
-- /Note:/ Consider using 'startedOn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsStartedOn :: Lens.Lens' GetMLTaskRunResponse (Lude.Maybe Lude.Timestamp)
gmltrrsStartedOn = Lens.lens (startedOn :: GetMLTaskRunResponse -> Lude.Maybe Lude.Timestamp) (\s a -> s {startedOn = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsStartedOn "Use generic-lens or generic-optics with 'startedOn' instead." #-}

-- | The names of the log groups that are associated with the task run.
--
-- /Note:/ Consider using 'logGroupName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsLogGroupName :: Lens.Lens' GetMLTaskRunResponse (Lude.Maybe Lude.Text)
gmltrrsLogGroupName = Lens.lens (logGroupName :: GetMLTaskRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {logGroupName = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsLogGroupName "Use generic-lens or generic-optics with 'logGroupName' instead." #-}

-- | The amount of time (in seconds) that the task run consumed resources.
--
-- /Note:/ Consider using 'executionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsExecutionTime :: Lens.Lens' GetMLTaskRunResponse (Lude.Maybe Lude.Int)
gmltrrsExecutionTime = Lens.lens (executionTime :: GetMLTaskRunResponse -> Lude.Maybe Lude.Int) (\s a -> s {executionTime = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsExecutionTime "Use generic-lens or generic-optics with 'executionTime' instead." #-}

-- | The list of properties that are associated with the task run.
--
-- /Note:/ Consider using 'properties' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsProperties :: Lens.Lens' GetMLTaskRunResponse (Lude.Maybe TaskRunProperties)
gmltrrsProperties = Lens.lens (properties :: GetMLTaskRunResponse -> Lude.Maybe TaskRunProperties) (\s a -> s {properties = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsProperties "Use generic-lens or generic-optics with 'properties' instead." #-}

-- | The unique identifier of the task run.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsTransformId :: Lens.Lens' GetMLTaskRunResponse (Lude.Maybe Lude.Text)
gmltrrsTransformId = Lens.lens (transformId :: GetMLTaskRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {transformId = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

-- | The unique run identifier associated with this run.
--
-- /Note:/ Consider using 'taskRunId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsTaskRunId :: Lens.Lens' GetMLTaskRunResponse (Lude.Maybe Lude.Text)
gmltrrsTaskRunId = Lens.lens (taskRunId :: GetMLTaskRunResponse -> Lude.Maybe Lude.Text) (\s a -> s {taskRunId = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsTaskRunId "Use generic-lens or generic-optics with 'taskRunId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrrsResponseStatus :: Lens.Lens' GetMLTaskRunResponse Lude.Int
gmltrrsResponseStatus = Lens.lens (responseStatus :: GetMLTaskRunResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMLTaskRunResponse)
{-# DEPRECATED gmltrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
