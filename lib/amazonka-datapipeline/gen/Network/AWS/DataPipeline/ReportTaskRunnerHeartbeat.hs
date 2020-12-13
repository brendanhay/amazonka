{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @ReportTaskRunnerHeartbeat@ every 15 minutes to indicate that they are operational. If the AWS Data Pipeline Task Runner is launched on a resource managed by AWS Data Pipeline, the web service can use this call to detect when the task runner application has failed and restart a new instance.
module Network.AWS.DataPipeline.ReportTaskRunnerHeartbeat
  ( -- * Creating a request
    ReportTaskRunnerHeartbeat (..),
    mkReportTaskRunnerHeartbeat,

    -- ** Request lenses
    rtrhHostname,
    rtrhWorkerGroup,
    rtrhTaskrunnerId,

    -- * Destructuring the response
    ReportTaskRunnerHeartbeatResponse (..),
    mkReportTaskRunnerHeartbeatResponse,

    -- ** Response lenses
    rtrhrsTerminate,
    rtrhrsResponseStatus,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ReportTaskRunnerHeartbeat.
--
-- /See:/ 'mkReportTaskRunnerHeartbeat' smart constructor.
data ReportTaskRunnerHeartbeat = ReportTaskRunnerHeartbeat'
  { -- | The public DNS name of the task runner.
    hostname :: Lude.Maybe Lude.Text,
    -- | The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
    workerGroup :: Lude.Maybe Lude.Text,
    -- | The ID of the task runner. This value should be unique across your AWS account. In the case of AWS Data Pipeline Task Runner launched on a resource managed by AWS Data Pipeline, the web service provides a unique identifier when it launches the application. If you have written a custom task runner, you should assign a unique identifier for the task runner.
    taskrunnerId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportTaskRunnerHeartbeat' with the minimum fields required to make a request.
--
-- * 'hostname' - The public DNS name of the task runner.
-- * 'workerGroup' - The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
-- * 'taskrunnerId' - The ID of the task runner. This value should be unique across your AWS account. In the case of AWS Data Pipeline Task Runner launched on a resource managed by AWS Data Pipeline, the web service provides a unique identifier when it launches the application. If you have written a custom task runner, you should assign a unique identifier for the task runner.
mkReportTaskRunnerHeartbeat ::
  -- | 'taskrunnerId'
  Lude.Text ->
  ReportTaskRunnerHeartbeat
mkReportTaskRunnerHeartbeat pTaskrunnerId_ =
  ReportTaskRunnerHeartbeat'
    { hostname = Lude.Nothing,
      workerGroup = Lude.Nothing,
      taskrunnerId = pTaskrunnerId_
    }

-- | The public DNS name of the task runner.
--
-- /Note:/ Consider using 'hostname' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrhHostname :: Lens.Lens' ReportTaskRunnerHeartbeat (Lude.Maybe Lude.Text)
rtrhHostname = Lens.lens (hostname :: ReportTaskRunnerHeartbeat -> Lude.Maybe Lude.Text) (\s a -> s {hostname = a} :: ReportTaskRunnerHeartbeat)
{-# DEPRECATED rtrhHostname "Use generic-lens or generic-optics with 'hostname' instead." #-}

-- | The type of task the task runner is configured to accept and process. The worker group is set as a field on objects in the pipeline when they are created. You can only specify a single value for @workerGroup@ . There are no wildcard values permitted in @workerGroup@ ; the string must be an exact, case-sensitive, match.
--
-- /Note:/ Consider using 'workerGroup' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrhWorkerGroup :: Lens.Lens' ReportTaskRunnerHeartbeat (Lude.Maybe Lude.Text)
rtrhWorkerGroup = Lens.lens (workerGroup :: ReportTaskRunnerHeartbeat -> Lude.Maybe Lude.Text) (\s a -> s {workerGroup = a} :: ReportTaskRunnerHeartbeat)
{-# DEPRECATED rtrhWorkerGroup "Use generic-lens or generic-optics with 'workerGroup' instead." #-}

-- | The ID of the task runner. This value should be unique across your AWS account. In the case of AWS Data Pipeline Task Runner launched on a resource managed by AWS Data Pipeline, the web service provides a unique identifier when it launches the application. If you have written a custom task runner, you should assign a unique identifier for the task runner.
--
-- /Note:/ Consider using 'taskrunnerId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrhTaskrunnerId :: Lens.Lens' ReportTaskRunnerHeartbeat Lude.Text
rtrhTaskrunnerId = Lens.lens (taskrunnerId :: ReportTaskRunnerHeartbeat -> Lude.Text) (\s a -> s {taskrunnerId = a} :: ReportTaskRunnerHeartbeat)
{-# DEPRECATED rtrhTaskrunnerId "Use generic-lens or generic-optics with 'taskrunnerId' instead." #-}

instance Lude.AWSRequest ReportTaskRunnerHeartbeat where
  type
    Rs ReportTaskRunnerHeartbeat =
      ReportTaskRunnerHeartbeatResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          ReportTaskRunnerHeartbeatResponse'
            Lude.<$> (x Lude..:> "terminate") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ReportTaskRunnerHeartbeat where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.ReportTaskRunnerHeartbeat" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ReportTaskRunnerHeartbeat where
  toJSON ReportTaskRunnerHeartbeat' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("hostname" Lude..=) Lude.<$> hostname,
            ("workerGroup" Lude..=) Lude.<$> workerGroup,
            Lude.Just ("taskrunnerId" Lude..= taskrunnerId)
          ]
      )

instance Lude.ToPath ReportTaskRunnerHeartbeat where
  toPath = Lude.const "/"

instance Lude.ToQuery ReportTaskRunnerHeartbeat where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of ReportTaskRunnerHeartbeat.
--
-- /See:/ 'mkReportTaskRunnerHeartbeatResponse' smart constructor.
data ReportTaskRunnerHeartbeatResponse = ReportTaskRunnerHeartbeatResponse'
  { -- | Indicates whether the calling task runner should terminate.
    terminate :: Lude.Bool,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportTaskRunnerHeartbeatResponse' with the minimum fields required to make a request.
--
-- * 'terminate' - Indicates whether the calling task runner should terminate.
-- * 'responseStatus' - The response status code.
mkReportTaskRunnerHeartbeatResponse ::
  -- | 'terminate'
  Lude.Bool ->
  -- | 'responseStatus'
  Lude.Int ->
  ReportTaskRunnerHeartbeatResponse
mkReportTaskRunnerHeartbeatResponse pTerminate_ pResponseStatus_ =
  ReportTaskRunnerHeartbeatResponse'
    { terminate = pTerminate_,
      responseStatus = pResponseStatus_
    }

-- | Indicates whether the calling task runner should terminate.
--
-- /Note:/ Consider using 'terminate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrhrsTerminate :: Lens.Lens' ReportTaskRunnerHeartbeatResponse Lude.Bool
rtrhrsTerminate = Lens.lens (terminate :: ReportTaskRunnerHeartbeatResponse -> Lude.Bool) (\s a -> s {terminate = a} :: ReportTaskRunnerHeartbeatResponse)
{-# DEPRECATED rtrhrsTerminate "Use generic-lens or generic-optics with 'terminate' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtrhrsResponseStatus :: Lens.Lens' ReportTaskRunnerHeartbeatResponse Lude.Int
rtrhrsResponseStatus = Lens.lens (responseStatus :: ReportTaskRunnerHeartbeatResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReportTaskRunnerHeartbeatResponse)
{-# DEPRECATED rtrhrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
