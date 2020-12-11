{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DataPipeline.ReportTaskProgress
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Task runners call @ReportTaskProgress@ when assigned a task to acknowledge that it has the task. If the web service does not receive this acknowledgement within 2 minutes, it assigns the task in a subsequent 'PollForTask' call. After this initial acknowledgement, the task runner only needs to report progress every 15 minutes to maintain its ownership of the task. You can change this reporting time from 15 minutes by specifying a @reportProgressTimeout@ field in your pipeline.
--
-- If a task runner does not report its status after 5 minutes, AWS Data Pipeline assumes that the task runner is unable to process the task and reassigns the task in a subsequent response to 'PollForTask' . Task runners should call @ReportTaskProgress@ every 60 seconds.
module Network.AWS.DataPipeline.ReportTaskProgress
  ( -- * Creating a request
    ReportTaskProgress (..),
    mkReportTaskProgress,

    -- ** Request lenses
    rtpFields,
    rtpTaskId,

    -- * Destructuring the response
    ReportTaskProgressResponse (..),
    mkReportTaskProgressResponse,

    -- ** Response lenses
    rtprsResponseStatus,
    rtprsCanceled,
  )
where

import Network.AWS.DataPipeline.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Contains the parameters for ReportTaskProgress.
--
-- /See:/ 'mkReportTaskProgress' smart constructor.
data ReportTaskProgress = ReportTaskProgress'
  { fields ::
      Lude.Maybe [Field],
    taskId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportTaskProgress' with the minimum fields required to make a request.
--
-- * 'fields' - Key-value pairs that define the properties of the ReportTaskProgressInput object.
-- * 'taskId' - The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
mkReportTaskProgress ::
  -- | 'taskId'
  Lude.Text ->
  ReportTaskProgress
mkReportTaskProgress pTaskId_ =
  ReportTaskProgress' {fields = Lude.Nothing, taskId = pTaskId_}

-- | Key-value pairs that define the properties of the ReportTaskProgressInput object.
--
-- /Note:/ Consider using 'fields' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtpFields :: Lens.Lens' ReportTaskProgress (Lude.Maybe [Field])
rtpFields = Lens.lens (fields :: ReportTaskProgress -> Lude.Maybe [Field]) (\s a -> s {fields = a} :: ReportTaskProgress)
{-# DEPRECATED rtpFields "Use generic-lens or generic-optics with 'fields' instead." #-}

-- | The ID of the task assigned to the task runner. This value is provided in the response for 'PollForTask' .
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtpTaskId :: Lens.Lens' ReportTaskProgress Lude.Text
rtpTaskId = Lens.lens (taskId :: ReportTaskProgress -> Lude.Text) (\s a -> s {taskId = a} :: ReportTaskProgress)
{-# DEPRECATED rtpTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

instance Lude.AWSRequest ReportTaskProgress where
  type Rs ReportTaskProgress = ReportTaskProgressResponse
  request = Req.postJSON dataPipelineService
  response =
    Res.receiveJSON
      ( \s h x ->
          ReportTaskProgressResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s)) Lude.<*> (x Lude..:> "canceled")
      )

instance Lude.ToHeaders ReportTaskProgress where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("DataPipeline.ReportTaskProgress" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ReportTaskProgress where
  toJSON ReportTaskProgress' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("fields" Lude..=) Lude.<$> fields,
            Lude.Just ("taskId" Lude..= taskId)
          ]
      )

instance Lude.ToPath ReportTaskProgress where
  toPath = Lude.const "/"

instance Lude.ToQuery ReportTaskProgress where
  toQuery = Lude.const Lude.mempty

-- | Contains the output of ReportTaskProgress.
--
-- /See:/ 'mkReportTaskProgressResponse' smart constructor.
data ReportTaskProgressResponse = ReportTaskProgressResponse'
  { responseStatus ::
      Lude.Int,
    canceled :: Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ReportTaskProgressResponse' with the minimum fields required to make a request.
--
-- * 'canceled' - If true, the calling task runner should cancel processing of the task. The task runner does not need to call 'SetTaskStatus' for canceled tasks.
-- * 'responseStatus' - The response status code.
mkReportTaskProgressResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  -- | 'canceled'
  Lude.Bool ->
  ReportTaskProgressResponse
mkReportTaskProgressResponse pResponseStatus_ pCanceled_ =
  ReportTaskProgressResponse'
    { responseStatus = pResponseStatus_,
      canceled = pCanceled_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtprsResponseStatus :: Lens.Lens' ReportTaskProgressResponse Lude.Int
rtprsResponseStatus = Lens.lens (responseStatus :: ReportTaskProgressResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ReportTaskProgressResponse)
{-# DEPRECATED rtprsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}

-- | If true, the calling task runner should cancel processing of the task. The task runner does not need to call 'SetTaskStatus' for canceled tasks.
--
-- /Note:/ Consider using 'canceled' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
rtprsCanceled :: Lens.Lens' ReportTaskProgressResponse Lude.Bool
rtprsCanceled = Lens.lens (canceled :: ReportTaskProgressResponse -> Lude.Bool) (\s a -> s {canceled = a} :: ReportTaskProgressResponse)
{-# DEPRECATED rtprsCanceled "Use generic-lens or generic-optics with 'canceled' instead." #-}
