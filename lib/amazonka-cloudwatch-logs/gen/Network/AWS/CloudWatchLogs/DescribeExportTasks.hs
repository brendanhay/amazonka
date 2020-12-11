{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CloudWatchLogs.DescribeExportTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the specified export tasks. You can list all your export tasks or filter the results based on task ID or task status.
--
-- This operation returns paginated results.
module Network.AWS.CloudWatchLogs.DescribeExportTasks
  ( -- * Creating a request
    DescribeExportTasks (..),
    mkDescribeExportTasks,

    -- ** Request lenses
    detTaskId,
    detNextToken,
    detLimit,
    detStatusCode,

    -- * Destructuring the response
    DescribeExportTasksResponse (..),
    mkDescribeExportTasksResponse,

    -- ** Response lenses
    detrsNextToken,
    detrsExportTasks,
    detrsResponseStatus,
  )
where

import Network.AWS.CloudWatchLogs.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeExportTasks' smart constructor.
data DescribeExportTasks = DescribeExportTasks'
  { taskId ::
      Lude.Maybe Lude.Text,
    nextToken :: Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    statusCode :: Lude.Maybe ExportTaskStatusCode
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeExportTasks' with the minimum fields required to make a request.
--
-- * 'limit' - The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
-- * 'nextToken' - The token for the next set of items to return. (You received this token from a previous call.)
-- * 'statusCode' - The status code of the export task. Specifying a status code filters the results to zero or more export tasks.
-- * 'taskId' - The ID of the export task. Specifying a task ID filters the results to zero or one export tasks.
mkDescribeExportTasks ::
  DescribeExportTasks
mkDescribeExportTasks =
  DescribeExportTasks'
    { taskId = Lude.Nothing,
      nextToken = Lude.Nothing,
      limit = Lude.Nothing,
      statusCode = Lude.Nothing
    }

-- | The ID of the export task. Specifying a task ID filters the results to zero or one export tasks.
--
-- /Note:/ Consider using 'taskId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detTaskId :: Lens.Lens' DescribeExportTasks (Lude.Maybe Lude.Text)
detTaskId = Lens.lens (taskId :: DescribeExportTasks -> Lude.Maybe Lude.Text) (\s a -> s {taskId = a} :: DescribeExportTasks)
{-# DEPRECATED detTaskId "Use generic-lens or generic-optics with 'taskId' instead." #-}

-- | The token for the next set of items to return. (You received this token from a previous call.)
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detNextToken :: Lens.Lens' DescribeExportTasks (Lude.Maybe Lude.Text)
detNextToken = Lens.lens (nextToken :: DescribeExportTasks -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeExportTasks)
{-# DEPRECATED detNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The maximum number of items returned. If you don't specify a value, the default is up to 50 items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detLimit :: Lens.Lens' DescribeExportTasks (Lude.Maybe Lude.Natural)
detLimit = Lens.lens (limit :: DescribeExportTasks -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeExportTasks)
{-# DEPRECATED detLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | The status code of the export task. Specifying a status code filters the results to zero or more export tasks.
--
-- /Note:/ Consider using 'statusCode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detStatusCode :: Lens.Lens' DescribeExportTasks (Lude.Maybe ExportTaskStatusCode)
detStatusCode = Lens.lens (statusCode :: DescribeExportTasks -> Lude.Maybe ExportTaskStatusCode) (\s a -> s {statusCode = a} :: DescribeExportTasks)
{-# DEPRECATED detStatusCode "Use generic-lens or generic-optics with 'statusCode' instead." #-}

instance Page.AWSPager DescribeExportTasks where
  page rq rs
    | Page.stop (rs Lens.^. detrsNextToken) = Lude.Nothing
    | Page.stop (rs Lens.^. detrsExportTasks) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& detNextToken Lens..~ rs Lens.^. detrsNextToken

instance Lude.AWSRequest DescribeExportTasks where
  type Rs DescribeExportTasks = DescribeExportTasksResponse
  request = Req.postJSON cloudWatchLogsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeExportTasksResponse'
            Lude.<$> (x Lude..?> "nextToken")
            Lude.<*> (x Lude..?> "exportTasks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeExportTasks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("Logs_20140328.DescribeExportTasks" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeExportTasks where
  toJSON DescribeExportTasks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("taskId" Lude..=) Lude.<$> taskId,
            ("nextToken" Lude..=) Lude.<$> nextToken,
            ("limit" Lude..=) Lude.<$> limit,
            ("statusCode" Lude..=) Lude.<$> statusCode
          ]
      )

instance Lude.ToPath DescribeExportTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeExportTasks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeExportTasksResponse' smart constructor.
data DescribeExportTasksResponse = DescribeExportTasksResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    exportTasks ::
      Lude.Maybe [ExportTask],
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

-- | Creates a value of 'DescribeExportTasksResponse' with the minimum fields required to make a request.
--
-- * 'exportTasks' - The export tasks.
-- * 'nextToken' - Undocumented field.
-- * 'responseStatus' - The response status code.
mkDescribeExportTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeExportTasksResponse
mkDescribeExportTasksResponse pResponseStatus_ =
  DescribeExportTasksResponse'
    { nextToken = Lude.Nothing,
      exportTasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsNextToken :: Lens.Lens' DescribeExportTasksResponse (Lude.Maybe Lude.Text)
detrsNextToken = Lens.lens (nextToken :: DescribeExportTasksResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The export tasks.
--
-- /Note:/ Consider using 'exportTasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsExportTasks :: Lens.Lens' DescribeExportTasksResponse (Lude.Maybe [ExportTask])
detrsExportTasks = Lens.lens (exportTasks :: DescribeExportTasksResponse -> Lude.Maybe [ExportTask]) (\s a -> s {exportTasks = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsExportTasks "Use generic-lens or generic-optics with 'exportTasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
detrsResponseStatus :: Lens.Lens' DescribeExportTasksResponse Lude.Int
detrsResponseStatus = Lens.lens (responseStatus :: DescribeExportTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeExportTasksResponse)
{-# DEPRECATED detrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
