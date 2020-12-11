{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Glue.GetMLTaskRuns
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of runs for a machine learning transform. Machine learning task runs are asynchronous tasks that AWS Glue runs on your behalf as part of various machine learning workflows. You can get a sortable, filterable list of machine learning task runs by calling @GetMLTaskRuns@ with their parent transform's @TransformID@ and other optional parameters as documented in this section.
--
-- This operation returns a list of historic runs and must be paginated.
module Network.AWS.Glue.GetMLTaskRuns
  ( -- * Creating a request
    GetMLTaskRuns (..),
    mkGetMLTaskRuns,

    -- ** Request lenses
    gmltrNextToken,
    gmltrSort,
    gmltrFilter,
    gmltrMaxResults,
    gmltrTransformId,

    -- * Destructuring the response
    GetMLTaskRunsResponse (..),
    mkGetMLTaskRunsResponse,

    -- ** Response lenses
    gmltrsrsNextToken,
    gmltrsrsTaskRuns,
    gmltrsrsResponseStatus,
  )
where

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkGetMLTaskRuns' smart constructor.
data GetMLTaskRuns = GetMLTaskRuns'
  { nextToken ::
      Lude.Maybe Lude.Text,
    sort :: Lude.Maybe TaskRunSortCriteria,
    filter :: Lude.Maybe TaskRunFilterCriteria,
    maxResults :: Lude.Maybe Lude.Natural,
    transformId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'GetMLTaskRuns' with the minimum fields required to make a request.
--
-- * 'filter' - The filter criteria, in the @TaskRunFilterCriteria@ structure, for the task run.
-- * 'maxResults' - The maximum number of results to return.
-- * 'nextToken' - A token for pagination of the results. The default is empty.
-- * 'sort' - The sorting criteria, in the @TaskRunSortCriteria@ structure, for the task run.
-- * 'transformId' - The unique identifier of the machine learning transform.
mkGetMLTaskRuns ::
  -- | 'transformId'
  Lude.Text ->
  GetMLTaskRuns
mkGetMLTaskRuns pTransformId_ =
  GetMLTaskRuns'
    { nextToken = Lude.Nothing,
      sort = Lude.Nothing,
      filter = Lude.Nothing,
      maxResults = Lude.Nothing,
      transformId = pTransformId_
    }

-- | A token for pagination of the results. The default is empty.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrNextToken :: Lens.Lens' GetMLTaskRuns (Lude.Maybe Lude.Text)
gmltrNextToken = Lens.lens (nextToken :: GetMLTaskRuns -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMLTaskRuns)
{-# DEPRECATED gmltrNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The sorting criteria, in the @TaskRunSortCriteria@ structure, for the task run.
--
-- /Note:/ Consider using 'sort' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrSort :: Lens.Lens' GetMLTaskRuns (Lude.Maybe TaskRunSortCriteria)
gmltrSort = Lens.lens (sort :: GetMLTaskRuns -> Lude.Maybe TaskRunSortCriteria) (\s a -> s {sort = a} :: GetMLTaskRuns)
{-# DEPRECATED gmltrSort "Use generic-lens or generic-optics with 'sort' instead." #-}

-- | The filter criteria, in the @TaskRunFilterCriteria@ structure, for the task run.
--
-- /Note:/ Consider using 'filter' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrFilter :: Lens.Lens' GetMLTaskRuns (Lude.Maybe TaskRunFilterCriteria)
gmltrFilter = Lens.lens (filter :: GetMLTaskRuns -> Lude.Maybe TaskRunFilterCriteria) (\s a -> s {filter = a} :: GetMLTaskRuns)
{-# DEPRECATED gmltrFilter "Use generic-lens or generic-optics with 'filter' instead." #-}

-- | The maximum number of results to return.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrMaxResults :: Lens.Lens' GetMLTaskRuns (Lude.Maybe Lude.Natural)
gmltrMaxResults = Lens.lens (maxResults :: GetMLTaskRuns -> Lude.Maybe Lude.Natural) (\s a -> s {maxResults = a} :: GetMLTaskRuns)
{-# DEPRECATED gmltrMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

-- | The unique identifier of the machine learning transform.
--
-- /Note:/ Consider using 'transformId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrTransformId :: Lens.Lens' GetMLTaskRuns Lude.Text
gmltrTransformId = Lens.lens (transformId :: GetMLTaskRuns -> Lude.Text) (\s a -> s {transformId = a} :: GetMLTaskRuns)
{-# DEPRECATED gmltrTransformId "Use generic-lens or generic-optics with 'transformId' instead." #-}

instance Lude.AWSRequest GetMLTaskRuns where
  type Rs GetMLTaskRuns = GetMLTaskRunsResponse
  request = Req.postJSON glueService
  response =
    Res.receiveJSON
      ( \s h x ->
          GetMLTaskRunsResponse'
            Lude.<$> (x Lude..?> "NextToken")
            Lude.<*> (x Lude..?> "TaskRuns" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders GetMLTaskRuns where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("AWSGlue.GetMLTaskRuns" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON GetMLTaskRuns where
  toJSON GetMLTaskRuns' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("NextToken" Lude..=) Lude.<$> nextToken,
            ("Sort" Lude..=) Lude.<$> sort,
            ("Filter" Lude..=) Lude.<$> filter,
            ("MaxResults" Lude..=) Lude.<$> maxResults,
            Lude.Just ("TransformId" Lude..= transformId)
          ]
      )

instance Lude.ToPath GetMLTaskRuns where
  toPath = Lude.const "/"

instance Lude.ToQuery GetMLTaskRuns where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkGetMLTaskRunsResponse' smart constructor.
data GetMLTaskRunsResponse = GetMLTaskRunsResponse'
  { nextToken ::
      Lude.Maybe Lude.Text,
    taskRuns :: Lude.Maybe [TaskRun],
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

-- | Creates a value of 'GetMLTaskRunsResponse' with the minimum fields required to make a request.
--
-- * 'nextToken' - A pagination token, if more results are available.
-- * 'responseStatus' - The response status code.
-- * 'taskRuns' - A list of task runs that are associated with the transform.
mkGetMLTaskRunsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  GetMLTaskRunsResponse
mkGetMLTaskRunsResponse pResponseStatus_ =
  GetMLTaskRunsResponse'
    { nextToken = Lude.Nothing,
      taskRuns = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A pagination token, if more results are available.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsrsNextToken :: Lens.Lens' GetMLTaskRunsResponse (Lude.Maybe Lude.Text)
gmltrsrsNextToken = Lens.lens (nextToken :: GetMLTaskRunsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: GetMLTaskRunsResponse)
{-# DEPRECATED gmltrsrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | A list of task runs that are associated with the transform.
--
-- /Note:/ Consider using 'taskRuns' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsrsTaskRuns :: Lens.Lens' GetMLTaskRunsResponse (Lude.Maybe [TaskRun])
gmltrsrsTaskRuns = Lens.lens (taskRuns :: GetMLTaskRunsResponse -> Lude.Maybe [TaskRun]) (\s a -> s {taskRuns = a} :: GetMLTaskRunsResponse)
{-# DEPRECATED gmltrsrsTaskRuns "Use generic-lens or generic-optics with 'taskRuns' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
gmltrsrsResponseStatus :: Lens.Lens' GetMLTaskRunsResponse Lude.Int
gmltrsrsResponseStatus = Lens.lens (responseStatus :: GetMLTaskRunsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: GetMLTaskRunsResponse)
{-# DEPRECATED gmltrsrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
