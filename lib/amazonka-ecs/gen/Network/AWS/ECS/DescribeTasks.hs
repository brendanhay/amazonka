{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeTasks
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes a specified task or tasks.
module Network.AWS.ECS.DescribeTasks
  ( -- * Creating a request
    DescribeTasks (..),
    mkDescribeTasks,

    -- ** Request lenses
    dtInclude,
    dtCluster,
    dtTasks,

    -- * Destructuring the response
    DescribeTasksResponse (..),
    mkDescribeTasksResponse,

    -- ** Response lenses
    dtrsFailures,
    dtrsTasks,
    dtrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTasks' smart constructor.
data DescribeTasks = DescribeTasks'
  { include ::
      Lude.Maybe [TaskField],
    cluster :: Lude.Maybe Lude.Text,
    tasks :: [Lude.Text]
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTasks' with the minimum fields required to make a request.
--
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task or tasks to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the task or tasks you are describing were launched in any cluster other than the default cluster.
-- * 'include' - Specifies whether you want to see the resource tags for the task. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
-- * 'tasks' - A list of up to 100 task IDs or full ARN entries.
mkDescribeTasks ::
  DescribeTasks
mkDescribeTasks =
  DescribeTasks'
    { include = Lude.Nothing,
      cluster = Lude.Nothing,
      tasks = Lude.mempty
    }

-- | Specifies whether you want to see the resource tags for the task. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtInclude :: Lens.Lens' DescribeTasks (Lude.Maybe [TaskField])
dtInclude = Lens.lens (include :: DescribeTasks -> Lude.Maybe [TaskField]) (\s a -> s {include = a} :: DescribeTasks)
{-# DEPRECATED dtInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the task or tasks to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the task or tasks you are describing were launched in any cluster other than the default cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtCluster :: Lens.Lens' DescribeTasks (Lude.Maybe Lude.Text)
dtCluster = Lens.lens (cluster :: DescribeTasks -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: DescribeTasks)
{-# DEPRECATED dtCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | A list of up to 100 task IDs or full ARN entries.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtTasks :: Lens.Lens' DescribeTasks [Lude.Text]
dtTasks = Lens.lens (tasks :: DescribeTasks -> [Lude.Text]) (\s a -> s {tasks = a} :: DescribeTasks)
{-# DEPRECATED dtTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

instance Lude.AWSRequest DescribeTasks where
  type Rs DescribeTasks = DescribeTasksResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTasksResponse'
            Lude.<$> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "tasks" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTasks where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DescribeTasks" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTasks where
  toJSON DescribeTasks' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("include" Lude..=) Lude.<$> include,
            ("cluster" Lude..=) Lude.<$> cluster,
            Lude.Just ("tasks" Lude..= tasks)
          ]
      )

instance Lude.ToPath DescribeTasks where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTasks where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTasksResponse' smart constructor.
data DescribeTasksResponse = DescribeTasksResponse'
  { failures ::
      Lude.Maybe [Failure],
    tasks :: Lude.Maybe [Task],
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

-- | Creates a value of 'DescribeTasksResponse' with the minimum fields required to make a request.
--
-- * 'failures' - Any failures associated with the call.
-- * 'responseStatus' - The response status code.
-- * 'tasks' - The list of tasks.
mkDescribeTasksResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTasksResponse
mkDescribeTasksResponse pResponseStatus_ =
  DescribeTasksResponse'
    { failures = Lude.Nothing,
      tasks = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsFailures :: Lens.Lens' DescribeTasksResponse (Lude.Maybe [Failure])
dtrsFailures = Lens.lens (failures :: DescribeTasksResponse -> Lude.Maybe [Failure]) (\s a -> s {failures = a} :: DescribeTasksResponse)
{-# DEPRECATED dtrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The list of tasks.
--
-- /Note:/ Consider using 'tasks' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsTasks :: Lens.Lens' DescribeTasksResponse (Lude.Maybe [Task])
dtrsTasks = Lens.lens (tasks :: DescribeTasksResponse -> Lude.Maybe [Task]) (\s a -> s {tasks = a} :: DescribeTasksResponse)
{-# DEPRECATED dtrsTasks "Use generic-lens or generic-optics with 'tasks' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtrsResponseStatus :: Lens.Lens' DescribeTasksResponse Lude.Int
dtrsResponseStatus = Lens.lens (responseStatus :: DescribeTasksResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTasksResponse)
{-# DEPRECATED dtrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
