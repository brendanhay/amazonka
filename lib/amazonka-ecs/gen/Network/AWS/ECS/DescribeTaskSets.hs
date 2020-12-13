{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeTaskSets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the task sets in the specified cluster and service. This is used when a service uses the @EXTERNAL@ deployment controller type. For more information, see <https://docs.aws.amazon.com/AmazonECS/latest/developerguide/deployment-types.html Amazon ECS Deployment Types> in the /Amazon Elastic Container Service Developer Guide/ .
module Network.AWS.ECS.DescribeTaskSets
  ( -- * Creating a request
    DescribeTaskSets (..),
    mkDescribeTaskSets,

    -- ** Request lenses
    dtssTaskSets,
    dtssInclude,
    dtssCluster,
    dtssService,

    -- * Destructuring the response
    DescribeTaskSetsResponse (..),
    mkDescribeTaskSetsResponse,

    -- ** Response lenses
    dtssrsTaskSets,
    dtssrsFailures,
    dtssrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeTaskSets' smart constructor.
data DescribeTaskSets = DescribeTaskSets'
  { -- | The ID or full Amazon Resource Name (ARN) of task sets to describe.
    taskSets :: Lude.Maybe [Lude.Text],
    -- | Specifies whether to see the resource tags for the task set. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
    include :: Lude.Maybe [TaskSetField],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task sets exist in.
    cluster :: Lude.Text,
    -- | The short name or full Amazon Resource Name (ARN) of the service that the task sets exist in.
    service :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTaskSets' with the minimum fields required to make a request.
--
-- * 'taskSets' - The ID or full Amazon Resource Name (ARN) of task sets to describe.
-- * 'include' - Specifies whether to see the resource tags for the task set. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task sets exist in.
-- * 'service' - The short name or full Amazon Resource Name (ARN) of the service that the task sets exist in.
mkDescribeTaskSets ::
  -- | 'cluster'
  Lude.Text ->
  -- | 'service'
  Lude.Text ->
  DescribeTaskSets
mkDescribeTaskSets pCluster_ pService_ =
  DescribeTaskSets'
    { taskSets = Lude.Nothing,
      include = Lude.Nothing,
      cluster = pCluster_,
      service = pService_
    }

-- | The ID or full Amazon Resource Name (ARN) of task sets to describe.
--
-- /Note:/ Consider using 'taskSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssTaskSets :: Lens.Lens' DescribeTaskSets (Lude.Maybe [Lude.Text])
dtssTaskSets = Lens.lens (taskSets :: DescribeTaskSets -> Lude.Maybe [Lude.Text]) (\s a -> s {taskSets = a} :: DescribeTaskSets)
{-# DEPRECATED dtssTaskSets "Use generic-lens or generic-optics with 'taskSets' instead." #-}

-- | Specifies whether to see the resource tags for the task set. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssInclude :: Lens.Lens' DescribeTaskSets (Lude.Maybe [TaskSetField])
dtssInclude = Lens.lens (include :: DescribeTaskSets -> Lude.Maybe [TaskSetField]) (\s a -> s {include = a} :: DescribeTaskSets)
{-# DEPRECATED dtssInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the service that the task sets exist in.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssCluster :: Lens.Lens' DescribeTaskSets Lude.Text
dtssCluster = Lens.lens (cluster :: DescribeTaskSets -> Lude.Text) (\s a -> s {cluster = a} :: DescribeTaskSets)
{-# DEPRECATED dtssCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the service that the task sets exist in.
--
-- /Note:/ Consider using 'service' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssService :: Lens.Lens' DescribeTaskSets Lude.Text
dtssService = Lens.lens (service :: DescribeTaskSets -> Lude.Text) (\s a -> s {service = a} :: DescribeTaskSets)
{-# DEPRECATED dtssService "Use generic-lens or generic-optics with 'service' instead." #-}

instance Lude.AWSRequest DescribeTaskSets where
  type Rs DescribeTaskSets = DescribeTaskSetsResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeTaskSetsResponse'
            Lude.<$> (x Lude..?> "taskSets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeTaskSets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DescribeTaskSets" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeTaskSets where
  toJSON DescribeTaskSets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("taskSets" Lude..=) Lude.<$> taskSets,
            ("include" Lude..=) Lude.<$> include,
            Lude.Just ("cluster" Lude..= cluster),
            Lude.Just ("service" Lude..= service)
          ]
      )

instance Lude.ToPath DescribeTaskSets where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeTaskSets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeTaskSetsResponse' smart constructor.
data DescribeTaskSetsResponse = DescribeTaskSetsResponse'
  { -- | The list of task sets described.
    taskSets :: Lude.Maybe [TaskSet],
    -- | Any failures associated with the call.
    failures :: Lude.Maybe [Failure],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeTaskSetsResponse' with the minimum fields required to make a request.
--
-- * 'taskSets' - The list of task sets described.
-- * 'failures' - Any failures associated with the call.
-- * 'responseStatus' - The response status code.
mkDescribeTaskSetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeTaskSetsResponse
mkDescribeTaskSetsResponse pResponseStatus_ =
  DescribeTaskSetsResponse'
    { taskSets = Lude.Nothing,
      failures = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of task sets described.
--
-- /Note:/ Consider using 'taskSets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssrsTaskSets :: Lens.Lens' DescribeTaskSetsResponse (Lude.Maybe [TaskSet])
dtssrsTaskSets = Lens.lens (taskSets :: DescribeTaskSetsResponse -> Lude.Maybe [TaskSet]) (\s a -> s {taskSets = a} :: DescribeTaskSetsResponse)
{-# DEPRECATED dtssrsTaskSets "Use generic-lens or generic-optics with 'taskSets' instead." #-}

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssrsFailures :: Lens.Lens' DescribeTaskSetsResponse (Lude.Maybe [Failure])
dtssrsFailures = Lens.lens (failures :: DescribeTaskSetsResponse -> Lude.Maybe [Failure]) (\s a -> s {failures = a} :: DescribeTaskSetsResponse)
{-# DEPRECATED dtssrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtssrsResponseStatus :: Lens.Lens' DescribeTaskSetsResponse Lude.Int
dtssrsResponseStatus = Lens.lens (responseStatus :: DescribeTaskSetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeTaskSetsResponse)
{-# DEPRECATED dtssrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
