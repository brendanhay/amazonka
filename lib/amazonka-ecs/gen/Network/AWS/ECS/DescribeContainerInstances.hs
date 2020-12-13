{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeContainerInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes Amazon Elastic Container Service container instances. Returns metadata about registered and remaining resources on each container instance requested.
module Network.AWS.ECS.DescribeContainerInstances
  ( -- * Creating a request
    DescribeContainerInstances (..),
    mkDescribeContainerInstances,

    -- ** Request lenses
    dcisInclude,
    dcisCluster,
    dcisContainerInstances,

    -- * Destructuring the response
    DescribeContainerInstancesResponse (..),
    mkDescribeContainerInstancesResponse,

    -- ** Response lenses
    dcisrsFailures,
    dcisrsContainerInstances,
    dcisrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeContainerInstances' smart constructor.
data DescribeContainerInstances = DescribeContainerInstances'
  { -- | Specifies whether you want to see the resource tags for the container instance. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
    include :: Lude.Maybe [ContainerInstanceField],
    -- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the container instance or container instances you are describing were launched in any cluster other than the default cluster.
    cluster :: Lude.Maybe Lude.Text,
    -- | A list of up to 100 container instance IDs or full Amazon Resource Name (ARN) entries.
    containerInstances :: [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeContainerInstances' with the minimum fields required to make a request.
--
-- * 'include' - Specifies whether you want to see the resource tags for the container instance. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
-- * 'cluster' - The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the container instance or container instances you are describing were launched in any cluster other than the default cluster.
-- * 'containerInstances' - A list of up to 100 container instance IDs or full Amazon Resource Name (ARN) entries.
mkDescribeContainerInstances ::
  DescribeContainerInstances
mkDescribeContainerInstances =
  DescribeContainerInstances'
    { include = Lude.Nothing,
      cluster = Lude.Nothing,
      containerInstances = Lude.mempty
    }

-- | Specifies whether you want to see the resource tags for the container instance. If @TAGS@ is specified, the tags are included in the response. If this field is omitted, tags are not included in the response.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcisInclude :: Lens.Lens' DescribeContainerInstances (Lude.Maybe [ContainerInstanceField])
dcisInclude = Lens.lens (include :: DescribeContainerInstances -> Lude.Maybe [ContainerInstanceField]) (\s a -> s {include = a} :: DescribeContainerInstances)
{-# DEPRECATED dcisInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | The short name or full Amazon Resource Name (ARN) of the cluster that hosts the container instances to describe. If you do not specify a cluster, the default cluster is assumed. This parameter is required if the container instance or container instances you are describing were launched in any cluster other than the default cluster.
--
-- /Note:/ Consider using 'cluster' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcisCluster :: Lens.Lens' DescribeContainerInstances (Lude.Maybe Lude.Text)
dcisCluster = Lens.lens (cluster :: DescribeContainerInstances -> Lude.Maybe Lude.Text) (\s a -> s {cluster = a} :: DescribeContainerInstances)
{-# DEPRECATED dcisCluster "Use generic-lens or generic-optics with 'cluster' instead." #-}

-- | A list of up to 100 container instance IDs or full Amazon Resource Name (ARN) entries.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcisContainerInstances :: Lens.Lens' DescribeContainerInstances [Lude.Text]
dcisContainerInstances = Lens.lens (containerInstances :: DescribeContainerInstances -> [Lude.Text]) (\s a -> s {containerInstances = a} :: DescribeContainerInstances)
{-# DEPRECATED dcisContainerInstances "Use generic-lens or generic-optics with 'containerInstances' instead." #-}

instance Lude.AWSRequest DescribeContainerInstances where
  type
    Rs DescribeContainerInstances =
      DescribeContainerInstancesResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeContainerInstancesResponse'
            Lude.<$> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "containerInstances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeContainerInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DescribeContainerInstances" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeContainerInstances where
  toJSON DescribeContainerInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("include" Lude..=) Lude.<$> include,
            ("cluster" Lude..=) Lude.<$> cluster,
            Lude.Just ("containerInstances" Lude..= containerInstances)
          ]
      )

instance Lude.ToPath DescribeContainerInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeContainerInstances where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeContainerInstancesResponse' smart constructor.
data DescribeContainerInstancesResponse = DescribeContainerInstancesResponse'
  { -- | Any failures associated with the call.
    failures :: Lude.Maybe [Failure],
    -- | The list of container instances.
    containerInstances :: Lude.Maybe [ContainerInstance],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeContainerInstancesResponse' with the minimum fields required to make a request.
--
-- * 'failures' - Any failures associated with the call.
-- * 'containerInstances' - The list of container instances.
-- * 'responseStatus' - The response status code.
mkDescribeContainerInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeContainerInstancesResponse
mkDescribeContainerInstancesResponse pResponseStatus_ =
  DescribeContainerInstancesResponse'
    { failures = Lude.Nothing,
      containerInstances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcisrsFailures :: Lens.Lens' DescribeContainerInstancesResponse (Lude.Maybe [Failure])
dcisrsFailures = Lens.lens (failures :: DescribeContainerInstancesResponse -> Lude.Maybe [Failure]) (\s a -> s {failures = a} :: DescribeContainerInstancesResponse)
{-# DEPRECATED dcisrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The list of container instances.
--
-- /Note:/ Consider using 'containerInstances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcisrsContainerInstances :: Lens.Lens' DescribeContainerInstancesResponse (Lude.Maybe [ContainerInstance])
dcisrsContainerInstances = Lens.lens (containerInstances :: DescribeContainerInstancesResponse -> Lude.Maybe [ContainerInstance]) (\s a -> s {containerInstances = a} :: DescribeContainerInstancesResponse)
{-# DEPRECATED dcisrsContainerInstances "Use generic-lens or generic-optics with 'containerInstances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcisrsResponseStatus :: Lens.Lens' DescribeContainerInstancesResponse Lude.Int
dcisrsResponseStatus = Lens.lens (responseStatus :: DescribeContainerInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeContainerInstancesResponse)
{-# DEPRECATED dcisrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
