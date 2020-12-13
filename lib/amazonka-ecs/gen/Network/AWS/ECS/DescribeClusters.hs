{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ECS.DescribeClusters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more of your clusters.
module Network.AWS.ECS.DescribeClusters
  ( -- * Creating a request
    DescribeClusters (..),
    mkDescribeClusters,

    -- ** Request lenses
    dcInclude,
    dcClusters,

    -- * Destructuring the response
    DescribeClustersResponse (..),
    mkDescribeClustersResponse,

    -- ** Response lenses
    dcrsFailures,
    dcrsClusters,
    dcrsResponseStatus,
  )
where

import Network.AWS.ECS.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDescribeClusters' smart constructor.
data DescribeClusters = DescribeClusters'
  { -- | Whether to include additional information about your clusters in the response. If this field is omitted, the attachments, statistics, and tags are not included.
    --
    -- If @ATTACHMENTS@ is specified, the attachments for the container instances or tasks within the cluster are included.
    -- If @SETTINGS@ is specified, the settings for the cluster are included.
    -- If @STATISTICS@ is specified, the following additional information, separated by launch type, is included:
    --
    --     * runningEC2TasksCount
    --
    --
    --     * runningFargateTasksCount
    --
    --
    --     * pendingEC2TasksCount
    --
    --
    --     * pendingFargateTasksCount
    --
    --
    --     * activeEC2ServiceCount
    --
    --
    --     * activeFargateServiceCount
    --
    --
    --     * drainingEC2ServiceCount
    --
    --
    --     * drainingFargateServiceCount
    --
    --
    -- If @TAGS@ is specified, the metadata tags associated with the cluster are included.
    include :: Lude.Maybe [ClusterField],
    -- | A list of up to 100 cluster names or full cluster Amazon Resource Name (ARN) entries. If you do not specify a cluster, the default cluster is assumed.
    clusters :: Lude.Maybe [Lude.Text]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClusters' with the minimum fields required to make a request.
--
-- * 'include' - Whether to include additional information about your clusters in the response. If this field is omitted, the attachments, statistics, and tags are not included.
--
-- If @ATTACHMENTS@ is specified, the attachments for the container instances or tasks within the cluster are included.
-- If @SETTINGS@ is specified, the settings for the cluster are included.
-- If @STATISTICS@ is specified, the following additional information, separated by launch type, is included:
--
--     * runningEC2TasksCount
--
--
--     * runningFargateTasksCount
--
--
--     * pendingEC2TasksCount
--
--
--     * pendingFargateTasksCount
--
--
--     * activeEC2ServiceCount
--
--
--     * activeFargateServiceCount
--
--
--     * drainingEC2ServiceCount
--
--
--     * drainingFargateServiceCount
--
--
-- If @TAGS@ is specified, the metadata tags associated with the cluster are included.
-- * 'clusters' - A list of up to 100 cluster names or full cluster Amazon Resource Name (ARN) entries. If you do not specify a cluster, the default cluster is assumed.
mkDescribeClusters ::
  DescribeClusters
mkDescribeClusters =
  DescribeClusters'
    { include = Lude.Nothing,
      clusters = Lude.Nothing
    }

-- | Whether to include additional information about your clusters in the response. If this field is omitted, the attachments, statistics, and tags are not included.
--
-- If @ATTACHMENTS@ is specified, the attachments for the container instances or tasks within the cluster are included.
-- If @SETTINGS@ is specified, the settings for the cluster are included.
-- If @STATISTICS@ is specified, the following additional information, separated by launch type, is included:
--
--     * runningEC2TasksCount
--
--
--     * runningFargateTasksCount
--
--
--     * pendingEC2TasksCount
--
--
--     * pendingFargateTasksCount
--
--
--     * activeEC2ServiceCount
--
--
--     * activeFargateServiceCount
--
--
--     * drainingEC2ServiceCount
--
--
--     * drainingFargateServiceCount
--
--
-- If @TAGS@ is specified, the metadata tags associated with the cluster are included.
--
-- /Note:/ Consider using 'include' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcInclude :: Lens.Lens' DescribeClusters (Lude.Maybe [ClusterField])
dcInclude = Lens.lens (include :: DescribeClusters -> Lude.Maybe [ClusterField]) (\s a -> s {include = a} :: DescribeClusters)
{-# DEPRECATED dcInclude "Use generic-lens or generic-optics with 'include' instead." #-}

-- | A list of up to 100 cluster names or full cluster Amazon Resource Name (ARN) entries. If you do not specify a cluster, the default cluster is assumed.
--
-- /Note:/ Consider using 'clusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcClusters :: Lens.Lens' DescribeClusters (Lude.Maybe [Lude.Text])
dcClusters = Lens.lens (clusters :: DescribeClusters -> Lude.Maybe [Lude.Text]) (\s a -> s {clusters = a} :: DescribeClusters)
{-# DEPRECATED dcClusters "Use generic-lens or generic-optics with 'clusters' instead." #-}

instance Lude.AWSRequest DescribeClusters where
  type Rs DescribeClusters = DescribeClustersResponse
  request = Req.postJSON ecsService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeClustersResponse'
            Lude.<$> (x Lude..?> "failures" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "clusters" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeClusters where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "AmazonEC2ContainerServiceV20141113.DescribeClusters" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeClusters where
  toJSON DescribeClusters' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("include" Lude..=) Lude.<$> include,
            ("clusters" Lude..=) Lude.<$> clusters
          ]
      )

instance Lude.ToPath DescribeClusters where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeClusters where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDescribeClustersResponse' smart constructor.
data DescribeClustersResponse = DescribeClustersResponse'
  { -- | Any failures associated with the call.
    failures :: Lude.Maybe [Failure],
    -- | The list of clusters.
    clusters :: Lude.Maybe [Cluster],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeClustersResponse' with the minimum fields required to make a request.
--
-- * 'failures' - Any failures associated with the call.
-- * 'clusters' - The list of clusters.
-- * 'responseStatus' - The response status code.
mkDescribeClustersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeClustersResponse
mkDescribeClustersResponse pResponseStatus_ =
  DescribeClustersResponse'
    { failures = Lude.Nothing,
      clusters = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Any failures associated with the call.
--
-- /Note:/ Consider using 'failures' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsFailures :: Lens.Lens' DescribeClustersResponse (Lude.Maybe [Failure])
dcrsFailures = Lens.lens (failures :: DescribeClustersResponse -> Lude.Maybe [Failure]) (\s a -> s {failures = a} :: DescribeClustersResponse)
{-# DEPRECATED dcrsFailures "Use generic-lens or generic-optics with 'failures' instead." #-}

-- | The list of clusters.
--
-- /Note:/ Consider using 'clusters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsClusters :: Lens.Lens' DescribeClustersResponse (Lude.Maybe [Cluster])
dcrsClusters = Lens.lens (clusters :: DescribeClustersResponse -> Lude.Maybe [Cluster]) (\s a -> s {clusters = a} :: DescribeClustersResponse)
{-# DEPRECATED dcrsClusters "Use generic-lens or generic-optics with 'clusters' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcrsResponseStatus :: Lens.Lens' DescribeClustersResponse Lude.Int
dcrsResponseStatus = Lens.lens (responseStatus :: DescribeClustersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeClustersResponse)
{-# DEPRECATED dcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
