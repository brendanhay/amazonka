{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.AddInstanceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Adds one or more instance groups to a running cluster.
module Network.AWS.EMR.AddInstanceGroups
  ( -- * Creating a request
    AddInstanceGroups (..),
    mkAddInstanceGroups,

    -- ** Request lenses
    aigJobFlowId,
    aigInstanceGroups,

    -- * Destructuring the response
    AddInstanceGroupsResponse (..),
    mkAddInstanceGroupsResponse,

    -- ** Response lenses
    aigrsClusterARN,
    aigrsJobFlowId,
    aigrsInstanceGroupIds,
    aigrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Input to an AddInstanceGroups call.
--
-- /See:/ 'mkAddInstanceGroups' smart constructor.
data AddInstanceGroups = AddInstanceGroups'
  { -- | Job flow in which to add the instance groups.
    jobFlowId :: Lude.Text,
    -- | Instance groups to add.
    instanceGroups :: [InstanceGroupConfig]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddInstanceGroups' with the minimum fields required to make a request.
--
-- * 'jobFlowId' - Job flow in which to add the instance groups.
-- * 'instanceGroups' - Instance groups to add.
mkAddInstanceGroups ::
  -- | 'jobFlowId'
  Lude.Text ->
  AddInstanceGroups
mkAddInstanceGroups pJobFlowId_ =
  AddInstanceGroups'
    { jobFlowId = pJobFlowId_,
      instanceGroups = Lude.mempty
    }

-- | Job flow in which to add the instance groups.
--
-- /Note:/ Consider using 'jobFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigJobFlowId :: Lens.Lens' AddInstanceGroups Lude.Text
aigJobFlowId = Lens.lens (jobFlowId :: AddInstanceGroups -> Lude.Text) (\s a -> s {jobFlowId = a} :: AddInstanceGroups)
{-# DEPRECATED aigJobFlowId "Use generic-lens or generic-optics with 'jobFlowId' instead." #-}

-- | Instance groups to add.
--
-- /Note:/ Consider using 'instanceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigInstanceGroups :: Lens.Lens' AddInstanceGroups [InstanceGroupConfig]
aigInstanceGroups = Lens.lens (instanceGroups :: AddInstanceGroups -> [InstanceGroupConfig]) (\s a -> s {instanceGroups = a} :: AddInstanceGroups)
{-# DEPRECATED aigInstanceGroups "Use generic-lens or generic-optics with 'instanceGroups' instead." #-}

instance Lude.AWSRequest AddInstanceGroups where
  type Rs AddInstanceGroups = AddInstanceGroupsResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          AddInstanceGroupsResponse'
            Lude.<$> (x Lude..?> "ClusterArn")
            Lude.<*> (x Lude..?> "JobFlowId")
            Lude.<*> (x Lude..?> "InstanceGroupIds" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders AddInstanceGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.AddInstanceGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON AddInstanceGroups where
  toJSON AddInstanceGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("JobFlowId" Lude..= jobFlowId),
            Lude.Just ("InstanceGroups" Lude..= instanceGroups)
          ]
      )

instance Lude.ToPath AddInstanceGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery AddInstanceGroups where
  toQuery = Lude.const Lude.mempty

-- | Output from an AddInstanceGroups call.
--
-- /See:/ 'mkAddInstanceGroupsResponse' smart constructor.
data AddInstanceGroupsResponse = AddInstanceGroupsResponse'
  { -- | The Amazon Resource Name of the cluster.
    clusterARN :: Lude.Maybe Lude.Text,
    -- | The job flow ID in which the instance groups are added.
    jobFlowId :: Lude.Maybe Lude.Text,
    -- | Instance group IDs of the newly created instance groups.
    instanceGroupIds :: Lude.Maybe [Lude.Text],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AddInstanceGroupsResponse' with the minimum fields required to make a request.
--
-- * 'clusterARN' - The Amazon Resource Name of the cluster.
-- * 'jobFlowId' - The job flow ID in which the instance groups are added.
-- * 'instanceGroupIds' - Instance group IDs of the newly created instance groups.
-- * 'responseStatus' - The response status code.
mkAddInstanceGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  AddInstanceGroupsResponse
mkAddInstanceGroupsResponse pResponseStatus_ =
  AddInstanceGroupsResponse'
    { clusterARN = Lude.Nothing,
      jobFlowId = Lude.Nothing,
      instanceGroupIds = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Amazon Resource Name of the cluster.
--
-- /Note:/ Consider using 'clusterARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigrsClusterARN :: Lens.Lens' AddInstanceGroupsResponse (Lude.Maybe Lude.Text)
aigrsClusterARN = Lens.lens (clusterARN :: AddInstanceGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {clusterARN = a} :: AddInstanceGroupsResponse)
{-# DEPRECATED aigrsClusterARN "Use generic-lens or generic-optics with 'clusterARN' instead." #-}

-- | The job flow ID in which the instance groups are added.
--
-- /Note:/ Consider using 'jobFlowId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigrsJobFlowId :: Lens.Lens' AddInstanceGroupsResponse (Lude.Maybe Lude.Text)
aigrsJobFlowId = Lens.lens (jobFlowId :: AddInstanceGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {jobFlowId = a} :: AddInstanceGroupsResponse)
{-# DEPRECATED aigrsJobFlowId "Use generic-lens or generic-optics with 'jobFlowId' instead." #-}

-- | Instance group IDs of the newly created instance groups.
--
-- /Note:/ Consider using 'instanceGroupIds' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigrsInstanceGroupIds :: Lens.Lens' AddInstanceGroupsResponse (Lude.Maybe [Lude.Text])
aigrsInstanceGroupIds = Lens.lens (instanceGroupIds :: AddInstanceGroupsResponse -> Lude.Maybe [Lude.Text]) (\s a -> s {instanceGroupIds = a} :: AddInstanceGroupsResponse)
{-# DEPRECATED aigrsInstanceGroupIds "Use generic-lens or generic-optics with 'instanceGroupIds' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
aigrsResponseStatus :: Lens.Lens' AddInstanceGroupsResponse Lude.Int
aigrsResponseStatus = Lens.lens (responseStatus :: AddInstanceGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: AddInstanceGroupsResponse)
{-# DEPRECATED aigrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
