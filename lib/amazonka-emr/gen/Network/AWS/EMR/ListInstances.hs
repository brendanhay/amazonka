{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListInstances
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information for all active EC2 instances and EC2 instances terminated in the last 30 days, up to a maximum of 2,000. EC2 instances in any of the following states are considered active: AWAITING_FULFILLMENT, PROVISIONING, BOOTSTRAPPING, RUNNING.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstances
  ( -- * Creating a request
    ListInstances (..),
    mkListInstances,

    -- ** Request lenses
    liInstanceGroupTypes,
    liInstanceFleetType,
    liClusterId,
    liMarker,
    liInstanceFleetId,
    liInstanceStates,
    liInstanceGroupId,

    -- * Destructuring the response
    ListInstancesResponse (..),
    mkListInstancesResponse,

    -- ** Response lenses
    lirsMarker,
    lirsInstances,
    lirsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | This input determines which instances to list.
--
-- /See:/ 'mkListInstances' smart constructor.
data ListInstances = ListInstances'
  { -- | The type of instance group for which to list the instances.
    instanceGroupTypes :: Lude.Maybe [InstanceGroupType],
    -- | The node type of the instance fleet. For example MASTER, CORE, or TASK.
    instanceFleetType :: Lude.Maybe InstanceFleetType,
    -- | The identifier of the cluster for which to list the instances.
    clusterId :: Lude.Text,
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Lude.Maybe Lude.Text,
    -- | The unique identifier of the instance fleet.
    instanceFleetId :: Lude.Maybe Lude.Text,
    -- | A list of instance states that will filter the instances returned with this request.
    instanceStates :: Lude.Maybe [InstanceState],
    -- | The identifier of the instance group for which to list the instances.
    instanceGroupId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstances' with the minimum fields required to make a request.
--
-- * 'instanceGroupTypes' - The type of instance group for which to list the instances.
-- * 'instanceFleetType' - The node type of the instance fleet. For example MASTER, CORE, or TASK.
-- * 'clusterId' - The identifier of the cluster for which to list the instances.
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
-- * 'instanceFleetId' - The unique identifier of the instance fleet.
-- * 'instanceStates' - A list of instance states that will filter the instances returned with this request.
-- * 'instanceGroupId' - The identifier of the instance group for which to list the instances.
mkListInstances ::
  -- | 'clusterId'
  Lude.Text ->
  ListInstances
mkListInstances pClusterId_ =
  ListInstances'
    { instanceGroupTypes = Lude.Nothing,
      instanceFleetType = Lude.Nothing,
      clusterId = pClusterId_,
      marker = Lude.Nothing,
      instanceFleetId = Lude.Nothing,
      instanceStates = Lude.Nothing,
      instanceGroupId = Lude.Nothing
    }

-- | The type of instance group for which to list the instances.
--
-- /Note:/ Consider using 'instanceGroupTypes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liInstanceGroupTypes :: Lens.Lens' ListInstances (Lude.Maybe [InstanceGroupType])
liInstanceGroupTypes = Lens.lens (instanceGroupTypes :: ListInstances -> Lude.Maybe [InstanceGroupType]) (\s a -> s {instanceGroupTypes = a} :: ListInstances)
{-# DEPRECATED liInstanceGroupTypes "Use generic-lens or generic-optics with 'instanceGroupTypes' instead." #-}

-- | The node type of the instance fleet. For example MASTER, CORE, or TASK.
--
-- /Note:/ Consider using 'instanceFleetType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liInstanceFleetType :: Lens.Lens' ListInstances (Lude.Maybe InstanceFleetType)
liInstanceFleetType = Lens.lens (instanceFleetType :: ListInstances -> Lude.Maybe InstanceFleetType) (\s a -> s {instanceFleetType = a} :: ListInstances)
{-# DEPRECATED liInstanceFleetType "Use generic-lens or generic-optics with 'instanceFleetType' instead." #-}

-- | The identifier of the cluster for which to list the instances.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liClusterId :: Lens.Lens' ListInstances Lude.Text
liClusterId = Lens.lens (clusterId :: ListInstances -> Lude.Text) (\s a -> s {clusterId = a} :: ListInstances)
{-# DEPRECATED liClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liMarker :: Lens.Lens' ListInstances (Lude.Maybe Lude.Text)
liMarker = Lens.lens (marker :: ListInstances -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInstances)
{-# DEPRECATED liMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The unique identifier of the instance fleet.
--
-- /Note:/ Consider using 'instanceFleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liInstanceFleetId :: Lens.Lens' ListInstances (Lude.Maybe Lude.Text)
liInstanceFleetId = Lens.lens (instanceFleetId :: ListInstances -> Lude.Maybe Lude.Text) (\s a -> s {instanceFleetId = a} :: ListInstances)
{-# DEPRECATED liInstanceFleetId "Use generic-lens or generic-optics with 'instanceFleetId' instead." #-}

-- | A list of instance states that will filter the instances returned with this request.
--
-- /Note:/ Consider using 'instanceStates' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liInstanceStates :: Lens.Lens' ListInstances (Lude.Maybe [InstanceState])
liInstanceStates = Lens.lens (instanceStates :: ListInstances -> Lude.Maybe [InstanceState]) (\s a -> s {instanceStates = a} :: ListInstances)
{-# DEPRECATED liInstanceStates "Use generic-lens or generic-optics with 'instanceStates' instead." #-}

-- | The identifier of the instance group for which to list the instances.
--
-- /Note:/ Consider using 'instanceGroupId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
liInstanceGroupId :: Lens.Lens' ListInstances (Lude.Maybe Lude.Text)
liInstanceGroupId = Lens.lens (instanceGroupId :: ListInstances -> Lude.Maybe Lude.Text) (\s a -> s {instanceGroupId = a} :: ListInstances)
{-# DEPRECATED liInstanceGroupId "Use generic-lens or generic-optics with 'instanceGroupId' instead." #-}

instance Page.AWSPager ListInstances where
  page rq rs
    | Page.stop (rs Lens.^. lirsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lirsInstances) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& liMarker Lens..~ rs Lens.^. lirsMarker

instance Lude.AWSRequest ListInstances where
  type Rs ListInstances = ListInstancesResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInstancesResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "Instances" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInstances where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ListInstances" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListInstances where
  toJSON ListInstances' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("InstanceGroupTypes" Lude..=) Lude.<$> instanceGroupTypes,
            ("InstanceFleetType" Lude..=) Lude.<$> instanceFleetType,
            Lude.Just ("ClusterId" Lude..= clusterId),
            ("Marker" Lude..=) Lude.<$> marker,
            ("InstanceFleetId" Lude..=) Lude.<$> instanceFleetId,
            ("InstanceStates" Lude..=) Lude.<$> instanceStates,
            ("InstanceGroupId" Lude..=) Lude.<$> instanceGroupId
          ]
      )

instance Lude.ToPath ListInstances where
  toPath = Lude.const "/"

instance Lude.ToQuery ListInstances where
  toQuery = Lude.const Lude.mempty

-- | This output contains the list of instances.
--
-- /See:/ 'mkListInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Lude.Maybe Lude.Text,
    -- | The list of instances for the cluster and given filters.
    instances :: Lude.Maybe [Instance],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstancesResponse' with the minimum fields required to make a request.
--
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
-- * 'instances' - The list of instances for the cluster and given filters.
-- * 'responseStatus' - The response status code.
mkListInstancesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInstancesResponse
mkListInstancesResponse pResponseStatus_ =
  ListInstancesResponse'
    { marker = Lude.Nothing,
      instances = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsMarker :: Lens.Lens' ListInstancesResponse (Lude.Maybe Lude.Text)
lirsMarker = Lens.lens (marker :: ListInstancesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInstancesResponse)
{-# DEPRECATED lirsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The list of instances for the cluster and given filters.
--
-- /Note:/ Consider using 'instances' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsInstances :: Lens.Lens' ListInstancesResponse (Lude.Maybe [Instance])
lirsInstances = Lens.lens (instances :: ListInstancesResponse -> Lude.Maybe [Instance]) (\s a -> s {instances = a} :: ListInstancesResponse)
{-# DEPRECATED lirsInstances "Use generic-lens or generic-optics with 'instances' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lirsResponseStatus :: Lens.Lens' ListInstancesResponse Lude.Int
lirsResponseStatus = Lens.lens (responseStatus :: ListInstancesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInstancesResponse)
{-# DEPRECATED lirsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
