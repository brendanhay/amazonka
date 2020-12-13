{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListInstanceGroups
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides all available details about the instance groups in a cluster.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstanceGroups
  ( -- * Creating a request
    ListInstanceGroups (..),
    mkListInstanceGroups,

    -- ** Request lenses
    ligClusterId,
    ligMarker,

    -- * Destructuring the response
    ListInstanceGroupsResponse (..),
    mkListInstanceGroupsResponse,

    -- ** Response lenses
    ligrsMarker,
    ligrsInstanceGroups,
    ligrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'mkListInstanceGroups' smart constructor.
data ListInstanceGroups = ListInstanceGroups'
  { -- | The identifier of the cluster for which to list the instance groups.
    clusterId :: Lude.Text,
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceGroups' with the minimum fields required to make a request.
--
-- * 'clusterId' - The identifier of the cluster for which to list the instance groups.
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
mkListInstanceGroups ::
  -- | 'clusterId'
  Lude.Text ->
  ListInstanceGroups
mkListInstanceGroups pClusterId_ =
  ListInstanceGroups'
    { clusterId = pClusterId_,
      marker = Lude.Nothing
    }

-- | The identifier of the cluster for which to list the instance groups.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ligClusterId :: Lens.Lens' ListInstanceGroups Lude.Text
ligClusterId = Lens.lens (clusterId :: ListInstanceGroups -> Lude.Text) (\s a -> s {clusterId = a} :: ListInstanceGroups)
{-# DEPRECATED ligClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ligMarker :: Lens.Lens' ListInstanceGroups (Lude.Maybe Lude.Text)
ligMarker = Lens.lens (marker :: ListInstanceGroups -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInstanceGroups)
{-# DEPRECATED ligMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Page.AWSPager ListInstanceGroups where
  page rq rs
    | Page.stop (rs Lens.^. ligrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. ligrsInstanceGroups) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& ligMarker Lens..~ rs Lens.^. ligrsMarker

instance Lude.AWSRequest ListInstanceGroups where
  type Rs ListInstanceGroups = ListInstanceGroupsResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInstanceGroupsResponse'
            Lude.<$> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "InstanceGroups" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInstanceGroups where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ListInstanceGroups" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListInstanceGroups where
  toJSON ListInstanceGroups' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClusterId" Lude..= clusterId),
            ("Marker" Lude..=) Lude.<$> marker
          ]
      )

instance Lude.ToPath ListInstanceGroups where
  toPath = Lude.const "/"

instance Lude.ToQuery ListInstanceGroups where
  toQuery = Lude.const Lude.mempty

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'mkListInstanceGroupsResponse' smart constructor.
data ListInstanceGroupsResponse = ListInstanceGroupsResponse'
  { -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Lude.Maybe Lude.Text,
    -- | The list of instance groups for the cluster and given filters.
    instanceGroups :: Lude.Maybe [InstanceGroup],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceGroupsResponse' with the minimum fields required to make a request.
--
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
-- * 'instanceGroups' - The list of instance groups for the cluster and given filters.
-- * 'responseStatus' - The response status code.
mkListInstanceGroupsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInstanceGroupsResponse
mkListInstanceGroupsResponse pResponseStatus_ =
  ListInstanceGroupsResponse'
    { marker = Lude.Nothing,
      instanceGroups = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ligrsMarker :: Lens.Lens' ListInstanceGroupsResponse (Lude.Maybe Lude.Text)
ligrsMarker = Lens.lens (marker :: ListInstanceGroupsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInstanceGroupsResponse)
{-# DEPRECATED ligrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The list of instance groups for the cluster and given filters.
--
-- /Note:/ Consider using 'instanceGroups' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ligrsInstanceGroups :: Lens.Lens' ListInstanceGroupsResponse (Lude.Maybe [InstanceGroup])
ligrsInstanceGroups = Lens.lens (instanceGroups :: ListInstanceGroupsResponse -> Lude.Maybe [InstanceGroup]) (\s a -> s {instanceGroups = a} :: ListInstanceGroupsResponse)
{-# DEPRECATED ligrsInstanceGroups "Use generic-lens or generic-optics with 'instanceGroups' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ligrsResponseStatus :: Lens.Lens' ListInstanceGroupsResponse Lude.Int
ligrsResponseStatus = Lens.lens (responseStatus :: ListInstanceGroupsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInstanceGroupsResponse)
{-# DEPRECATED ligrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
