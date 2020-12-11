{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListInstanceFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available details about the instance fleets in a cluster.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstanceFleets
  ( -- * Creating a request
    ListInstanceFleets (..),
    mkListInstanceFleets,

    -- ** Request lenses
    lifMarker,
    lifClusterId,

    -- * Destructuring the response
    ListInstanceFleetsResponse (..),
    mkListInstanceFleetsResponse,

    -- ** Response lenses
    lifrsInstanceFleets,
    lifrsMarker,
    lifrsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListInstanceFleets' smart constructor.
data ListInstanceFleets = ListInstanceFleets'
  { marker ::
      Lude.Maybe Lude.Text,
    clusterId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListInstanceFleets' with the minimum fields required to make a request.
--
-- * 'clusterId' - The unique identifier of the cluster.
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
mkListInstanceFleets ::
  -- | 'clusterId'
  Lude.Text ->
  ListInstanceFleets
mkListInstanceFleets pClusterId_ =
  ListInstanceFleets'
    { marker = Lude.Nothing,
      clusterId = pClusterId_
    }

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifMarker :: Lens.Lens' ListInstanceFleets (Lude.Maybe Lude.Text)
lifMarker = Lens.lens (marker :: ListInstanceFleets -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInstanceFleets)
{-# DEPRECATED lifMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The unique identifier of the cluster.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifClusterId :: Lens.Lens' ListInstanceFleets Lude.Text
lifClusterId = Lens.lens (clusterId :: ListInstanceFleets -> Lude.Text) (\s a -> s {clusterId = a} :: ListInstanceFleets)
{-# DEPRECATED lifClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

instance Page.AWSPager ListInstanceFleets where
  page rq rs
    | Page.stop (rs Lens.^. lifrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lifrsInstanceFleets) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lifMarker Lens..~ rs Lens.^. lifrsMarker

instance Lude.AWSRequest ListInstanceFleets where
  type Rs ListInstanceFleets = ListInstanceFleetsResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListInstanceFleetsResponse'
            Lude.<$> (x Lude..?> "InstanceFleets" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListInstanceFleets where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ListInstanceFleets" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListInstanceFleets where
  toJSON ListInstanceFleets' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            Lude.Just ("ClusterId" Lude..= clusterId)
          ]
      )

instance Lude.ToPath ListInstanceFleets where
  toPath = Lude.const "/"

instance Lude.ToQuery ListInstanceFleets where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkListInstanceFleetsResponse' smart constructor.
data ListInstanceFleetsResponse = ListInstanceFleetsResponse'
  { instanceFleets ::
      Lude.Maybe [InstanceFleet],
    marker :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'ListInstanceFleetsResponse' with the minimum fields required to make a request.
--
-- * 'instanceFleets' - The list of instance fleets for the cluster and given filters.
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
-- * 'responseStatus' - The response status code.
mkListInstanceFleetsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListInstanceFleetsResponse
mkListInstanceFleetsResponse pResponseStatus_ =
  ListInstanceFleetsResponse'
    { instanceFleets = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The list of instance fleets for the cluster and given filters.
--
-- /Note:/ Consider using 'instanceFleets' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifrsInstanceFleets :: Lens.Lens' ListInstanceFleetsResponse (Lude.Maybe [InstanceFleet])
lifrsInstanceFleets = Lens.lens (instanceFleets :: ListInstanceFleetsResponse -> Lude.Maybe [InstanceFleet]) (\s a -> s {instanceFleets = a} :: ListInstanceFleetsResponse)
{-# DEPRECATED lifrsInstanceFleets "Use generic-lens or generic-optics with 'instanceFleets' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifrsMarker :: Lens.Lens' ListInstanceFleetsResponse (Lude.Maybe Lude.Text)
lifrsMarker = Lens.lens (marker :: ListInstanceFleetsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListInstanceFleetsResponse)
{-# DEPRECATED lifrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lifrsResponseStatus :: Lens.Lens' ListInstanceFleetsResponse Lude.Int
lifrsResponseStatus = Lens.lens (responseStatus :: ListInstanceFleetsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListInstanceFleetsResponse)
{-# DEPRECATED lifrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
