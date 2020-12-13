{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListBootstrapActions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the bootstrap actions associated with a cluster.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListBootstrapActions
  ( -- * Creating a request
    ListBootstrapActions (..),
    mkListBootstrapActions,

    -- ** Request lenses
    lbaClusterId,
    lbaMarker,

    -- * Destructuring the response
    ListBootstrapActionsResponse (..),
    mkListBootstrapActionsResponse,

    -- ** Response lenses
    lbarsBootstrapActions,
    lbarsMarker,
    lbarsResponseStatus,
  )
where

import Network.AWS.EMR.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | This input determines which bootstrap actions to retrieve.
--
-- /See:/ 'mkListBootstrapActions' smart constructor.
data ListBootstrapActions = ListBootstrapActions'
  { -- | The cluster identifier for the bootstrap actions to list.
    clusterId :: Lude.Text,
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBootstrapActions' with the minimum fields required to make a request.
--
-- * 'clusterId' - The cluster identifier for the bootstrap actions to list.
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
mkListBootstrapActions ::
  -- | 'clusterId'
  Lude.Text ->
  ListBootstrapActions
mkListBootstrapActions pClusterId_ =
  ListBootstrapActions'
    { clusterId = pClusterId_,
      marker = Lude.Nothing
    }

-- | The cluster identifier for the bootstrap actions to list.
--
-- /Note:/ Consider using 'clusterId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaClusterId :: Lens.Lens' ListBootstrapActions Lude.Text
lbaClusterId = Lens.lens (clusterId :: ListBootstrapActions -> Lude.Text) (\s a -> s {clusterId = a} :: ListBootstrapActions)
{-# DEPRECATED lbaClusterId "Use generic-lens or generic-optics with 'clusterId' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbaMarker :: Lens.Lens' ListBootstrapActions (Lude.Maybe Lude.Text)
lbaMarker = Lens.lens (marker :: ListBootstrapActions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListBootstrapActions)
{-# DEPRECATED lbaMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Page.AWSPager ListBootstrapActions where
  page rq rs
    | Page.stop (rs Lens.^. lbarsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lbarsBootstrapActions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lbaMarker Lens..~ rs Lens.^. lbarsMarker

instance Lude.AWSRequest ListBootstrapActions where
  type Rs ListBootstrapActions = ListBootstrapActionsResponse
  request = Req.postJSON emrService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListBootstrapActionsResponse'
            Lude.<$> (x Lude..?> "BootstrapActions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListBootstrapActions where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("ElasticMapReduce.ListBootstrapActions" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListBootstrapActions where
  toJSON ListBootstrapActions' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("ClusterId" Lude..= clusterId),
            ("Marker" Lude..=) Lude.<$> marker
          ]
      )

instance Lude.ToPath ListBootstrapActions where
  toPath = Lude.const "/"

instance Lude.ToQuery ListBootstrapActions where
  toQuery = Lude.const Lude.mempty

-- | This output contains the bootstrap actions detail.
--
-- /See:/ 'mkListBootstrapActionsResponse' smart constructor.
data ListBootstrapActionsResponse = ListBootstrapActionsResponse'
  { -- | The bootstrap actions associated with the cluster.
    bootstrapActions :: Lude.Maybe [Command],
    -- | The pagination token that indicates the next set of results to retrieve.
    marker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListBootstrapActionsResponse' with the minimum fields required to make a request.
--
-- * 'bootstrapActions' - The bootstrap actions associated with the cluster.
-- * 'marker' - The pagination token that indicates the next set of results to retrieve.
-- * 'responseStatus' - The response status code.
mkListBootstrapActionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListBootstrapActionsResponse
mkListBootstrapActionsResponse pResponseStatus_ =
  ListBootstrapActionsResponse'
    { bootstrapActions = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The bootstrap actions associated with the cluster.
--
-- /Note:/ Consider using 'bootstrapActions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbarsBootstrapActions :: Lens.Lens' ListBootstrapActionsResponse (Lude.Maybe [Command])
lbarsBootstrapActions = Lens.lens (bootstrapActions :: ListBootstrapActionsResponse -> Lude.Maybe [Command]) (\s a -> s {bootstrapActions = a} :: ListBootstrapActionsResponse)
{-# DEPRECATED lbarsBootstrapActions "Use generic-lens or generic-optics with 'bootstrapActions' instead." #-}

-- | The pagination token that indicates the next set of results to retrieve.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbarsMarker :: Lens.Lens' ListBootstrapActionsResponse (Lude.Maybe Lude.Text)
lbarsMarker = Lens.lens (marker :: ListBootstrapActionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListBootstrapActionsResponse)
{-# DEPRECATED lbarsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lbarsResponseStatus :: Lens.Lens' ListBootstrapActionsResponse Lude.Int
lbarsResponseStatus = Lens.lens (responseStatus :: ListBootstrapActionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListBootstrapActionsResponse)
{-# DEPRECATED lbarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
