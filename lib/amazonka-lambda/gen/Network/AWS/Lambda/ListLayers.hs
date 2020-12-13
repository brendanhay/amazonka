{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListLayers
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layers> and shows information about the latest version of each. Specify a <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime identifier> to list only layers that indicate that they're compatible with that runtime.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListLayers
  ( -- * Creating a request
    ListLayers (..),
    mkListLayers,

    -- ** Request lenses
    llCompatibleRuntime,
    llMarker,
    llMaxItems,

    -- * Destructuring the response
    ListLayersResponse (..),
    mkListLayersResponse,

    -- ** Response lenses
    llrsNextMarker,
    llrsLayers,
    llrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListLayers' smart constructor.
data ListLayers = ListLayers'
  { -- | A runtime identifier. For example, @go1.x@ .
    compatibleRuntime :: Lude.Maybe Runtime,
    -- | A pagination token returned by a previous call.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of layers to return.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLayers' with the minimum fields required to make a request.
--
-- * 'compatibleRuntime' - A runtime identifier. For example, @go1.x@ .
-- * 'marker' - A pagination token returned by a previous call.
-- * 'maxItems' - The maximum number of layers to return.
mkListLayers ::
  ListLayers
mkListLayers =
  ListLayers'
    { compatibleRuntime = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | A runtime identifier. For example, @go1.x@ .
--
-- /Note:/ Consider using 'compatibleRuntime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llCompatibleRuntime :: Lens.Lens' ListLayers (Lude.Maybe Runtime)
llCompatibleRuntime = Lens.lens (compatibleRuntime :: ListLayers -> Lude.Maybe Runtime) (\s a -> s {compatibleRuntime = a} :: ListLayers)
{-# DEPRECATED llCompatibleRuntime "Use generic-lens or generic-optics with 'compatibleRuntime' instead." #-}

-- | A pagination token returned by a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llMarker :: Lens.Lens' ListLayers (Lude.Maybe Lude.Text)
llMarker = Lens.lens (marker :: ListLayers -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListLayers)
{-# DEPRECATED llMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of layers to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llMaxItems :: Lens.Lens' ListLayers (Lude.Maybe Lude.Natural)
llMaxItems = Lens.lens (maxItems :: ListLayers -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListLayers)
{-# DEPRECATED llMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListLayers where
  page rq rs
    | Page.stop (rs Lens.^. llrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. llrsLayers) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& llMarker Lens..~ rs Lens.^. llrsNextMarker

instance Lude.AWSRequest ListLayers where
  type Rs ListLayers = ListLayersResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLayersResponse'
            Lude.<$> (x Lude..?> "NextMarker")
            Lude.<*> (x Lude..?> "Layers" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLayers where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListLayers where
  toPath = Lude.const "/2018-10-31/layers"

instance Lude.ToQuery ListLayers where
  toQuery ListLayers' {..} =
    Lude.mconcat
      [ "CompatibleRuntime" Lude.=: compatibleRuntime,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | /See:/ 'mkListLayersResponse' smart constructor.
data ListLayersResponse = ListLayersResponse'
  { -- | A pagination token returned when the response doesn't contain all layers.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | A list of function layers.
    layers :: Lude.Maybe [LayersListItem],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLayersResponse' with the minimum fields required to make a request.
--
-- * 'nextMarker' - A pagination token returned when the response doesn't contain all layers.
-- * 'layers' - A list of function layers.
-- * 'responseStatus' - The response status code.
mkListLayersResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLayersResponse
mkListLayersResponse pResponseStatus_ =
  ListLayersResponse'
    { nextMarker = Lude.Nothing,
      layers = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A pagination token returned when the response doesn't contain all layers.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrsNextMarker :: Lens.Lens' ListLayersResponse (Lude.Maybe Lude.Text)
llrsNextMarker = Lens.lens (nextMarker :: ListLayersResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListLayersResponse)
{-# DEPRECATED llrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | A list of function layers.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrsLayers :: Lens.Lens' ListLayersResponse (Lude.Maybe [LayersListItem])
llrsLayers = Lens.lens (layers :: ListLayersResponse -> Lude.Maybe [LayersListItem]) (\s a -> s {layers = a} :: ListLayersResponse)
{-# DEPRECATED llrsLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrsResponseStatus :: Lens.Lens' ListLayersResponse Lude.Int
llrsResponseStatus = Lens.lens (responseStatus :: ListLayersResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLayersResponse)
{-# DEPRECATED llrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
