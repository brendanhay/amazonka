{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lambda.ListLayerVersions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the versions of an <https://docs.aws.amazon.com/lambda/latest/dg/configuration-layers.html AWS Lambda layer> . Versions that have been deleted aren't listed. Specify a <https://docs.aws.amazon.com/lambda/latest/dg/lambda-runtimes.html runtime identifier> to list only versions that indicate that they're compatible with that runtime.
--
-- This operation returns paginated results.
module Network.AWS.Lambda.ListLayerVersions
  ( -- * Creating a request
    ListLayerVersions (..),
    mkListLayerVersions,

    -- ** Request lenses
    llvLayerName,
    llvCompatibleRuntime,
    llvMarker,
    llvMaxItems,

    -- * Destructuring the response
    ListLayerVersionsResponse (..),
    mkListLayerVersionsResponse,

    -- ** Response lenses
    llvrsLayerVersions,
    llvrsNextMarker,
    llvrsResponseStatus,
  )
where

import Network.AWS.Lambda.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkListLayerVersions' smart constructor.
data ListLayerVersions = ListLayerVersions'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Lude.Text,
    -- | A runtime identifier. For example, @go1.x@ .
    compatibleRuntime :: Lude.Maybe Runtime,
    -- | A pagination token returned by a previous call.
    marker :: Lude.Maybe Lude.Text,
    -- | The maximum number of versions to return.
    maxItems :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLayerVersions' with the minimum fields required to make a request.
--
-- * 'layerName' - The name or Amazon Resource Name (ARN) of the layer.
-- * 'compatibleRuntime' - A runtime identifier. For example, @go1.x@ .
-- * 'marker' - A pagination token returned by a previous call.
-- * 'maxItems' - The maximum number of versions to return.
mkListLayerVersions ::
  -- | 'layerName'
  Lude.Text ->
  ListLayerVersions
mkListLayerVersions pLayerName_ =
  ListLayerVersions'
    { layerName = pLayerName_,
      compatibleRuntime = Lude.Nothing,
      marker = Lude.Nothing,
      maxItems = Lude.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvLayerName :: Lens.Lens' ListLayerVersions Lude.Text
llvLayerName = Lens.lens (layerName :: ListLayerVersions -> Lude.Text) (\s a -> s {layerName = a} :: ListLayerVersions)
{-# DEPRECATED llvLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | A runtime identifier. For example, @go1.x@ .
--
-- /Note:/ Consider using 'compatibleRuntime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvCompatibleRuntime :: Lens.Lens' ListLayerVersions (Lude.Maybe Runtime)
llvCompatibleRuntime = Lens.lens (compatibleRuntime :: ListLayerVersions -> Lude.Maybe Runtime) (\s a -> s {compatibleRuntime = a} :: ListLayerVersions)
{-# DEPRECATED llvCompatibleRuntime "Use generic-lens or generic-optics with 'compatibleRuntime' instead." #-}

-- | A pagination token returned by a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvMarker :: Lens.Lens' ListLayerVersions (Lude.Maybe Lude.Text)
llvMarker = Lens.lens (marker :: ListLayerVersions -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListLayerVersions)
{-# DEPRECATED llvMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of versions to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvMaxItems :: Lens.Lens' ListLayerVersions (Lude.Maybe Lude.Natural)
llvMaxItems = Lens.lens (maxItems :: ListLayerVersions -> Lude.Maybe Lude.Natural) (\s a -> s {maxItems = a} :: ListLayerVersions)
{-# DEPRECATED llvMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Page.AWSPager ListLayerVersions where
  page rq rs
    | Page.stop (rs Lens.^. llvrsNextMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. llvrsLayerVersions) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& llvMarker Lens..~ rs Lens.^. llvrsNextMarker

instance Lude.AWSRequest ListLayerVersions where
  type Rs ListLayerVersions = ListLayerVersionsResponse
  request = Req.get lambdaService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListLayerVersionsResponse'
            Lude.<$> (x Lude..?> "LayerVersions" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextMarker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListLayerVersions where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath ListLayerVersions where
  toPath ListLayerVersions' {..} =
    Lude.mconcat
      ["/2018-10-31/layers/", Lude.toBS layerName, "/versions"]

instance Lude.ToQuery ListLayerVersions where
  toQuery ListLayerVersions' {..} =
    Lude.mconcat
      [ "CompatibleRuntime" Lude.=: compatibleRuntime,
        "Marker" Lude.=: marker,
        "MaxItems" Lude.=: maxItems
      ]

-- | /See:/ 'mkListLayerVersionsResponse' smart constructor.
data ListLayerVersionsResponse = ListLayerVersionsResponse'
  { -- | A list of versions.
    layerVersions :: Lude.Maybe [LayerVersionsListItem],
    -- | A pagination token returned when the response doesn't contain all versions.
    nextMarker :: Lude.Maybe Lude.Text,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListLayerVersionsResponse' with the minimum fields required to make a request.
--
-- * 'layerVersions' - A list of versions.
-- * 'nextMarker' - A pagination token returned when the response doesn't contain all versions.
-- * 'responseStatus' - The response status code.
mkListLayerVersionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListLayerVersionsResponse
mkListLayerVersionsResponse pResponseStatus_ =
  ListLayerVersionsResponse'
    { layerVersions = Lude.Nothing,
      nextMarker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A list of versions.
--
-- /Note:/ Consider using 'layerVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvrsLayerVersions :: Lens.Lens' ListLayerVersionsResponse (Lude.Maybe [LayerVersionsListItem])
llvrsLayerVersions = Lens.lens (layerVersions :: ListLayerVersionsResponse -> Lude.Maybe [LayerVersionsListItem]) (\s a -> s {layerVersions = a} :: ListLayerVersionsResponse)
{-# DEPRECATED llvrsLayerVersions "Use generic-lens or generic-optics with 'layerVersions' instead." #-}

-- | A pagination token returned when the response doesn't contain all versions.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvrsNextMarker :: Lens.Lens' ListLayerVersionsResponse (Lude.Maybe Lude.Text)
llvrsNextMarker = Lens.lens (nextMarker :: ListLayerVersionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextMarker = a} :: ListLayerVersionsResponse)
{-# DEPRECATED llvrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvrsResponseStatus :: Lens.Lens' ListLayerVersionsResponse Lude.Int
llvrsResponseStatus = Lens.lens (responseStatus :: ListLayerVersionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListLayerVersionsResponse)
{-# DEPRECATED llvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
