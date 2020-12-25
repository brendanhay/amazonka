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
    llvrrsLayerVersions,
    llvrrsNextMarker,
    llvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLayerVersions' smart constructor.
data ListLayerVersions = ListLayerVersions'
  { -- | The name or Amazon Resource Name (ARN) of the layer.
    layerName :: Types.LayerName,
    -- | A runtime identifier. For example, @go1.x@ .
    compatibleRuntime :: Core.Maybe Types.Runtime,
    -- | A pagination token returned by a previous call.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of versions to return.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLayerVersions' value with any optional fields omitted.
mkListLayerVersions ::
  -- | 'layerName'
  Types.LayerName ->
  ListLayerVersions
mkListLayerVersions layerName =
  ListLayerVersions'
    { layerName,
      compatibleRuntime = Core.Nothing,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvLayerName :: Lens.Lens' ListLayerVersions Types.LayerName
llvLayerName = Lens.field @"layerName"
{-# DEPRECATED llvLayerName "Use generic-lens or generic-optics with 'layerName' instead." #-}

-- | A runtime identifier. For example, @go1.x@ .
--
-- /Note:/ Consider using 'compatibleRuntime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvCompatibleRuntime :: Lens.Lens' ListLayerVersions (Core.Maybe Types.Runtime)
llvCompatibleRuntime = Lens.field @"compatibleRuntime"
{-# DEPRECATED llvCompatibleRuntime "Use generic-lens or generic-optics with 'compatibleRuntime' instead." #-}

-- | A pagination token returned by a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvMarker :: Lens.Lens' ListLayerVersions (Core.Maybe Types.String)
llvMarker = Lens.field @"marker"
{-# DEPRECATED llvMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of versions to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvMaxItems :: Lens.Lens' ListLayerVersions (Core.Maybe Core.Natural)
llvMaxItems = Lens.field @"maxItems"
{-# DEPRECATED llvMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListLayerVersions where
  type Rs ListLayerVersions = ListLayerVersionsResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath =
          Core.rawPath
            ( "/2018-10-31/layers/" Core.<> (Core.toText layerName)
                Core.<> ("/versions")
            ),
        Core._rqQuery =
          Core.toQueryValue "CompatibleRuntime" Core.<$> compatibleRuntime
            Core.<> (Core.toQueryValue "Marker" Core.<$> marker)
            Core.<> (Core.toQueryValue "MaxItems" Core.<$> maxItems),
        Core._rqHeaders = Core.mempty,
        Core._rqBody = ""
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLayerVersionsResponse'
            Core.<$> (x Core..:? "LayerVersions")
            Core.<*> (x Core..:? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLayerVersions where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"layerVersions" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListLayerVersionsResponse' smart constructor.
data ListLayerVersionsResponse = ListLayerVersionsResponse'
  { -- | A list of versions.
    layerVersions :: Core.Maybe [Types.LayerVersionsListItem],
    -- | A pagination token returned when the response doesn't contain all versions.
    nextMarker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLayerVersionsResponse' value with any optional fields omitted.
mkListLayerVersionsResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLayerVersionsResponse
mkListLayerVersionsResponse responseStatus =
  ListLayerVersionsResponse'
    { layerVersions = Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | A list of versions.
--
-- /Note:/ Consider using 'layerVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvrrsLayerVersions :: Lens.Lens' ListLayerVersionsResponse (Core.Maybe [Types.LayerVersionsListItem])
llvrrsLayerVersions = Lens.field @"layerVersions"
{-# DEPRECATED llvrrsLayerVersions "Use generic-lens or generic-optics with 'layerVersions' instead." #-}

-- | A pagination token returned when the response doesn't contain all versions.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvrrsNextMarker :: Lens.Lens' ListLayerVersionsResponse (Core.Maybe Types.String)
llvrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED llvrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvrrsResponseStatus :: Lens.Lens' ListLayerVersionsResponse Core.Int
llvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED llvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
