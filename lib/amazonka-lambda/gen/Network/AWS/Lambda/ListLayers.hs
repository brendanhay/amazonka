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
    llrrsLayers,
    llrrsNextMarker,
    llrrsResponseStatus,
  )
where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLayers' smart constructor.
data ListLayers = ListLayers'
  { -- | A runtime identifier. For example, @go1.x@ .
    compatibleRuntime :: Core.Maybe Types.Runtime,
    -- | A pagination token returned by a previous call.
    marker :: Core.Maybe Types.String,
    -- | The maximum number of layers to return.
    maxItems :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLayers' value with any optional fields omitted.
mkListLayers ::
  ListLayers
mkListLayers =
  ListLayers'
    { compatibleRuntime = Core.Nothing,
      marker = Core.Nothing,
      maxItems = Core.Nothing
    }

-- | A runtime identifier. For example, @go1.x@ .
--
-- /Note:/ Consider using 'compatibleRuntime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llCompatibleRuntime :: Lens.Lens' ListLayers (Core.Maybe Types.Runtime)
llCompatibleRuntime = Lens.field @"compatibleRuntime"
{-# DEPRECATED llCompatibleRuntime "Use generic-lens or generic-optics with 'compatibleRuntime' instead." #-}

-- | A pagination token returned by a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llMarker :: Lens.Lens' ListLayers (Core.Maybe Types.String)
llMarker = Lens.field @"marker"
{-# DEPRECATED llMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The maximum number of layers to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llMaxItems :: Lens.Lens' ListLayers (Core.Maybe Core.Natural)
llMaxItems = Lens.field @"maxItems"
{-# DEPRECATED llMaxItems "Use generic-lens or generic-optics with 'maxItems' instead." #-}

instance Core.AWSRequest ListLayers where
  type Rs ListLayers = ListLayersResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.GET,
        Core._rqPath = Core.rawPath "/2018-10-31/layers",
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
          ListLayersResponse'
            Core.<$> (x Core..:? "Layers")
            Core.<*> (x Core..:? "NextMarker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListLayers where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
    | Pager.stop (rs Lens.^? Lens.field @"layers" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker"
        )

-- | /See:/ 'mkListLayersResponse' smart constructor.
data ListLayersResponse = ListLayersResponse'
  { -- | A list of function layers.
    layers :: Core.Maybe [Types.LayersListItem],
    -- | A pagination token returned when the response doesn't contain all layers.
    nextMarker :: Core.Maybe Types.String,
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLayersResponse' value with any optional fields omitted.
mkListLayersResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListLayersResponse
mkListLayersResponse responseStatus =
  ListLayersResponse'
    { layers = Core.Nothing,
      nextMarker = Core.Nothing,
      responseStatus
    }

-- | A list of function layers.
--
-- /Note:/ Consider using 'layers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrrsLayers :: Lens.Lens' ListLayersResponse (Core.Maybe [Types.LayersListItem])
llrrsLayers = Lens.field @"layers"
{-# DEPRECATED llrrsLayers "Use generic-lens or generic-optics with 'layers' instead." #-}

-- | A pagination token returned when the response doesn't contain all layers.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrrsNextMarker :: Lens.Lens' ListLayersResponse (Core.Maybe Types.String)
llrrsNextMarker = Lens.field @"nextMarker"
{-# DEPRECATED llrrsNextMarker "Use generic-lens or generic-optics with 'nextMarker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llrrsResponseStatus :: Lens.Lens' ListLayersResponse Core.Int
llrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED llrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
