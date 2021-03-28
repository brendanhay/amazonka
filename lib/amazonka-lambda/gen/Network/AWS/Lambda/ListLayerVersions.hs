{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-deprecations   #-}

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
    (
    -- * Creating a request
      ListLayerVersions (..)
    , mkListLayerVersions
    -- ** Request lenses
    , llvLayerName
    , llvCompatibleRuntime
    , llvMarker
    , llvMaxItems

    -- * Destructuring the response
    , ListLayerVersionsResponse (..)
    , mkListLayerVersionsResponse
    -- ** Response lenses
    , llvrrsLayerVersions
    , llvrrsNextMarker
    , llvrrsResponseStatus
    ) where

import qualified Network.AWS.Lambda.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkListLayerVersions' smart constructor.
data ListLayerVersions = ListLayerVersions'
  { layerName :: Types.LayerName
    -- ^ The name or Amazon Resource Name (ARN) of the layer.
  , compatibleRuntime :: Core.Maybe Types.Runtime
    -- ^ A runtime identifier. For example, @go1.x@ .
  , marker :: Core.Maybe Core.Text
    -- ^ A pagination token returned by a previous call.
  , maxItems :: Core.Maybe Core.Natural
    -- ^ The maximum number of versions to return.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLayerVersions' value with any optional fields omitted.
mkListLayerVersions
    :: Types.LayerName -- ^ 'layerName'
    -> ListLayerVersions
mkListLayerVersions layerName
  = ListLayerVersions'{layerName, compatibleRuntime = Core.Nothing,
                       marker = Core.Nothing, maxItems = Core.Nothing}

-- | The name or Amazon Resource Name (ARN) of the layer.
--
-- /Note:/ Consider using 'layerName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvLayerName :: Lens.Lens' ListLayerVersions Types.LayerName
llvLayerName = Lens.field @"layerName"
{-# INLINEABLE llvLayerName #-}
{-# DEPRECATED layerName "Use generic-lens or generic-optics with 'layerName' instead"  #-}

-- | A runtime identifier. For example, @go1.x@ .
--
-- /Note:/ Consider using 'compatibleRuntime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvCompatibleRuntime :: Lens.Lens' ListLayerVersions (Core.Maybe Types.Runtime)
llvCompatibleRuntime = Lens.field @"compatibleRuntime"
{-# INLINEABLE llvCompatibleRuntime #-}
{-# DEPRECATED compatibleRuntime "Use generic-lens or generic-optics with 'compatibleRuntime' instead"  #-}

-- | A pagination token returned by a previous call.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvMarker :: Lens.Lens' ListLayerVersions (Core.Maybe Core.Text)
llvMarker = Lens.field @"marker"
{-# INLINEABLE llvMarker #-}
{-# DEPRECATED marker "Use generic-lens or generic-optics with 'marker' instead"  #-}

-- | The maximum number of versions to return.
--
-- /Note:/ Consider using 'maxItems' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvMaxItems :: Lens.Lens' ListLayerVersions (Core.Maybe Core.Natural)
llvMaxItems = Lens.field @"maxItems"
{-# INLINEABLE llvMaxItems #-}
{-# DEPRECATED maxItems "Use generic-lens or generic-optics with 'maxItems' instead"  #-}

instance Core.ToQuery ListLayerVersions where
        toQuery ListLayerVersions{..}
          = Core.maybe Core.mempty (Core.toQueryPair "CompatibleRuntime")
              compatibleRuntime
              Core.<> Core.maybe Core.mempty (Core.toQueryPair "Marker") marker
              Core.<>
              Core.maybe Core.mempty (Core.toQueryPair "MaxItems") maxItems

instance Core.ToHeaders ListLayerVersions where
        toHeaders _ = Core.pure Core.mempty

instance Core.AWSRequest ListLayerVersions where
        type Rs ListLayerVersions = ListLayerVersionsResponse
        toRequest x@Core.Request{..}
          = Core.Request{Core._rqService = Types.mkServiceConfig,
                         Core._rqMethod = Request.GET,
                         Core._rqPath =
                           "/2018-10-31/layers/" Core.<> Core.toText layerName Core.<>
                             "/versions",
                         Core._rqQuery = Core.toQuery x, Core._rqHeaders = Core.toHeaders x,
                         Core._rqBody = ""}
        
        {-# INLINE toRequest #-}
        parseResponse
          = Response.receiveJSON
              (\ s h x ->
                 ListLayerVersionsResponse' Core.<$>
                   (x Core..:? "LayerVersions") Core.<*> x Core..:? "NextMarker"
                     Core.<*> Core.pure (Core.fromEnum s))
        
        {-# INLINE parseResponse #-}

instance Pager.AWSPager ListLayerVersions where
        page rq rs
          | Pager.stop (rs Lens.^. Lens.field @"nextMarker") = Core.Nothing
          | Pager.stop
              (rs Lens.^? Lens.field @"layerVersions" Core.. Lens._Just)
            = Core.Nothing
          | Core.otherwise =
            Core.Just
              (rq Core.&
                 Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"nextMarker")

-- | /See:/ 'mkListLayerVersionsResponse' smart constructor.
data ListLayerVersionsResponse = ListLayerVersionsResponse'
  { layerVersions :: Core.Maybe [Types.LayerVersionsListItem]
    -- ^ A list of versions.
  , nextMarker :: Core.Maybe Core.Text
    -- ^ A pagination token returned when the response doesn't contain all versions.
  , responseStatus :: Core.Int
    -- ^ The response status code.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListLayerVersionsResponse' value with any optional fields omitted.
mkListLayerVersionsResponse
    :: Core.Int -- ^ 'responseStatus'
    -> ListLayerVersionsResponse
mkListLayerVersionsResponse responseStatus
  = ListLayerVersionsResponse'{layerVersions = Core.Nothing,
                               nextMarker = Core.Nothing, responseStatus}

-- | A list of versions.
--
-- /Note:/ Consider using 'layerVersions' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvrrsLayerVersions :: Lens.Lens' ListLayerVersionsResponse (Core.Maybe [Types.LayerVersionsListItem])
llvrrsLayerVersions = Lens.field @"layerVersions"
{-# INLINEABLE llvrrsLayerVersions #-}
{-# DEPRECATED layerVersions "Use generic-lens or generic-optics with 'layerVersions' instead"  #-}

-- | A pagination token returned when the response doesn't contain all versions.
--
-- /Note:/ Consider using 'nextMarker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvrrsNextMarker :: Lens.Lens' ListLayerVersionsResponse (Core.Maybe Core.Text)
llvrrsNextMarker = Lens.field @"nextMarker"
{-# INLINEABLE llvrrsNextMarker #-}
{-# DEPRECATED nextMarker "Use generic-lens or generic-optics with 'nextMarker' instead"  #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
llvrrsResponseStatus :: Lens.Lens' ListLayerVersionsResponse Core.Int
llvrrsResponseStatus = Lens.field @"responseStatus"
{-# INLINEABLE llvrrsResponseStatus #-}
{-# DEPRECATED responseStatus "Use generic-lens or generic-optics with 'responseStatus' instead"  #-}
