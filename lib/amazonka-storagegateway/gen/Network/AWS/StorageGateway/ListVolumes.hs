{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.ListVolumes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the iSCSI stored volumes of a gateway. Results are sorted by volume ARN. The response includes only the volume ARNs. If you want additional volume information, use the 'DescribeStorediSCSIVolumes' or the 'DescribeCachediSCSIVolumes' API.
--
-- The operation supports pagination. By default, the operation returns a maximum of up to 100 volumes. You can optionally specify the @Limit@ field in the body to limit the number of volumes in the response. If the number of volumes returned in the response is truncated, the response includes a Marker field. You can use this Marker value in your subsequent request to retrieve the next set of volumes. This operation is only supported in the cached volume and stored volume gateway types.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.ListVolumes
  ( -- * Creating a request
    ListVolumes (..),
    mkListVolumes,

    -- ** Request lenses
    lvGatewayARN,
    lvLimit,
    lvMarker,

    -- * Destructuring the response
    ListVolumesResponse (..),
    mkListVolumesResponse,

    -- ** Response lenses
    lvrrsGatewayARN,
    lvrrsMarker,
    lvrrsVolumeInfos,
    lvrrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import qualified Network.AWS.StorageGateway.Types as Types

-- | A JSON object that contains one or more of the following fields:
--
--
--     * 'ListVolumesInput$Limit'
--
--
--     * 'ListVolumesInput$Marker'
--
--
--
-- /See:/ 'mkListVolumes' smart constructor.
data ListVolumes = ListVolumes'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | Specifies that the list of volumes returned be limited to the specified number of items.
    limit :: Core.Maybe Core.Natural,
    -- | A string that indicates the position at which to begin the returned list of volumes. Obtain the marker from the response of a previous List iSCSI Volumes request.
    marker :: Core.Maybe Types.Marker
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVolumes' value with any optional fields omitted.
mkListVolumes ::
  ListVolumes
mkListVolumes =
  ListVolumes'
    { gatewayARN = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvGatewayARN :: Lens.Lens' ListVolumes (Core.Maybe Types.GatewayARN)
lvGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED lvGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Specifies that the list of volumes returned be limited to the specified number of items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvLimit :: Lens.Lens' ListVolumes (Core.Maybe Core.Natural)
lvLimit = Lens.field @"limit"
{-# DEPRECATED lvLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | A string that indicates the position at which to begin the returned list of volumes. Obtain the marker from the response of a previous List iSCSI Volumes request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvMarker :: Lens.Lens' ListVolumes (Core.Maybe Types.Marker)
lvMarker = Lens.field @"marker"
{-# DEPRECATED lvMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

instance Core.FromJSON ListVolumes where
  toJSON ListVolumes {..} =
    Core.object
      ( Core.catMaybes
          [ ("GatewayARN" Core..=) Core.<$> gatewayARN,
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker
          ]
      )

instance Core.AWSRequest ListVolumes where
  type Rs ListVolumes = ListVolumesResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure ("X-Amz-Target", "StorageGateway_20130630.ListVolumes")
            Core.<> (Core.pure ("Content-Type", "application/x-amz-json-1.1")),
        Core._rqBody = Core.toJSONBody x
      }
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVolumesResponse'
            Core.<$> (x Core..:? "GatewayARN")
            Core.<*> (x Core..:? "Marker")
            Core.<*> (x Core..:? "VolumeInfos")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Pager.AWSPager ListVolumes where
  page rq rs
    | Pager.stop (rs Lens.^. Lens.field @"marker") = Core.Nothing
    | Pager.stop
        (rs Lens.^? Lens.field @"volumeInfos" Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just
        ( rq
            Core.& Lens.field @"marker" Lens..~ rs Lens.^. Lens.field @"marker"
        )

-- | A JSON object containing the following fields:
--
--
--     * 'ListVolumesOutput$Marker'
--
--
--     * 'ListVolumesOutput$VolumeInfos'
--
--
--
-- /See:/ 'mkListVolumesResponse' smart constructor.
data ListVolumesResponse = ListVolumesResponse'
  { gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | Use the marker in your next request to continue pagination of iSCSI volumes. If there are no more volumes to list, this field does not appear in the response body.
    marker :: Core.Maybe Types.Marker,
    -- | An array of 'VolumeInfo' objects, where each object describes an iSCSI volume. If no volumes are defined for the gateway, then @VolumeInfos@ is an empty array "[]".
    volumeInfos :: Core.Maybe [Types.VolumeInfo],
    -- | The response status code.
    responseStatus :: Core.Int
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ListVolumesResponse' value with any optional fields omitted.
mkListVolumesResponse ::
  -- | 'responseStatus'
  Core.Int ->
  ListVolumesResponse
mkListVolumesResponse responseStatus =
  ListVolumesResponse'
    { gatewayARN = Core.Nothing,
      marker = Core.Nothing,
      volumeInfos = Core.Nothing,
      responseStatus
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsGatewayARN :: Lens.Lens' ListVolumesResponse (Core.Maybe Types.GatewayARN)
lvrrsGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED lvrrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Use the marker in your next request to continue pagination of iSCSI volumes. If there are no more volumes to list, this field does not appear in the response body.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsMarker :: Lens.Lens' ListVolumesResponse (Core.Maybe Types.Marker)
lvrrsMarker = Lens.field @"marker"
{-# DEPRECATED lvrrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An array of 'VolumeInfo' objects, where each object describes an iSCSI volume. If no volumes are defined for the gateway, then @VolumeInfos@ is an empty array "[]".
--
-- /Note:/ Consider using 'volumeInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsVolumeInfos :: Lens.Lens' ListVolumesResponse (Core.Maybe [Types.VolumeInfo])
lvrrsVolumeInfos = Lens.field @"volumeInfos"
{-# DEPRECATED lvrrsVolumeInfos "Use generic-lens or generic-optics with 'volumeInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrrsResponseStatus :: Lens.Lens' ListVolumesResponse Core.Int
lvrrsResponseStatus = Lens.field @"responseStatus"
{-# DEPRECATED lvrrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
