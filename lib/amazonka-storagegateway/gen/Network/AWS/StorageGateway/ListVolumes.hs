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
    lvMarker,
    lvLimit,

    -- * Destructuring the response
    ListVolumesResponse (..),
    mkListVolumesResponse,

    -- ** Response lenses
    lvrsGatewayARN,
    lvrsMarker,
    lvrsVolumeInfos,
    lvrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

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
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | A string that indicates the position at which to begin the returned list of volumes. Obtain the marker from the response of a previous List iSCSI Volumes request.
    marker :: Lude.Maybe Lude.Text,
    -- | Specifies that the list of volumes returned be limited to the specified number of items.
    limit :: Lude.Maybe Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVolumes' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'marker' - A string that indicates the position at which to begin the returned list of volumes. Obtain the marker from the response of a previous List iSCSI Volumes request.
-- * 'limit' - Specifies that the list of volumes returned be limited to the specified number of items.
mkListVolumes ::
  ListVolumes
mkListVolumes =
  ListVolumes'
    { gatewayARN = Lude.Nothing,
      marker = Lude.Nothing,
      limit = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvGatewayARN :: Lens.Lens' ListVolumes (Lude.Maybe Lude.Text)
lvGatewayARN = Lens.lens (gatewayARN :: ListVolumes -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: ListVolumes)
{-# DEPRECATED lvGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | A string that indicates the position at which to begin the returned list of volumes. Obtain the marker from the response of a previous List iSCSI Volumes request.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvMarker :: Lens.Lens' ListVolumes (Lude.Maybe Lude.Text)
lvMarker = Lens.lens (marker :: ListVolumes -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListVolumes)
{-# DEPRECATED lvMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies that the list of volumes returned be limited to the specified number of items.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvLimit :: Lens.Lens' ListVolumes (Lude.Maybe Lude.Natural)
lvLimit = Lens.lens (limit :: ListVolumes -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: ListVolumes)
{-# DEPRECATED lvLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

instance Page.AWSPager ListVolumes where
  page rq rs
    | Page.stop (rs Lens.^. lvrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. lvrsVolumeInfos) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$ rq Lude.& lvMarker Lens..~ rs Lens.^. lvrsMarker

instance Lude.AWSRequest ListVolumes where
  type Rs ListVolumes = ListVolumesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          ListVolumesResponse'
            Lude.<$> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (x Lude..?> "VolumeInfos" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders ListVolumes where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.ListVolumes" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON ListVolumes where
  toJSON ListVolumes' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("GatewayARN" Lude..=) Lude.<$> gatewayARN,
            ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit
          ]
      )

instance Lude.ToPath ListVolumes where
  toPath = Lude.const "/"

instance Lude.ToQuery ListVolumes where
  toQuery = Lude.const Lude.mempty

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
  { gatewayARN :: Lude.Maybe Lude.Text,
    -- | Use the marker in your next request to continue pagination of iSCSI volumes. If there are no more volumes to list, this field does not appear in the response body.
    marker :: Lude.Maybe Lude.Text,
    -- | An array of 'VolumeInfo' objects, where each object describes an iSCSI volume. If no volumes are defined for the gateway, then @VolumeInfos@ is an empty array "[]".
    volumeInfos :: Lude.Maybe [VolumeInfo],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'ListVolumesResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' -
-- * 'marker' - Use the marker in your next request to continue pagination of iSCSI volumes. If there are no more volumes to list, this field does not appear in the response body.
-- * 'volumeInfos' - An array of 'VolumeInfo' objects, where each object describes an iSCSI volume. If no volumes are defined for the gateway, then @VolumeInfos@ is an empty array "[]".
-- * 'responseStatus' - The response status code.
mkListVolumesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  ListVolumesResponse
mkListVolumesResponse pResponseStatus_ =
  ListVolumesResponse'
    { gatewayARN = Lude.Nothing,
      marker = Lude.Nothing,
      volumeInfos = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsGatewayARN :: Lens.Lens' ListVolumesResponse (Lude.Maybe Lude.Text)
lvrsGatewayARN = Lens.lens (gatewayARN :: ListVolumesResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: ListVolumesResponse)
{-# DEPRECATED lvrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | Use the marker in your next request to continue pagination of iSCSI volumes. If there are no more volumes to list, this field does not appear in the response body.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsMarker :: Lens.Lens' ListVolumesResponse (Lude.Maybe Lude.Text)
lvrsMarker = Lens.lens (marker :: ListVolumesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: ListVolumesResponse)
{-# DEPRECATED lvrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | An array of 'VolumeInfo' objects, where each object describes an iSCSI volume. If no volumes are defined for the gateway, then @VolumeInfos@ is an empty array "[]".
--
-- /Note:/ Consider using 'volumeInfos' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsVolumeInfos :: Lens.Lens' ListVolumesResponse (Lude.Maybe [VolumeInfo])
lvrsVolumeInfos = Lens.lens (volumeInfos :: ListVolumesResponse -> Lude.Maybe [VolumeInfo]) (\s a -> s {volumeInfos = a} :: ListVolumesResponse)
{-# DEPRECATED lvrsVolumeInfos "Use generic-lens or generic-optics with 'volumeInfos' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
lvrsResponseStatus :: Lens.Lens' ListVolumesResponse Lude.Int
lvrsResponseStatus = Lens.lens (responseStatus :: ListVolumesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: ListVolumesResponse)
{-# DEPRECATED lvrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
