{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeVTLDevices
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of virtual tape library (VTL) devices for the specified tape gateway. In the response, AWS Storage Gateway returns VTL device information.
--
-- This operation is only supported in the tape gateway type.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeVTLDevices
  ( -- * Creating a request
    DescribeVTLDevices (..),
    mkDescribeVTLDevices,

    -- ** Request lenses
    dvtldMarker,
    dvtldLimit,
    dvtldVTLDeviceARNs,
    dvtldGatewayARN,

    -- * Destructuring the response
    DescribeVTLDevicesResponse (..),
    mkDescribeVTLDevicesResponse,

    -- ** Response lenses
    dvtldrsVTLDevices,
    dvtldrsGatewayARN,
    dvtldrsMarker,
    dvtldrsResponseStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Page
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.StorageGateway.Types

-- | DescribeVTLDevicesInput
--
-- /See:/ 'mkDescribeVTLDevices' smart constructor.
data DescribeVTLDevices = DescribeVTLDevices'
  { marker ::
      Lude.Maybe Lude.Text,
    limit :: Lude.Maybe Lude.Natural,
    vTLDeviceARNs :: Lude.Maybe [Lude.Text],
    gatewayARN :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVTLDevices' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'limit' - Specifies that the number of VTL devices described be limited to the specified number.
-- * 'marker' - An opaque string that indicates the position at which to begin describing the VTL devices.
-- * 'vTLDeviceARNs' - An array of strings, where each string represents the Amazon Resource Name (ARN) of a VTL device.
mkDescribeVTLDevices ::
  -- | 'gatewayARN'
  Lude.Text ->
  DescribeVTLDevices
mkDescribeVTLDevices pGatewayARN_ =
  DescribeVTLDevices'
    { marker = Lude.Nothing,
      limit = Lude.Nothing,
      vTLDeviceARNs = Lude.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | An opaque string that indicates the position at which to begin describing the VTL devices.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldMarker :: Lens.Lens' DescribeVTLDevices (Lude.Maybe Lude.Text)
dvtldMarker = Lens.lens (marker :: DescribeVTLDevices -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeVTLDevices)
{-# DEPRECATED dvtldMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | Specifies that the number of VTL devices described be limited to the specified number.
--
-- /Note:/ Consider using 'limit' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldLimit :: Lens.Lens' DescribeVTLDevices (Lude.Maybe Lude.Natural)
dvtldLimit = Lens.lens (limit :: DescribeVTLDevices -> Lude.Maybe Lude.Natural) (\s a -> s {limit = a} :: DescribeVTLDevices)
{-# DEPRECATED dvtldLimit "Use generic-lens or generic-optics with 'limit' instead." #-}

-- | An array of strings, where each string represents the Amazon Resource Name (ARN) of a VTL device.
--
-- /Note:/ Consider using 'vTLDeviceARNs' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldVTLDeviceARNs :: Lens.Lens' DescribeVTLDevices (Lude.Maybe [Lude.Text])
dvtldVTLDeviceARNs = Lens.lens (vTLDeviceARNs :: DescribeVTLDevices -> Lude.Maybe [Lude.Text]) (\s a -> s {vTLDeviceARNs = a} :: DescribeVTLDevices)
{-# DEPRECATED dvtldVTLDeviceARNs "Use generic-lens or generic-optics with 'vTLDeviceARNs' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldGatewayARN :: Lens.Lens' DescribeVTLDevices Lude.Text
dvtldGatewayARN = Lens.lens (gatewayARN :: DescribeVTLDevices -> Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeVTLDevices)
{-# DEPRECATED dvtldGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

instance Page.AWSPager DescribeVTLDevices where
  page rq rs
    | Page.stop (rs Lens.^. dvtldrsMarker) = Lude.Nothing
    | Page.stop (rs Lens.^. dvtldrsVTLDevices) = Lude.Nothing
    | Lude.otherwise =
      Lude.Just Lude.$
        rq
          Lude.& dvtldMarker Lens..~ rs Lens.^. dvtldrsMarker

instance Lude.AWSRequest DescribeVTLDevices where
  type Rs DescribeVTLDevices = DescribeVTLDevicesResponse
  request = Req.postJSON storageGatewayService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeVTLDevicesResponse'
            Lude.<$> (x Lude..?> "VTLDevices" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "GatewayARN")
            Lude.<*> (x Lude..?> "Marker")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVTLDevices where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("StorageGateway_20130630.DescribeVTLDevices" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeVTLDevices where
  toJSON DescribeVTLDevices' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Marker" Lude..=) Lude.<$> marker,
            ("Limit" Lude..=) Lude.<$> limit,
            ("VTLDeviceARNs" Lude..=) Lude.<$> vTLDeviceARNs,
            Lude.Just ("GatewayARN" Lude..= gatewayARN)
          ]
      )

instance Lude.ToPath DescribeVTLDevices where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVTLDevices where
  toQuery = Lude.const Lude.mempty

-- | DescribeVTLDevicesOutput
--
-- /See:/ 'mkDescribeVTLDevicesResponse' smart constructor.
data DescribeVTLDevicesResponse = DescribeVTLDevicesResponse'
  { vTLDevices ::
      Lude.Maybe [VTLDevice],
    gatewayARN :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'DescribeVTLDevicesResponse' with the minimum fields required to make a request.
--
-- * 'gatewayARN' - Undocumented field.
-- * 'marker' - An opaque string that indicates the position at which the VTL devices that were fetched for description ended. Use the marker in your next request to fetch the next set of VTL devices in the list. If there are no more VTL devices to describe, this field does not appear in the response.
-- * 'responseStatus' - The response status code.
-- * 'vTLDevices' - An array of VTL device objects composed of the Amazon Resource Name (ARN) of the VTL devices.
mkDescribeVTLDevicesResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVTLDevicesResponse
mkDescribeVTLDevicesResponse pResponseStatus_ =
  DescribeVTLDevicesResponse'
    { vTLDevices = Lude.Nothing,
      gatewayARN = Lude.Nothing,
      marker = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | An array of VTL device objects composed of the Amazon Resource Name (ARN) of the VTL devices.
--
-- /Note:/ Consider using 'vTLDevices' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldrsVTLDevices :: Lens.Lens' DescribeVTLDevicesResponse (Lude.Maybe [VTLDevice])
dvtldrsVTLDevices = Lens.lens (vTLDevices :: DescribeVTLDevicesResponse -> Lude.Maybe [VTLDevice]) (\s a -> s {vTLDevices = a} :: DescribeVTLDevicesResponse)
{-# DEPRECATED dvtldrsVTLDevices "Use generic-lens or generic-optics with 'vTLDevices' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldrsGatewayARN :: Lens.Lens' DescribeVTLDevicesResponse (Lude.Maybe Lude.Text)
dvtldrsGatewayARN = Lens.lens (gatewayARN :: DescribeVTLDevicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {gatewayARN = a} :: DescribeVTLDevicesResponse)
{-# DEPRECATED dvtldrsGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | An opaque string that indicates the position at which the VTL devices that were fetched for description ended. Use the marker in your next request to fetch the next set of VTL devices in the list. If there are no more VTL devices to describe, this field does not appear in the response.
--
-- /Note:/ Consider using 'marker' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldrsMarker :: Lens.Lens' DescribeVTLDevicesResponse (Lude.Maybe Lude.Text)
dvtldrsMarker = Lens.lens (marker :: DescribeVTLDevicesResponse -> Lude.Maybe Lude.Text) (\s a -> s {marker = a} :: DescribeVTLDevicesResponse)
{-# DEPRECATED dvtldrsMarker "Use generic-lens or generic-optics with 'marker' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvtldrsResponseStatus :: Lens.Lens' DescribeVTLDevicesResponse Lude.Int
dvtldrsResponseStatus = Lens.lens (responseStatus :: DescribeVTLDevicesResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVTLDevicesResponse)
{-# DEPRECATED dvtldrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
