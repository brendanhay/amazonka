{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.DescribeVTLDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a description of virtual tape library (VTL) devices for the
-- specified tape gateway. In the response, AWS Storage Gateway returns VTL
-- device information.
--
-- This operation is only supported in the tape gateway type.
--
-- This operation returns paginated results.
module Network.AWS.StorageGateway.DescribeVTLDevices
  ( -- * Creating a Request
    DescribeVTLDevices (..),
    newDescribeVTLDevices,

    -- * Request Lenses
    describeVTLDevices_vTLDeviceARNs,
    describeVTLDevices_limit,
    describeVTLDevices_marker,
    describeVTLDevices_gatewayARN,

    -- * Destructuring the Response
    DescribeVTLDevicesResponse (..),
    newDescribeVTLDevicesResponse,

    -- * Response Lenses
    describeVTLDevicesResponse_vTLDevices,
    describeVTLDevicesResponse_gatewayARN,
    describeVTLDevicesResponse_marker,
    describeVTLDevicesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | DescribeVTLDevicesInput
--
-- /See:/ 'newDescribeVTLDevices' smart constructor.
data DescribeVTLDevices = DescribeVTLDevices'
  { -- | An array of strings, where each string represents the Amazon Resource
    -- Name (ARN) of a VTL device.
    --
    -- All of the specified VTL devices must be from the same gateway. If no
    -- VTL devices are specified, the result will contain all devices on the
    -- specified gateway.
    vTLDeviceARNs :: Core.Maybe [Core.Text],
    -- | Specifies that the number of VTL devices described be limited to the
    -- specified number.
    limit :: Core.Maybe Core.Natural,
    -- | An opaque string that indicates the position at which to begin
    -- describing the VTL devices.
    marker :: Core.Maybe Core.Text,
    gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVTLDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vTLDeviceARNs', 'describeVTLDevices_vTLDeviceARNs' - An array of strings, where each string represents the Amazon Resource
-- Name (ARN) of a VTL device.
--
-- All of the specified VTL devices must be from the same gateway. If no
-- VTL devices are specified, the result will contain all devices on the
-- specified gateway.
--
-- 'limit', 'describeVTLDevices_limit' - Specifies that the number of VTL devices described be limited to the
-- specified number.
--
-- 'marker', 'describeVTLDevices_marker' - An opaque string that indicates the position at which to begin
-- describing the VTL devices.
--
-- 'gatewayARN', 'describeVTLDevices_gatewayARN' - Undocumented member.
newDescribeVTLDevices ::
  -- | 'gatewayARN'
  Core.Text ->
  DescribeVTLDevices
newDescribeVTLDevices pGatewayARN_ =
  DescribeVTLDevices'
    { vTLDeviceARNs = Core.Nothing,
      limit = Core.Nothing,
      marker = Core.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | An array of strings, where each string represents the Amazon Resource
-- Name (ARN) of a VTL device.
--
-- All of the specified VTL devices must be from the same gateway. If no
-- VTL devices are specified, the result will contain all devices on the
-- specified gateway.
describeVTLDevices_vTLDeviceARNs :: Lens.Lens' DescribeVTLDevices (Core.Maybe [Core.Text])
describeVTLDevices_vTLDeviceARNs = Lens.lens (\DescribeVTLDevices' {vTLDeviceARNs} -> vTLDeviceARNs) (\s@DescribeVTLDevices' {} a -> s {vTLDeviceARNs = a} :: DescribeVTLDevices) Core.. Lens.mapping Lens._Coerce

-- | Specifies that the number of VTL devices described be limited to the
-- specified number.
describeVTLDevices_limit :: Lens.Lens' DescribeVTLDevices (Core.Maybe Core.Natural)
describeVTLDevices_limit = Lens.lens (\DescribeVTLDevices' {limit} -> limit) (\s@DescribeVTLDevices' {} a -> s {limit = a} :: DescribeVTLDevices)

-- | An opaque string that indicates the position at which to begin
-- describing the VTL devices.
describeVTLDevices_marker :: Lens.Lens' DescribeVTLDevices (Core.Maybe Core.Text)
describeVTLDevices_marker = Lens.lens (\DescribeVTLDevices' {marker} -> marker) (\s@DescribeVTLDevices' {} a -> s {marker = a} :: DescribeVTLDevices)

-- | Undocumented member.
describeVTLDevices_gatewayARN :: Lens.Lens' DescribeVTLDevices Core.Text
describeVTLDevices_gatewayARN = Lens.lens (\DescribeVTLDevices' {gatewayARN} -> gatewayARN) (\s@DescribeVTLDevices' {} a -> s {gatewayARN = a} :: DescribeVTLDevices)

instance Core.AWSPager DescribeVTLDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? describeVTLDevicesResponse_marker Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? describeVTLDevicesResponse_vTLDevices
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& describeVTLDevices_marker
          Lens..~ rs
          Lens.^? describeVTLDevicesResponse_marker Core.. Lens._Just

instance Core.AWSRequest DescribeVTLDevices where
  type
    AWSResponse DescribeVTLDevices =
      DescribeVTLDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVTLDevicesResponse'
            Core.<$> (x Core..?> "VTLDevices" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (x Core..?> "Marker")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVTLDevices

instance Core.NFData DescribeVTLDevices

instance Core.ToHeaders DescribeVTLDevices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeVTLDevices" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeVTLDevices where
  toJSON DescribeVTLDevices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("VTLDeviceARNs" Core..=) Core.<$> vTLDeviceARNs,
            ("Limit" Core..=) Core.<$> limit,
            ("Marker" Core..=) Core.<$> marker,
            Core.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.ToPath DescribeVTLDevices where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVTLDevices where
  toQuery = Core.const Core.mempty

-- | DescribeVTLDevicesOutput
--
-- /See:/ 'newDescribeVTLDevicesResponse' smart constructor.
data DescribeVTLDevicesResponse = DescribeVTLDevicesResponse'
  { -- | An array of VTL device objects composed of the Amazon Resource Name
    -- (ARN) of the VTL devices.
    vTLDevices :: Core.Maybe [VTLDevice],
    gatewayARN :: Core.Maybe Core.Text,
    -- | An opaque string that indicates the position at which the VTL devices
    -- that were fetched for description ended. Use the marker in your next
    -- request to fetch the next set of VTL devices in the list. If there are
    -- no more VTL devices to describe, this field does not appear in the
    -- response.
    marker :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVTLDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vTLDevices', 'describeVTLDevicesResponse_vTLDevices' - An array of VTL device objects composed of the Amazon Resource Name
-- (ARN) of the VTL devices.
--
-- 'gatewayARN', 'describeVTLDevicesResponse_gatewayARN' - Undocumented member.
--
-- 'marker', 'describeVTLDevicesResponse_marker' - An opaque string that indicates the position at which the VTL devices
-- that were fetched for description ended. Use the marker in your next
-- request to fetch the next set of VTL devices in the list. If there are
-- no more VTL devices to describe, this field does not appear in the
-- response.
--
-- 'httpStatus', 'describeVTLDevicesResponse_httpStatus' - The response's http status code.
newDescribeVTLDevicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVTLDevicesResponse
newDescribeVTLDevicesResponse pHttpStatus_ =
  DescribeVTLDevicesResponse'
    { vTLDevices =
        Core.Nothing,
      gatewayARN = Core.Nothing,
      marker = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of VTL device objects composed of the Amazon Resource Name
-- (ARN) of the VTL devices.
describeVTLDevicesResponse_vTLDevices :: Lens.Lens' DescribeVTLDevicesResponse (Core.Maybe [VTLDevice])
describeVTLDevicesResponse_vTLDevices = Lens.lens (\DescribeVTLDevicesResponse' {vTLDevices} -> vTLDevices) (\s@DescribeVTLDevicesResponse' {} a -> s {vTLDevices = a} :: DescribeVTLDevicesResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
describeVTLDevicesResponse_gatewayARN :: Lens.Lens' DescribeVTLDevicesResponse (Core.Maybe Core.Text)
describeVTLDevicesResponse_gatewayARN = Lens.lens (\DescribeVTLDevicesResponse' {gatewayARN} -> gatewayARN) (\s@DescribeVTLDevicesResponse' {} a -> s {gatewayARN = a} :: DescribeVTLDevicesResponse)

-- | An opaque string that indicates the position at which the VTL devices
-- that were fetched for description ended. Use the marker in your next
-- request to fetch the next set of VTL devices in the list. If there are
-- no more VTL devices to describe, this field does not appear in the
-- response.
describeVTLDevicesResponse_marker :: Lens.Lens' DescribeVTLDevicesResponse (Core.Maybe Core.Text)
describeVTLDevicesResponse_marker = Lens.lens (\DescribeVTLDevicesResponse' {marker} -> marker) (\s@DescribeVTLDevicesResponse' {} a -> s {marker = a} :: DescribeVTLDevicesResponse)

-- | The response's http status code.
describeVTLDevicesResponse_httpStatus :: Lens.Lens' DescribeVTLDevicesResponse Core.Int
describeVTLDevicesResponse_httpStatus = Lens.lens (\DescribeVTLDevicesResponse' {httpStatus} -> httpStatus) (\s@DescribeVTLDevicesResponse' {} a -> s {httpStatus = a} :: DescribeVTLDevicesResponse)

instance Core.NFData DescribeVTLDevicesResponse
