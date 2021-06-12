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
-- Module      : Network.AWS.DeviceFarm.ListDevicePools
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about device pools.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListDevicePools
  ( -- * Creating a Request
    ListDevicePools (..),
    newListDevicePools,

    -- * Request Lenses
    listDevicePools_nextToken,
    listDevicePools_type,
    listDevicePools_arn,

    -- * Destructuring the Response
    ListDevicePoolsResponse (..),
    newListDevicePoolsResponse,

    -- * Response Lenses
    listDevicePoolsResponse_nextToken,
    listDevicePoolsResponse_devicePools,
    listDevicePoolsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the result of a list device pools request.
--
-- /See:/ 'newListDevicePools' smart constructor.
data ListDevicePools = ListDevicePools'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | The device pools\' type.
    --
    -- Allowed values include:
    --
    -- -   CURATED: A device pool that is created and managed by AWS Device
    --     Farm.
    --
    -- -   PRIVATE: A device pool that is created and managed by the device
    --     pool developer.
    type' :: Core.Maybe DevicePoolType,
    -- | The project ARN.
    arn :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDevicePools' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDevicePools_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'type'', 'listDevicePools_type' - The device pools\' type.
--
-- Allowed values include:
--
-- -   CURATED: A device pool that is created and managed by AWS Device
--     Farm.
--
-- -   PRIVATE: A device pool that is created and managed by the device
--     pool developer.
--
-- 'arn', 'listDevicePools_arn' - The project ARN.
newListDevicePools ::
  -- | 'arn'
  Core.Text ->
  ListDevicePools
newListDevicePools pArn_ =
  ListDevicePools'
    { nextToken = Core.Nothing,
      type' = Core.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listDevicePools_nextToken :: Lens.Lens' ListDevicePools (Core.Maybe Core.Text)
listDevicePools_nextToken = Lens.lens (\ListDevicePools' {nextToken} -> nextToken) (\s@ListDevicePools' {} a -> s {nextToken = a} :: ListDevicePools)

-- | The device pools\' type.
--
-- Allowed values include:
--
-- -   CURATED: A device pool that is created and managed by AWS Device
--     Farm.
--
-- -   PRIVATE: A device pool that is created and managed by the device
--     pool developer.
listDevicePools_type :: Lens.Lens' ListDevicePools (Core.Maybe DevicePoolType)
listDevicePools_type = Lens.lens (\ListDevicePools' {type'} -> type') (\s@ListDevicePools' {} a -> s {type' = a} :: ListDevicePools)

-- | The project ARN.
listDevicePools_arn :: Lens.Lens' ListDevicePools Core.Text
listDevicePools_arn = Lens.lens (\ListDevicePools' {arn} -> arn) (\s@ListDevicePools' {} a -> s {arn = a} :: ListDevicePools)

instance Core.AWSPager ListDevicePools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDevicePoolsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDevicePoolsResponse_devicePools
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDevicePools_nextToken
          Lens..~ rs
          Lens.^? listDevicePoolsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDevicePools where
  type
    AWSResponse ListDevicePools =
      ListDevicePoolsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicePoolsResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "devicePools" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDevicePools

instance Core.NFData ListDevicePools

instance Core.ToHeaders ListDevicePools where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListDevicePools" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDevicePools where
  toJSON ListDevicePools' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("type" Core..=) Core.<$> type',
            Core.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListDevicePools where
  toPath = Core.const "/"

instance Core.ToQuery ListDevicePools where
  toQuery = Core.const Core.mempty

-- | Represents the result of a list device pools request.
--
-- /See:/ 'newListDevicePoolsResponse' smart constructor.
data ListDevicePoolsResponse = ListDevicePoolsResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | Information about the device pools.
    devicePools :: Core.Maybe [DevicePool],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDevicePoolsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDevicePoolsResponse_nextToken' - If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
--
-- 'devicePools', 'listDevicePoolsResponse_devicePools' - Information about the device pools.
--
-- 'httpStatus', 'listDevicePoolsResponse_httpStatus' - The response's http status code.
newListDevicePoolsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDevicePoolsResponse
newListDevicePoolsResponse pHttpStatus_ =
  ListDevicePoolsResponse'
    { nextToken = Core.Nothing,
      devicePools = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listDevicePoolsResponse_nextToken :: Lens.Lens' ListDevicePoolsResponse (Core.Maybe Core.Text)
listDevicePoolsResponse_nextToken = Lens.lens (\ListDevicePoolsResponse' {nextToken} -> nextToken) (\s@ListDevicePoolsResponse' {} a -> s {nextToken = a} :: ListDevicePoolsResponse)

-- | Information about the device pools.
listDevicePoolsResponse_devicePools :: Lens.Lens' ListDevicePoolsResponse (Core.Maybe [DevicePool])
listDevicePoolsResponse_devicePools = Lens.lens (\ListDevicePoolsResponse' {devicePools} -> devicePools) (\s@ListDevicePoolsResponse' {} a -> s {devicePools = a} :: ListDevicePoolsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDevicePoolsResponse_httpStatus :: Lens.Lens' ListDevicePoolsResponse Core.Int
listDevicePoolsResponse_httpStatus = Lens.lens (\ListDevicePoolsResponse' {httpStatus} -> httpStatus) (\s@ListDevicePoolsResponse' {} a -> s {httpStatus = a} :: ListDevicePoolsResponse)

instance Core.NFData ListDevicePoolsResponse
