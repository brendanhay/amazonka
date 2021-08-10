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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the result of a list device pools request.
--
-- /See:/ 'newListDevicePools' smart constructor.
data ListDevicePools = ListDevicePools'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The device pools\' type.
    --
    -- Allowed values include:
    --
    -- -   CURATED: A device pool that is created and managed by AWS Device
    --     Farm.
    --
    -- -   PRIVATE: A device pool that is created and managed by the device
    --     pool developer.
    type' :: Prelude.Maybe DevicePoolType,
    -- | The project ARN.
    arn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  ListDevicePools
newListDevicePools pArn_ =
  ListDevicePools'
    { nextToken = Prelude.Nothing,
      type' = Prelude.Nothing,
      arn = pArn_
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listDevicePools_nextToken :: Lens.Lens' ListDevicePools (Prelude.Maybe Prelude.Text)
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
listDevicePools_type :: Lens.Lens' ListDevicePools (Prelude.Maybe DevicePoolType)
listDevicePools_type = Lens.lens (\ListDevicePools' {type'} -> type') (\s@ListDevicePools' {} a -> s {type' = a} :: ListDevicePools)

-- | The project ARN.
listDevicePools_arn :: Lens.Lens' ListDevicePools Prelude.Text
listDevicePools_arn = Lens.lens (\ListDevicePools' {arn} -> arn) (\s@ListDevicePools' {} a -> s {arn = a} :: ListDevicePools)

instance Core.AWSPager ListDevicePools where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDevicePoolsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDevicePoolsResponse_devicePools
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDevicePools_nextToken
          Lens..~ rs
          Lens.^? listDevicePoolsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListDevicePools where
  type
    AWSResponse ListDevicePools =
      ListDevicePoolsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicePoolsResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "devicePools" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDevicePools

instance Prelude.NFData ListDevicePools

instance Core.ToHeaders ListDevicePools where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListDevicePools" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDevicePools where
  toJSON ListDevicePools' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("type" Core..=) Prelude.<$> type',
            Prelude.Just ("arn" Core..= arn)
          ]
      )

instance Core.ToPath ListDevicePools where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDevicePools where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the result of a list device pools request.
--
-- /See:/ 'newListDevicePoolsResponse' smart constructor.
data ListDevicePoolsResponse = ListDevicePoolsResponse'
  { -- | If the number of items that are returned is significantly large, this is
    -- an identifier that is also returned. It can be used in a subsequent call
    -- to this operation to return the next set of items in the list.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the device pools.
    devicePools :: Prelude.Maybe [DevicePool],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListDevicePoolsResponse
newListDevicePoolsResponse pHttpStatus_ =
  ListDevicePoolsResponse'
    { nextToken =
        Prelude.Nothing,
      devicePools = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the number of items that are returned is significantly large, this is
-- an identifier that is also returned. It can be used in a subsequent call
-- to this operation to return the next set of items in the list.
listDevicePoolsResponse_nextToken :: Lens.Lens' ListDevicePoolsResponse (Prelude.Maybe Prelude.Text)
listDevicePoolsResponse_nextToken = Lens.lens (\ListDevicePoolsResponse' {nextToken} -> nextToken) (\s@ListDevicePoolsResponse' {} a -> s {nextToken = a} :: ListDevicePoolsResponse)

-- | Information about the device pools.
listDevicePoolsResponse_devicePools :: Lens.Lens' ListDevicePoolsResponse (Prelude.Maybe [DevicePool])
listDevicePoolsResponse_devicePools = Lens.lens (\ListDevicePoolsResponse' {devicePools} -> devicePools) (\s@ListDevicePoolsResponse' {} a -> s {devicePools = a} :: ListDevicePoolsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDevicePoolsResponse_httpStatus :: Lens.Lens' ListDevicePoolsResponse Prelude.Int
listDevicePoolsResponse_httpStatus = Lens.lens (\ListDevicePoolsResponse' {httpStatus} -> httpStatus) (\s@ListDevicePoolsResponse' {} a -> s {httpStatus = a} :: ListDevicePoolsResponse)

instance Prelude.NFData ListDevicePoolsResponse
