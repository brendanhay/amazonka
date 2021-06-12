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
-- Module      : Network.AWS.DeviceFarm.ListDeviceInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the private device instances associated with
-- one or more AWS accounts.
--
-- This operation returns paginated results.
module Network.AWS.DeviceFarm.ListDeviceInstances
  ( -- * Creating a Request
    ListDeviceInstances (..),
    newListDeviceInstances,

    -- * Request Lenses
    listDeviceInstances_nextToken,
    listDeviceInstances_maxResults,

    -- * Destructuring the Response
    ListDeviceInstancesResponse (..),
    newListDeviceInstancesResponse,

    -- * Response Lenses
    listDeviceInstancesResponse_nextToken,
    listDeviceInstancesResponse_deviceInstances,
    listDeviceInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DeviceFarm.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListDeviceInstances' smart constructor.
data ListDeviceInstances = ListDeviceInstances'
  { -- | An identifier that was returned from the previous call to this
    -- operation, which can be used to return the next set of items in the
    -- list.
    nextToken :: Core.Maybe Core.Text,
    -- | An integer that specifies the maximum number of items you want to return
    -- in the API response.
    maxResults :: Core.Maybe Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeviceInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceInstances_nextToken' - An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
--
-- 'maxResults', 'listDeviceInstances_maxResults' - An integer that specifies the maximum number of items you want to return
-- in the API response.
newListDeviceInstances ::
  ListDeviceInstances
newListDeviceInstances =
  ListDeviceInstances'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | An identifier that was returned from the previous call to this
-- operation, which can be used to return the next set of items in the
-- list.
listDeviceInstances_nextToken :: Lens.Lens' ListDeviceInstances (Core.Maybe Core.Text)
listDeviceInstances_nextToken = Lens.lens (\ListDeviceInstances' {nextToken} -> nextToken) (\s@ListDeviceInstances' {} a -> s {nextToken = a} :: ListDeviceInstances)

-- | An integer that specifies the maximum number of items you want to return
-- in the API response.
listDeviceInstances_maxResults :: Lens.Lens' ListDeviceInstances (Core.Maybe Core.Int)
listDeviceInstances_maxResults = Lens.lens (\ListDeviceInstances' {maxResults} -> maxResults) (\s@ListDeviceInstances' {} a -> s {maxResults = a} :: ListDeviceInstances)

instance Core.AWSPager ListDeviceInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDeviceInstancesResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listDeviceInstancesResponse_deviceInstances
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDeviceInstances_nextToken
          Lens..~ rs
          Lens.^? listDeviceInstancesResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListDeviceInstances where
  type
    AWSResponse ListDeviceInstances =
      ListDeviceInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDeviceInstancesResponse'
            Core.<$> (x Core..?> "nextToken")
            Core.<*> (x Core..?> "deviceInstances" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListDeviceInstances

instance Core.NFData ListDeviceInstances

instance Core.ToHeaders ListDeviceInstances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "DeviceFarm_20150623.ListDeviceInstances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDeviceInstances where
  toJSON ListDeviceInstances' {..} =
    Core.object
      ( Core.catMaybes
          [ ("nextToken" Core..=) Core.<$> nextToken,
            ("maxResults" Core..=) Core.<$> maxResults
          ]
      )

instance Core.ToPath ListDeviceInstances where
  toPath = Core.const "/"

instance Core.ToQuery ListDeviceInstances where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDeviceInstancesResponse' smart constructor.
data ListDeviceInstancesResponse = ListDeviceInstancesResponse'
  { -- | An identifier that can be used in the next call to this operation to
    -- return the next set of items in the list.
    nextToken :: Core.Maybe Core.Text,
    -- | An object that contains information about your device instances.
    deviceInstances :: Core.Maybe [DeviceInstance],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDeviceInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDeviceInstancesResponse_nextToken' - An identifier that can be used in the next call to this operation to
-- return the next set of items in the list.
--
-- 'deviceInstances', 'listDeviceInstancesResponse_deviceInstances' - An object that contains information about your device instances.
--
-- 'httpStatus', 'listDeviceInstancesResponse_httpStatus' - The response's http status code.
newListDeviceInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDeviceInstancesResponse
newListDeviceInstancesResponse pHttpStatus_ =
  ListDeviceInstancesResponse'
    { nextToken =
        Core.Nothing,
      deviceInstances = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An identifier that can be used in the next call to this operation to
-- return the next set of items in the list.
listDeviceInstancesResponse_nextToken :: Lens.Lens' ListDeviceInstancesResponse (Core.Maybe Core.Text)
listDeviceInstancesResponse_nextToken = Lens.lens (\ListDeviceInstancesResponse' {nextToken} -> nextToken) (\s@ListDeviceInstancesResponse' {} a -> s {nextToken = a} :: ListDeviceInstancesResponse)

-- | An object that contains information about your device instances.
listDeviceInstancesResponse_deviceInstances :: Lens.Lens' ListDeviceInstancesResponse (Core.Maybe [DeviceInstance])
listDeviceInstancesResponse_deviceInstances = Lens.lens (\ListDeviceInstancesResponse' {deviceInstances} -> deviceInstances) (\s@ListDeviceInstancesResponse' {} a -> s {deviceInstances = a} :: ListDeviceInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listDeviceInstancesResponse_httpStatus :: Lens.Lens' ListDeviceInstancesResponse Core.Int
listDeviceInstancesResponse_httpStatus = Lens.lens (\ListDeviceInstancesResponse' {httpStatus} -> httpStatus) (\s@ListDeviceInstancesResponse' {} a -> s {httpStatus = a} :: ListDeviceInstancesResponse)

instance Core.NFData ListDeviceInstancesResponse
