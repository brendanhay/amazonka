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
-- Module      : Network.AWS.SageMaker.ListDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of devices.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListDevices
  ( -- * Creating a Request
    ListDevices (..),
    newListDevices,

    -- * Request Lenses
    listDevices_latestHeartbeatAfter,
    listDevices_nextToken,
    listDevices_deviceFleetName,
    listDevices_maxResults,
    listDevices_modelName,

    -- * Destructuring the Response
    ListDevicesResponse (..),
    newListDevicesResponse,

    -- * Response Lenses
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
    listDevicesResponse_deviceSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | Select fleets where the job was updated after X
    latestHeartbeatAfter :: Core.Maybe Core.POSIX,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Core.Maybe Core.Text,
    -- | Filter for fleets containing this name in their device fleet name.
    deviceFleetName :: Core.Maybe Core.Text,
    -- | Maximum number of results to select.
    maxResults :: Core.Maybe Core.Int,
    -- | A filter that searches devices that contains this name in any of their
    -- models.
    modelName :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'latestHeartbeatAfter', 'listDevices_latestHeartbeatAfter' - Select fleets where the job was updated after X
--
-- 'nextToken', 'listDevices_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'deviceFleetName', 'listDevices_deviceFleetName' - Filter for fleets containing this name in their device fleet name.
--
-- 'maxResults', 'listDevices_maxResults' - Maximum number of results to select.
--
-- 'modelName', 'listDevices_modelName' - A filter that searches devices that contains this name in any of their
-- models.
newListDevices ::
  ListDevices
newListDevices =
  ListDevices'
    { latestHeartbeatAfter = Core.Nothing,
      nextToken = Core.Nothing,
      deviceFleetName = Core.Nothing,
      maxResults = Core.Nothing,
      modelName = Core.Nothing
    }

-- | Select fleets where the job was updated after X
listDevices_latestHeartbeatAfter :: Lens.Lens' ListDevices (Core.Maybe Core.UTCTime)
listDevices_latestHeartbeatAfter = Lens.lens (\ListDevices' {latestHeartbeatAfter} -> latestHeartbeatAfter) (\s@ListDevices' {} a -> s {latestHeartbeatAfter = a} :: ListDevices) Core.. Lens.mapping Core._Time

-- | The response from the last list when returning a list large enough to
-- need tokening.
listDevices_nextToken :: Lens.Lens' ListDevices (Core.Maybe Core.Text)
listDevices_nextToken = Lens.lens (\ListDevices' {nextToken} -> nextToken) (\s@ListDevices' {} a -> s {nextToken = a} :: ListDevices)

-- | Filter for fleets containing this name in their device fleet name.
listDevices_deviceFleetName :: Lens.Lens' ListDevices (Core.Maybe Core.Text)
listDevices_deviceFleetName = Lens.lens (\ListDevices' {deviceFleetName} -> deviceFleetName) (\s@ListDevices' {} a -> s {deviceFleetName = a} :: ListDevices)

-- | Maximum number of results to select.
listDevices_maxResults :: Lens.Lens' ListDevices (Core.Maybe Core.Int)
listDevices_maxResults = Lens.lens (\ListDevices' {maxResults} -> maxResults) (\s@ListDevices' {} a -> s {maxResults = a} :: ListDevices)

-- | A filter that searches devices that contains this name in any of their
-- models.
listDevices_modelName :: Lens.Lens' ListDevices (Core.Maybe Core.Text)
listDevices_modelName = Lens.lens (\ListDevices' {modelName} -> modelName) (\s@ListDevices' {} a -> s {modelName = a} :: ListDevices)

instance Core.AWSPager ListDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDevicesResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listDevicesResponse_deviceSummaries) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDevices_nextToken
          Lens..~ rs
          Lens.^? listDevicesResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListDevices where
  type AWSResponse ListDevices = ListDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "DeviceSummaries" Core..!@ Core.mempty)
      )

instance Core.Hashable ListDevices

instance Core.NFData ListDevices

instance Core.ToHeaders ListDevices where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListDevices" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDevices where
  toJSON ListDevices' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LatestHeartbeatAfter" Core..=)
              Core.<$> latestHeartbeatAfter,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("DeviceFleetName" Core..=) Core.<$> deviceFleetName,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("ModelName" Core..=) Core.<$> modelName
          ]
      )

instance Core.ToPath ListDevices where
  toPath = Core.const "/"

instance Core.ToQuery ListDevices where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | Summary of devices.
    deviceSummaries :: [DeviceSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDevicesResponse_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'httpStatus', 'listDevicesResponse_httpStatus' - The response's http status code.
--
-- 'deviceSummaries', 'listDevicesResponse_deviceSummaries' - Summary of devices.
newListDevicesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDevicesResponse
newListDevicesResponse pHttpStatus_ =
  ListDevicesResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      deviceSummaries = Core.mempty
    }

-- | The response from the last list when returning a list large enough to
-- need tokening.
listDevicesResponse_nextToken :: Lens.Lens' ListDevicesResponse (Core.Maybe Core.Text)
listDevicesResponse_nextToken = Lens.lens (\ListDevicesResponse' {nextToken} -> nextToken) (\s@ListDevicesResponse' {} a -> s {nextToken = a} :: ListDevicesResponse)

-- | The response's http status code.
listDevicesResponse_httpStatus :: Lens.Lens' ListDevicesResponse Core.Int
listDevicesResponse_httpStatus = Lens.lens (\ListDevicesResponse' {httpStatus} -> httpStatus) (\s@ListDevicesResponse' {} a -> s {httpStatus = a} :: ListDevicesResponse)

-- | Summary of devices.
listDevicesResponse_deviceSummaries :: Lens.Lens' ListDevicesResponse [DeviceSummary]
listDevicesResponse_deviceSummaries = Lens.lens (\ListDevicesResponse' {deviceSummaries} -> deviceSummaries) (\s@ListDevicesResponse' {} a -> s {deviceSummaries = a} :: ListDevicesResponse) Core.. Lens._Coerce

instance Core.NFData ListDevicesResponse
