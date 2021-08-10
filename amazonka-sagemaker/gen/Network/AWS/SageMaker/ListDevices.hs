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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | Select fleets where the job was updated after X
    latestHeartbeatAfter :: Prelude.Maybe Core.POSIX,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Filter for fleets containing this name in their device fleet name.
    deviceFleetName :: Prelude.Maybe Prelude.Text,
    -- | Maximum number of results to select.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A filter that searches devices that contains this name in any of their
    -- models.
    modelName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
    { latestHeartbeatAfter =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      deviceFleetName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modelName = Prelude.Nothing
    }

-- | Select fleets where the job was updated after X
listDevices_latestHeartbeatAfter :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.UTCTime)
listDevices_latestHeartbeatAfter = Lens.lens (\ListDevices' {latestHeartbeatAfter} -> latestHeartbeatAfter) (\s@ListDevices' {} a -> s {latestHeartbeatAfter = a} :: ListDevices) Prelude.. Lens.mapping Core._Time

-- | The response from the last list when returning a list large enough to
-- need tokening.
listDevices_nextToken :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_nextToken = Lens.lens (\ListDevices' {nextToken} -> nextToken) (\s@ListDevices' {} a -> s {nextToken = a} :: ListDevices)

-- | Filter for fleets containing this name in their device fleet name.
listDevices_deviceFleetName :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_deviceFleetName = Lens.lens (\ListDevices' {deviceFleetName} -> deviceFleetName) (\s@ListDevices' {} a -> s {deviceFleetName = a} :: ListDevices)

-- | Maximum number of results to select.
listDevices_maxResults :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Int)
listDevices_maxResults = Lens.lens (\ListDevices' {maxResults} -> maxResults) (\s@ListDevices' {} a -> s {maxResults = a} :: ListDevices)

-- | A filter that searches devices that contains this name in any of their
-- models.
listDevices_modelName :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_modelName = Lens.lens (\ListDevices' {modelName} -> modelName) (\s@ListDevices' {} a -> s {modelName = a} :: ListDevices)

instance Core.AWSPager ListDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDevicesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listDevicesResponse_deviceSummaries) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDevices_nextToken
          Lens..~ rs
          Lens.^? listDevicesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDevices where
  type AWSResponse ListDevices = ListDevicesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "DeviceSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListDevices

instance Prelude.NFData ListDevices

instance Core.ToHeaders ListDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListDevices" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDevices where
  toJSON ListDevices' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("LatestHeartbeatAfter" Core..=)
              Prelude.<$> latestHeartbeatAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("DeviceFleetName" Core..=)
              Prelude.<$> deviceFleetName,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("ModelName" Core..=) Prelude.<$> modelName
          ]
      )

instance Core.ToPath ListDevices where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDevices where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | Summary of devices.
    deviceSummaries :: [DeviceSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListDevicesResponse
newListDevicesResponse pHttpStatus_ =
  ListDevicesResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      deviceSummaries = Prelude.mempty
    }

-- | The response from the last list when returning a list large enough to
-- need tokening.
listDevicesResponse_nextToken :: Lens.Lens' ListDevicesResponse (Prelude.Maybe Prelude.Text)
listDevicesResponse_nextToken = Lens.lens (\ListDevicesResponse' {nextToken} -> nextToken) (\s@ListDevicesResponse' {} a -> s {nextToken = a} :: ListDevicesResponse)

-- | The response's http status code.
listDevicesResponse_httpStatus :: Lens.Lens' ListDevicesResponse Prelude.Int
listDevicesResponse_httpStatus = Lens.lens (\ListDevicesResponse' {httpStatus} -> httpStatus) (\s@ListDevicesResponse' {} a -> s {httpStatus = a} :: ListDevicesResponse)

-- | Summary of devices.
listDevicesResponse_deviceSummaries :: Lens.Lens' ListDevicesResponse [DeviceSummary]
listDevicesResponse_deviceSummaries = Lens.lens (\ListDevicesResponse' {deviceSummaries} -> deviceSummaries) (\s@ListDevicesResponse' {} a -> s {deviceSummaries = a} :: ListDevicesResponse) Prelude.. Lens._Coerce

instance Prelude.NFData ListDevicesResponse
