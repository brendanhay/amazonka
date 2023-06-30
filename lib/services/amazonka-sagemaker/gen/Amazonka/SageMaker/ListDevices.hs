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
-- Module      : Amazonka.SageMaker.ListDevices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of devices.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListDevices
  ( -- * Creating a Request
    ListDevices (..),
    newListDevices,

    -- * Request Lenses
    listDevices_deviceFleetName,
    listDevices_latestHeartbeatAfter,
    listDevices_maxResults,
    listDevices_modelName,
    listDevices_nextToken,

    -- * Destructuring the Response
    ListDevicesResponse (..),
    newListDevicesResponse,

    -- * Response Lenses
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
    listDevicesResponse_deviceSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | Filter for fleets containing this name in their device fleet name.
    deviceFleetName :: Prelude.Maybe Prelude.Text,
    -- | Select fleets where the job was updated after X
    latestHeartbeatAfter :: Prelude.Maybe Data.POSIX,
    -- | Maximum number of results to select.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | A filter that searches devices that contains this name in any of their
    -- models.
    modelName :: Prelude.Maybe Prelude.Text,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text
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
-- 'deviceFleetName', 'listDevices_deviceFleetName' - Filter for fleets containing this name in their device fleet name.
--
-- 'latestHeartbeatAfter', 'listDevices_latestHeartbeatAfter' - Select fleets where the job was updated after X
--
-- 'maxResults', 'listDevices_maxResults' - Maximum number of results to select.
--
-- 'modelName', 'listDevices_modelName' - A filter that searches devices that contains this name in any of their
-- models.
--
-- 'nextToken', 'listDevices_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
newListDevices ::
  ListDevices
newListDevices =
  ListDevices'
    { deviceFleetName = Prelude.Nothing,
      latestHeartbeatAfter = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      modelName = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Filter for fleets containing this name in their device fleet name.
listDevices_deviceFleetName :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_deviceFleetName = Lens.lens (\ListDevices' {deviceFleetName} -> deviceFleetName) (\s@ListDevices' {} a -> s {deviceFleetName = a} :: ListDevices)

-- | Select fleets where the job was updated after X
listDevices_latestHeartbeatAfter :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.UTCTime)
listDevices_latestHeartbeatAfter = Lens.lens (\ListDevices' {latestHeartbeatAfter} -> latestHeartbeatAfter) (\s@ListDevices' {} a -> s {latestHeartbeatAfter = a} :: ListDevices) Prelude.. Lens.mapping Data._Time

-- | Maximum number of results to select.
listDevices_maxResults :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Int)
listDevices_maxResults = Lens.lens (\ListDevices' {maxResults} -> maxResults) (\s@ListDevices' {} a -> s {maxResults = a} :: ListDevices)

-- | A filter that searches devices that contains this name in any of their
-- models.
listDevices_modelName :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_modelName = Lens.lens (\ListDevices' {modelName} -> modelName) (\s@ListDevices' {} a -> s {modelName = a} :: ListDevices)

-- | The response from the last list when returning a list large enough to
-- need tokening.
listDevices_nextToken :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_nextToken = Lens.lens (\ListDevices' {nextToken} -> nextToken) (\s@ListDevices' {} a -> s {nextToken = a} :: ListDevices)

instance Core.AWSPager ListDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDevicesResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        (rs Lens.^. listDevicesResponse_deviceSummaries) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listDevices_nextToken
          Lens..~ rs
          Lens.^? listDevicesResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListDevices where
  type AWSResponse ListDevices = ListDevicesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "DeviceSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListDevices where
  hashWithSalt _salt ListDevices' {..} =
    _salt
      `Prelude.hashWithSalt` deviceFleetName
      `Prelude.hashWithSalt` latestHeartbeatAfter
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` modelName
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDevices where
  rnf ListDevices' {..} =
    Prelude.rnf deviceFleetName
      `Prelude.seq` Prelude.rnf latestHeartbeatAfter
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf modelName
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListDevices" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListDevices where
  toJSON ListDevices' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("DeviceFleetName" Data..=)
              Prelude.<$> deviceFleetName,
            ("LatestHeartbeatAfter" Data..=)
              Prelude.<$> latestHeartbeatAfter,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("ModelName" Data..=) Prelude.<$> modelName,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListDevices where
  toPath = Prelude.const "/"

instance Data.ToQuery ListDevices where
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
listDevicesResponse_deviceSummaries = Lens.lens (\ListDevicesResponse' {deviceSummaries} -> deviceSummaries) (\s@ListDevicesResponse' {} a -> s {deviceSummaries = a} :: ListDevicesResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListDevicesResponse where
  rnf ListDevicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf deviceSummaries
