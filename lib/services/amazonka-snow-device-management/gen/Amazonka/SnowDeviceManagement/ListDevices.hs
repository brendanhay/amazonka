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
-- Module      : Amazonka.SnowDeviceManagement.ListDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all devices on your Amazon Web Services account that
-- have Amazon Web Services Snow Device Management enabled in the Amazon
-- Web Services Region where the command is run.
--
-- This operation returns paginated results.
module Amazonka.SnowDeviceManagement.ListDevices
  ( -- * Creating a Request
    ListDevices (..),
    newListDevices,

    -- * Request Lenses
    listDevices_jobId,
    listDevices_nextToken,
    listDevices_maxResults,

    -- * Destructuring the Response
    ListDevicesResponse (..),
    newListDevicesResponse,

    -- * Response Lenses
    listDevicesResponse_nextToken,
    listDevicesResponse_devices,
    listDevicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SnowDeviceManagement.Types

-- | /See:/ 'newListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | The ID of the job used to order the device.
    jobId :: Prelude.Maybe Prelude.Text,
    -- | A pagination token to continue to the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of devices to list per page.
    maxResults :: Prelude.Maybe Prelude.Natural
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
-- 'jobId', 'listDevices_jobId' - The ID of the job used to order the device.
--
-- 'nextToken', 'listDevices_nextToken' - A pagination token to continue to the next page of results.
--
-- 'maxResults', 'listDevices_maxResults' - The maximum number of devices to list per page.
newListDevices ::
  ListDevices
newListDevices =
  ListDevices'
    { jobId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The ID of the job used to order the device.
listDevices_jobId :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_jobId = Lens.lens (\ListDevices' {jobId} -> jobId) (\s@ListDevices' {} a -> s {jobId = a} :: ListDevices)

-- | A pagination token to continue to the next page of results.
listDevices_nextToken :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_nextToken = Lens.lens (\ListDevices' {nextToken} -> nextToken) (\s@ListDevices' {} a -> s {nextToken = a} :: ListDevices)

-- | The maximum number of devices to list per page.
listDevices_maxResults :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Natural)
listDevices_maxResults = Lens.lens (\ListDevices' {maxResults} -> maxResults) (\s@ListDevices' {} a -> s {maxResults = a} :: ListDevices)

instance Core.AWSPager ListDevices where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDevicesResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listDevicesResponse_devices Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDevices_nextToken
          Lens..~ rs
          Lens.^? listDevicesResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListDevices where
  type AWSResponse ListDevices = ListDevicesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "devices" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDevices where
  hashWithSalt salt' ListDevices' {..} =
    salt' `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` jobId

instance Prelude.NFData ListDevices where
  rnf ListDevices' {..} =
    Prelude.rnf jobId
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Core.ToHeaders ListDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath ListDevices where
  toPath = Prelude.const "/managed-devices"

instance Core.ToQuery ListDevices where
  toQuery ListDevices' {..} =
    Prelude.mconcat
      [ "jobId" Core.=: jobId,
        "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { -- | A pagination token to continue to the next page of devices.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of device structures that contain information about the device.
    devices :: Prelude.Maybe [DeviceSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
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
-- 'nextToken', 'listDevicesResponse_nextToken' - A pagination token to continue to the next page of devices.
--
-- 'devices', 'listDevicesResponse_devices' - A list of device structures that contain information about the device.
--
-- 'httpStatus', 'listDevicesResponse_httpStatus' - The response's http status code.
newListDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDevicesResponse
newListDevicesResponse pHttpStatus_ =
  ListDevicesResponse'
    { nextToken = Prelude.Nothing,
      devices = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A pagination token to continue to the next page of devices.
listDevicesResponse_nextToken :: Lens.Lens' ListDevicesResponse (Prelude.Maybe Prelude.Text)
listDevicesResponse_nextToken = Lens.lens (\ListDevicesResponse' {nextToken} -> nextToken) (\s@ListDevicesResponse' {} a -> s {nextToken = a} :: ListDevicesResponse)

-- | A list of device structures that contain information about the device.
listDevicesResponse_devices :: Lens.Lens' ListDevicesResponse (Prelude.Maybe [DeviceSummary])
listDevicesResponse_devices = Lens.lens (\ListDevicesResponse' {devices} -> devices) (\s@ListDevicesResponse' {} a -> s {devices = a} :: ListDevicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listDevicesResponse_httpStatus :: Lens.Lens' ListDevicesResponse Prelude.Int
listDevicesResponse_httpStatus = Lens.lens (\ListDevicesResponse' {httpStatus} -> httpStatus) (\s@ListDevicesResponse' {} a -> s {httpStatus = a} :: ListDevicesResponse)

instance Prelude.NFData ListDevicesResponse where
  rnf ListDevicesResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf devices
