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
-- Module      : Amazonka.IoT1ClickDevices.ListDevices
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the 1-Click compatible devices associated with your AWS account.
--
-- This operation returns paginated results.
module Amazonka.IoT1ClickDevices.ListDevices
  ( -- * Creating a Request
    ListDevices (..),
    newListDevices,

    -- * Request Lenses
    listDevices_deviceType,
    listDevices_maxResults,
    listDevices_nextToken,

    -- * Destructuring the Response
    ListDevicesResponse (..),
    newListDevicesResponse,

    -- * Response Lenses
    listDevicesResponse_devices,
    listDevicesResponse_nextToken,
    listDevicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoT1ClickDevices.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListDevices' smart constructor.
data ListDevices = ListDevices'
  { -- | The type of the device, such as \"button\".
    deviceType :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return per request. If not set, a
    -- default value of 100 is used.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token to retrieve the next set of results.
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
-- 'deviceType', 'listDevices_deviceType' - The type of the device, such as \"button\".
--
-- 'maxResults', 'listDevices_maxResults' - The maximum number of results to return per request. If not set, a
-- default value of 100 is used.
--
-- 'nextToken', 'listDevices_nextToken' - The token to retrieve the next set of results.
newListDevices ::
  ListDevices
newListDevices =
  ListDevices'
    { deviceType = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The type of the device, such as \"button\".
listDevices_deviceType :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Text)
listDevices_deviceType = Lens.lens (\ListDevices' {deviceType} -> deviceType) (\s@ListDevices' {} a -> s {deviceType = a} :: ListDevices)

-- | The maximum number of results to return per request. If not set, a
-- default value of 100 is used.
listDevices_maxResults :: Lens.Lens' ListDevices (Prelude.Maybe Prelude.Natural)
listDevices_maxResults = Lens.lens (\ListDevices' {maxResults} -> maxResults) (\s@ListDevices' {} a -> s {maxResults = a} :: ListDevices)

-- | The token to retrieve the next set of results.
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
        ( rs
            Lens.^? listDevicesResponse_devices
            Prelude.. Lens._Just
        ) =
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
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDevicesResponse'
            Prelude.<$> (x Data..?> "devices" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListDevices where
  hashWithSalt _salt ListDevices' {..} =
    _salt
      `Prelude.hashWithSalt` deviceType
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListDevices where
  rnf ListDevices' {..} =
    Prelude.rnf deviceType
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListDevices where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListDevices where
  toPath = Prelude.const "/devices"

instance Data.ToQuery ListDevices where
  toQuery ListDevices' {..} =
    Prelude.mconcat
      [ "deviceType" Data.=: deviceType,
        "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListDevicesResponse' smart constructor.
data ListDevicesResponse = ListDevicesResponse'
  { -- | A list of devices.
    devices :: Prelude.Maybe [DeviceDescription],
    -- | The token to retrieve the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'devices', 'listDevicesResponse_devices' - A list of devices.
--
-- 'nextToken', 'listDevicesResponse_nextToken' - The token to retrieve the next set of results.
--
-- 'httpStatus', 'listDevicesResponse_httpStatus' - The response's http status code.
newListDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListDevicesResponse
newListDevicesResponse pHttpStatus_ =
  ListDevicesResponse'
    { devices = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of devices.
listDevicesResponse_devices :: Lens.Lens' ListDevicesResponse (Prelude.Maybe [DeviceDescription])
listDevicesResponse_devices = Lens.lens (\ListDevicesResponse' {devices} -> devices) (\s@ListDevicesResponse' {} a -> s {devices = a} :: ListDevicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to retrieve the next set of results.
listDevicesResponse_nextToken :: Lens.Lens' ListDevicesResponse (Prelude.Maybe Prelude.Text)
listDevicesResponse_nextToken = Lens.lens (\ListDevicesResponse' {nextToken} -> nextToken) (\s@ListDevicesResponse' {} a -> s {nextToken = a} :: ListDevicesResponse)

-- | The response's http status code.
listDevicesResponse_httpStatus :: Lens.Lens' ListDevicesResponse Prelude.Int
listDevicesResponse_httpStatus = Lens.lens (\ListDevicesResponse' {httpStatus} -> httpStatus) (\s@ListDevicesResponse' {} a -> s {httpStatus = a} :: ListDevicesResponse)

instance Prelude.NFData ListDevicesResponse where
  rnf ListDevicesResponse' {..} =
    Prelude.rnf devices
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
