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
-- Module      : Amazonka.IoTWireless.ListWirelessDevices
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the wireless devices registered to your AWS account.
module Amazonka.IoTWireless.ListWirelessDevices
  ( -- * Creating a Request
    ListWirelessDevices (..),
    newListWirelessDevices,

    -- * Request Lenses
    listWirelessDevices_serviceProfileId,
    listWirelessDevices_deviceProfileId,
    listWirelessDevices_nextToken,
    listWirelessDevices_wirelessDeviceType,
    listWirelessDevices_destinationName,
    listWirelessDevices_maxResults,

    -- * Destructuring the Response
    ListWirelessDevicesResponse (..),
    newListWirelessDevicesResponse,

    -- * Response Lenses
    listWirelessDevicesResponse_wirelessDeviceList,
    listWirelessDevicesResponse_nextToken,
    listWirelessDevicesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import Amazonka.IoTWireless.Types
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListWirelessDevices' smart constructor.
data ListWirelessDevices = ListWirelessDevices'
  { -- | A filter to list only the wireless devices that use this service
    -- profile.
    serviceProfileId :: Prelude.Maybe Prelude.Text,
    -- | A filter to list only the wireless devices that use this device profile.
    deviceProfileId :: Prelude.Maybe Prelude.Text,
    -- | To retrieve the next set of results, the @nextToken@ value from a
    -- previous response; otherwise __null__ to receive the first set of
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter to list only the wireless devices that use this wireless device
    -- type.
    wirelessDeviceType :: Prelude.Maybe WirelessDeviceType,
    -- | A filter to list only the wireless devices that use this destination.
    destinationName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in this operation.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWirelessDevices' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceProfileId', 'listWirelessDevices_serviceProfileId' - A filter to list only the wireless devices that use this service
-- profile.
--
-- 'deviceProfileId', 'listWirelessDevices_deviceProfileId' - A filter to list only the wireless devices that use this device profile.
--
-- 'nextToken', 'listWirelessDevices_nextToken' - To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
--
-- 'wirelessDeviceType', 'listWirelessDevices_wirelessDeviceType' - A filter to list only the wireless devices that use this wireless device
-- type.
--
-- 'destinationName', 'listWirelessDevices_destinationName' - A filter to list only the wireless devices that use this destination.
--
-- 'maxResults', 'listWirelessDevices_maxResults' - The maximum number of results to return in this operation.
newListWirelessDevices ::
  ListWirelessDevices
newListWirelessDevices =
  ListWirelessDevices'
    { serviceProfileId =
        Prelude.Nothing,
      deviceProfileId = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      wirelessDeviceType = Prelude.Nothing,
      destinationName = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A filter to list only the wireless devices that use this service
-- profile.
listWirelessDevices_serviceProfileId :: Lens.Lens' ListWirelessDevices (Prelude.Maybe Prelude.Text)
listWirelessDevices_serviceProfileId = Lens.lens (\ListWirelessDevices' {serviceProfileId} -> serviceProfileId) (\s@ListWirelessDevices' {} a -> s {serviceProfileId = a} :: ListWirelessDevices)

-- | A filter to list only the wireless devices that use this device profile.
listWirelessDevices_deviceProfileId :: Lens.Lens' ListWirelessDevices (Prelude.Maybe Prelude.Text)
listWirelessDevices_deviceProfileId = Lens.lens (\ListWirelessDevices' {deviceProfileId} -> deviceProfileId) (\s@ListWirelessDevices' {} a -> s {deviceProfileId = a} :: ListWirelessDevices)

-- | To retrieve the next set of results, the @nextToken@ value from a
-- previous response; otherwise __null__ to receive the first set of
-- results.
listWirelessDevices_nextToken :: Lens.Lens' ListWirelessDevices (Prelude.Maybe Prelude.Text)
listWirelessDevices_nextToken = Lens.lens (\ListWirelessDevices' {nextToken} -> nextToken) (\s@ListWirelessDevices' {} a -> s {nextToken = a} :: ListWirelessDevices)

-- | A filter to list only the wireless devices that use this wireless device
-- type.
listWirelessDevices_wirelessDeviceType :: Lens.Lens' ListWirelessDevices (Prelude.Maybe WirelessDeviceType)
listWirelessDevices_wirelessDeviceType = Lens.lens (\ListWirelessDevices' {wirelessDeviceType} -> wirelessDeviceType) (\s@ListWirelessDevices' {} a -> s {wirelessDeviceType = a} :: ListWirelessDevices)

-- | A filter to list only the wireless devices that use this destination.
listWirelessDevices_destinationName :: Lens.Lens' ListWirelessDevices (Prelude.Maybe Prelude.Text)
listWirelessDevices_destinationName = Lens.lens (\ListWirelessDevices' {destinationName} -> destinationName) (\s@ListWirelessDevices' {} a -> s {destinationName = a} :: ListWirelessDevices)

-- | The maximum number of results to return in this operation.
listWirelessDevices_maxResults :: Lens.Lens' ListWirelessDevices (Prelude.Maybe Prelude.Natural)
listWirelessDevices_maxResults = Lens.lens (\ListWirelessDevices' {maxResults} -> maxResults) (\s@ListWirelessDevices' {} a -> s {maxResults = a} :: ListWirelessDevices)

instance Core.AWSRequest ListWirelessDevices where
  type
    AWSResponse ListWirelessDevices =
      ListWirelessDevicesResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListWirelessDevicesResponse'
            Prelude.<$> ( x Core..?> "WirelessDeviceList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListWirelessDevices where
  hashWithSalt _salt ListWirelessDevices' {..} =
    _salt `Prelude.hashWithSalt` serviceProfileId
      `Prelude.hashWithSalt` deviceProfileId
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` wirelessDeviceType
      `Prelude.hashWithSalt` destinationName
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData ListWirelessDevices where
  rnf ListWirelessDevices' {..} =
    Prelude.rnf serviceProfileId
      `Prelude.seq` Prelude.rnf deviceProfileId
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf wirelessDeviceType
      `Prelude.seq` Prelude.rnf destinationName
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders ListWirelessDevices where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath ListWirelessDevices where
  toPath = Prelude.const "/wireless-devices"

instance Core.ToQuery ListWirelessDevices where
  toQuery ListWirelessDevices' {..} =
    Prelude.mconcat
      [ "serviceProfileId" Core.=: serviceProfileId,
        "deviceProfileId" Core.=: deviceProfileId,
        "nextToken" Core.=: nextToken,
        "wirelessDeviceType" Core.=: wirelessDeviceType,
        "destinationName" Core.=: destinationName,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListWirelessDevicesResponse' smart constructor.
data ListWirelessDevicesResponse = ListWirelessDevicesResponse'
  { -- | The ID of the wireless device.
    wirelessDeviceList :: Prelude.Maybe [WirelessDeviceStatistics],
    -- | The token to use to get the next set of results, or __null__ if there
    -- are no additional results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListWirelessDevicesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wirelessDeviceList', 'listWirelessDevicesResponse_wirelessDeviceList' - The ID of the wireless device.
--
-- 'nextToken', 'listWirelessDevicesResponse_nextToken' - The token to use to get the next set of results, or __null__ if there
-- are no additional results.
--
-- 'httpStatus', 'listWirelessDevicesResponse_httpStatus' - The response's http status code.
newListWirelessDevicesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListWirelessDevicesResponse
newListWirelessDevicesResponse pHttpStatus_ =
  ListWirelessDevicesResponse'
    { wirelessDeviceList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the wireless device.
listWirelessDevicesResponse_wirelessDeviceList :: Lens.Lens' ListWirelessDevicesResponse (Prelude.Maybe [WirelessDeviceStatistics])
listWirelessDevicesResponse_wirelessDeviceList = Lens.lens (\ListWirelessDevicesResponse' {wirelessDeviceList} -> wirelessDeviceList) (\s@ListWirelessDevicesResponse' {} a -> s {wirelessDeviceList = a} :: ListWirelessDevicesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to get the next set of results, or __null__ if there
-- are no additional results.
listWirelessDevicesResponse_nextToken :: Lens.Lens' ListWirelessDevicesResponse (Prelude.Maybe Prelude.Text)
listWirelessDevicesResponse_nextToken = Lens.lens (\ListWirelessDevicesResponse' {nextToken} -> nextToken) (\s@ListWirelessDevicesResponse' {} a -> s {nextToken = a} :: ListWirelessDevicesResponse)

-- | The response's http status code.
listWirelessDevicesResponse_httpStatus :: Lens.Lens' ListWirelessDevicesResponse Prelude.Int
listWirelessDevicesResponse_httpStatus = Lens.lens (\ListWirelessDevicesResponse' {httpStatus} -> httpStatus) (\s@ListWirelessDevicesResponse' {} a -> s {httpStatus = a} :: ListWirelessDevicesResponse)

instance Prelude.NFData ListWirelessDevicesResponse where
  rnf ListWirelessDevicesResponse' {..} =
    Prelude.rnf wirelessDeviceList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
