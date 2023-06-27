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
-- Module      : Amazonka.IoTWireless.GetWirelessGatewayStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets operating information about a wireless gateway.
module Amazonka.IoTWireless.GetWirelessGatewayStatistics
  ( -- * Creating a Request
    GetWirelessGatewayStatistics (..),
    newGetWirelessGatewayStatistics,

    -- * Request Lenses
    getWirelessGatewayStatistics_wirelessGatewayId,

    -- * Destructuring the Response
    GetWirelessGatewayStatisticsResponse (..),
    newGetWirelessGatewayStatisticsResponse,

    -- * Response Lenses
    getWirelessGatewayStatisticsResponse_connectionStatus,
    getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt,
    getWirelessGatewayStatisticsResponse_wirelessGatewayId,
    getWirelessGatewayStatisticsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetWirelessGatewayStatistics' smart constructor.
data GetWirelessGatewayStatistics = GetWirelessGatewayStatistics'
  { -- | The ID of the wireless gateway for which to get the data.
    wirelessGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessGatewayStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'wirelessGatewayId', 'getWirelessGatewayStatistics_wirelessGatewayId' - The ID of the wireless gateway for which to get the data.
newGetWirelessGatewayStatistics ::
  -- | 'wirelessGatewayId'
  Prelude.Text ->
  GetWirelessGatewayStatistics
newGetWirelessGatewayStatistics pWirelessGatewayId_ =
  GetWirelessGatewayStatistics'
    { wirelessGatewayId =
        pWirelessGatewayId_
    }

-- | The ID of the wireless gateway for which to get the data.
getWirelessGatewayStatistics_wirelessGatewayId :: Lens.Lens' GetWirelessGatewayStatistics Prelude.Text
getWirelessGatewayStatistics_wirelessGatewayId = Lens.lens (\GetWirelessGatewayStatistics' {wirelessGatewayId} -> wirelessGatewayId) (\s@GetWirelessGatewayStatistics' {} a -> s {wirelessGatewayId = a} :: GetWirelessGatewayStatistics)

instance Core.AWSRequest GetWirelessGatewayStatistics where
  type
    AWSResponse GetWirelessGatewayStatistics =
      GetWirelessGatewayStatisticsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetWirelessGatewayStatisticsResponse'
            Prelude.<$> (x Data..?> "ConnectionStatus")
            Prelude.<*> (x Data..?> "LastUplinkReceivedAt")
            Prelude.<*> (x Data..?> "WirelessGatewayId")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetWirelessGatewayStatistics
  where
  hashWithSalt _salt GetWirelessGatewayStatistics' {..} =
    _salt `Prelude.hashWithSalt` wirelessGatewayId

instance Prelude.NFData GetWirelessGatewayStatistics where
  rnf GetWirelessGatewayStatistics' {..} =
    Prelude.rnf wirelessGatewayId

instance Data.ToHeaders GetWirelessGatewayStatistics where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetWirelessGatewayStatistics where
  toPath GetWirelessGatewayStatistics' {..} =
    Prelude.mconcat
      [ "/wireless-gateways/",
        Data.toBS wirelessGatewayId,
        "/statistics"
      ]

instance Data.ToQuery GetWirelessGatewayStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWirelessGatewayStatisticsResponse' smart constructor.
data GetWirelessGatewayStatisticsResponse = GetWirelessGatewayStatisticsResponse'
  { -- | The connection status of the wireless gateway.
    connectionStatus :: Prelude.Maybe ConnectionStatus,
    -- | The date and time when the most recent uplink was received.
    --
    -- This value is only valid for 3 months.
    lastUplinkReceivedAt :: Prelude.Maybe Prelude.Text,
    -- | The ID of the wireless gateway.
    wirelessGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetWirelessGatewayStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionStatus', 'getWirelessGatewayStatisticsResponse_connectionStatus' - The connection status of the wireless gateway.
--
-- 'lastUplinkReceivedAt', 'getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt' - The date and time when the most recent uplink was received.
--
-- This value is only valid for 3 months.
--
-- 'wirelessGatewayId', 'getWirelessGatewayStatisticsResponse_wirelessGatewayId' - The ID of the wireless gateway.
--
-- 'httpStatus', 'getWirelessGatewayStatisticsResponse_httpStatus' - The response's http status code.
newGetWirelessGatewayStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWirelessGatewayStatisticsResponse
newGetWirelessGatewayStatisticsResponse pHttpStatus_ =
  GetWirelessGatewayStatisticsResponse'
    { connectionStatus =
        Prelude.Nothing,
      lastUplinkReceivedAt =
        Prelude.Nothing,
      wirelessGatewayId = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The connection status of the wireless gateway.
getWirelessGatewayStatisticsResponse_connectionStatus :: Lens.Lens' GetWirelessGatewayStatisticsResponse (Prelude.Maybe ConnectionStatus)
getWirelessGatewayStatisticsResponse_connectionStatus = Lens.lens (\GetWirelessGatewayStatisticsResponse' {connectionStatus} -> connectionStatus) (\s@GetWirelessGatewayStatisticsResponse' {} a -> s {connectionStatus = a} :: GetWirelessGatewayStatisticsResponse)

-- | The date and time when the most recent uplink was received.
--
-- This value is only valid for 3 months.
getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt :: Lens.Lens' GetWirelessGatewayStatisticsResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt = Lens.lens (\GetWirelessGatewayStatisticsResponse' {lastUplinkReceivedAt} -> lastUplinkReceivedAt) (\s@GetWirelessGatewayStatisticsResponse' {} a -> s {lastUplinkReceivedAt = a} :: GetWirelessGatewayStatisticsResponse)

-- | The ID of the wireless gateway.
getWirelessGatewayStatisticsResponse_wirelessGatewayId :: Lens.Lens' GetWirelessGatewayStatisticsResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayStatisticsResponse_wirelessGatewayId = Lens.lens (\GetWirelessGatewayStatisticsResponse' {wirelessGatewayId} -> wirelessGatewayId) (\s@GetWirelessGatewayStatisticsResponse' {} a -> s {wirelessGatewayId = a} :: GetWirelessGatewayStatisticsResponse)

-- | The response's http status code.
getWirelessGatewayStatisticsResponse_httpStatus :: Lens.Lens' GetWirelessGatewayStatisticsResponse Prelude.Int
getWirelessGatewayStatisticsResponse_httpStatus = Lens.lens (\GetWirelessGatewayStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetWirelessGatewayStatisticsResponse' {} a -> s {httpStatus = a} :: GetWirelessGatewayStatisticsResponse)

instance
  Prelude.NFData
    GetWirelessGatewayStatisticsResponse
  where
  rnf GetWirelessGatewayStatisticsResponse' {..} =
    Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf lastUplinkReceivedAt
      `Prelude.seq` Prelude.rnf wirelessGatewayId
      `Prelude.seq` Prelude.rnf httpStatus
