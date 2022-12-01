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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt,
    getWirelessGatewayStatisticsResponse_wirelessGatewayId,
    getWirelessGatewayStatisticsResponse_connectionStatus,
    getWirelessGatewayStatisticsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
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
            Prelude.<$> (x Core..?> "LastUplinkReceivedAt")
            Prelude.<*> (x Core..?> "WirelessGatewayId")
            Prelude.<*> (x Core..?> "ConnectionStatus")
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

instance Core.ToHeaders GetWirelessGatewayStatistics where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetWirelessGatewayStatistics where
  toPath GetWirelessGatewayStatistics' {..} =
    Prelude.mconcat
      [ "/wireless-gateways/",
        Core.toBS wirelessGatewayId,
        "/statistics"
      ]

instance Core.ToQuery GetWirelessGatewayStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetWirelessGatewayStatisticsResponse' smart constructor.
data GetWirelessGatewayStatisticsResponse = GetWirelessGatewayStatisticsResponse'
  { -- | The date and time when the most recent uplink was received.
    lastUplinkReceivedAt :: Prelude.Maybe Prelude.Text,
    -- | The ID of the wireless gateway.
    wirelessGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The connection status of the wireless gateway.
    connectionStatus :: Prelude.Maybe ConnectionStatus,
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
-- 'lastUplinkReceivedAt', 'getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt' - The date and time when the most recent uplink was received.
--
-- 'wirelessGatewayId', 'getWirelessGatewayStatisticsResponse_wirelessGatewayId' - The ID of the wireless gateway.
--
-- 'connectionStatus', 'getWirelessGatewayStatisticsResponse_connectionStatus' - The connection status of the wireless gateway.
--
-- 'httpStatus', 'getWirelessGatewayStatisticsResponse_httpStatus' - The response's http status code.
newGetWirelessGatewayStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetWirelessGatewayStatisticsResponse
newGetWirelessGatewayStatisticsResponse pHttpStatus_ =
  GetWirelessGatewayStatisticsResponse'
    { lastUplinkReceivedAt =
        Prelude.Nothing,
      wirelessGatewayId = Prelude.Nothing,
      connectionStatus = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The date and time when the most recent uplink was received.
getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt :: Lens.Lens' GetWirelessGatewayStatisticsResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayStatisticsResponse_lastUplinkReceivedAt = Lens.lens (\GetWirelessGatewayStatisticsResponse' {lastUplinkReceivedAt} -> lastUplinkReceivedAt) (\s@GetWirelessGatewayStatisticsResponse' {} a -> s {lastUplinkReceivedAt = a} :: GetWirelessGatewayStatisticsResponse)

-- | The ID of the wireless gateway.
getWirelessGatewayStatisticsResponse_wirelessGatewayId :: Lens.Lens' GetWirelessGatewayStatisticsResponse (Prelude.Maybe Prelude.Text)
getWirelessGatewayStatisticsResponse_wirelessGatewayId = Lens.lens (\GetWirelessGatewayStatisticsResponse' {wirelessGatewayId} -> wirelessGatewayId) (\s@GetWirelessGatewayStatisticsResponse' {} a -> s {wirelessGatewayId = a} :: GetWirelessGatewayStatisticsResponse)

-- | The connection status of the wireless gateway.
getWirelessGatewayStatisticsResponse_connectionStatus :: Lens.Lens' GetWirelessGatewayStatisticsResponse (Prelude.Maybe ConnectionStatus)
getWirelessGatewayStatisticsResponse_connectionStatus = Lens.lens (\GetWirelessGatewayStatisticsResponse' {connectionStatus} -> connectionStatus) (\s@GetWirelessGatewayStatisticsResponse' {} a -> s {connectionStatus = a} :: GetWirelessGatewayStatisticsResponse)

-- | The response's http status code.
getWirelessGatewayStatisticsResponse_httpStatus :: Lens.Lens' GetWirelessGatewayStatisticsResponse Prelude.Int
getWirelessGatewayStatisticsResponse_httpStatus = Lens.lens (\GetWirelessGatewayStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetWirelessGatewayStatisticsResponse' {} a -> s {httpStatus = a} :: GetWirelessGatewayStatisticsResponse)

instance
  Prelude.NFData
    GetWirelessGatewayStatisticsResponse
  where
  rnf GetWirelessGatewayStatisticsResponse' {..} =
    Prelude.rnf lastUplinkReceivedAt
      `Prelude.seq` Prelude.rnf wirelessGatewayId
      `Prelude.seq` Prelude.rnf connectionStatus
      `Prelude.seq` Prelude.rnf httpStatus
