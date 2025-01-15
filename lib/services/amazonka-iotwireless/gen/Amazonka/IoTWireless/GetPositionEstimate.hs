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
-- Module      : Amazonka.IoTWireless.GetPositionEstimate
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Get estimated position information as a payload in GeoJSON format. The
-- payload measurement data is resolved using solvers that are provided by
-- third-party vendors.
module Amazonka.IoTWireless.GetPositionEstimate
  ( -- * Creating a Request
    GetPositionEstimate (..),
    newGetPositionEstimate,

    -- * Request Lenses
    getPositionEstimate_cellTowers,
    getPositionEstimate_gnss,
    getPositionEstimate_ip,
    getPositionEstimate_timestamp,
    getPositionEstimate_wiFiAccessPoints,

    -- * Destructuring the Response
    GetPositionEstimateResponse (..),
    newGetPositionEstimateResponse,

    -- * Response Lenses
    getPositionEstimateResponse_geoJsonPayload,
    getPositionEstimateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IoTWireless.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPositionEstimate' smart constructor.
data GetPositionEstimate = GetPositionEstimate'
  { -- | Retrieves an estimated device position by resolving measurement data
    -- from cellular radio towers. The position is resolved using HERE\'s
    -- cellular-based solver.
    cellTowers :: Prelude.Maybe CellTowers,
    -- | Retrieves an estimated device position by resolving the global
    -- navigation satellite system (GNSS) scan data. The position is resolved
    -- using the GNSS solver powered by LoRa Cloud.
    gnss :: Prelude.Maybe Gnss,
    -- | Retrieves an estimated device position by resolving the IP address
    -- information from the device. The position is resolved using MaxMind\'s
    -- IP-based solver.
    ip :: Prelude.Maybe Ip,
    -- | Optional information that specifies the time when the position
    -- information will be resolved. It uses the UNIX timestamp format. If not
    -- specified, the time at which the request was received will be used.
    timestamp :: Prelude.Maybe Data.POSIX,
    -- | Retrieves an estimated device position by resolving WLAN measurement
    -- data. The position is resolved using HERE\'s Wi-Fi based solver.
    wiFiAccessPoints :: Prelude.Maybe [WiFiAccessPoint]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPositionEstimate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cellTowers', 'getPositionEstimate_cellTowers' - Retrieves an estimated device position by resolving measurement data
-- from cellular radio towers. The position is resolved using HERE\'s
-- cellular-based solver.
--
-- 'gnss', 'getPositionEstimate_gnss' - Retrieves an estimated device position by resolving the global
-- navigation satellite system (GNSS) scan data. The position is resolved
-- using the GNSS solver powered by LoRa Cloud.
--
-- 'ip', 'getPositionEstimate_ip' - Retrieves an estimated device position by resolving the IP address
-- information from the device. The position is resolved using MaxMind\'s
-- IP-based solver.
--
-- 'timestamp', 'getPositionEstimate_timestamp' - Optional information that specifies the time when the position
-- information will be resolved. It uses the UNIX timestamp format. If not
-- specified, the time at which the request was received will be used.
--
-- 'wiFiAccessPoints', 'getPositionEstimate_wiFiAccessPoints' - Retrieves an estimated device position by resolving WLAN measurement
-- data. The position is resolved using HERE\'s Wi-Fi based solver.
newGetPositionEstimate ::
  GetPositionEstimate
newGetPositionEstimate =
  GetPositionEstimate'
    { cellTowers = Prelude.Nothing,
      gnss = Prelude.Nothing,
      ip = Prelude.Nothing,
      timestamp = Prelude.Nothing,
      wiFiAccessPoints = Prelude.Nothing
    }

-- | Retrieves an estimated device position by resolving measurement data
-- from cellular radio towers. The position is resolved using HERE\'s
-- cellular-based solver.
getPositionEstimate_cellTowers :: Lens.Lens' GetPositionEstimate (Prelude.Maybe CellTowers)
getPositionEstimate_cellTowers = Lens.lens (\GetPositionEstimate' {cellTowers} -> cellTowers) (\s@GetPositionEstimate' {} a -> s {cellTowers = a} :: GetPositionEstimate)

-- | Retrieves an estimated device position by resolving the global
-- navigation satellite system (GNSS) scan data. The position is resolved
-- using the GNSS solver powered by LoRa Cloud.
getPositionEstimate_gnss :: Lens.Lens' GetPositionEstimate (Prelude.Maybe Gnss)
getPositionEstimate_gnss = Lens.lens (\GetPositionEstimate' {gnss} -> gnss) (\s@GetPositionEstimate' {} a -> s {gnss = a} :: GetPositionEstimate)

-- | Retrieves an estimated device position by resolving the IP address
-- information from the device. The position is resolved using MaxMind\'s
-- IP-based solver.
getPositionEstimate_ip :: Lens.Lens' GetPositionEstimate (Prelude.Maybe Ip)
getPositionEstimate_ip = Lens.lens (\GetPositionEstimate' {ip} -> ip) (\s@GetPositionEstimate' {} a -> s {ip = a} :: GetPositionEstimate)

-- | Optional information that specifies the time when the position
-- information will be resolved. It uses the UNIX timestamp format. If not
-- specified, the time at which the request was received will be used.
getPositionEstimate_timestamp :: Lens.Lens' GetPositionEstimate (Prelude.Maybe Prelude.UTCTime)
getPositionEstimate_timestamp = Lens.lens (\GetPositionEstimate' {timestamp} -> timestamp) (\s@GetPositionEstimate' {} a -> s {timestamp = a} :: GetPositionEstimate) Prelude.. Lens.mapping Data._Time

-- | Retrieves an estimated device position by resolving WLAN measurement
-- data. The position is resolved using HERE\'s Wi-Fi based solver.
getPositionEstimate_wiFiAccessPoints :: Lens.Lens' GetPositionEstimate (Prelude.Maybe [WiFiAccessPoint])
getPositionEstimate_wiFiAccessPoints = Lens.lens (\GetPositionEstimate' {wiFiAccessPoints} -> wiFiAccessPoints) (\s@GetPositionEstimate' {} a -> s {wiFiAccessPoints = a} :: GetPositionEstimate) Prelude.. Lens.mapping Lens.coerced

instance Core.AWSRequest GetPositionEstimate where
  type
    AWSResponse GetPositionEstimate =
      GetPositionEstimateResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveBytes
      ( \s h x ->
          GetPositionEstimateResponse'
            Prelude.<$> (Prelude.pure (Prelude.Just (Prelude.coerce x)))
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPositionEstimate where
  hashWithSalt _salt GetPositionEstimate' {..} =
    _salt
      `Prelude.hashWithSalt` cellTowers
      `Prelude.hashWithSalt` gnss
      `Prelude.hashWithSalt` ip
      `Prelude.hashWithSalt` timestamp
      `Prelude.hashWithSalt` wiFiAccessPoints

instance Prelude.NFData GetPositionEstimate where
  rnf GetPositionEstimate' {..} =
    Prelude.rnf cellTowers `Prelude.seq`
      Prelude.rnf gnss `Prelude.seq`
        Prelude.rnf ip `Prelude.seq`
          Prelude.rnf timestamp `Prelude.seq`
            Prelude.rnf wiFiAccessPoints

instance Data.ToHeaders GetPositionEstimate where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON GetPositionEstimate where
  toJSON GetPositionEstimate' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CellTowers" Data..=) Prelude.<$> cellTowers,
            ("Gnss" Data..=) Prelude.<$> gnss,
            ("Ip" Data..=) Prelude.<$> ip,
            ("Timestamp" Data..=) Prelude.<$> timestamp,
            ("WiFiAccessPoints" Data..=)
              Prelude.<$> wiFiAccessPoints
          ]
      )

instance Data.ToPath GetPositionEstimate where
  toPath = Prelude.const "/position-estimate"

instance Data.ToQuery GetPositionEstimate where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPositionEstimateResponse' smart constructor.
data GetPositionEstimateResponse = GetPositionEstimateResponse'
  { -- | The position information of the resource, displayed as a JSON payload.
    -- The payload uses the GeoJSON format, which a format that\'s used to
    -- encode geographic data structures. For more information, see
    -- <https://geojson.org/ GeoJSON>.
    geoJsonPayload :: Prelude.Maybe Prelude.ByteString,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPositionEstimateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'geoJsonPayload', 'getPositionEstimateResponse_geoJsonPayload' - The position information of the resource, displayed as a JSON payload.
-- The payload uses the GeoJSON format, which a format that\'s used to
-- encode geographic data structures. For more information, see
-- <https://geojson.org/ GeoJSON>.
--
-- 'httpStatus', 'getPositionEstimateResponse_httpStatus' - The response's http status code.
newGetPositionEstimateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPositionEstimateResponse
newGetPositionEstimateResponse pHttpStatus_ =
  GetPositionEstimateResponse'
    { geoJsonPayload =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The position information of the resource, displayed as a JSON payload.
-- The payload uses the GeoJSON format, which a format that\'s used to
-- encode geographic data structures. For more information, see
-- <https://geojson.org/ GeoJSON>.
getPositionEstimateResponse_geoJsonPayload :: Lens.Lens' GetPositionEstimateResponse (Prelude.Maybe Prelude.ByteString)
getPositionEstimateResponse_geoJsonPayload = Lens.lens (\GetPositionEstimateResponse' {geoJsonPayload} -> geoJsonPayload) (\s@GetPositionEstimateResponse' {} a -> s {geoJsonPayload = a} :: GetPositionEstimateResponse)

-- | The response's http status code.
getPositionEstimateResponse_httpStatus :: Lens.Lens' GetPositionEstimateResponse Prelude.Int
getPositionEstimateResponse_httpStatus = Lens.lens (\GetPositionEstimateResponse' {httpStatus} -> httpStatus) (\s@GetPositionEstimateResponse' {} a -> s {httpStatus = a} :: GetPositionEstimateResponse)

instance Prelude.NFData GetPositionEstimateResponse where
  rnf GetPositionEstimateResponse' {..} =
    Prelude.rnf geoJsonPayload `Prelude.seq`
      Prelude.rnf httpStatus
