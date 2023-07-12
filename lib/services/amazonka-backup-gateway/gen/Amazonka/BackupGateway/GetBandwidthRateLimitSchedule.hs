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
-- Module      : Amazonka.BackupGateway.GetBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the bandwidth rate limit schedule for a specified gateway. By
-- default, gateways do not have bandwidth rate limit schedules, which
-- means no bandwidth rate limiting is in effect. Use this to get a
-- gateway\'s bandwidth rate limit schedule.
module Amazonka.BackupGateway.GetBandwidthRateLimitSchedule
  ( -- * Creating a Request
    GetBandwidthRateLimitSchedule (..),
    newGetBandwidthRateLimitSchedule,

    -- * Request Lenses
    getBandwidthRateLimitSchedule_gatewayArn,

    -- * Destructuring the Response
    GetBandwidthRateLimitScheduleResponse (..),
    newGetBandwidthRateLimitScheduleResponse,

    -- * Response Lenses
    getBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals,
    getBandwidthRateLimitScheduleResponse_gatewayArn,
    getBandwidthRateLimitScheduleResponse_httpStatus,
  )
where

import Amazonka.BackupGateway.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBandwidthRateLimitSchedule' smart constructor.
data GetBandwidthRateLimitSchedule = GetBandwidthRateLimitSchedule'
  { -- | The Amazon Resource Name (ARN) of the gateway. Use the
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
    -- operation to return a list of gateways for your account and Amazon Web
    -- Services Region.
    gatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBandwidthRateLimitSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayArn', 'getBandwidthRateLimitSchedule_gatewayArn' - The Amazon Resource Name (ARN) of the gateway. Use the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
newGetBandwidthRateLimitSchedule ::
  -- | 'gatewayArn'
  Prelude.Text ->
  GetBandwidthRateLimitSchedule
newGetBandwidthRateLimitSchedule pGatewayArn_ =
  GetBandwidthRateLimitSchedule'
    { gatewayArn =
        pGatewayArn_
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
getBandwidthRateLimitSchedule_gatewayArn :: Lens.Lens' GetBandwidthRateLimitSchedule Prelude.Text
getBandwidthRateLimitSchedule_gatewayArn = Lens.lens (\GetBandwidthRateLimitSchedule' {gatewayArn} -> gatewayArn) (\s@GetBandwidthRateLimitSchedule' {} a -> s {gatewayArn = a} :: GetBandwidthRateLimitSchedule)

instance
  Core.AWSRequest
    GetBandwidthRateLimitSchedule
  where
  type
    AWSResponse GetBandwidthRateLimitSchedule =
      GetBandwidthRateLimitScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBandwidthRateLimitScheduleResponse'
            Prelude.<$> ( x
                            Data..?> "BandwidthRateLimitIntervals"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "GatewayArn")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetBandwidthRateLimitSchedule
  where
  hashWithSalt _salt GetBandwidthRateLimitSchedule' {..} =
    _salt `Prelude.hashWithSalt` gatewayArn

instance Prelude.NFData GetBandwidthRateLimitSchedule where
  rnf GetBandwidthRateLimitSchedule' {..} =
    Prelude.rnf gatewayArn

instance Data.ToHeaders GetBandwidthRateLimitSchedule where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "BackupOnPremises_v20210101.GetBandwidthRateLimitSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBandwidthRateLimitSchedule where
  toJSON GetBandwidthRateLimitSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayArn" Data..= gatewayArn)]
      )

instance Data.ToPath GetBandwidthRateLimitSchedule where
  toPath = Prelude.const "/"

instance Data.ToQuery GetBandwidthRateLimitSchedule where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBandwidthRateLimitScheduleResponse' smart constructor.
data GetBandwidthRateLimitScheduleResponse = GetBandwidthRateLimitScheduleResponse'
  { -- | An array containing bandwidth rate limit schedule intervals for a
    -- gateway. When no bandwidth rate limit intervals have been scheduled, the
    -- array is empty.
    bandwidthRateLimitIntervals :: Prelude.Maybe [BandwidthRateLimitInterval],
    -- | The Amazon Resource Name (ARN) of the gateway. Use the
    -- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
    -- operation to return a list of gateways for your account and Amazon Web
    -- Services Region.
    gatewayArn :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBandwidthRateLimitScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidthRateLimitIntervals', 'getBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals' - An array containing bandwidth rate limit schedule intervals for a
-- gateway. When no bandwidth rate limit intervals have been scheduled, the
-- array is empty.
--
-- 'gatewayArn', 'getBandwidthRateLimitScheduleResponse_gatewayArn' - The Amazon Resource Name (ARN) of the gateway. Use the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
--
-- 'httpStatus', 'getBandwidthRateLimitScheduleResponse_httpStatus' - The response's http status code.
newGetBandwidthRateLimitScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBandwidthRateLimitScheduleResponse
newGetBandwidthRateLimitScheduleResponse pHttpStatus_ =
  GetBandwidthRateLimitScheduleResponse'
    { bandwidthRateLimitIntervals =
        Prelude.Nothing,
      gatewayArn = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array containing bandwidth rate limit schedule intervals for a
-- gateway. When no bandwidth rate limit intervals have been scheduled, the
-- array is empty.
getBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals :: Lens.Lens' GetBandwidthRateLimitScheduleResponse (Prelude.Maybe [BandwidthRateLimitInterval])
getBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals = Lens.lens (\GetBandwidthRateLimitScheduleResponse' {bandwidthRateLimitIntervals} -> bandwidthRateLimitIntervals) (\s@GetBandwidthRateLimitScheduleResponse' {} a -> s {bandwidthRateLimitIntervals = a} :: GetBandwidthRateLimitScheduleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The Amazon Resource Name (ARN) of the gateway. Use the
-- <https://docs.aws.amazon.com/aws-backup/latest/devguide/API_BGW_ListGateways.html ListGateways>
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
getBandwidthRateLimitScheduleResponse_gatewayArn :: Lens.Lens' GetBandwidthRateLimitScheduleResponse (Prelude.Maybe Prelude.Text)
getBandwidthRateLimitScheduleResponse_gatewayArn = Lens.lens (\GetBandwidthRateLimitScheduleResponse' {gatewayArn} -> gatewayArn) (\s@GetBandwidthRateLimitScheduleResponse' {} a -> s {gatewayArn = a} :: GetBandwidthRateLimitScheduleResponse)

-- | The response's http status code.
getBandwidthRateLimitScheduleResponse_httpStatus :: Lens.Lens' GetBandwidthRateLimitScheduleResponse Prelude.Int
getBandwidthRateLimitScheduleResponse_httpStatus = Lens.lens (\GetBandwidthRateLimitScheduleResponse' {httpStatus} -> httpStatus) (\s@GetBandwidthRateLimitScheduleResponse' {} a -> s {httpStatus = a} :: GetBandwidthRateLimitScheduleResponse)

instance
  Prelude.NFData
    GetBandwidthRateLimitScheduleResponse
  where
  rnf GetBandwidthRateLimitScheduleResponse' {..} =
    Prelude.rnf bandwidthRateLimitIntervals
      `Prelude.seq` Prelude.rnf gatewayArn
      `Prelude.seq` Prelude.rnf httpStatus
