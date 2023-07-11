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
-- Module      : Amazonka.StorageGateway.DescribeBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the bandwidth rate limit schedule of a
-- gateway. By default, gateways do not have bandwidth rate limit
-- schedules, which means no bandwidth rate limiting is in effect. This
-- operation is supported only for volume, tape and S3 file gateways. FSx
-- file gateways do not support bandwidth rate limits.
--
-- This operation returns information about a gateway\'s bandwidth rate
-- limit schedule. A bandwidth rate limit schedule consists of one or more
-- bandwidth rate limit intervals. A bandwidth rate limit interval defines
-- a period of time on one or more days of the week, during which bandwidth
-- rate limits are specified for uploading, downloading, or both.
--
-- A bandwidth rate limit interval consists of one or more days of the
-- week, a start hour and minute, an ending hour and minute, and bandwidth
-- rate limits for uploading and downloading
--
-- If no bandwidth rate limit schedule intervals are set for the gateway,
-- this operation returns an empty response. To specify which gateway to
-- describe, use the Amazon Resource Name (ARN) of the gateway in your
-- request.
module Amazonka.StorageGateway.DescribeBandwidthRateLimitSchedule
  ( -- * Creating a Request
    DescribeBandwidthRateLimitSchedule (..),
    newDescribeBandwidthRateLimitSchedule,

    -- * Request Lenses
    describeBandwidthRateLimitSchedule_gatewayARN,

    -- * Destructuring the Response
    DescribeBandwidthRateLimitScheduleResponse (..),
    newDescribeBandwidthRateLimitScheduleResponse,

    -- * Response Lenses
    describeBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals,
    describeBandwidthRateLimitScheduleResponse_gatewayARN,
    describeBandwidthRateLimitScheduleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newDescribeBandwidthRateLimitSchedule' smart constructor.
data DescribeBandwidthRateLimitSchedule = DescribeBandwidthRateLimitSchedule'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBandwidthRateLimitSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'describeBandwidthRateLimitSchedule_gatewayARN' - Undocumented member.
newDescribeBandwidthRateLimitSchedule ::
  -- | 'gatewayARN'
  Prelude.Text ->
  DescribeBandwidthRateLimitSchedule
newDescribeBandwidthRateLimitSchedule pGatewayARN_ =
  DescribeBandwidthRateLimitSchedule'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
describeBandwidthRateLimitSchedule_gatewayARN :: Lens.Lens' DescribeBandwidthRateLimitSchedule Prelude.Text
describeBandwidthRateLimitSchedule_gatewayARN = Lens.lens (\DescribeBandwidthRateLimitSchedule' {gatewayARN} -> gatewayARN) (\s@DescribeBandwidthRateLimitSchedule' {} a -> s {gatewayARN = a} :: DescribeBandwidthRateLimitSchedule)

instance
  Core.AWSRequest
    DescribeBandwidthRateLimitSchedule
  where
  type
    AWSResponse DescribeBandwidthRateLimitSchedule =
      DescribeBandwidthRateLimitScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBandwidthRateLimitScheduleResponse'
            Prelude.<$> ( x
                            Data..?> "BandwidthRateLimitIntervals"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeBandwidthRateLimitSchedule
  where
  hashWithSalt
    _salt
    DescribeBandwidthRateLimitSchedule' {..} =
      _salt `Prelude.hashWithSalt` gatewayARN

instance
  Prelude.NFData
    DescribeBandwidthRateLimitSchedule
  where
  rnf DescribeBandwidthRateLimitSchedule' {..} =
    Prelude.rnf gatewayARN

instance
  Data.ToHeaders
    DescribeBandwidthRateLimitSchedule
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeBandwidthRateLimitSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DescribeBandwidthRateLimitSchedule
  where
  toJSON DescribeBandwidthRateLimitSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Data..= gatewayARN)]
      )

instance
  Data.ToPath
    DescribeBandwidthRateLimitSchedule
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DescribeBandwidthRateLimitSchedule
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDescribeBandwidthRateLimitScheduleResponse' smart constructor.
data DescribeBandwidthRateLimitScheduleResponse = DescribeBandwidthRateLimitScheduleResponse'
  { -- | An array that contains the bandwidth rate limit intervals for a tape or
    -- volume gateway.
    bandwidthRateLimitIntervals :: Prelude.Maybe [BandwidthRateLimitInterval],
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBandwidthRateLimitScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bandwidthRateLimitIntervals', 'describeBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals' - An array that contains the bandwidth rate limit intervals for a tape or
-- volume gateway.
--
-- 'gatewayARN', 'describeBandwidthRateLimitScheduleResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'describeBandwidthRateLimitScheduleResponse_httpStatus' - The response's http status code.
newDescribeBandwidthRateLimitScheduleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBandwidthRateLimitScheduleResponse
newDescribeBandwidthRateLimitScheduleResponse
  pHttpStatus_ =
    DescribeBandwidthRateLimitScheduleResponse'
      { bandwidthRateLimitIntervals =
          Prelude.Nothing,
        gatewayARN = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array that contains the bandwidth rate limit intervals for a tape or
-- volume gateway.
describeBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse (Prelude.Maybe [BandwidthRateLimitInterval])
describeBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals = Lens.lens (\DescribeBandwidthRateLimitScheduleResponse' {bandwidthRateLimitIntervals} -> bandwidthRateLimitIntervals) (\s@DescribeBandwidthRateLimitScheduleResponse' {} a -> s {bandwidthRateLimitIntervals = a} :: DescribeBandwidthRateLimitScheduleResponse) Prelude.. Lens.mapping Lens.coerced

-- | Undocumented member.
describeBandwidthRateLimitScheduleResponse_gatewayARN :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse (Prelude.Maybe Prelude.Text)
describeBandwidthRateLimitScheduleResponse_gatewayARN = Lens.lens (\DescribeBandwidthRateLimitScheduleResponse' {gatewayARN} -> gatewayARN) (\s@DescribeBandwidthRateLimitScheduleResponse' {} a -> s {gatewayARN = a} :: DescribeBandwidthRateLimitScheduleResponse)

-- | The response's http status code.
describeBandwidthRateLimitScheduleResponse_httpStatus :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse Prelude.Int
describeBandwidthRateLimitScheduleResponse_httpStatus = Lens.lens (\DescribeBandwidthRateLimitScheduleResponse' {httpStatus} -> httpStatus) (\s@DescribeBandwidthRateLimitScheduleResponse' {} a -> s {httpStatus = a} :: DescribeBandwidthRateLimitScheduleResponse)

instance
  Prelude.NFData
    DescribeBandwidthRateLimitScheduleResponse
  where
  rnf DescribeBandwidthRateLimitScheduleResponse' {..} =
    Prelude.rnf bandwidthRateLimitIntervals
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
