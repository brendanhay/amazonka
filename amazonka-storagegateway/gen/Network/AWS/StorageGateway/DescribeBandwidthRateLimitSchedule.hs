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
-- Module      : Network.AWS.StorageGateway.DescribeBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about the bandwidth rate limit schedule of a
-- gateway. By default, gateways do not have bandwidth rate limit
-- schedules, which means no bandwidth rate limiting is in effect. This
-- operation is supported only in the volume and tape gateway types.
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
module Network.AWS.StorageGateway.DescribeBandwidthRateLimitSchedule
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newDescribeBandwidthRateLimitSchedule' smart constructor.
data DescribeBandwidthRateLimitSchedule = DescribeBandwidthRateLimitSchedule'
  { gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Text ->
  DescribeBandwidthRateLimitSchedule
newDescribeBandwidthRateLimitSchedule pGatewayARN_ =
  DescribeBandwidthRateLimitSchedule'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
describeBandwidthRateLimitSchedule_gatewayARN :: Lens.Lens' DescribeBandwidthRateLimitSchedule Core.Text
describeBandwidthRateLimitSchedule_gatewayARN = Lens.lens (\DescribeBandwidthRateLimitSchedule' {gatewayARN} -> gatewayARN) (\s@DescribeBandwidthRateLimitSchedule' {} a -> s {gatewayARN = a} :: DescribeBandwidthRateLimitSchedule)

instance
  Core.AWSRequest
    DescribeBandwidthRateLimitSchedule
  where
  type
    AWSResponse DescribeBandwidthRateLimitSchedule =
      DescribeBandwidthRateLimitScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBandwidthRateLimitScheduleResponse'
            Core.<$> ( x Core..?> "BandwidthRateLimitIntervals"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DescribeBandwidthRateLimitSchedule

instance
  Core.NFData
    DescribeBandwidthRateLimitSchedule

instance
  Core.ToHeaders
    DescribeBandwidthRateLimitSchedule
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DescribeBandwidthRateLimitSchedule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DescribeBandwidthRateLimitSchedule
  where
  toJSON DescribeBandwidthRateLimitSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("GatewayARN" Core..= gatewayARN)]
      )

instance
  Core.ToPath
    DescribeBandwidthRateLimitSchedule
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DescribeBandwidthRateLimitSchedule
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDescribeBandwidthRateLimitScheduleResponse' smart constructor.
data DescribeBandwidthRateLimitScheduleResponse = DescribeBandwidthRateLimitScheduleResponse'
  { -- | An array that contains the bandwidth rate limit intervals for a tape or
    -- volume gateway.
    bandwidthRateLimitIntervals :: Core.Maybe [BandwidthRateLimitInterval],
    gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  DescribeBandwidthRateLimitScheduleResponse
newDescribeBandwidthRateLimitScheduleResponse
  pHttpStatus_ =
    DescribeBandwidthRateLimitScheduleResponse'
      { bandwidthRateLimitIntervals =
          Core.Nothing,
        gatewayARN = Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | An array that contains the bandwidth rate limit intervals for a tape or
-- volume gateway.
describeBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse (Core.Maybe [BandwidthRateLimitInterval])
describeBandwidthRateLimitScheduleResponse_bandwidthRateLimitIntervals = Lens.lens (\DescribeBandwidthRateLimitScheduleResponse' {bandwidthRateLimitIntervals} -> bandwidthRateLimitIntervals) (\s@DescribeBandwidthRateLimitScheduleResponse' {} a -> s {bandwidthRateLimitIntervals = a} :: DescribeBandwidthRateLimitScheduleResponse) Core.. Lens.mapping Lens._Coerce

-- | Undocumented member.
describeBandwidthRateLimitScheduleResponse_gatewayARN :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse (Core.Maybe Core.Text)
describeBandwidthRateLimitScheduleResponse_gatewayARN = Lens.lens (\DescribeBandwidthRateLimitScheduleResponse' {gatewayARN} -> gatewayARN) (\s@DescribeBandwidthRateLimitScheduleResponse' {} a -> s {gatewayARN = a} :: DescribeBandwidthRateLimitScheduleResponse)

-- | The response's http status code.
describeBandwidthRateLimitScheduleResponse_httpStatus :: Lens.Lens' DescribeBandwidthRateLimitScheduleResponse Core.Int
describeBandwidthRateLimitScheduleResponse_httpStatus = Lens.lens (\DescribeBandwidthRateLimitScheduleResponse' {httpStatus} -> httpStatus) (\s@DescribeBandwidthRateLimitScheduleResponse' {} a -> s {httpStatus = a} :: DescribeBandwidthRateLimitScheduleResponse)

instance
  Core.NFData
    DescribeBandwidthRateLimitScheduleResponse
