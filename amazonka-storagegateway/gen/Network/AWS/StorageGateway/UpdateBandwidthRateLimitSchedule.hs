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
-- Module      : Network.AWS.StorageGateway.UpdateBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bandwidth rate limit schedule for a specified gateway. By
-- default, gateways do not have bandwidth rate limit schedules, which
-- means no bandwidth rate limiting is in effect. Use this to initiate or
-- update a gateway\'s bandwidth rate limit schedule. This operation is
-- supported in the volume and tape gateway types.
module Network.AWS.StorageGateway.UpdateBandwidthRateLimitSchedule
  ( -- * Creating a Request
    UpdateBandwidthRateLimitSchedule (..),
    newUpdateBandwidthRateLimitSchedule,

    -- * Request Lenses
    updateBandwidthRateLimitSchedule_gatewayARN,
    updateBandwidthRateLimitSchedule_bandwidthRateLimitIntervals,

    -- * Destructuring the Response
    UpdateBandwidthRateLimitScheduleResponse (..),
    newUpdateBandwidthRateLimitScheduleResponse,

    -- * Response Lenses
    updateBandwidthRateLimitScheduleResponse_gatewayARN,
    updateBandwidthRateLimitScheduleResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newUpdateBandwidthRateLimitSchedule' smart constructor.
data UpdateBandwidthRateLimitSchedule = UpdateBandwidthRateLimitSchedule'
  { gatewayARN :: Core.Text,
    -- | An array containing bandwidth rate limit schedule intervals for a
    -- gateway. When no bandwidth rate limit intervals have been scheduled, the
    -- array is empty.
    bandwidthRateLimitIntervals :: [BandwidthRateLimitInterval]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBandwidthRateLimitSchedule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'updateBandwidthRateLimitSchedule_gatewayARN' - Undocumented member.
--
-- 'bandwidthRateLimitIntervals', 'updateBandwidthRateLimitSchedule_bandwidthRateLimitIntervals' - An array containing bandwidth rate limit schedule intervals for a
-- gateway. When no bandwidth rate limit intervals have been scheduled, the
-- array is empty.
newUpdateBandwidthRateLimitSchedule ::
  -- | 'gatewayARN'
  Core.Text ->
  UpdateBandwidthRateLimitSchedule
newUpdateBandwidthRateLimitSchedule pGatewayARN_ =
  UpdateBandwidthRateLimitSchedule'
    { gatewayARN =
        pGatewayARN_,
      bandwidthRateLimitIntervals = Core.mempty
    }

-- | Undocumented member.
updateBandwidthRateLimitSchedule_gatewayARN :: Lens.Lens' UpdateBandwidthRateLimitSchedule Core.Text
updateBandwidthRateLimitSchedule_gatewayARN = Lens.lens (\UpdateBandwidthRateLimitSchedule' {gatewayARN} -> gatewayARN) (\s@UpdateBandwidthRateLimitSchedule' {} a -> s {gatewayARN = a} :: UpdateBandwidthRateLimitSchedule)

-- | An array containing bandwidth rate limit schedule intervals for a
-- gateway. When no bandwidth rate limit intervals have been scheduled, the
-- array is empty.
updateBandwidthRateLimitSchedule_bandwidthRateLimitIntervals :: Lens.Lens' UpdateBandwidthRateLimitSchedule [BandwidthRateLimitInterval]
updateBandwidthRateLimitSchedule_bandwidthRateLimitIntervals = Lens.lens (\UpdateBandwidthRateLimitSchedule' {bandwidthRateLimitIntervals} -> bandwidthRateLimitIntervals) (\s@UpdateBandwidthRateLimitSchedule' {} a -> s {bandwidthRateLimitIntervals = a} :: UpdateBandwidthRateLimitSchedule) Core.. Lens._Coerce

instance
  Core.AWSRequest
    UpdateBandwidthRateLimitSchedule
  where
  type
    AWSResponse UpdateBandwidthRateLimitSchedule =
      UpdateBandwidthRateLimitScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBandwidthRateLimitScheduleResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateBandwidthRateLimitSchedule

instance Core.NFData UpdateBandwidthRateLimitSchedule

instance
  Core.ToHeaders
    UpdateBandwidthRateLimitSchedule
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.UpdateBandwidthRateLimitSchedule" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateBandwidthRateLimitSchedule where
  toJSON UpdateBandwidthRateLimitSchedule' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just
              ( "BandwidthRateLimitIntervals"
                  Core..= bandwidthRateLimitIntervals
              )
          ]
      )

instance Core.ToPath UpdateBandwidthRateLimitSchedule where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateBandwidthRateLimitSchedule
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateBandwidthRateLimitScheduleResponse' smart constructor.
data UpdateBandwidthRateLimitScheduleResponse = UpdateBandwidthRateLimitScheduleResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBandwidthRateLimitScheduleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'updateBandwidthRateLimitScheduleResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'updateBandwidthRateLimitScheduleResponse_httpStatus' - The response's http status code.
newUpdateBandwidthRateLimitScheduleResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateBandwidthRateLimitScheduleResponse
newUpdateBandwidthRateLimitScheduleResponse
  pHttpStatus_ =
    UpdateBandwidthRateLimitScheduleResponse'
      { gatewayARN =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
updateBandwidthRateLimitScheduleResponse_gatewayARN :: Lens.Lens' UpdateBandwidthRateLimitScheduleResponse (Core.Maybe Core.Text)
updateBandwidthRateLimitScheduleResponse_gatewayARN = Lens.lens (\UpdateBandwidthRateLimitScheduleResponse' {gatewayARN} -> gatewayARN) (\s@UpdateBandwidthRateLimitScheduleResponse' {} a -> s {gatewayARN = a} :: UpdateBandwidthRateLimitScheduleResponse)

-- | The response's http status code.
updateBandwidthRateLimitScheduleResponse_httpStatus :: Lens.Lens' UpdateBandwidthRateLimitScheduleResponse Core.Int
updateBandwidthRateLimitScheduleResponse_httpStatus = Lens.lens (\UpdateBandwidthRateLimitScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateBandwidthRateLimitScheduleResponse' {} a -> s {httpStatus = a} :: UpdateBandwidthRateLimitScheduleResponse)

instance
  Core.NFData
    UpdateBandwidthRateLimitScheduleResponse
