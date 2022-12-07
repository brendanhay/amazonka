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
-- Module      : Amazonka.StorageGateway.UpdateBandwidthRateLimitSchedule
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bandwidth rate limit schedule for a specified gateway. By
-- default, gateways do not have bandwidth rate limit schedules, which
-- means no bandwidth rate limiting is in effect. Use this to initiate or
-- update a gateway\'s bandwidth rate limit schedule. This operation is
-- supported only for volume, tape and S3 file gateways. FSx file gateways
-- do not support bandwidth rate limits.
module Amazonka.StorageGateway.UpdateBandwidthRateLimitSchedule
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | /See:/ 'newUpdateBandwidthRateLimitSchedule' smart constructor.
data UpdateBandwidthRateLimitSchedule = UpdateBandwidthRateLimitSchedule'
  { gatewayARN :: Prelude.Text,
    -- | An array containing bandwidth rate limit schedule intervals for a
    -- gateway. When no bandwidth rate limit intervals have been scheduled, the
    -- array is empty.
    bandwidthRateLimitIntervals :: [BandwidthRateLimitInterval]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  UpdateBandwidthRateLimitSchedule
newUpdateBandwidthRateLimitSchedule pGatewayARN_ =
  UpdateBandwidthRateLimitSchedule'
    { gatewayARN =
        pGatewayARN_,
      bandwidthRateLimitIntervals =
        Prelude.mempty
    }

-- | Undocumented member.
updateBandwidthRateLimitSchedule_gatewayARN :: Lens.Lens' UpdateBandwidthRateLimitSchedule Prelude.Text
updateBandwidthRateLimitSchedule_gatewayARN = Lens.lens (\UpdateBandwidthRateLimitSchedule' {gatewayARN} -> gatewayARN) (\s@UpdateBandwidthRateLimitSchedule' {} a -> s {gatewayARN = a} :: UpdateBandwidthRateLimitSchedule)

-- | An array containing bandwidth rate limit schedule intervals for a
-- gateway. When no bandwidth rate limit intervals have been scheduled, the
-- array is empty.
updateBandwidthRateLimitSchedule_bandwidthRateLimitIntervals :: Lens.Lens' UpdateBandwidthRateLimitSchedule [BandwidthRateLimitInterval]
updateBandwidthRateLimitSchedule_bandwidthRateLimitIntervals = Lens.lens (\UpdateBandwidthRateLimitSchedule' {bandwidthRateLimitIntervals} -> bandwidthRateLimitIntervals) (\s@UpdateBandwidthRateLimitSchedule' {} a -> s {bandwidthRateLimitIntervals = a} :: UpdateBandwidthRateLimitSchedule) Prelude.. Lens.coerced

instance
  Core.AWSRequest
    UpdateBandwidthRateLimitSchedule
  where
  type
    AWSResponse UpdateBandwidthRateLimitSchedule =
      UpdateBandwidthRateLimitScheduleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBandwidthRateLimitScheduleResponse'
            Prelude.<$> (x Data..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateBandwidthRateLimitSchedule
  where
  hashWithSalt
    _salt
    UpdateBandwidthRateLimitSchedule' {..} =
      _salt `Prelude.hashWithSalt` gatewayARN
        `Prelude.hashWithSalt` bandwidthRateLimitIntervals

instance
  Prelude.NFData
    UpdateBandwidthRateLimitSchedule
  where
  rnf UpdateBandwidthRateLimitSchedule' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf bandwidthRateLimitIntervals

instance
  Data.ToHeaders
    UpdateBandwidthRateLimitSchedule
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.UpdateBandwidthRateLimitSchedule" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBandwidthRateLimitSchedule where
  toJSON UpdateBandwidthRateLimitSchedule' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Data..= gatewayARN),
            Prelude.Just
              ( "BandwidthRateLimitIntervals"
                  Data..= bandwidthRateLimitIntervals
              )
          ]
      )

instance Data.ToPath UpdateBandwidthRateLimitSchedule where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    UpdateBandwidthRateLimitSchedule
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBandwidthRateLimitScheduleResponse' smart constructor.
data UpdateBandwidthRateLimitScheduleResponse = UpdateBandwidthRateLimitScheduleResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  UpdateBandwidthRateLimitScheduleResponse
newUpdateBandwidthRateLimitScheduleResponse
  pHttpStatus_ =
    UpdateBandwidthRateLimitScheduleResponse'
      { gatewayARN =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
updateBandwidthRateLimitScheduleResponse_gatewayARN :: Lens.Lens' UpdateBandwidthRateLimitScheduleResponse (Prelude.Maybe Prelude.Text)
updateBandwidthRateLimitScheduleResponse_gatewayARN = Lens.lens (\UpdateBandwidthRateLimitScheduleResponse' {gatewayARN} -> gatewayARN) (\s@UpdateBandwidthRateLimitScheduleResponse' {} a -> s {gatewayARN = a} :: UpdateBandwidthRateLimitScheduleResponse)

-- | The response's http status code.
updateBandwidthRateLimitScheduleResponse_httpStatus :: Lens.Lens' UpdateBandwidthRateLimitScheduleResponse Prelude.Int
updateBandwidthRateLimitScheduleResponse_httpStatus = Lens.lens (\UpdateBandwidthRateLimitScheduleResponse' {httpStatus} -> httpStatus) (\s@UpdateBandwidthRateLimitScheduleResponse' {} a -> s {httpStatus = a} :: UpdateBandwidthRateLimitScheduleResponse)

instance
  Prelude.NFData
    UpdateBandwidthRateLimitScheduleResponse
  where
  rnf UpdateBandwidthRateLimitScheduleResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
