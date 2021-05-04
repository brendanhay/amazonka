{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | /See:/ 'newUpdateBandwidthRateLimitSchedule' smart constructor.
data UpdateBandwidthRateLimitSchedule = UpdateBandwidthRateLimitSchedule'
  { gatewayARN :: Prelude.Text,
    -- | An array containing bandwidth rate limit schedule intervals for a
    -- gateway. When no bandwidth rate limit intervals have been scheduled, the
    -- array is empty.
    bandwidthRateLimitIntervals :: [BandwidthRateLimitInterval]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
updateBandwidthRateLimitSchedule_bandwidthRateLimitIntervals = Lens.lens (\UpdateBandwidthRateLimitSchedule' {bandwidthRateLimitIntervals} -> bandwidthRateLimitIntervals) (\s@UpdateBandwidthRateLimitSchedule' {} a -> s {bandwidthRateLimitIntervals = a} :: UpdateBandwidthRateLimitSchedule) Prelude.. Prelude._Coerce

instance
  Prelude.AWSRequest
    UpdateBandwidthRateLimitSchedule
  where
  type
    Rs UpdateBandwidthRateLimitSchedule =
      UpdateBandwidthRateLimitScheduleResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBandwidthRateLimitScheduleResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    UpdateBandwidthRateLimitSchedule

instance
  Prelude.NFData
    UpdateBandwidthRateLimitSchedule

instance
  Prelude.ToHeaders
    UpdateBandwidthRateLimitSchedule
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.UpdateBandwidthRateLimitSchedule" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance
  Prelude.ToJSON
    UpdateBandwidthRateLimitSchedule
  where
  toJSON UpdateBandwidthRateLimitSchedule' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Prelude..= gatewayARN),
            Prelude.Just
              ( "BandwidthRateLimitIntervals"
                  Prelude..= bandwidthRateLimitIntervals
              )
          ]
      )

instance
  Prelude.ToPath
    UpdateBandwidthRateLimitSchedule
  where
  toPath = Prelude.const "/"

instance
  Prelude.ToQuery
    UpdateBandwidthRateLimitSchedule
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBandwidthRateLimitScheduleResponse' smart constructor.
data UpdateBandwidthRateLimitScheduleResponse = UpdateBandwidthRateLimitScheduleResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
