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
-- Module      : Amazonka.StorageGateway.UpdateBandwidthRateLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bandwidth rate limits of a gateway. You can update both the
-- upload and download bandwidth rate limit or specify only one of the two.
-- If you don\'t set a bandwidth rate limit, the existing rate limit
-- remains. This operation is supported only for the stored volume, cached
-- volume, and tape gateway types. To update bandwidth rate limits for S3
-- file gateways, use UpdateBandwidthRateLimitSchedule.
--
-- By default, a gateway\'s bandwidth rate limits are not set. If you
-- don\'t set any limit, the gateway does not have any limitations on its
-- bandwidth usage and could potentially use the maximum available
-- bandwidth.
--
-- To specify which gateway to update, use the Amazon Resource Name (ARN)
-- of the gateway in your request.
module Amazonka.StorageGateway.UpdateBandwidthRateLimit
  ( -- * Creating a Request
    UpdateBandwidthRateLimit (..),
    newUpdateBandwidthRateLimit,

    -- * Request Lenses
    updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_gatewayARN,

    -- * Destructuring the Response
    UpdateBandwidthRateLimitResponse (..),
    newUpdateBandwidthRateLimitResponse,

    -- * Response Lenses
    updateBandwidthRateLimitResponse_gatewayARN,
    updateBandwidthRateLimitResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   UpdateBandwidthRateLimitInput$AverageDownloadRateLimitInBitsPerSec
--
-- -   UpdateBandwidthRateLimitInput$AverageUploadRateLimitInBitsPerSec
--
-- /See:/ 'newUpdateBandwidthRateLimit' smart constructor.
data UpdateBandwidthRateLimit = UpdateBandwidthRateLimit'
  { -- | The average download bandwidth rate limit in bits per second.
    averageDownloadRateLimitInBitsPerSec :: Prelude.Maybe Prelude.Natural,
    -- | The average upload bandwidth rate limit in bits per second.
    averageUploadRateLimitInBitsPerSec :: Prelude.Maybe Prelude.Natural,
    gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBandwidthRateLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'averageDownloadRateLimitInBitsPerSec', 'updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec' - The average download bandwidth rate limit in bits per second.
--
-- 'averageUploadRateLimitInBitsPerSec', 'updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec' - The average upload bandwidth rate limit in bits per second.
--
-- 'gatewayARN', 'updateBandwidthRateLimit_gatewayARN' - Undocumented member.
newUpdateBandwidthRateLimit ::
  -- | 'gatewayARN'
  Prelude.Text ->
  UpdateBandwidthRateLimit
newUpdateBandwidthRateLimit pGatewayARN_ =
  UpdateBandwidthRateLimit'
    { averageDownloadRateLimitInBitsPerSec =
        Prelude.Nothing,
      averageUploadRateLimitInBitsPerSec =
        Prelude.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | The average download bandwidth rate limit in bits per second.
updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec :: Lens.Lens' UpdateBandwidthRateLimit (Prelude.Maybe Prelude.Natural)
updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec = Lens.lens (\UpdateBandwidthRateLimit' {averageDownloadRateLimitInBitsPerSec} -> averageDownloadRateLimitInBitsPerSec) (\s@UpdateBandwidthRateLimit' {} a -> s {averageDownloadRateLimitInBitsPerSec = a} :: UpdateBandwidthRateLimit)

-- | The average upload bandwidth rate limit in bits per second.
updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec :: Lens.Lens' UpdateBandwidthRateLimit (Prelude.Maybe Prelude.Natural)
updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec = Lens.lens (\UpdateBandwidthRateLimit' {averageUploadRateLimitInBitsPerSec} -> averageUploadRateLimitInBitsPerSec) (\s@UpdateBandwidthRateLimit' {} a -> s {averageUploadRateLimitInBitsPerSec = a} :: UpdateBandwidthRateLimit)

-- | Undocumented member.
updateBandwidthRateLimit_gatewayARN :: Lens.Lens' UpdateBandwidthRateLimit Prelude.Text
updateBandwidthRateLimit_gatewayARN = Lens.lens (\UpdateBandwidthRateLimit' {gatewayARN} -> gatewayARN) (\s@UpdateBandwidthRateLimit' {} a -> s {gatewayARN = a} :: UpdateBandwidthRateLimit)

instance Core.AWSRequest UpdateBandwidthRateLimit where
  type
    AWSResponse UpdateBandwidthRateLimit =
      UpdateBandwidthRateLimitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBandwidthRateLimitResponse'
            Prelude.<$> (x Data..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBandwidthRateLimit where
  hashWithSalt _salt UpdateBandwidthRateLimit' {..} =
    _salt
      `Prelude.hashWithSalt` averageDownloadRateLimitInBitsPerSec
      `Prelude.hashWithSalt` averageUploadRateLimitInBitsPerSec
      `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData UpdateBandwidthRateLimit where
  rnf UpdateBandwidthRateLimit' {..} =
    Prelude.rnf averageDownloadRateLimitInBitsPerSec
      `Prelude.seq` Prelude.rnf averageUploadRateLimitInBitsPerSec
      `Prelude.seq` Prelude.rnf gatewayARN

instance Data.ToHeaders UpdateBandwidthRateLimit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.UpdateBandwidthRateLimit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBandwidthRateLimit where
  toJSON UpdateBandwidthRateLimit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AverageDownloadRateLimitInBitsPerSec" Data..=)
              Prelude.<$> averageDownloadRateLimitInBitsPerSec,
            ("AverageUploadRateLimitInBitsPerSec" Data..=)
              Prelude.<$> averageUploadRateLimitInBitsPerSec,
            Prelude.Just ("GatewayARN" Data..= gatewayARN)
          ]
      )

instance Data.ToPath UpdateBandwidthRateLimit where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateBandwidthRateLimit where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- whose throttle information was updated.
--
-- /See:/ 'newUpdateBandwidthRateLimitResponse' smart constructor.
data UpdateBandwidthRateLimitResponse = UpdateBandwidthRateLimitResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBandwidthRateLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'updateBandwidthRateLimitResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'updateBandwidthRateLimitResponse_httpStatus' - The response's http status code.
newUpdateBandwidthRateLimitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBandwidthRateLimitResponse
newUpdateBandwidthRateLimitResponse pHttpStatus_ =
  UpdateBandwidthRateLimitResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateBandwidthRateLimitResponse_gatewayARN :: Lens.Lens' UpdateBandwidthRateLimitResponse (Prelude.Maybe Prelude.Text)
updateBandwidthRateLimitResponse_gatewayARN = Lens.lens (\UpdateBandwidthRateLimitResponse' {gatewayARN} -> gatewayARN) (\s@UpdateBandwidthRateLimitResponse' {} a -> s {gatewayARN = a} :: UpdateBandwidthRateLimitResponse)

-- | The response's http status code.
updateBandwidthRateLimitResponse_httpStatus :: Lens.Lens' UpdateBandwidthRateLimitResponse Prelude.Int
updateBandwidthRateLimitResponse_httpStatus = Lens.lens (\UpdateBandwidthRateLimitResponse' {httpStatus} -> httpStatus) (\s@UpdateBandwidthRateLimitResponse' {} a -> s {httpStatus = a} :: UpdateBandwidthRateLimitResponse)

instance
  Prelude.NFData
    UpdateBandwidthRateLimitResponse
  where
  rnf UpdateBandwidthRateLimitResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
