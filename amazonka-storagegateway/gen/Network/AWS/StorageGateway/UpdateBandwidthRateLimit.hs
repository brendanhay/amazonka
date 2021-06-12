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
-- Module      : Network.AWS.StorageGateway.UpdateBandwidthRateLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bandwidth rate limits of a gateway. You can update both the
-- upload and download bandwidth rate limit or specify only one of the two.
-- If you don\'t set a bandwidth rate limit, the existing rate limit
-- remains. This operation is supported for the stored volume, cached
-- volume, and tape gateway types.
--
-- By default, a gateway\'s bandwidth rate limits are not set. If you
-- don\'t set any limit, the gateway does not have any limitations on its
-- bandwidth usage and could potentially use the maximum available
-- bandwidth.
--
-- To specify which gateway to update, use the Amazon Resource Name (ARN)
-- of the gateway in your request.
module Network.AWS.StorageGateway.UpdateBandwidthRateLimit
  ( -- * Creating a Request
    UpdateBandwidthRateLimit (..),
    newUpdateBandwidthRateLimit,

    -- * Request Lenses
    updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec,
    updateBandwidthRateLimit_gatewayARN,

    -- * Destructuring the Response
    UpdateBandwidthRateLimitResponse (..),
    newUpdateBandwidthRateLimitResponse,

    -- * Response Lenses
    updateBandwidthRateLimitResponse_gatewayARN,
    updateBandwidthRateLimitResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing one or more of the following fields:
--
-- -   UpdateBandwidthRateLimitInput$AverageDownloadRateLimitInBitsPerSec
--
-- -   UpdateBandwidthRateLimitInput$AverageUploadRateLimitInBitsPerSec
--
-- /See:/ 'newUpdateBandwidthRateLimit' smart constructor.
data UpdateBandwidthRateLimit = UpdateBandwidthRateLimit'
  { -- | The average upload bandwidth rate limit in bits per second.
    averageUploadRateLimitInBitsPerSec :: Core.Maybe Core.Natural,
    -- | The average download bandwidth rate limit in bits per second.
    averageDownloadRateLimitInBitsPerSec :: Core.Maybe Core.Natural,
    gatewayARN :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateBandwidthRateLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'averageUploadRateLimitInBitsPerSec', 'updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec' - The average upload bandwidth rate limit in bits per second.
--
-- 'averageDownloadRateLimitInBitsPerSec', 'updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec' - The average download bandwidth rate limit in bits per second.
--
-- 'gatewayARN', 'updateBandwidthRateLimit_gatewayARN' - Undocumented member.
newUpdateBandwidthRateLimit ::
  -- | 'gatewayARN'
  Core.Text ->
  UpdateBandwidthRateLimit
newUpdateBandwidthRateLimit pGatewayARN_ =
  UpdateBandwidthRateLimit'
    { averageUploadRateLimitInBitsPerSec =
        Core.Nothing,
      averageDownloadRateLimitInBitsPerSec =
        Core.Nothing,
      gatewayARN = pGatewayARN_
    }

-- | The average upload bandwidth rate limit in bits per second.
updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec :: Lens.Lens' UpdateBandwidthRateLimit (Core.Maybe Core.Natural)
updateBandwidthRateLimit_averageUploadRateLimitInBitsPerSec = Lens.lens (\UpdateBandwidthRateLimit' {averageUploadRateLimitInBitsPerSec} -> averageUploadRateLimitInBitsPerSec) (\s@UpdateBandwidthRateLimit' {} a -> s {averageUploadRateLimitInBitsPerSec = a} :: UpdateBandwidthRateLimit)

-- | The average download bandwidth rate limit in bits per second.
updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec :: Lens.Lens' UpdateBandwidthRateLimit (Core.Maybe Core.Natural)
updateBandwidthRateLimit_averageDownloadRateLimitInBitsPerSec = Lens.lens (\UpdateBandwidthRateLimit' {averageDownloadRateLimitInBitsPerSec} -> averageDownloadRateLimitInBitsPerSec) (\s@UpdateBandwidthRateLimit' {} a -> s {averageDownloadRateLimitInBitsPerSec = a} :: UpdateBandwidthRateLimit)

-- | Undocumented member.
updateBandwidthRateLimit_gatewayARN :: Lens.Lens' UpdateBandwidthRateLimit Core.Text
updateBandwidthRateLimit_gatewayARN = Lens.lens (\UpdateBandwidthRateLimit' {gatewayARN} -> gatewayARN) (\s@UpdateBandwidthRateLimit' {} a -> s {gatewayARN = a} :: UpdateBandwidthRateLimit)

instance Core.AWSRequest UpdateBandwidthRateLimit where
  type
    AWSResponse UpdateBandwidthRateLimit =
      UpdateBandwidthRateLimitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBandwidthRateLimitResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable UpdateBandwidthRateLimit

instance Core.NFData UpdateBandwidthRateLimit

instance Core.ToHeaders UpdateBandwidthRateLimit where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.UpdateBandwidthRateLimit" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON UpdateBandwidthRateLimit where
  toJSON UpdateBandwidthRateLimit' {..} =
    Core.object
      ( Core.catMaybes
          [ ("AverageUploadRateLimitInBitsPerSec" Core..=)
              Core.<$> averageUploadRateLimitInBitsPerSec,
            ("AverageDownloadRateLimitInBitsPerSec" Core..=)
              Core.<$> averageDownloadRateLimitInBitsPerSec,
            Core.Just ("GatewayARN" Core..= gatewayARN)
          ]
      )

instance Core.ToPath UpdateBandwidthRateLimit where
  toPath = Core.const "/"

instance Core.ToQuery UpdateBandwidthRateLimit where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- whose throttle information was updated.
--
-- /See:/ 'newUpdateBandwidthRateLimitResponse' smart constructor.
data UpdateBandwidthRateLimitResponse = UpdateBandwidthRateLimitResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  UpdateBandwidthRateLimitResponse
newUpdateBandwidthRateLimitResponse pHttpStatus_ =
  UpdateBandwidthRateLimitResponse'
    { gatewayARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
updateBandwidthRateLimitResponse_gatewayARN :: Lens.Lens' UpdateBandwidthRateLimitResponse (Core.Maybe Core.Text)
updateBandwidthRateLimitResponse_gatewayARN = Lens.lens (\UpdateBandwidthRateLimitResponse' {gatewayARN} -> gatewayARN) (\s@UpdateBandwidthRateLimitResponse' {} a -> s {gatewayARN = a} :: UpdateBandwidthRateLimitResponse)

-- | The response's http status code.
updateBandwidthRateLimitResponse_httpStatus :: Lens.Lens' UpdateBandwidthRateLimitResponse Core.Int
updateBandwidthRateLimitResponse_httpStatus = Lens.lens (\UpdateBandwidthRateLimitResponse' {httpStatus} -> httpStatus) (\s@UpdateBandwidthRateLimitResponse' {} a -> s {httpStatus = a} :: UpdateBandwidthRateLimitResponse)

instance Core.NFData UpdateBandwidthRateLimitResponse
