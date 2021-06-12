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
-- Module      : Network.AWS.StorageGateway.DeleteBandwidthRateLimit
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the bandwidth rate limits of a gateway. You can delete either
-- the upload and download bandwidth rate limit, or you can delete both. If
-- you delete only one of the limits, the other limit remains unchanged. To
-- specify which gateway to work with, use the Amazon Resource Name (ARN)
-- of the gateway in your request. This operation is supported for the
-- stored volume, cached volume and tape gateway types.
module Network.AWS.StorageGateway.DeleteBandwidthRateLimit
  ( -- * Creating a Request
    DeleteBandwidthRateLimit (..),
    newDeleteBandwidthRateLimit,

    -- * Request Lenses
    deleteBandwidthRateLimit_gatewayARN,
    deleteBandwidthRateLimit_bandwidthType,

    -- * Destructuring the Response
    DeleteBandwidthRateLimitResponse (..),
    newDeleteBandwidthRateLimitResponse,

    -- * Response Lenses
    deleteBandwidthRateLimitResponse_gatewayARN,
    deleteBandwidthRateLimitResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the following fields:
--
-- -   DeleteBandwidthRateLimitInput$BandwidthType
--
-- /See:/ 'newDeleteBandwidthRateLimit' smart constructor.
data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit'
  { gatewayARN :: Core.Text,
    -- | One of the BandwidthType values that indicates the gateway bandwidth
    -- rate limit to delete.
    --
    -- Valid Values: @UPLOAD@ | @DOWNLOAD@ | @ALL@
    bandwidthType :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBandwidthRateLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'deleteBandwidthRateLimit_gatewayARN' - Undocumented member.
--
-- 'bandwidthType', 'deleteBandwidthRateLimit_bandwidthType' - One of the BandwidthType values that indicates the gateway bandwidth
-- rate limit to delete.
--
-- Valid Values: @UPLOAD@ | @DOWNLOAD@ | @ALL@
newDeleteBandwidthRateLimit ::
  -- | 'gatewayARN'
  Core.Text ->
  -- | 'bandwidthType'
  Core.Text ->
  DeleteBandwidthRateLimit
newDeleteBandwidthRateLimit
  pGatewayARN_
  pBandwidthType_ =
    DeleteBandwidthRateLimit'
      { gatewayARN =
          pGatewayARN_,
        bandwidthType = pBandwidthType_
      }

-- | Undocumented member.
deleteBandwidthRateLimit_gatewayARN :: Lens.Lens' DeleteBandwidthRateLimit Core.Text
deleteBandwidthRateLimit_gatewayARN = Lens.lens (\DeleteBandwidthRateLimit' {gatewayARN} -> gatewayARN) (\s@DeleteBandwidthRateLimit' {} a -> s {gatewayARN = a} :: DeleteBandwidthRateLimit)

-- | One of the BandwidthType values that indicates the gateway bandwidth
-- rate limit to delete.
--
-- Valid Values: @UPLOAD@ | @DOWNLOAD@ | @ALL@
deleteBandwidthRateLimit_bandwidthType :: Lens.Lens' DeleteBandwidthRateLimit Core.Text
deleteBandwidthRateLimit_bandwidthType = Lens.lens (\DeleteBandwidthRateLimit' {bandwidthType} -> bandwidthType) (\s@DeleteBandwidthRateLimit' {} a -> s {bandwidthType = a} :: DeleteBandwidthRateLimit)

instance Core.AWSRequest DeleteBandwidthRateLimit where
  type
    AWSResponse DeleteBandwidthRateLimit =
      DeleteBandwidthRateLimitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBandwidthRateLimitResponse'
            Core.<$> (x Core..?> "GatewayARN")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteBandwidthRateLimit

instance Core.NFData DeleteBandwidthRateLimit

instance Core.ToHeaders DeleteBandwidthRateLimit where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StorageGateway_20130630.DeleteBandwidthRateLimit" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DeleteBandwidthRateLimit where
  toJSON DeleteBandwidthRateLimit' {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("GatewayARN" Core..= gatewayARN),
            Core.Just ("BandwidthType" Core..= bandwidthType)
          ]
      )

instance Core.ToPath DeleteBandwidthRateLimit where
  toPath = Core.const "/"

instance Core.ToQuery DeleteBandwidthRateLimit where
  toQuery = Core.const Core.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- whose bandwidth rate information was deleted.
--
-- /See:/ 'newDeleteBandwidthRateLimitResponse' smart constructor.
data DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'
  { gatewayARN :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteBandwidthRateLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'deleteBandwidthRateLimitResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'deleteBandwidthRateLimitResponse_httpStatus' - The response's http status code.
newDeleteBandwidthRateLimitResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteBandwidthRateLimitResponse
newDeleteBandwidthRateLimitResponse pHttpStatus_ =
  DeleteBandwidthRateLimitResponse'
    { gatewayARN =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteBandwidthRateLimitResponse_gatewayARN :: Lens.Lens' DeleteBandwidthRateLimitResponse (Core.Maybe Core.Text)
deleteBandwidthRateLimitResponse_gatewayARN = Lens.lens (\DeleteBandwidthRateLimitResponse' {gatewayARN} -> gatewayARN) (\s@DeleteBandwidthRateLimitResponse' {} a -> s {gatewayARN = a} :: DeleteBandwidthRateLimitResponse)

-- | The response's http status code.
deleteBandwidthRateLimitResponse_httpStatus :: Lens.Lens' DeleteBandwidthRateLimitResponse Core.Int
deleteBandwidthRateLimitResponse_httpStatus = Lens.lens (\DeleteBandwidthRateLimitResponse' {httpStatus} -> httpStatus) (\s@DeleteBandwidthRateLimitResponse' {} a -> s {httpStatus = a} :: DeleteBandwidthRateLimitResponse)

instance Core.NFData DeleteBandwidthRateLimitResponse
