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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.StorageGateway.Types

-- | A JSON object containing the following fields:
--
-- -   DeleteBandwidthRateLimitInput$BandwidthType
--
-- /See:/ 'newDeleteBandwidthRateLimit' smart constructor.
data DeleteBandwidthRateLimit = DeleteBandwidthRateLimit'
  { gatewayARN :: Prelude.Text,
    -- | One of the BandwidthType values that indicates the gateway bandwidth
    -- rate limit to delete.
    --
    -- Valid Values: @UPLOAD@ | @DOWNLOAD@ | @ALL@
    bandwidthType :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Text ->
  -- | 'bandwidthType'
  Prelude.Text ->
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
deleteBandwidthRateLimit_gatewayARN :: Lens.Lens' DeleteBandwidthRateLimit Prelude.Text
deleteBandwidthRateLimit_gatewayARN = Lens.lens (\DeleteBandwidthRateLimit' {gatewayARN} -> gatewayARN) (\s@DeleteBandwidthRateLimit' {} a -> s {gatewayARN = a} :: DeleteBandwidthRateLimit)

-- | One of the BandwidthType values that indicates the gateway bandwidth
-- rate limit to delete.
--
-- Valid Values: @UPLOAD@ | @DOWNLOAD@ | @ALL@
deleteBandwidthRateLimit_bandwidthType :: Lens.Lens' DeleteBandwidthRateLimit Prelude.Text
deleteBandwidthRateLimit_bandwidthType = Lens.lens (\DeleteBandwidthRateLimit' {bandwidthType} -> bandwidthType) (\s@DeleteBandwidthRateLimit' {} a -> s {bandwidthType = a} :: DeleteBandwidthRateLimit)

instance Prelude.AWSRequest DeleteBandwidthRateLimit where
  type
    Rs DeleteBandwidthRateLimit =
      DeleteBandwidthRateLimitResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBandwidthRateLimitResponse'
            Prelude.<$> (x Prelude..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBandwidthRateLimit

instance Prelude.NFData DeleteBandwidthRateLimit

instance Prelude.ToHeaders DeleteBandwidthRateLimit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "StorageGateway_20130630.DeleteBandwidthRateLimit" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON DeleteBandwidthRateLimit where
  toJSON DeleteBandwidthRateLimit' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Prelude..= gatewayARN),
            Prelude.Just
              ("BandwidthType" Prelude..= bandwidthType)
          ]
      )

instance Prelude.ToPath DeleteBandwidthRateLimit where
  toPath = Prelude.const "/"

instance Prelude.ToQuery DeleteBandwidthRateLimit where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway
-- whose bandwidth rate information was deleted.
--
-- /See:/ 'newDeleteBandwidthRateLimitResponse' smart constructor.
data DeleteBandwidthRateLimitResponse = DeleteBandwidthRateLimitResponse'
  { gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  DeleteBandwidthRateLimitResponse
newDeleteBandwidthRateLimitResponse pHttpStatus_ =
  DeleteBandwidthRateLimitResponse'
    { gatewayARN =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
deleteBandwidthRateLimitResponse_gatewayARN :: Lens.Lens' DeleteBandwidthRateLimitResponse (Prelude.Maybe Prelude.Text)
deleteBandwidthRateLimitResponse_gatewayARN = Lens.lens (\DeleteBandwidthRateLimitResponse' {gatewayARN} -> gatewayARN) (\s@DeleteBandwidthRateLimitResponse' {} a -> s {gatewayARN = a} :: DeleteBandwidthRateLimitResponse)

-- | The response's http status code.
deleteBandwidthRateLimitResponse_httpStatus :: Lens.Lens' DeleteBandwidthRateLimitResponse Prelude.Int
deleteBandwidthRateLimitResponse_httpStatus = Lens.lens (\DeleteBandwidthRateLimitResponse' {httpStatus} -> httpStatus) (\s@DeleteBandwidthRateLimitResponse' {} a -> s {httpStatus = a} :: DeleteBandwidthRateLimitResponse)

instance
  Prelude.NFData
    DeleteBandwidthRateLimitResponse
