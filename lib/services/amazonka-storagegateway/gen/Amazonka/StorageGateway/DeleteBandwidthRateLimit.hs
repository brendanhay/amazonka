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
-- Module      : Amazonka.StorageGateway.DeleteBandwidthRateLimit
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the bandwidth rate limits of a gateway. You can delete either
-- the upload and download bandwidth rate limit, or you can delete both. If
-- you delete only one of the limits, the other limit remains unchanged. To
-- specify which gateway to work with, use the Amazon Resource Name (ARN)
-- of the gateway in your request. This operation is supported only for the
-- stored volume, cached volume, and tape gateway types.
module Amazonka.StorageGateway.DeleteBandwidthRateLimit
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSRequest DeleteBandwidthRateLimit where
  type
    AWSResponse DeleteBandwidthRateLimit =
      DeleteBandwidthRateLimitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteBandwidthRateLimitResponse'
            Prelude.<$> (x Data..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DeleteBandwidthRateLimit where
  hashWithSalt _salt DeleteBandwidthRateLimit' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` bandwidthType

instance Prelude.NFData DeleteBandwidthRateLimit where
  rnf DeleteBandwidthRateLimit' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf bandwidthType

instance Data.ToHeaders DeleteBandwidthRateLimit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DeleteBandwidthRateLimit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DeleteBandwidthRateLimit where
  toJSON DeleteBandwidthRateLimit' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("GatewayARN" Data..= gatewayARN),
            Prelude.Just
              ("BandwidthType" Data..= bandwidthType)
          ]
      )

instance Data.ToPath DeleteBandwidthRateLimit where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteBandwidthRateLimit where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  where
  rnf DeleteBandwidthRateLimitResponse' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
