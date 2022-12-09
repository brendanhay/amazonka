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
-- Module      : Amazonka.StorageGateway.DescribeBandwidthRateLimit
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the bandwidth rate limits of a gateway. By default, these limits
-- are not set, which means no bandwidth rate limiting is in effect. This
-- operation is supported only for the stored volume, cached volume, and
-- tape gateway types. To describe bandwidth rate limits for S3 file
-- gateways, use DescribeBandwidthRateLimitSchedule.
--
-- This operation returns a value for a bandwidth rate limit only if the
-- limit is set. If no limits are set for the gateway, then this operation
-- returns only the gateway ARN in the response body. To specify which
-- gateway to describe, use the Amazon Resource Name (ARN) of the gateway
-- in your request.
module Amazonka.StorageGateway.DescribeBandwidthRateLimit
  ( -- * Creating a Request
    DescribeBandwidthRateLimit (..),
    newDescribeBandwidthRateLimit,

    -- * Request Lenses
    describeBandwidthRateLimit_gatewayARN,

    -- * Destructuring the Response
    DescribeBandwidthRateLimitResponse (..),
    newDescribeBandwidthRateLimitResponse,

    -- * Response Lenses
    describeBandwidthRateLimitResponse_averageDownloadRateLimitInBitsPerSec,
    describeBandwidthRateLimitResponse_averageUploadRateLimitInBitsPerSec,
    describeBandwidthRateLimitResponse_gatewayARN,
    describeBandwidthRateLimitResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.StorageGateway.Types

-- | A JSON object containing the Amazon Resource Name (ARN) of the gateway.
--
-- /See:/ 'newDescribeBandwidthRateLimit' smart constructor.
data DescribeBandwidthRateLimit = DescribeBandwidthRateLimit'
  { gatewayARN :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBandwidthRateLimit' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'describeBandwidthRateLimit_gatewayARN' - Undocumented member.
newDescribeBandwidthRateLimit ::
  -- | 'gatewayARN'
  Prelude.Text ->
  DescribeBandwidthRateLimit
newDescribeBandwidthRateLimit pGatewayARN_ =
  DescribeBandwidthRateLimit'
    { gatewayARN =
        pGatewayARN_
    }

-- | Undocumented member.
describeBandwidthRateLimit_gatewayARN :: Lens.Lens' DescribeBandwidthRateLimit Prelude.Text
describeBandwidthRateLimit_gatewayARN = Lens.lens (\DescribeBandwidthRateLimit' {gatewayARN} -> gatewayARN) (\s@DescribeBandwidthRateLimit' {} a -> s {gatewayARN = a} :: DescribeBandwidthRateLimit)

instance Core.AWSRequest DescribeBandwidthRateLimit where
  type
    AWSResponse DescribeBandwidthRateLimit =
      DescribeBandwidthRateLimitResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeBandwidthRateLimitResponse'
            Prelude.<$> (x Data..?> "AverageDownloadRateLimitInBitsPerSec")
            Prelude.<*> (x Data..?> "AverageUploadRateLimitInBitsPerSec")
            Prelude.<*> (x Data..?> "GatewayARN")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeBandwidthRateLimit where
  hashWithSalt _salt DescribeBandwidthRateLimit' {..} =
    _salt `Prelude.hashWithSalt` gatewayARN

instance Prelude.NFData DescribeBandwidthRateLimit where
  rnf DescribeBandwidthRateLimit' {..} =
    Prelude.rnf gatewayARN

instance Data.ToHeaders DescribeBandwidthRateLimit where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StorageGateway_20130630.DescribeBandwidthRateLimit" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON DescribeBandwidthRateLimit where
  toJSON DescribeBandwidthRateLimit' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("GatewayARN" Data..= gatewayARN)]
      )

instance Data.ToPath DescribeBandwidthRateLimit where
  toPath = Prelude.const "/"

instance Data.ToQuery DescribeBandwidthRateLimit where
  toQuery = Prelude.const Prelude.mempty

-- | A JSON object containing the following fields:
--
-- /See:/ 'newDescribeBandwidthRateLimitResponse' smart constructor.
data DescribeBandwidthRateLimitResponse = DescribeBandwidthRateLimitResponse'
  { -- | The average download bandwidth rate limit in bits per second. This field
    -- does not appear in the response if the download rate limit is not set.
    averageDownloadRateLimitInBitsPerSec :: Prelude.Maybe Prelude.Natural,
    -- | The average upload bandwidth rate limit in bits per second. This field
    -- does not appear in the response if the upload rate limit is not set.
    averageUploadRateLimitInBitsPerSec :: Prelude.Maybe Prelude.Natural,
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeBandwidthRateLimitResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'averageDownloadRateLimitInBitsPerSec', 'describeBandwidthRateLimitResponse_averageDownloadRateLimitInBitsPerSec' - The average download bandwidth rate limit in bits per second. This field
-- does not appear in the response if the download rate limit is not set.
--
-- 'averageUploadRateLimitInBitsPerSec', 'describeBandwidthRateLimitResponse_averageUploadRateLimitInBitsPerSec' - The average upload bandwidth rate limit in bits per second. This field
-- does not appear in the response if the upload rate limit is not set.
--
-- 'gatewayARN', 'describeBandwidthRateLimitResponse_gatewayARN' - Undocumented member.
--
-- 'httpStatus', 'describeBandwidthRateLimitResponse_httpStatus' - The response's http status code.
newDescribeBandwidthRateLimitResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeBandwidthRateLimitResponse
newDescribeBandwidthRateLimitResponse pHttpStatus_ =
  DescribeBandwidthRateLimitResponse'
    { averageDownloadRateLimitInBitsPerSec =
        Prelude.Nothing,
      averageUploadRateLimitInBitsPerSec =
        Prelude.Nothing,
      gatewayARN = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The average download bandwidth rate limit in bits per second. This field
-- does not appear in the response if the download rate limit is not set.
describeBandwidthRateLimitResponse_averageDownloadRateLimitInBitsPerSec :: Lens.Lens' DescribeBandwidthRateLimitResponse (Prelude.Maybe Prelude.Natural)
describeBandwidthRateLimitResponse_averageDownloadRateLimitInBitsPerSec = Lens.lens (\DescribeBandwidthRateLimitResponse' {averageDownloadRateLimitInBitsPerSec} -> averageDownloadRateLimitInBitsPerSec) (\s@DescribeBandwidthRateLimitResponse' {} a -> s {averageDownloadRateLimitInBitsPerSec = a} :: DescribeBandwidthRateLimitResponse)

-- | The average upload bandwidth rate limit in bits per second. This field
-- does not appear in the response if the upload rate limit is not set.
describeBandwidthRateLimitResponse_averageUploadRateLimitInBitsPerSec :: Lens.Lens' DescribeBandwidthRateLimitResponse (Prelude.Maybe Prelude.Natural)
describeBandwidthRateLimitResponse_averageUploadRateLimitInBitsPerSec = Lens.lens (\DescribeBandwidthRateLimitResponse' {averageUploadRateLimitInBitsPerSec} -> averageUploadRateLimitInBitsPerSec) (\s@DescribeBandwidthRateLimitResponse' {} a -> s {averageUploadRateLimitInBitsPerSec = a} :: DescribeBandwidthRateLimitResponse)

-- | Undocumented member.
describeBandwidthRateLimitResponse_gatewayARN :: Lens.Lens' DescribeBandwidthRateLimitResponse (Prelude.Maybe Prelude.Text)
describeBandwidthRateLimitResponse_gatewayARN = Lens.lens (\DescribeBandwidthRateLimitResponse' {gatewayARN} -> gatewayARN) (\s@DescribeBandwidthRateLimitResponse' {} a -> s {gatewayARN = a} :: DescribeBandwidthRateLimitResponse)

-- | The response's http status code.
describeBandwidthRateLimitResponse_httpStatus :: Lens.Lens' DescribeBandwidthRateLimitResponse Prelude.Int
describeBandwidthRateLimitResponse_httpStatus = Lens.lens (\DescribeBandwidthRateLimitResponse' {httpStatus} -> httpStatus) (\s@DescribeBandwidthRateLimitResponse' {} a -> s {httpStatus = a} :: DescribeBandwidthRateLimitResponse)

instance
  Prelude.NFData
    DescribeBandwidthRateLimitResponse
  where
  rnf DescribeBandwidthRateLimitResponse' {..} =
    Prelude.rnf averageDownloadRateLimitInBitsPerSec
      `Prelude.seq` Prelude.rnf averageUploadRateLimitInBitsPerSec
      `Prelude.seq` Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf httpStatus
