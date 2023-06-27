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
-- Module      : Amazonka.Pricing.GetPriceListFileUrl
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- /__This feature is in preview release and is subject to change. Your use
-- of Amazon Web Services Price List API is subject to the Beta Service
-- Participation terms of the
-- <https://aws.amazon.com/service-terms/ Amazon Web Services Service Terms>
-- (Section 1.10).__/
--
-- This returns the URL that you can retrieve your Price List file from.
-- This URL is based on the @PriceListArn@ and @FileFormat@ that you
-- retrieve from the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceLists>
-- response.
module Amazonka.Pricing.GetPriceListFileUrl
  ( -- * Creating a Request
    GetPriceListFileUrl (..),
    newGetPriceListFileUrl,

    -- * Request Lenses
    getPriceListFileUrl_priceListArn,
    getPriceListFileUrl_fileFormat,

    -- * Destructuring the Response
    GetPriceListFileUrlResponse (..),
    newGetPriceListFileUrlResponse,

    -- * Response Lenses
    getPriceListFileUrlResponse_url,
    getPriceListFileUrlResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.Pricing.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetPriceListFileUrl' smart constructor.
data GetPriceListFileUrl = GetPriceListFileUrl'
  { -- | The unique identifier that maps to where your Price List files are
    -- located. @PriceListArn@ can be obtained from the
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceLists>
    -- response.
    priceListArn :: Prelude.Text,
    -- | The format that you want to retrieve your Price List files in. The
    -- @FileFormat@ can be obtained from the
    -- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceLists>
    -- response.
    fileFormat :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPriceListFileUrl' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'priceListArn', 'getPriceListFileUrl_priceListArn' - The unique identifier that maps to where your Price List files are
-- located. @PriceListArn@ can be obtained from the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceLists>
-- response.
--
-- 'fileFormat', 'getPriceListFileUrl_fileFormat' - The format that you want to retrieve your Price List files in. The
-- @FileFormat@ can be obtained from the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceLists>
-- response.
newGetPriceListFileUrl ::
  -- | 'priceListArn'
  Prelude.Text ->
  -- | 'fileFormat'
  Prelude.Text ->
  GetPriceListFileUrl
newGetPriceListFileUrl pPriceListArn_ pFileFormat_ =
  GetPriceListFileUrl'
    { priceListArn = pPriceListArn_,
      fileFormat = pFileFormat_
    }

-- | The unique identifier that maps to where your Price List files are
-- located. @PriceListArn@ can be obtained from the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceLists>
-- response.
getPriceListFileUrl_priceListArn :: Lens.Lens' GetPriceListFileUrl Prelude.Text
getPriceListFileUrl_priceListArn = Lens.lens (\GetPriceListFileUrl' {priceListArn} -> priceListArn) (\s@GetPriceListFileUrl' {} a -> s {priceListArn = a} :: GetPriceListFileUrl)

-- | The format that you want to retrieve your Price List files in. The
-- @FileFormat@ can be obtained from the
-- <https://docs.aws.amazon.com/aws-cost-management/latest/APIReference/API_pricing_ListPriceLists.html ListPriceLists>
-- response.
getPriceListFileUrl_fileFormat :: Lens.Lens' GetPriceListFileUrl Prelude.Text
getPriceListFileUrl_fileFormat = Lens.lens (\GetPriceListFileUrl' {fileFormat} -> fileFormat) (\s@GetPriceListFileUrl' {} a -> s {fileFormat = a} :: GetPriceListFileUrl)

instance Core.AWSRequest GetPriceListFileUrl where
  type
    AWSResponse GetPriceListFileUrl =
      GetPriceListFileUrlResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetPriceListFileUrlResponse'
            Prelude.<$> (x Data..?> "Url")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetPriceListFileUrl where
  hashWithSalt _salt GetPriceListFileUrl' {..} =
    _salt
      `Prelude.hashWithSalt` priceListArn
      `Prelude.hashWithSalt` fileFormat

instance Prelude.NFData GetPriceListFileUrl where
  rnf GetPriceListFileUrl' {..} =
    Prelude.rnf priceListArn
      `Prelude.seq` Prelude.rnf fileFormat

instance Data.ToHeaders GetPriceListFileUrl where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWSPriceListService.GetPriceListFileUrl" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetPriceListFileUrl where
  toJSON GetPriceListFileUrl' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("PriceListArn" Data..= priceListArn),
            Prelude.Just ("FileFormat" Data..= fileFormat)
          ]
      )

instance Data.ToPath GetPriceListFileUrl where
  toPath = Prelude.const "/"

instance Data.ToQuery GetPriceListFileUrl where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetPriceListFileUrlResponse' smart constructor.
data GetPriceListFileUrlResponse = GetPriceListFileUrlResponse'
  { -- | The URL to download your Price List file from.
    url :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetPriceListFileUrlResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'url', 'getPriceListFileUrlResponse_url' - The URL to download your Price List file from.
--
-- 'httpStatus', 'getPriceListFileUrlResponse_httpStatus' - The response's http status code.
newGetPriceListFileUrlResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetPriceListFileUrlResponse
newGetPriceListFileUrlResponse pHttpStatus_ =
  GetPriceListFileUrlResponse'
    { url = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The URL to download your Price List file from.
getPriceListFileUrlResponse_url :: Lens.Lens' GetPriceListFileUrlResponse (Prelude.Maybe Prelude.Text)
getPriceListFileUrlResponse_url = Lens.lens (\GetPriceListFileUrlResponse' {url} -> url) (\s@GetPriceListFileUrlResponse' {} a -> s {url = a} :: GetPriceListFileUrlResponse)

-- | The response's http status code.
getPriceListFileUrlResponse_httpStatus :: Lens.Lens' GetPriceListFileUrlResponse Prelude.Int
getPriceListFileUrlResponse_httpStatus = Lens.lens (\GetPriceListFileUrlResponse' {httpStatus} -> httpStatus) (\s@GetPriceListFileUrlResponse' {} a -> s {httpStatus = a} :: GetPriceListFileUrlResponse)

instance Prelude.NFData GetPriceListFileUrlResponse where
  rnf GetPriceListFileUrlResponse' {..} =
    Prelude.rnf url
      `Prelude.seq` Prelude.rnf httpStatus
