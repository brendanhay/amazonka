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
-- Module      : Amazonka.ServiceCatalog.GetProvisionedProductOutputs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API takes either a @ProvisonedProductId@ or a
-- @ProvisionedProductName@, along with a list of one or more output keys,
-- and responds with the key\/value pairs of those outputs.
module Amazonka.ServiceCatalog.GetProvisionedProductOutputs
  ( -- * Creating a Request
    GetProvisionedProductOutputs (..),
    newGetProvisionedProductOutputs,

    -- * Request Lenses
    getProvisionedProductOutputs_acceptLanguage,
    getProvisionedProductOutputs_outputKeys,
    getProvisionedProductOutputs_pageSize,
    getProvisionedProductOutputs_pageToken,
    getProvisionedProductOutputs_provisionedProductId,
    getProvisionedProductOutputs_provisionedProductName,

    -- * Destructuring the Response
    GetProvisionedProductOutputsResponse (..),
    newGetProvisionedProductOutputsResponse,

    -- * Response Lenses
    getProvisionedProductOutputsResponse_nextPageToken,
    getProvisionedProductOutputsResponse_outputs,
    getProvisionedProductOutputsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceCatalog.Types

-- | /See:/ 'newGetProvisionedProductOutputs' smart constructor.
data GetProvisionedProductOutputs = GetProvisionedProductOutputs'
  { -- | The language code.
    --
    -- -   @jp@ - Japanese
    --
    -- -   @zh@ - Chinese
    acceptLanguage :: Prelude.Maybe Prelude.Text,
    -- | The list of keys that the API should return with their values. If none
    -- are provided, the API will return all outputs of the provisioned
    -- product.
    outputKeys :: Prelude.Maybe [Prelude.Text],
    -- | The maximum number of items to return with this call.
    pageSize :: Prelude.Maybe Prelude.Natural,
    -- | The page token for the next set of results. To retrieve the first set of
    -- results, use null.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The identifier of the provisioned product that you want the outputs
    -- from.
    provisionedProductId :: Prelude.Maybe Prelude.Text,
    -- | The name of the provisioned product that you want the outputs from.
    provisionedProductName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProvisionedProductOutputs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'acceptLanguage', 'getProvisionedProductOutputs_acceptLanguage' - The language code.
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
--
-- 'outputKeys', 'getProvisionedProductOutputs_outputKeys' - The list of keys that the API should return with their values. If none
-- are provided, the API will return all outputs of the provisioned
-- product.
--
-- 'pageSize', 'getProvisionedProductOutputs_pageSize' - The maximum number of items to return with this call.
--
-- 'pageToken', 'getProvisionedProductOutputs_pageToken' - The page token for the next set of results. To retrieve the first set of
-- results, use null.
--
-- 'provisionedProductId', 'getProvisionedProductOutputs_provisionedProductId' - The identifier of the provisioned product that you want the outputs
-- from.
--
-- 'provisionedProductName', 'getProvisionedProductOutputs_provisionedProductName' - The name of the provisioned product that you want the outputs from.
newGetProvisionedProductOutputs ::
  GetProvisionedProductOutputs
newGetProvisionedProductOutputs =
  GetProvisionedProductOutputs'
    { acceptLanguage =
        Prelude.Nothing,
      outputKeys = Prelude.Nothing,
      pageSize = Prelude.Nothing,
      pageToken = Prelude.Nothing,
      provisionedProductId = Prelude.Nothing,
      provisionedProductName = Prelude.Nothing
    }

-- | The language code.
--
-- -   @jp@ - Japanese
--
-- -   @zh@ - Chinese
getProvisionedProductOutputs_acceptLanguage :: Lens.Lens' GetProvisionedProductOutputs (Prelude.Maybe Prelude.Text)
getProvisionedProductOutputs_acceptLanguage = Lens.lens (\GetProvisionedProductOutputs' {acceptLanguage} -> acceptLanguage) (\s@GetProvisionedProductOutputs' {} a -> s {acceptLanguage = a} :: GetProvisionedProductOutputs)

-- | The list of keys that the API should return with their values. If none
-- are provided, the API will return all outputs of the provisioned
-- product.
getProvisionedProductOutputs_outputKeys :: Lens.Lens' GetProvisionedProductOutputs (Prelude.Maybe [Prelude.Text])
getProvisionedProductOutputs_outputKeys = Lens.lens (\GetProvisionedProductOutputs' {outputKeys} -> outputKeys) (\s@GetProvisionedProductOutputs' {} a -> s {outputKeys = a} :: GetProvisionedProductOutputs) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of items to return with this call.
getProvisionedProductOutputs_pageSize :: Lens.Lens' GetProvisionedProductOutputs (Prelude.Maybe Prelude.Natural)
getProvisionedProductOutputs_pageSize = Lens.lens (\GetProvisionedProductOutputs' {pageSize} -> pageSize) (\s@GetProvisionedProductOutputs' {} a -> s {pageSize = a} :: GetProvisionedProductOutputs)

-- | The page token for the next set of results. To retrieve the first set of
-- results, use null.
getProvisionedProductOutputs_pageToken :: Lens.Lens' GetProvisionedProductOutputs (Prelude.Maybe Prelude.Text)
getProvisionedProductOutputs_pageToken = Lens.lens (\GetProvisionedProductOutputs' {pageToken} -> pageToken) (\s@GetProvisionedProductOutputs' {} a -> s {pageToken = a} :: GetProvisionedProductOutputs)

-- | The identifier of the provisioned product that you want the outputs
-- from.
getProvisionedProductOutputs_provisionedProductId :: Lens.Lens' GetProvisionedProductOutputs (Prelude.Maybe Prelude.Text)
getProvisionedProductOutputs_provisionedProductId = Lens.lens (\GetProvisionedProductOutputs' {provisionedProductId} -> provisionedProductId) (\s@GetProvisionedProductOutputs' {} a -> s {provisionedProductId = a} :: GetProvisionedProductOutputs)

-- | The name of the provisioned product that you want the outputs from.
getProvisionedProductOutputs_provisionedProductName :: Lens.Lens' GetProvisionedProductOutputs (Prelude.Maybe Prelude.Text)
getProvisionedProductOutputs_provisionedProductName = Lens.lens (\GetProvisionedProductOutputs' {provisionedProductName} -> provisionedProductName) (\s@GetProvisionedProductOutputs' {} a -> s {provisionedProductName = a} :: GetProvisionedProductOutputs)

instance Core.AWSRequest GetProvisionedProductOutputs where
  type
    AWSResponse GetProvisionedProductOutputs =
      GetProvisionedProductOutputsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetProvisionedProductOutputsResponse'
            Prelude.<$> (x Data..?> "NextPageToken")
            Prelude.<*> (x Data..?> "Outputs" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetProvisionedProductOutputs
  where
  hashWithSalt _salt GetProvisionedProductOutputs' {..} =
    _salt
      `Prelude.hashWithSalt` acceptLanguage
      `Prelude.hashWithSalt` outputKeys
      `Prelude.hashWithSalt` pageSize
      `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` provisionedProductId
      `Prelude.hashWithSalt` provisionedProductName

instance Prelude.NFData GetProvisionedProductOutputs where
  rnf GetProvisionedProductOutputs' {..} =
    Prelude.rnf acceptLanguage
      `Prelude.seq` Prelude.rnf outputKeys
      `Prelude.seq` Prelude.rnf pageSize
      `Prelude.seq` Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf provisionedProductId
      `Prelude.seq` Prelude.rnf provisionedProductName

instance Data.ToHeaders GetProvisionedProductOutputs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "AWS242ServiceCatalogService.GetProvisionedProductOutputs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetProvisionedProductOutputs where
  toJSON GetProvisionedProductOutputs' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("AcceptLanguage" Data..=)
              Prelude.<$> acceptLanguage,
            ("OutputKeys" Data..=) Prelude.<$> outputKeys,
            ("PageSize" Data..=) Prelude.<$> pageSize,
            ("PageToken" Data..=) Prelude.<$> pageToken,
            ("ProvisionedProductId" Data..=)
              Prelude.<$> provisionedProductId,
            ("ProvisionedProductName" Data..=)
              Prelude.<$> provisionedProductName
          ]
      )

instance Data.ToPath GetProvisionedProductOutputs where
  toPath = Prelude.const "/"

instance Data.ToQuery GetProvisionedProductOutputs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetProvisionedProductOutputsResponse' smart constructor.
data GetProvisionedProductOutputsResponse = GetProvisionedProductOutputsResponse'
  { -- | The page token to use to retrieve the next set of results. If there are
    -- no additional results, this value is null.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the product created as the result of a request. For
    -- example, the output for a CloudFormation-backed product that creates an
    -- S3 bucket would include the S3 bucket URL.
    outputs :: Prelude.Maybe [RecordOutput],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetProvisionedProductOutputsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getProvisionedProductOutputsResponse_nextPageToken' - The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
--
-- 'outputs', 'getProvisionedProductOutputsResponse_outputs' - Information about the product created as the result of a request. For
-- example, the output for a CloudFormation-backed product that creates an
-- S3 bucket would include the S3 bucket URL.
--
-- 'httpStatus', 'getProvisionedProductOutputsResponse_httpStatus' - The response's http status code.
newGetProvisionedProductOutputsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetProvisionedProductOutputsResponse
newGetProvisionedProductOutputsResponse pHttpStatus_ =
  GetProvisionedProductOutputsResponse'
    { nextPageToken =
        Prelude.Nothing,
      outputs = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The page token to use to retrieve the next set of results. If there are
-- no additional results, this value is null.
getProvisionedProductOutputsResponse_nextPageToken :: Lens.Lens' GetProvisionedProductOutputsResponse (Prelude.Maybe Prelude.Text)
getProvisionedProductOutputsResponse_nextPageToken = Lens.lens (\GetProvisionedProductOutputsResponse' {nextPageToken} -> nextPageToken) (\s@GetProvisionedProductOutputsResponse' {} a -> s {nextPageToken = a} :: GetProvisionedProductOutputsResponse)

-- | Information about the product created as the result of a request. For
-- example, the output for a CloudFormation-backed product that creates an
-- S3 bucket would include the S3 bucket URL.
getProvisionedProductOutputsResponse_outputs :: Lens.Lens' GetProvisionedProductOutputsResponse (Prelude.Maybe [RecordOutput])
getProvisionedProductOutputsResponse_outputs = Lens.lens (\GetProvisionedProductOutputsResponse' {outputs} -> outputs) (\s@GetProvisionedProductOutputsResponse' {} a -> s {outputs = a} :: GetProvisionedProductOutputsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getProvisionedProductOutputsResponse_httpStatus :: Lens.Lens' GetProvisionedProductOutputsResponse Prelude.Int
getProvisionedProductOutputsResponse_httpStatus = Lens.lens (\GetProvisionedProductOutputsResponse' {httpStatus} -> httpStatus) (\s@GetProvisionedProductOutputsResponse' {} a -> s {httpStatus = a} :: GetProvisionedProductOutputsResponse)

instance
  Prelude.NFData
    GetProvisionedProductOutputsResponse
  where
  rnf GetProvisionedProductOutputsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf outputs
      `Prelude.seq` Prelude.rnf httpStatus
