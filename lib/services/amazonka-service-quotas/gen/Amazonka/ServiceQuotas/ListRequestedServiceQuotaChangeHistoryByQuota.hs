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
-- Module      : Amazonka.ServiceQuotas.ListRequestedServiceQuotaChangeHistoryByQuota
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the quota increase requests for the specified quota.
--
-- This operation returns paginated results.
module Amazonka.ServiceQuotas.ListRequestedServiceQuotaChangeHistoryByQuota
  ( -- * Creating a Request
    ListRequestedServiceQuotaChangeHistoryByQuota (..),
    newListRequestedServiceQuotaChangeHistoryByQuota,

    -- * Request Lenses
    listRequestedServiceQuotaChangeHistoryByQuota_maxResults,
    listRequestedServiceQuotaChangeHistoryByQuota_nextToken,
    listRequestedServiceQuotaChangeHistoryByQuota_status,
    listRequestedServiceQuotaChangeHistoryByQuota_serviceCode,
    listRequestedServiceQuotaChangeHistoryByQuota_quotaCode,

    -- * Destructuring the Response
    ListRequestedServiceQuotaChangeHistoryByQuotaResponse (..),
    newListRequestedServiceQuotaChangeHistoryByQuotaResponse,

    -- * Response Lenses
    listRequestedServiceQuotaChangeHistoryByQuotaResponse_nextToken,
    listRequestedServiceQuotaChangeHistoryByQuotaResponse_requestedQuotas,
    listRequestedServiceQuotaChangeHistoryByQuotaResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newListRequestedServiceQuotaChangeHistoryByQuota' smart constructor.
data ListRequestedServiceQuotaChangeHistoryByQuota = ListRequestedServiceQuotaChangeHistoryByQuota'
  { -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, if any, make another call with the token returned
    -- from this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The status value of the quota increase request.
    status :: Prelude.Maybe RequestStatus,
    -- | The service identifier.
    serviceCode :: Prelude.Text,
    -- | The quota identifier.
    quotaCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRequestedServiceQuotaChangeHistoryByQuota' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listRequestedServiceQuotaChangeHistoryByQuota_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, if any, make another call with the token returned
-- from this call.
--
-- 'nextToken', 'listRequestedServiceQuotaChangeHistoryByQuota_nextToken' - The token for the next page of results.
--
-- 'status', 'listRequestedServiceQuotaChangeHistoryByQuota_status' - The status value of the quota increase request.
--
-- 'serviceCode', 'listRequestedServiceQuotaChangeHistoryByQuota_serviceCode' - The service identifier.
--
-- 'quotaCode', 'listRequestedServiceQuotaChangeHistoryByQuota_quotaCode' - The quota identifier.
newListRequestedServiceQuotaChangeHistoryByQuota ::
  -- | 'serviceCode'
  Prelude.Text ->
  -- | 'quotaCode'
  Prelude.Text ->
  ListRequestedServiceQuotaChangeHistoryByQuota
newListRequestedServiceQuotaChangeHistoryByQuota
  pServiceCode_
  pQuotaCode_ =
    ListRequestedServiceQuotaChangeHistoryByQuota'
      { maxResults =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        status = Prelude.Nothing,
        serviceCode = pServiceCode_,
        quotaCode = pQuotaCode_
      }

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, if any, make another call with the token returned
-- from this call.
listRequestedServiceQuotaChangeHistoryByQuota_maxResults :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryByQuota (Prelude.Maybe Prelude.Natural)
listRequestedServiceQuotaChangeHistoryByQuota_maxResults = Lens.lens (\ListRequestedServiceQuotaChangeHistoryByQuota' {maxResults} -> maxResults) (\s@ListRequestedServiceQuotaChangeHistoryByQuota' {} a -> s {maxResults = a} :: ListRequestedServiceQuotaChangeHistoryByQuota)

-- | The token for the next page of results.
listRequestedServiceQuotaChangeHistoryByQuota_nextToken :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryByQuota (Prelude.Maybe Prelude.Text)
listRequestedServiceQuotaChangeHistoryByQuota_nextToken = Lens.lens (\ListRequestedServiceQuotaChangeHistoryByQuota' {nextToken} -> nextToken) (\s@ListRequestedServiceQuotaChangeHistoryByQuota' {} a -> s {nextToken = a} :: ListRequestedServiceQuotaChangeHistoryByQuota)

-- | The status value of the quota increase request.
listRequestedServiceQuotaChangeHistoryByQuota_status :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryByQuota (Prelude.Maybe RequestStatus)
listRequestedServiceQuotaChangeHistoryByQuota_status = Lens.lens (\ListRequestedServiceQuotaChangeHistoryByQuota' {status} -> status) (\s@ListRequestedServiceQuotaChangeHistoryByQuota' {} a -> s {status = a} :: ListRequestedServiceQuotaChangeHistoryByQuota)

-- | The service identifier.
listRequestedServiceQuotaChangeHistoryByQuota_serviceCode :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryByQuota Prelude.Text
listRequestedServiceQuotaChangeHistoryByQuota_serviceCode = Lens.lens (\ListRequestedServiceQuotaChangeHistoryByQuota' {serviceCode} -> serviceCode) (\s@ListRequestedServiceQuotaChangeHistoryByQuota' {} a -> s {serviceCode = a} :: ListRequestedServiceQuotaChangeHistoryByQuota)

-- | The quota identifier.
listRequestedServiceQuotaChangeHistoryByQuota_quotaCode :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryByQuota Prelude.Text
listRequestedServiceQuotaChangeHistoryByQuota_quotaCode = Lens.lens (\ListRequestedServiceQuotaChangeHistoryByQuota' {quotaCode} -> quotaCode) (\s@ListRequestedServiceQuotaChangeHistoryByQuota' {} a -> s {quotaCode = a} :: ListRequestedServiceQuotaChangeHistoryByQuota)

instance
  Core.AWSPager
    ListRequestedServiceQuotaChangeHistoryByQuota
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRequestedServiceQuotaChangeHistoryByQuotaResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRequestedServiceQuotaChangeHistoryByQuotaResponse_requestedQuotas
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRequestedServiceQuotaChangeHistoryByQuota_nextToken
          Lens..~ rs
            Lens.^? listRequestedServiceQuotaChangeHistoryByQuotaResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListRequestedServiceQuotaChangeHistoryByQuota
  where
  type
    AWSResponse
      ListRequestedServiceQuotaChangeHistoryByQuota =
      ListRequestedServiceQuotaChangeHistoryByQuotaResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRequestedServiceQuotaChangeHistoryByQuotaResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> ( x Data..?> "RequestedQuotas"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListRequestedServiceQuotaChangeHistoryByQuota
  where
  hashWithSalt
    _salt
    ListRequestedServiceQuotaChangeHistoryByQuota' {..} =
      _salt `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` status
        `Prelude.hashWithSalt` serviceCode
        `Prelude.hashWithSalt` quotaCode

instance
  Prelude.NFData
    ListRequestedServiceQuotaChangeHistoryByQuota
  where
  rnf
    ListRequestedServiceQuotaChangeHistoryByQuota' {..} =
      Prelude.rnf maxResults
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf status
        `Prelude.seq` Prelude.rnf serviceCode
        `Prelude.seq` Prelude.rnf quotaCode

instance
  Data.ToHeaders
    ListRequestedServiceQuotaChangeHistoryByQuota
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ServiceQuotasV20190624.ListRequestedServiceQuotaChangeHistoryByQuota" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListRequestedServiceQuotaChangeHistoryByQuota
  where
  toJSON
    ListRequestedServiceQuotaChangeHistoryByQuota' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("MaxResults" Data..=) Prelude.<$> maxResults,
              ("NextToken" Data..=) Prelude.<$> nextToken,
              ("Status" Data..=) Prelude.<$> status,
              Prelude.Just ("ServiceCode" Data..= serviceCode),
              Prelude.Just ("QuotaCode" Data..= quotaCode)
            ]
        )

instance
  Data.ToPath
    ListRequestedServiceQuotaChangeHistoryByQuota
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListRequestedServiceQuotaChangeHistoryByQuota
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRequestedServiceQuotaChangeHistoryByQuotaResponse' smart constructor.
data ListRequestedServiceQuotaChangeHistoryByQuotaResponse = ListRequestedServiceQuotaChangeHistoryByQuotaResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the quota increase requests.
    requestedQuotas :: Prelude.Maybe [RequestedServiceQuotaChange],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRequestedServiceQuotaChangeHistoryByQuotaResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRequestedServiceQuotaChangeHistoryByQuotaResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'requestedQuotas', 'listRequestedServiceQuotaChangeHistoryByQuotaResponse_requestedQuotas' - Information about the quota increase requests.
--
-- 'httpStatus', 'listRequestedServiceQuotaChangeHistoryByQuotaResponse_httpStatus' - The response's http status code.
newListRequestedServiceQuotaChangeHistoryByQuotaResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRequestedServiceQuotaChangeHistoryByQuotaResponse
newListRequestedServiceQuotaChangeHistoryByQuotaResponse
  pHttpStatus_ =
    ListRequestedServiceQuotaChangeHistoryByQuotaResponse'
      { nextToken =
          Prelude.Nothing,
        requestedQuotas =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
listRequestedServiceQuotaChangeHistoryByQuotaResponse_nextToken :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryByQuotaResponse (Prelude.Maybe Prelude.Text)
listRequestedServiceQuotaChangeHistoryByQuotaResponse_nextToken = Lens.lens (\ListRequestedServiceQuotaChangeHistoryByQuotaResponse' {nextToken} -> nextToken) (\s@ListRequestedServiceQuotaChangeHistoryByQuotaResponse' {} a -> s {nextToken = a} :: ListRequestedServiceQuotaChangeHistoryByQuotaResponse)

-- | Information about the quota increase requests.
listRequestedServiceQuotaChangeHistoryByQuotaResponse_requestedQuotas :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryByQuotaResponse (Prelude.Maybe [RequestedServiceQuotaChange])
listRequestedServiceQuotaChangeHistoryByQuotaResponse_requestedQuotas = Lens.lens (\ListRequestedServiceQuotaChangeHistoryByQuotaResponse' {requestedQuotas} -> requestedQuotas) (\s@ListRequestedServiceQuotaChangeHistoryByQuotaResponse' {} a -> s {requestedQuotas = a} :: ListRequestedServiceQuotaChangeHistoryByQuotaResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRequestedServiceQuotaChangeHistoryByQuotaResponse_httpStatus :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryByQuotaResponse Prelude.Int
listRequestedServiceQuotaChangeHistoryByQuotaResponse_httpStatus = Lens.lens (\ListRequestedServiceQuotaChangeHistoryByQuotaResponse' {httpStatus} -> httpStatus) (\s@ListRequestedServiceQuotaChangeHistoryByQuotaResponse' {} a -> s {httpStatus = a} :: ListRequestedServiceQuotaChangeHistoryByQuotaResponse)

instance
  Prelude.NFData
    ListRequestedServiceQuotaChangeHistoryByQuotaResponse
  where
  rnf
    ListRequestedServiceQuotaChangeHistoryByQuotaResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf requestedQuotas
        `Prelude.seq` Prelude.rnf httpStatus
