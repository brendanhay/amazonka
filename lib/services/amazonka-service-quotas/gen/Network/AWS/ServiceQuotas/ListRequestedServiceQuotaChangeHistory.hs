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
-- Module      : Network.AWS.ServiceQuotas.ListRequestedServiceQuotaChangeHistory
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the quota increase requests for the specified service.
--
-- This operation returns paginated results.
module Network.AWS.ServiceQuotas.ListRequestedServiceQuotaChangeHistory
  ( -- * Creating a Request
    ListRequestedServiceQuotaChangeHistory (..),
    newListRequestedServiceQuotaChangeHistory,

    -- * Request Lenses
    listRequestedServiceQuotaChangeHistory_status,
    listRequestedServiceQuotaChangeHistory_nextToken,
    listRequestedServiceQuotaChangeHistory_serviceCode,
    listRequestedServiceQuotaChangeHistory_maxResults,

    -- * Destructuring the Response
    ListRequestedServiceQuotaChangeHistoryResponse (..),
    newListRequestedServiceQuotaChangeHistoryResponse,

    -- * Response Lenses
    listRequestedServiceQuotaChangeHistoryResponse_nextToken,
    listRequestedServiceQuotaChangeHistoryResponse_requestedQuotas,
    listRequestedServiceQuotaChangeHistoryResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.ServiceQuotas.Types

-- | /See:/ 'newListRequestedServiceQuotaChangeHistory' smart constructor.
data ListRequestedServiceQuotaChangeHistory = ListRequestedServiceQuotaChangeHistory'
  { -- | The status of the quota increase request.
    status :: Prelude.Maybe RequestStatus,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The service identifier.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, if any, make another call with the token returned
    -- from this call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListRequestedServiceQuotaChangeHistory' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'status', 'listRequestedServiceQuotaChangeHistory_status' - The status of the quota increase request.
--
-- 'nextToken', 'listRequestedServiceQuotaChangeHistory_nextToken' - The token for the next page of results.
--
-- 'serviceCode', 'listRequestedServiceQuotaChangeHistory_serviceCode' - The service identifier.
--
-- 'maxResults', 'listRequestedServiceQuotaChangeHistory_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, if any, make another call with the token returned
-- from this call.
newListRequestedServiceQuotaChangeHistory ::
  ListRequestedServiceQuotaChangeHistory
newListRequestedServiceQuotaChangeHistory =
  ListRequestedServiceQuotaChangeHistory'
    { status =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The status of the quota increase request.
listRequestedServiceQuotaChangeHistory_status :: Lens.Lens' ListRequestedServiceQuotaChangeHistory (Prelude.Maybe RequestStatus)
listRequestedServiceQuotaChangeHistory_status = Lens.lens (\ListRequestedServiceQuotaChangeHistory' {status} -> status) (\s@ListRequestedServiceQuotaChangeHistory' {} a -> s {status = a} :: ListRequestedServiceQuotaChangeHistory)

-- | The token for the next page of results.
listRequestedServiceQuotaChangeHistory_nextToken :: Lens.Lens' ListRequestedServiceQuotaChangeHistory (Prelude.Maybe Prelude.Text)
listRequestedServiceQuotaChangeHistory_nextToken = Lens.lens (\ListRequestedServiceQuotaChangeHistory' {nextToken} -> nextToken) (\s@ListRequestedServiceQuotaChangeHistory' {} a -> s {nextToken = a} :: ListRequestedServiceQuotaChangeHistory)

-- | The service identifier.
listRequestedServiceQuotaChangeHistory_serviceCode :: Lens.Lens' ListRequestedServiceQuotaChangeHistory (Prelude.Maybe Prelude.Text)
listRequestedServiceQuotaChangeHistory_serviceCode = Lens.lens (\ListRequestedServiceQuotaChangeHistory' {serviceCode} -> serviceCode) (\s@ListRequestedServiceQuotaChangeHistory' {} a -> s {serviceCode = a} :: ListRequestedServiceQuotaChangeHistory)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, if any, make another call with the token returned
-- from this call.
listRequestedServiceQuotaChangeHistory_maxResults :: Lens.Lens' ListRequestedServiceQuotaChangeHistory (Prelude.Maybe Prelude.Natural)
listRequestedServiceQuotaChangeHistory_maxResults = Lens.lens (\ListRequestedServiceQuotaChangeHistory' {maxResults} -> maxResults) (\s@ListRequestedServiceQuotaChangeHistory' {} a -> s {maxResults = a} :: ListRequestedServiceQuotaChangeHistory)

instance
  Core.AWSPager
    ListRequestedServiceQuotaChangeHistory
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listRequestedServiceQuotaChangeHistoryResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listRequestedServiceQuotaChangeHistoryResponse_requestedQuotas
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listRequestedServiceQuotaChangeHistory_nextToken
          Lens..~ rs
            Lens.^? listRequestedServiceQuotaChangeHistoryResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListRequestedServiceQuotaChangeHistory
  where
  type
    AWSResponse
      ListRequestedServiceQuotaChangeHistory =
      ListRequestedServiceQuotaChangeHistoryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListRequestedServiceQuotaChangeHistoryResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "RequestedQuotas"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListRequestedServiceQuotaChangeHistory

instance
  Prelude.NFData
    ListRequestedServiceQuotaChangeHistory

instance
  Core.ToHeaders
    ListRequestedServiceQuotaChangeHistory
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ServiceQuotasV20190624.ListRequestedServiceQuotaChangeHistory" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListRequestedServiceQuotaChangeHistory
  where
  toJSON ListRequestedServiceQuotaChangeHistory' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("Status" Core..=) Prelude.<$> status,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("ServiceCode" Core..=) Prelude.<$> serviceCode,
            ("MaxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance
  Core.ToPath
    ListRequestedServiceQuotaChangeHistory
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListRequestedServiceQuotaChangeHistory
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListRequestedServiceQuotaChangeHistoryResponse' smart constructor.
data ListRequestedServiceQuotaChangeHistoryResponse = ListRequestedServiceQuotaChangeHistoryResponse'
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
-- Create a value of 'ListRequestedServiceQuotaChangeHistoryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listRequestedServiceQuotaChangeHistoryResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'requestedQuotas', 'listRequestedServiceQuotaChangeHistoryResponse_requestedQuotas' - Information about the quota increase requests.
--
-- 'httpStatus', 'listRequestedServiceQuotaChangeHistoryResponse_httpStatus' - The response's http status code.
newListRequestedServiceQuotaChangeHistoryResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListRequestedServiceQuotaChangeHistoryResponse
newListRequestedServiceQuotaChangeHistoryResponse
  pHttpStatus_ =
    ListRequestedServiceQuotaChangeHistoryResponse'
      { nextToken =
          Prelude.Nothing,
        requestedQuotas =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
listRequestedServiceQuotaChangeHistoryResponse_nextToken :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryResponse (Prelude.Maybe Prelude.Text)
listRequestedServiceQuotaChangeHistoryResponse_nextToken = Lens.lens (\ListRequestedServiceQuotaChangeHistoryResponse' {nextToken} -> nextToken) (\s@ListRequestedServiceQuotaChangeHistoryResponse' {} a -> s {nextToken = a} :: ListRequestedServiceQuotaChangeHistoryResponse)

-- | Information about the quota increase requests.
listRequestedServiceQuotaChangeHistoryResponse_requestedQuotas :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryResponse (Prelude.Maybe [RequestedServiceQuotaChange])
listRequestedServiceQuotaChangeHistoryResponse_requestedQuotas = Lens.lens (\ListRequestedServiceQuotaChangeHistoryResponse' {requestedQuotas} -> requestedQuotas) (\s@ListRequestedServiceQuotaChangeHistoryResponse' {} a -> s {requestedQuotas = a} :: ListRequestedServiceQuotaChangeHistoryResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listRequestedServiceQuotaChangeHistoryResponse_httpStatus :: Lens.Lens' ListRequestedServiceQuotaChangeHistoryResponse Prelude.Int
listRequestedServiceQuotaChangeHistoryResponse_httpStatus = Lens.lens (\ListRequestedServiceQuotaChangeHistoryResponse' {httpStatus} -> httpStatus) (\s@ListRequestedServiceQuotaChangeHistoryResponse' {} a -> s {httpStatus = a} :: ListRequestedServiceQuotaChangeHistoryResponse)

instance
  Prelude.NFData
    ListRequestedServiceQuotaChangeHistoryResponse
