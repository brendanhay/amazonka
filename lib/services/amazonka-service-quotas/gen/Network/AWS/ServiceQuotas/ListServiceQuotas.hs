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
-- Module      : Amazonka.ServiceQuotas.ListServiceQuotas
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the applied quota values for the specified AWS service. For some
-- quotas, only the default values are available. If the applied quota
-- value is not available for a quota, the quota is not retrieved.
--
-- This operation returns paginated results.
module Amazonka.ServiceQuotas.ListServiceQuotas
  ( -- * Creating a Request
    ListServiceQuotas (..),
    newListServiceQuotas,

    -- * Request Lenses
    listServiceQuotas_nextToken,
    listServiceQuotas_maxResults,
    listServiceQuotas_serviceCode,

    -- * Destructuring the Response
    ListServiceQuotasResponse (..),
    newListServiceQuotasResponse,

    -- * Response Lenses
    listServiceQuotasResponse_nextToken,
    listServiceQuotasResponse_quotas,
    listServiceQuotasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newListServiceQuotas' smart constructor.
data ListServiceQuotas = ListServiceQuotas'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, if any, make another call with the token returned
    -- from this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The service identifier.
    serviceCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceQuotas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceQuotas_nextToken' - The token for the next page of results.
--
-- 'maxResults', 'listServiceQuotas_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, if any, make another call with the token returned
-- from this call.
--
-- 'serviceCode', 'listServiceQuotas_serviceCode' - The service identifier.
newListServiceQuotas ::
  -- | 'serviceCode'
  Prelude.Text ->
  ListServiceQuotas
newListServiceQuotas pServiceCode_ =
  ListServiceQuotas'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      serviceCode = pServiceCode_
    }

-- | The token for the next page of results.
listServiceQuotas_nextToken :: Lens.Lens' ListServiceQuotas (Prelude.Maybe Prelude.Text)
listServiceQuotas_nextToken = Lens.lens (\ListServiceQuotas' {nextToken} -> nextToken) (\s@ListServiceQuotas' {} a -> s {nextToken = a} :: ListServiceQuotas)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, if any, make another call with the token returned
-- from this call.
listServiceQuotas_maxResults :: Lens.Lens' ListServiceQuotas (Prelude.Maybe Prelude.Natural)
listServiceQuotas_maxResults = Lens.lens (\ListServiceQuotas' {maxResults} -> maxResults) (\s@ListServiceQuotas' {} a -> s {maxResults = a} :: ListServiceQuotas)

-- | The service identifier.
listServiceQuotas_serviceCode :: Lens.Lens' ListServiceQuotas Prelude.Text
listServiceQuotas_serviceCode = Lens.lens (\ListServiceQuotas' {serviceCode} -> serviceCode) (\s@ListServiceQuotas' {} a -> s {serviceCode = a} :: ListServiceQuotas)

instance Core.AWSPager ListServiceQuotas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServiceQuotasResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listServiceQuotasResponse_quotas
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listServiceQuotas_nextToken
          Lens..~ rs
          Lens.^? listServiceQuotasResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListServiceQuotas where
  type
    AWSResponse ListServiceQuotas =
      ListServiceQuotasResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceQuotasResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "Quotas" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListServiceQuotas

instance Prelude.NFData ListServiceQuotas

instance Core.ToHeaders ListServiceQuotas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ServiceQuotasV20190624.ListServiceQuotas" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListServiceQuotas where
  toJSON ListServiceQuotas' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            Prelude.Just ("ServiceCode" Core..= serviceCode)
          ]
      )

instance Core.ToPath ListServiceQuotas where
  toPath = Prelude.const "/"

instance Core.ToQuery ListServiceQuotas where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServiceQuotasResponse' smart constructor.
data ListServiceQuotasResponse = ListServiceQuotasResponse'
  { -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Information about the quotas.
    quotas :: Prelude.Maybe [ServiceQuota],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceQuotasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceQuotasResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'quotas', 'listServiceQuotasResponse_quotas' - Information about the quotas.
--
-- 'httpStatus', 'listServiceQuotasResponse_httpStatus' - The response's http status code.
newListServiceQuotasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceQuotasResponse
newListServiceQuotasResponse pHttpStatus_ =
  ListServiceQuotasResponse'
    { nextToken =
        Prelude.Nothing,
      quotas = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
listServiceQuotasResponse_nextToken :: Lens.Lens' ListServiceQuotasResponse (Prelude.Maybe Prelude.Text)
listServiceQuotasResponse_nextToken = Lens.lens (\ListServiceQuotasResponse' {nextToken} -> nextToken) (\s@ListServiceQuotasResponse' {} a -> s {nextToken = a} :: ListServiceQuotasResponse)

-- | Information about the quotas.
listServiceQuotasResponse_quotas :: Lens.Lens' ListServiceQuotasResponse (Prelude.Maybe [ServiceQuota])
listServiceQuotasResponse_quotas = Lens.lens (\ListServiceQuotasResponse' {quotas} -> quotas) (\s@ListServiceQuotasResponse' {} a -> s {quotas = a} :: ListServiceQuotasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listServiceQuotasResponse_httpStatus :: Lens.Lens' ListServiceQuotasResponse Prelude.Int
listServiceQuotasResponse_httpStatus = Lens.lens (\ListServiceQuotasResponse' {httpStatus} -> httpStatus) (\s@ListServiceQuotasResponse' {} a -> s {httpStatus = a} :: ListServiceQuotasResponse)

instance Prelude.NFData ListServiceQuotasResponse
