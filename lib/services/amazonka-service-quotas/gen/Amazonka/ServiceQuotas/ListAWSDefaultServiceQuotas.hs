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
-- Module      : Amazonka.ServiceQuotas.ListAWSDefaultServiceQuotas
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the default values for the quotas for the specified AWS service. A
-- default value does not reflect any quota increases.
--
-- This operation returns paginated results.
module Amazonka.ServiceQuotas.ListAWSDefaultServiceQuotas
  ( -- * Creating a Request
    ListAWSDefaultServiceQuotas (..),
    newListAWSDefaultServiceQuotas,

    -- * Request Lenses
    listAWSDefaultServiceQuotas_maxResults,
    listAWSDefaultServiceQuotas_nextToken,
    listAWSDefaultServiceQuotas_serviceCode,

    -- * Destructuring the Response
    ListAWSDefaultServiceQuotasResponse (..),
    newListAWSDefaultServiceQuotasResponse,

    -- * Response Lenses
    listAWSDefaultServiceQuotasResponse_nextToken,
    listAWSDefaultServiceQuotasResponse_quotas,
    listAWSDefaultServiceQuotasResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newListAWSDefaultServiceQuotas' smart constructor.
data ListAWSDefaultServiceQuotas = ListAWSDefaultServiceQuotas'
  { -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, if any, make another call with the token returned
    -- from this call.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The service identifier.
    serviceCode :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAWSDefaultServiceQuotas' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listAWSDefaultServiceQuotas_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, if any, make another call with the token returned
-- from this call.
--
-- 'nextToken', 'listAWSDefaultServiceQuotas_nextToken' - The token for the next page of results.
--
-- 'serviceCode', 'listAWSDefaultServiceQuotas_serviceCode' - The service identifier.
newListAWSDefaultServiceQuotas ::
  -- | 'serviceCode'
  Prelude.Text ->
  ListAWSDefaultServiceQuotas
newListAWSDefaultServiceQuotas pServiceCode_ =
  ListAWSDefaultServiceQuotas'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serviceCode = pServiceCode_
    }

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, if any, make another call with the token returned
-- from this call.
listAWSDefaultServiceQuotas_maxResults :: Lens.Lens' ListAWSDefaultServiceQuotas (Prelude.Maybe Prelude.Natural)
listAWSDefaultServiceQuotas_maxResults = Lens.lens (\ListAWSDefaultServiceQuotas' {maxResults} -> maxResults) (\s@ListAWSDefaultServiceQuotas' {} a -> s {maxResults = a} :: ListAWSDefaultServiceQuotas)

-- | The token for the next page of results.
listAWSDefaultServiceQuotas_nextToken :: Lens.Lens' ListAWSDefaultServiceQuotas (Prelude.Maybe Prelude.Text)
listAWSDefaultServiceQuotas_nextToken = Lens.lens (\ListAWSDefaultServiceQuotas' {nextToken} -> nextToken) (\s@ListAWSDefaultServiceQuotas' {} a -> s {nextToken = a} :: ListAWSDefaultServiceQuotas)

-- | The service identifier.
listAWSDefaultServiceQuotas_serviceCode :: Lens.Lens' ListAWSDefaultServiceQuotas Prelude.Text
listAWSDefaultServiceQuotas_serviceCode = Lens.lens (\ListAWSDefaultServiceQuotas' {serviceCode} -> serviceCode) (\s@ListAWSDefaultServiceQuotas' {} a -> s {serviceCode = a} :: ListAWSDefaultServiceQuotas)

instance Core.AWSPager ListAWSDefaultServiceQuotas where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAWSDefaultServiceQuotasResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listAWSDefaultServiceQuotasResponse_quotas
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listAWSDefaultServiceQuotas_nextToken
          Lens..~ rs
          Lens.^? listAWSDefaultServiceQuotasResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListAWSDefaultServiceQuotas where
  type
    AWSResponse ListAWSDefaultServiceQuotas =
      ListAWSDefaultServiceQuotasResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAWSDefaultServiceQuotasResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "Quotas" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListAWSDefaultServiceQuotas where
  hashWithSalt _salt ListAWSDefaultServiceQuotas' {..} =
    _salt `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serviceCode

instance Prelude.NFData ListAWSDefaultServiceQuotas where
  rnf ListAWSDefaultServiceQuotas' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serviceCode

instance Data.ToHeaders ListAWSDefaultServiceQuotas where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "ServiceQuotasV20190624.ListAWSDefaultServiceQuotas" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAWSDefaultServiceQuotas where
  toJSON ListAWSDefaultServiceQuotas' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            Prelude.Just ("ServiceCode" Data..= serviceCode)
          ]
      )

instance Data.ToPath ListAWSDefaultServiceQuotas where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAWSDefaultServiceQuotas where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAWSDefaultServiceQuotasResponse' smart constructor.
data ListAWSDefaultServiceQuotasResponse = ListAWSDefaultServiceQuotasResponse'
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
-- Create a value of 'ListAWSDefaultServiceQuotasResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAWSDefaultServiceQuotasResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'quotas', 'listAWSDefaultServiceQuotasResponse_quotas' - Information about the quotas.
--
-- 'httpStatus', 'listAWSDefaultServiceQuotasResponse_httpStatus' - The response's http status code.
newListAWSDefaultServiceQuotasResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAWSDefaultServiceQuotasResponse
newListAWSDefaultServiceQuotasResponse pHttpStatus_ =
  ListAWSDefaultServiceQuotasResponse'
    { nextToken =
        Prelude.Nothing,
      quotas = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
listAWSDefaultServiceQuotasResponse_nextToken :: Lens.Lens' ListAWSDefaultServiceQuotasResponse (Prelude.Maybe Prelude.Text)
listAWSDefaultServiceQuotasResponse_nextToken = Lens.lens (\ListAWSDefaultServiceQuotasResponse' {nextToken} -> nextToken) (\s@ListAWSDefaultServiceQuotasResponse' {} a -> s {nextToken = a} :: ListAWSDefaultServiceQuotasResponse)

-- | Information about the quotas.
listAWSDefaultServiceQuotasResponse_quotas :: Lens.Lens' ListAWSDefaultServiceQuotasResponse (Prelude.Maybe [ServiceQuota])
listAWSDefaultServiceQuotasResponse_quotas = Lens.lens (\ListAWSDefaultServiceQuotasResponse' {quotas} -> quotas) (\s@ListAWSDefaultServiceQuotasResponse' {} a -> s {quotas = a} :: ListAWSDefaultServiceQuotasResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listAWSDefaultServiceQuotasResponse_httpStatus :: Lens.Lens' ListAWSDefaultServiceQuotasResponse Prelude.Int
listAWSDefaultServiceQuotasResponse_httpStatus = Lens.lens (\ListAWSDefaultServiceQuotasResponse' {httpStatus} -> httpStatus) (\s@ListAWSDefaultServiceQuotasResponse' {} a -> s {httpStatus = a} :: ListAWSDefaultServiceQuotasResponse)

instance
  Prelude.NFData
    ListAWSDefaultServiceQuotasResponse
  where
  rnf ListAWSDefaultServiceQuotasResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf quotas
      `Prelude.seq` Prelude.rnf httpStatus
