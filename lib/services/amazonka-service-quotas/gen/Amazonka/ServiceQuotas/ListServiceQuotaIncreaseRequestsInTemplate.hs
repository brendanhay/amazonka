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
-- Module      : Amazonka.ServiceQuotas.ListServiceQuotaIncreaseRequestsInTemplate
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the quota increase requests in the specified quota request
-- template.
--
-- This operation returns paginated results.
module Amazonka.ServiceQuotas.ListServiceQuotaIncreaseRequestsInTemplate
  ( -- * Creating a Request
    ListServiceQuotaIncreaseRequestsInTemplate (..),
    newListServiceQuotaIncreaseRequestsInTemplate,

    -- * Request Lenses
    listServiceQuotaIncreaseRequestsInTemplate_nextToken,
    listServiceQuotaIncreaseRequestsInTemplate_awsRegion,
    listServiceQuotaIncreaseRequestsInTemplate_serviceCode,
    listServiceQuotaIncreaseRequestsInTemplate_maxResults,

    -- * Destructuring the Response
    ListServiceQuotaIncreaseRequestsInTemplateResponse (..),
    newListServiceQuotaIncreaseRequestsInTemplateResponse,

    -- * Response Lenses
    listServiceQuotaIncreaseRequestsInTemplateResponse_serviceQuotaIncreaseRequestInTemplateList,
    listServiceQuotaIncreaseRequestsInTemplateResponse_nextToken,
    listServiceQuotaIncreaseRequestsInTemplateResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.ServiceQuotas.Types

-- | /See:/ 'newListServiceQuotaIncreaseRequestsInTemplate' smart constructor.
data ListServiceQuotaIncreaseRequestsInTemplate = ListServiceQuotaIncreaseRequestsInTemplate'
  { -- | The token for the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The AWS Region.
    awsRegion :: Prelude.Maybe Prelude.Text,
    -- | The service identifier.
    serviceCode :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return with a single call. To retrieve
    -- the remaining results, if any, make another call with the token returned
    -- from this call.
    maxResults :: Prelude.Maybe Prelude.Natural
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceQuotaIncreaseRequestsInTemplate' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listServiceQuotaIncreaseRequestsInTemplate_nextToken' - The token for the next page of results.
--
-- 'awsRegion', 'listServiceQuotaIncreaseRequestsInTemplate_awsRegion' - The AWS Region.
--
-- 'serviceCode', 'listServiceQuotaIncreaseRequestsInTemplate_serviceCode' - The service identifier.
--
-- 'maxResults', 'listServiceQuotaIncreaseRequestsInTemplate_maxResults' - The maximum number of results to return with a single call. To retrieve
-- the remaining results, if any, make another call with the token returned
-- from this call.
newListServiceQuotaIncreaseRequestsInTemplate ::
  ListServiceQuotaIncreaseRequestsInTemplate
newListServiceQuotaIncreaseRequestsInTemplate =
  ListServiceQuotaIncreaseRequestsInTemplate'
    { nextToken =
        Prelude.Nothing,
      awsRegion = Prelude.Nothing,
      serviceCode = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next page of results.
listServiceQuotaIncreaseRequestsInTemplate_nextToken :: Lens.Lens' ListServiceQuotaIncreaseRequestsInTemplate (Prelude.Maybe Prelude.Text)
listServiceQuotaIncreaseRequestsInTemplate_nextToken = Lens.lens (\ListServiceQuotaIncreaseRequestsInTemplate' {nextToken} -> nextToken) (\s@ListServiceQuotaIncreaseRequestsInTemplate' {} a -> s {nextToken = a} :: ListServiceQuotaIncreaseRequestsInTemplate)

-- | The AWS Region.
listServiceQuotaIncreaseRequestsInTemplate_awsRegion :: Lens.Lens' ListServiceQuotaIncreaseRequestsInTemplate (Prelude.Maybe Prelude.Text)
listServiceQuotaIncreaseRequestsInTemplate_awsRegion = Lens.lens (\ListServiceQuotaIncreaseRequestsInTemplate' {awsRegion} -> awsRegion) (\s@ListServiceQuotaIncreaseRequestsInTemplate' {} a -> s {awsRegion = a} :: ListServiceQuotaIncreaseRequestsInTemplate)

-- | The service identifier.
listServiceQuotaIncreaseRequestsInTemplate_serviceCode :: Lens.Lens' ListServiceQuotaIncreaseRequestsInTemplate (Prelude.Maybe Prelude.Text)
listServiceQuotaIncreaseRequestsInTemplate_serviceCode = Lens.lens (\ListServiceQuotaIncreaseRequestsInTemplate' {serviceCode} -> serviceCode) (\s@ListServiceQuotaIncreaseRequestsInTemplate' {} a -> s {serviceCode = a} :: ListServiceQuotaIncreaseRequestsInTemplate)

-- | The maximum number of results to return with a single call. To retrieve
-- the remaining results, if any, make another call with the token returned
-- from this call.
listServiceQuotaIncreaseRequestsInTemplate_maxResults :: Lens.Lens' ListServiceQuotaIncreaseRequestsInTemplate (Prelude.Maybe Prelude.Natural)
listServiceQuotaIncreaseRequestsInTemplate_maxResults = Lens.lens (\ListServiceQuotaIncreaseRequestsInTemplate' {maxResults} -> maxResults) (\s@ListServiceQuotaIncreaseRequestsInTemplate' {} a -> s {maxResults = a} :: ListServiceQuotaIncreaseRequestsInTemplate)

instance
  Core.AWSPager
    ListServiceQuotaIncreaseRequestsInTemplate
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listServiceQuotaIncreaseRequestsInTemplateResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listServiceQuotaIncreaseRequestsInTemplateResponse_serviceQuotaIncreaseRequestInTemplateList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listServiceQuotaIncreaseRequestsInTemplate_nextToken
          Lens..~ rs
            Lens.^? listServiceQuotaIncreaseRequestsInTemplateResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListServiceQuotaIncreaseRequestsInTemplate
  where
  type
    AWSResponse
      ListServiceQuotaIncreaseRequestsInTemplate =
      ListServiceQuotaIncreaseRequestsInTemplateResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListServiceQuotaIncreaseRequestsInTemplateResponse'
            Prelude.<$> ( x
                            Core..?> "ServiceQuotaIncreaseRequestInTemplateList"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListServiceQuotaIncreaseRequestsInTemplate
  where
  hashWithSalt
    _salt
    ListServiceQuotaIncreaseRequestsInTemplate' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` awsRegion
        `Prelude.hashWithSalt` serviceCode
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    ListServiceQuotaIncreaseRequestsInTemplate
  where
  rnf ListServiceQuotaIncreaseRequestsInTemplate' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsRegion
      `Prelude.seq` Prelude.rnf serviceCode
      `Prelude.seq` Prelude.rnf maxResults

instance
  Core.ToHeaders
    ListServiceQuotaIncreaseRequestsInTemplate
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "ServiceQuotasV20190624.ListServiceQuotaIncreaseRequestsInTemplate" ::
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
    ListServiceQuotaIncreaseRequestsInTemplate
  where
  toJSON
    ListServiceQuotaIncreaseRequestsInTemplate' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("NextToken" Core..=) Prelude.<$> nextToken,
              ("AwsRegion" Core..=) Prelude.<$> awsRegion,
              ("ServiceCode" Core..=) Prelude.<$> serviceCode,
              ("MaxResults" Core..=) Prelude.<$> maxResults
            ]
        )

instance
  Core.ToPath
    ListServiceQuotaIncreaseRequestsInTemplate
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListServiceQuotaIncreaseRequestsInTemplate
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListServiceQuotaIncreaseRequestsInTemplateResponse' smart constructor.
data ListServiceQuotaIncreaseRequestsInTemplateResponse = ListServiceQuotaIncreaseRequestsInTemplateResponse'
  { -- | Information about the quota increase requests.
    serviceQuotaIncreaseRequestInTemplateList :: Prelude.Maybe [ServiceQuotaIncreaseRequestInTemplate],
    -- | The token to use to retrieve the next page of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListServiceQuotaIncreaseRequestsInTemplateResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'serviceQuotaIncreaseRequestInTemplateList', 'listServiceQuotaIncreaseRequestsInTemplateResponse_serviceQuotaIncreaseRequestInTemplateList' - Information about the quota increase requests.
--
-- 'nextToken', 'listServiceQuotaIncreaseRequestsInTemplateResponse_nextToken' - The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
--
-- 'httpStatus', 'listServiceQuotaIncreaseRequestsInTemplateResponse_httpStatus' - The response's http status code.
newListServiceQuotaIncreaseRequestsInTemplateResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListServiceQuotaIncreaseRequestsInTemplateResponse
newListServiceQuotaIncreaseRequestsInTemplateResponse
  pHttpStatus_ =
    ListServiceQuotaIncreaseRequestsInTemplateResponse'
      { serviceQuotaIncreaseRequestInTemplateList =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Information about the quota increase requests.
listServiceQuotaIncreaseRequestsInTemplateResponse_serviceQuotaIncreaseRequestInTemplateList :: Lens.Lens' ListServiceQuotaIncreaseRequestsInTemplateResponse (Prelude.Maybe [ServiceQuotaIncreaseRequestInTemplate])
listServiceQuotaIncreaseRequestsInTemplateResponse_serviceQuotaIncreaseRequestInTemplateList = Lens.lens (\ListServiceQuotaIncreaseRequestsInTemplateResponse' {serviceQuotaIncreaseRequestInTemplateList} -> serviceQuotaIncreaseRequestInTemplateList) (\s@ListServiceQuotaIncreaseRequestsInTemplateResponse' {} a -> s {serviceQuotaIncreaseRequestInTemplateList = a} :: ListServiceQuotaIncreaseRequestsInTemplateResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use to retrieve the next page of results. This value is
-- null when there are no more results to return.
listServiceQuotaIncreaseRequestsInTemplateResponse_nextToken :: Lens.Lens' ListServiceQuotaIncreaseRequestsInTemplateResponse (Prelude.Maybe Prelude.Text)
listServiceQuotaIncreaseRequestsInTemplateResponse_nextToken = Lens.lens (\ListServiceQuotaIncreaseRequestsInTemplateResponse' {nextToken} -> nextToken) (\s@ListServiceQuotaIncreaseRequestsInTemplateResponse' {} a -> s {nextToken = a} :: ListServiceQuotaIncreaseRequestsInTemplateResponse)

-- | The response's http status code.
listServiceQuotaIncreaseRequestsInTemplateResponse_httpStatus :: Lens.Lens' ListServiceQuotaIncreaseRequestsInTemplateResponse Prelude.Int
listServiceQuotaIncreaseRequestsInTemplateResponse_httpStatus = Lens.lens (\ListServiceQuotaIncreaseRequestsInTemplateResponse' {httpStatus} -> httpStatus) (\s@ListServiceQuotaIncreaseRequestsInTemplateResponse' {} a -> s {httpStatus = a} :: ListServiceQuotaIncreaseRequestsInTemplateResponse)

instance
  Prelude.NFData
    ListServiceQuotaIncreaseRequestsInTemplateResponse
  where
  rnf
    ListServiceQuotaIncreaseRequestsInTemplateResponse' {..} =
      Prelude.rnf
        serviceQuotaIncreaseRequestInTemplateList
        `Prelude.seq` Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf httpStatus
