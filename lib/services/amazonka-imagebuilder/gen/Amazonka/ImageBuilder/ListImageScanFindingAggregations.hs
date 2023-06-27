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
-- Module      : Amazonka.ImageBuilder.ListImageScanFindingAggregations
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of image scan aggregations for your account. You can
-- filter by the type of key that Image Builder uses to group results. For
-- example, if you want to get a list of findings by severity level for one
-- of your pipelines, you might specify your pipeline with the
-- @imagePipelineArn@ filter. If you don\'t specify a filter, Image Builder
-- returns an aggregation for your account.
--
-- To streamline results, you can use the following filters in your
-- request:
--
-- -   @accountId@
--
-- -   @imageBuildVersionArn@
--
-- -   @imagePipelineArn@
--
-- -   @vulnerabilityId@
module Amazonka.ImageBuilder.ListImageScanFindingAggregations
  ( -- * Creating a Request
    ListImageScanFindingAggregations (..),
    newListImageScanFindingAggregations,

    -- * Request Lenses
    listImageScanFindingAggregations_filter,
    listImageScanFindingAggregations_nextToken,

    -- * Destructuring the Response
    ListImageScanFindingAggregationsResponse (..),
    newListImageScanFindingAggregationsResponse,

    -- * Response Lenses
    listImageScanFindingAggregationsResponse_aggregationType,
    listImageScanFindingAggregationsResponse_nextToken,
    listImageScanFindingAggregationsResponse_requestId,
    listImageScanFindingAggregationsResponse_responses,
    listImageScanFindingAggregationsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ImageBuilder.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListImageScanFindingAggregations' smart constructor.
data ListImageScanFindingAggregations = ListImageScanFindingAggregations'
  { filter' :: Prelude.Maybe Filter,
    -- | A token to specify where to start paginating. This is the NextToken from
    -- a previously truncated response.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImageScanFindingAggregations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filter'', 'listImageScanFindingAggregations_filter' - Undocumented member.
--
-- 'nextToken', 'listImageScanFindingAggregations_nextToken' - A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
newListImageScanFindingAggregations ::
  ListImageScanFindingAggregations
newListImageScanFindingAggregations =
  ListImageScanFindingAggregations'
    { filter' =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | Undocumented member.
listImageScanFindingAggregations_filter :: Lens.Lens' ListImageScanFindingAggregations (Prelude.Maybe Filter)
listImageScanFindingAggregations_filter = Lens.lens (\ListImageScanFindingAggregations' {filter'} -> filter') (\s@ListImageScanFindingAggregations' {} a -> s {filter' = a} :: ListImageScanFindingAggregations)

-- | A token to specify where to start paginating. This is the NextToken from
-- a previously truncated response.
listImageScanFindingAggregations_nextToken :: Lens.Lens' ListImageScanFindingAggregations (Prelude.Maybe Prelude.Text)
listImageScanFindingAggregations_nextToken = Lens.lens (\ListImageScanFindingAggregations' {nextToken} -> nextToken) (\s@ListImageScanFindingAggregations' {} a -> s {nextToken = a} :: ListImageScanFindingAggregations)

instance
  Core.AWSRequest
    ListImageScanFindingAggregations
  where
  type
    AWSResponse ListImageScanFindingAggregations =
      ListImageScanFindingAggregationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListImageScanFindingAggregationsResponse'
            Prelude.<$> (x Data..?> "aggregationType")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "requestId")
            Prelude.<*> (x Data..?> "responses" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListImageScanFindingAggregations
  where
  hashWithSalt
    _salt
    ListImageScanFindingAggregations' {..} =
      _salt
        `Prelude.hashWithSalt` filter'
        `Prelude.hashWithSalt` nextToken

instance
  Prelude.NFData
    ListImageScanFindingAggregations
  where
  rnf ListImageScanFindingAggregations' {..} =
    Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf nextToken

instance
  Data.ToHeaders
    ListImageScanFindingAggregations
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListImageScanFindingAggregations where
  toJSON ListImageScanFindingAggregations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filter" Data..=) Prelude.<$> filter',
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListImageScanFindingAggregations where
  toPath =
    Prelude.const "/ListImageScanFindingAggregations"

instance
  Data.ToQuery
    ListImageScanFindingAggregations
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListImageScanFindingAggregationsResponse' smart constructor.
data ListImageScanFindingAggregationsResponse = ListImageScanFindingAggregationsResponse'
  { -- | The aggregation type specifies what type of key is used to group the
    -- image scan findings. Image Builder returns results based on the request
    -- filter. If you didn\'t specify a filter in the request, the type
    -- defaults to @accountId@.
    --
    -- __Aggregation types__
    --
    -- -   accountId
    --
    -- -   imageBuildVersionArn
    --
    -- -   imagePipelineArn
    --
    -- -   vulnerabilityId
    --
    -- Each aggregation includes counts by severity level for medium severity
    -- and higher level findings, plus a total for all of the findings for each
    -- key value.
    aggregationType :: Prelude.Maybe Prelude.Text,
    -- | The next token used for paginated responses. When this field isn\'t
    -- empty, there are additional elements that the service has\'ot included
    -- in this request. Use this token with the next request to retrieve
    -- additional objects.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The request ID that uniquely identifies this request.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | An array of image scan finding aggregations that match the filter
    -- criteria.
    responses :: Prelude.Maybe [ImageScanFindingAggregation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListImageScanFindingAggregationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'aggregationType', 'listImageScanFindingAggregationsResponse_aggregationType' - The aggregation type specifies what type of key is used to group the
-- image scan findings. Image Builder returns results based on the request
-- filter. If you didn\'t specify a filter in the request, the type
-- defaults to @accountId@.
--
-- __Aggregation types__
--
-- -   accountId
--
-- -   imageBuildVersionArn
--
-- -   imagePipelineArn
--
-- -   vulnerabilityId
--
-- Each aggregation includes counts by severity level for medium severity
-- and higher level findings, plus a total for all of the findings for each
-- key value.
--
-- 'nextToken', 'listImageScanFindingAggregationsResponse_nextToken' - The next token used for paginated responses. When this field isn\'t
-- empty, there are additional elements that the service has\'ot included
-- in this request. Use this token with the next request to retrieve
-- additional objects.
--
-- 'requestId', 'listImageScanFindingAggregationsResponse_requestId' - The request ID that uniquely identifies this request.
--
-- 'responses', 'listImageScanFindingAggregationsResponse_responses' - An array of image scan finding aggregations that match the filter
-- criteria.
--
-- 'httpStatus', 'listImageScanFindingAggregationsResponse_httpStatus' - The response's http status code.
newListImageScanFindingAggregationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListImageScanFindingAggregationsResponse
newListImageScanFindingAggregationsResponse
  pHttpStatus_ =
    ListImageScanFindingAggregationsResponse'
      { aggregationType =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        requestId = Prelude.Nothing,
        responses = Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The aggregation type specifies what type of key is used to group the
-- image scan findings. Image Builder returns results based on the request
-- filter. If you didn\'t specify a filter in the request, the type
-- defaults to @accountId@.
--
-- __Aggregation types__
--
-- -   accountId
--
-- -   imageBuildVersionArn
--
-- -   imagePipelineArn
--
-- -   vulnerabilityId
--
-- Each aggregation includes counts by severity level for medium severity
-- and higher level findings, plus a total for all of the findings for each
-- key value.
listImageScanFindingAggregationsResponse_aggregationType :: Lens.Lens' ListImageScanFindingAggregationsResponse (Prelude.Maybe Prelude.Text)
listImageScanFindingAggregationsResponse_aggregationType = Lens.lens (\ListImageScanFindingAggregationsResponse' {aggregationType} -> aggregationType) (\s@ListImageScanFindingAggregationsResponse' {} a -> s {aggregationType = a} :: ListImageScanFindingAggregationsResponse)

-- | The next token used for paginated responses. When this field isn\'t
-- empty, there are additional elements that the service has\'ot included
-- in this request. Use this token with the next request to retrieve
-- additional objects.
listImageScanFindingAggregationsResponse_nextToken :: Lens.Lens' ListImageScanFindingAggregationsResponse (Prelude.Maybe Prelude.Text)
listImageScanFindingAggregationsResponse_nextToken = Lens.lens (\ListImageScanFindingAggregationsResponse' {nextToken} -> nextToken) (\s@ListImageScanFindingAggregationsResponse' {} a -> s {nextToken = a} :: ListImageScanFindingAggregationsResponse)

-- | The request ID that uniquely identifies this request.
listImageScanFindingAggregationsResponse_requestId :: Lens.Lens' ListImageScanFindingAggregationsResponse (Prelude.Maybe Prelude.Text)
listImageScanFindingAggregationsResponse_requestId = Lens.lens (\ListImageScanFindingAggregationsResponse' {requestId} -> requestId) (\s@ListImageScanFindingAggregationsResponse' {} a -> s {requestId = a} :: ListImageScanFindingAggregationsResponse)

-- | An array of image scan finding aggregations that match the filter
-- criteria.
listImageScanFindingAggregationsResponse_responses :: Lens.Lens' ListImageScanFindingAggregationsResponse (Prelude.Maybe [ImageScanFindingAggregation])
listImageScanFindingAggregationsResponse_responses = Lens.lens (\ListImageScanFindingAggregationsResponse' {responses} -> responses) (\s@ListImageScanFindingAggregationsResponse' {} a -> s {responses = a} :: ListImageScanFindingAggregationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listImageScanFindingAggregationsResponse_httpStatus :: Lens.Lens' ListImageScanFindingAggregationsResponse Prelude.Int
listImageScanFindingAggregationsResponse_httpStatus = Lens.lens (\ListImageScanFindingAggregationsResponse' {httpStatus} -> httpStatus) (\s@ListImageScanFindingAggregationsResponse' {} a -> s {httpStatus = a} :: ListImageScanFindingAggregationsResponse)

instance
  Prelude.NFData
    ListImageScanFindingAggregationsResponse
  where
  rnf ListImageScanFindingAggregationsResponse' {..} =
    Prelude.rnf aggregationType
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf responses
      `Prelude.seq` Prelude.rnf httpStatus
