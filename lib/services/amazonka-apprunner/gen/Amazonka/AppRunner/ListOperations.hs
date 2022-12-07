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
-- Module      : Amazonka.AppRunner.ListOperations
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Return a list of operations that occurred on an App Runner service.
--
-- The resulting list of OperationSummary objects is sorted in reverse
-- chronological order. The first object on the list represents the last
-- started operation.
module Amazonka.AppRunner.ListOperations
  ( -- * Creating a Request
    ListOperations (..),
    newListOperations,

    -- * Request Lenses
    listOperations_nextToken,
    listOperations_maxResults,
    listOperations_serviceArn,

    -- * Destructuring the Response
    ListOperationsResponse (..),
    newListOperationsResponse,

    -- * Response Lenses
    listOperationsResponse_nextToken,
    listOperationsResponse_operationSummaryList,
    listOperationsResponse_httpStatus,
  )
where

import Amazonka.AppRunner.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListOperations' smart constructor.
data ListOperations = ListOperations'
  { -- | A token from a previous result page. It\'s used for a paginated request.
    -- The request retrieves the next result page. All other parameter values
    -- must be identical to the ones specified in the initial request.
    --
    -- If you don\'t specify @NextToken@, the request retrieves the first
    -- result page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to include in each response (result page).
    -- It\'s used for a paginated request.
    --
    -- If you don\'t specify @MaxResults@, the request retrieves all available
    -- results in a single response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The Amazon Resource Name (ARN) of the App Runner service that you want a
    -- list of operations for.
    serviceArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOperations' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOperations_nextToken' - A token from a previous result page. It\'s used for a paginated request.
-- The request retrieves the next result page. All other parameter values
-- must be identical to the ones specified in the initial request.
--
-- If you don\'t specify @NextToken@, the request retrieves the first
-- result page.
--
-- 'maxResults', 'listOperations_maxResults' - The maximum number of results to include in each response (result page).
-- It\'s used for a paginated request.
--
-- If you don\'t specify @MaxResults@, the request retrieves all available
-- results in a single response.
--
-- 'serviceArn', 'listOperations_serviceArn' - The Amazon Resource Name (ARN) of the App Runner service that you want a
-- list of operations for.
newListOperations ::
  -- | 'serviceArn'
  Prelude.Text ->
  ListOperations
newListOperations pServiceArn_ =
  ListOperations'
    { nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      serviceArn = pServiceArn_
    }

-- | A token from a previous result page. It\'s used for a paginated request.
-- The request retrieves the next result page. All other parameter values
-- must be identical to the ones specified in the initial request.
--
-- If you don\'t specify @NextToken@, the request retrieves the first
-- result page.
listOperations_nextToken :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.Text)
listOperations_nextToken = Lens.lens (\ListOperations' {nextToken} -> nextToken) (\s@ListOperations' {} a -> s {nextToken = a} :: ListOperations)

-- | The maximum number of results to include in each response (result page).
-- It\'s used for a paginated request.
--
-- If you don\'t specify @MaxResults@, the request retrieves all available
-- results in a single response.
listOperations_maxResults :: Lens.Lens' ListOperations (Prelude.Maybe Prelude.Natural)
listOperations_maxResults = Lens.lens (\ListOperations' {maxResults} -> maxResults) (\s@ListOperations' {} a -> s {maxResults = a} :: ListOperations)

-- | The Amazon Resource Name (ARN) of the App Runner service that you want a
-- list of operations for.
listOperations_serviceArn :: Lens.Lens' ListOperations Prelude.Text
listOperations_serviceArn = Lens.lens (\ListOperations' {serviceArn} -> serviceArn) (\s@ListOperations' {} a -> s {serviceArn = a} :: ListOperations)

instance Core.AWSRequest ListOperations where
  type
    AWSResponse ListOperations =
      ListOperationsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOperationsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "OperationSummaryList"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListOperations where
  hashWithSalt _salt ListOperations' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` serviceArn

instance Prelude.NFData ListOperations where
  rnf ListOperations' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf serviceArn

instance Data.ToHeaders ListOperations where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AppRunner.ListOperations" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListOperations where
  toJSON ListOperations' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("NextToken" Data..=) Prelude.<$> nextToken,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            Prelude.Just ("ServiceArn" Data..= serviceArn)
          ]
      )

instance Data.ToPath ListOperations where
  toPath = Prelude.const "/"

instance Data.ToQuery ListOperations where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListOperationsResponse' smart constructor.
data ListOperationsResponse = ListOperationsResponse'
  { -- | The token that you can pass in a subsequent request to get the next
    -- result page. It\'s returned in a paginated request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of operation summary information records. In a paginated request,
    -- the request returns up to @MaxResults@ records for each call.
    operationSummaryList :: Prelude.Maybe [OperationSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListOperationsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOperationsResponse_nextToken' - The token that you can pass in a subsequent request to get the next
-- result page. It\'s returned in a paginated request.
--
-- 'operationSummaryList', 'listOperationsResponse_operationSummaryList' - A list of operation summary information records. In a paginated request,
-- the request returns up to @MaxResults@ records for each call.
--
-- 'httpStatus', 'listOperationsResponse_httpStatus' - The response's http status code.
newListOperationsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListOperationsResponse
newListOperationsResponse pHttpStatus_ =
  ListOperationsResponse'
    { nextToken =
        Prelude.Nothing,
      operationSummaryList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token that you can pass in a subsequent request to get the next
-- result page. It\'s returned in a paginated request.
listOperationsResponse_nextToken :: Lens.Lens' ListOperationsResponse (Prelude.Maybe Prelude.Text)
listOperationsResponse_nextToken = Lens.lens (\ListOperationsResponse' {nextToken} -> nextToken) (\s@ListOperationsResponse' {} a -> s {nextToken = a} :: ListOperationsResponse)

-- | A list of operation summary information records. In a paginated request,
-- the request returns up to @MaxResults@ records for each call.
listOperationsResponse_operationSummaryList :: Lens.Lens' ListOperationsResponse (Prelude.Maybe [OperationSummary])
listOperationsResponse_operationSummaryList = Lens.lens (\ListOperationsResponse' {operationSummaryList} -> operationSummaryList) (\s@ListOperationsResponse' {} a -> s {operationSummaryList = a} :: ListOperationsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listOperationsResponse_httpStatus :: Lens.Lens' ListOperationsResponse Prelude.Int
listOperationsResponse_httpStatus = Lens.lens (\ListOperationsResponse' {httpStatus} -> httpStatus) (\s@ListOperationsResponse' {} a -> s {httpStatus = a} :: ListOperationsResponse)

instance Prelude.NFData ListOperationsResponse where
  rnf ListOperationsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf operationSummaryList
      `Prelude.seq` Prelude.rnf httpStatus
