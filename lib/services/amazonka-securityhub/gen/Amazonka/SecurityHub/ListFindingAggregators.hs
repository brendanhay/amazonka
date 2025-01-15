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
-- Module      : Amazonka.SecurityHub.ListFindingAggregators
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- If finding aggregation is enabled, then @ListFindingAggregators@ returns
-- the ARN of the finding aggregator. You can run this operation from any
-- Region.
--
-- This operation returns paginated results.
module Amazonka.SecurityHub.ListFindingAggregators
  ( -- * Creating a Request
    ListFindingAggregators (..),
    newListFindingAggregators,

    -- * Request Lenses
    listFindingAggregators_maxResults,
    listFindingAggregators_nextToken,

    -- * Destructuring the Response
    ListFindingAggregatorsResponse (..),
    newListFindingAggregatorsResponse,

    -- * Response Lenses
    listFindingAggregatorsResponse_findingAggregators,
    listFindingAggregatorsResponse_nextToken,
    listFindingAggregatorsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SecurityHub.Types

-- | /See:/ 'newListFindingAggregators' smart constructor.
data ListFindingAggregators = ListFindingAggregators'
  { -- | The maximum number of results to return. This operation currently only
    -- returns a single result.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token returned with the previous set of results. Identifies the next
    -- set of results to return.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFindingAggregators' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listFindingAggregators_maxResults' - The maximum number of results to return. This operation currently only
-- returns a single result.
--
-- 'nextToken', 'listFindingAggregators_nextToken' - The token returned with the previous set of results. Identifies the next
-- set of results to return.
newListFindingAggregators ::
  ListFindingAggregators
newListFindingAggregators =
  ListFindingAggregators'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return. This operation currently only
-- returns a single result.
listFindingAggregators_maxResults :: Lens.Lens' ListFindingAggregators (Prelude.Maybe Prelude.Natural)
listFindingAggregators_maxResults = Lens.lens (\ListFindingAggregators' {maxResults} -> maxResults) (\s@ListFindingAggregators' {} a -> s {maxResults = a} :: ListFindingAggregators)

-- | The token returned with the previous set of results. Identifies the next
-- set of results to return.
listFindingAggregators_nextToken :: Lens.Lens' ListFindingAggregators (Prelude.Maybe Prelude.Text)
listFindingAggregators_nextToken = Lens.lens (\ListFindingAggregators' {nextToken} -> nextToken) (\s@ListFindingAggregators' {} a -> s {nextToken = a} :: ListFindingAggregators)

instance Core.AWSPager ListFindingAggregators where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFindingAggregatorsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listFindingAggregatorsResponse_findingAggregators
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listFindingAggregators_nextToken
              Lens..~ rs
              Lens.^? listFindingAggregatorsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListFindingAggregators where
  type
    AWSResponse ListFindingAggregators =
      ListFindingAggregatorsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFindingAggregatorsResponse'
            Prelude.<$> ( x
                            Data..?> "FindingAggregators"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListFindingAggregators where
  hashWithSalt _salt ListFindingAggregators' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListFindingAggregators where
  rnf ListFindingAggregators' {..} =
    Prelude.rnf maxResults `Prelude.seq`
      Prelude.rnf nextToken

instance Data.ToHeaders ListFindingAggregators where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListFindingAggregators where
  toPath = Prelude.const "/findingAggregator/list"

instance Data.ToQuery ListFindingAggregators where
  toQuery ListFindingAggregators' {..} =
    Prelude.mconcat
      [ "MaxResults" Data.=: maxResults,
        "NextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListFindingAggregatorsResponse' smart constructor.
data ListFindingAggregatorsResponse = ListFindingAggregatorsResponse'
  { -- | The list of finding aggregators. This operation currently only returns a
    -- single result.
    findingAggregators :: Prelude.Maybe [FindingAggregator],
    -- | If there are more results, this is the token to provide in the next call
    -- to @ListFindingAggregators@.
    --
    -- This operation currently only returns a single result.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFindingAggregatorsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'findingAggregators', 'listFindingAggregatorsResponse_findingAggregators' - The list of finding aggregators. This operation currently only returns a
-- single result.
--
-- 'nextToken', 'listFindingAggregatorsResponse_nextToken' - If there are more results, this is the token to provide in the next call
-- to @ListFindingAggregators@.
--
-- This operation currently only returns a single result.
--
-- 'httpStatus', 'listFindingAggregatorsResponse_httpStatus' - The response's http status code.
newListFindingAggregatorsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFindingAggregatorsResponse
newListFindingAggregatorsResponse pHttpStatus_ =
  ListFindingAggregatorsResponse'
    { findingAggregators =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The list of finding aggregators. This operation currently only returns a
-- single result.
listFindingAggregatorsResponse_findingAggregators :: Lens.Lens' ListFindingAggregatorsResponse (Prelude.Maybe [FindingAggregator])
listFindingAggregatorsResponse_findingAggregators = Lens.lens (\ListFindingAggregatorsResponse' {findingAggregators} -> findingAggregators) (\s@ListFindingAggregatorsResponse' {} a -> s {findingAggregators = a} :: ListFindingAggregatorsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If there are more results, this is the token to provide in the next call
-- to @ListFindingAggregators@.
--
-- This operation currently only returns a single result.
listFindingAggregatorsResponse_nextToken :: Lens.Lens' ListFindingAggregatorsResponse (Prelude.Maybe Prelude.Text)
listFindingAggregatorsResponse_nextToken = Lens.lens (\ListFindingAggregatorsResponse' {nextToken} -> nextToken) (\s@ListFindingAggregatorsResponse' {} a -> s {nextToken = a} :: ListFindingAggregatorsResponse)

-- | The response's http status code.
listFindingAggregatorsResponse_httpStatus :: Lens.Lens' ListFindingAggregatorsResponse Prelude.Int
listFindingAggregatorsResponse_httpStatus = Lens.lens (\ListFindingAggregatorsResponse' {httpStatus} -> httpStatus) (\s@ListFindingAggregatorsResponse' {} a -> s {httpStatus = a} :: ListFindingAggregatorsResponse)

instance
  Prelude.NFData
    ListFindingAggregatorsResponse
  where
  rnf ListFindingAggregatorsResponse' {..} =
    Prelude.rnf findingAggregators `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
