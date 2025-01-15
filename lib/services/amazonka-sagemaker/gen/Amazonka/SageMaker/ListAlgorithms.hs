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
-- Module      : Amazonka.SageMaker.ListAlgorithms
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the machine learning algorithms that have been created.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListAlgorithms
  ( -- * Creating a Request
    ListAlgorithms (..),
    newListAlgorithms,

    -- * Request Lenses
    listAlgorithms_creationTimeAfter,
    listAlgorithms_creationTimeBefore,
    listAlgorithms_maxResults,
    listAlgorithms_nameContains,
    listAlgorithms_nextToken,
    listAlgorithms_sortBy,
    listAlgorithms_sortOrder,

    -- * Destructuring the Response
    ListAlgorithmsResponse (..),
    newListAlgorithmsResponse,

    -- * Response Lenses
    listAlgorithmsResponse_nextToken,
    listAlgorithmsResponse_httpStatus,
    listAlgorithmsResponse_algorithmSummaryList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListAlgorithms' smart constructor.
data ListAlgorithms = ListAlgorithms'
  { -- | A filter that returns only algorithms created after the specified time
    -- (timestamp).
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only algorithms created before the specified time
    -- (timestamp).
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of algorithms to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A string in the algorithm name. This filter returns only algorithms
    -- whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the response to a previous @ListAlgorithms@ request was truncated,
    -- the response includes a @NextToken@. To retrieve the next set of
    -- algorithms, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The parameter by which to sort the results. The default is
    -- @CreationTime@.
    sortBy :: Prelude.Maybe AlgorithmSortBy,
    -- | The sort order for the results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlgorithms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listAlgorithms_creationTimeAfter' - A filter that returns only algorithms created after the specified time
-- (timestamp).
--
-- 'creationTimeBefore', 'listAlgorithms_creationTimeBefore' - A filter that returns only algorithms created before the specified time
-- (timestamp).
--
-- 'maxResults', 'listAlgorithms_maxResults' - The maximum number of algorithms to return in the response.
--
-- 'nameContains', 'listAlgorithms_nameContains' - A string in the algorithm name. This filter returns only algorithms
-- whose name contains the specified string.
--
-- 'nextToken', 'listAlgorithms_nextToken' - If the response to a previous @ListAlgorithms@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- algorithms, use the token in the next request.
--
-- 'sortBy', 'listAlgorithms_sortBy' - The parameter by which to sort the results. The default is
-- @CreationTime@.
--
-- 'sortOrder', 'listAlgorithms_sortOrder' - The sort order for the results. The default is @Ascending@.
newListAlgorithms ::
  ListAlgorithms
newListAlgorithms =
  ListAlgorithms'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | A filter that returns only algorithms created after the specified time
-- (timestamp).
listAlgorithms_creationTimeAfter :: Lens.Lens' ListAlgorithms (Prelude.Maybe Prelude.UTCTime)
listAlgorithms_creationTimeAfter = Lens.lens (\ListAlgorithms' {creationTimeAfter} -> creationTimeAfter) (\s@ListAlgorithms' {} a -> s {creationTimeAfter = a} :: ListAlgorithms) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only algorithms created before the specified time
-- (timestamp).
listAlgorithms_creationTimeBefore :: Lens.Lens' ListAlgorithms (Prelude.Maybe Prelude.UTCTime)
listAlgorithms_creationTimeBefore = Lens.lens (\ListAlgorithms' {creationTimeBefore} -> creationTimeBefore) (\s@ListAlgorithms' {} a -> s {creationTimeBefore = a} :: ListAlgorithms) Prelude.. Lens.mapping Data._Time

-- | The maximum number of algorithms to return in the response.
listAlgorithms_maxResults :: Lens.Lens' ListAlgorithms (Prelude.Maybe Prelude.Natural)
listAlgorithms_maxResults = Lens.lens (\ListAlgorithms' {maxResults} -> maxResults) (\s@ListAlgorithms' {} a -> s {maxResults = a} :: ListAlgorithms)

-- | A string in the algorithm name. This filter returns only algorithms
-- whose name contains the specified string.
listAlgorithms_nameContains :: Lens.Lens' ListAlgorithms (Prelude.Maybe Prelude.Text)
listAlgorithms_nameContains = Lens.lens (\ListAlgorithms' {nameContains} -> nameContains) (\s@ListAlgorithms' {} a -> s {nameContains = a} :: ListAlgorithms)

-- | If the response to a previous @ListAlgorithms@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- algorithms, use the token in the next request.
listAlgorithms_nextToken :: Lens.Lens' ListAlgorithms (Prelude.Maybe Prelude.Text)
listAlgorithms_nextToken = Lens.lens (\ListAlgorithms' {nextToken} -> nextToken) (\s@ListAlgorithms' {} a -> s {nextToken = a} :: ListAlgorithms)

-- | The parameter by which to sort the results. The default is
-- @CreationTime@.
listAlgorithms_sortBy :: Lens.Lens' ListAlgorithms (Prelude.Maybe AlgorithmSortBy)
listAlgorithms_sortBy = Lens.lens (\ListAlgorithms' {sortBy} -> sortBy) (\s@ListAlgorithms' {} a -> s {sortBy = a} :: ListAlgorithms)

-- | The sort order for the results. The default is @Ascending@.
listAlgorithms_sortOrder :: Lens.Lens' ListAlgorithms (Prelude.Maybe SortOrder)
listAlgorithms_sortOrder = Lens.lens (\ListAlgorithms' {sortOrder} -> sortOrder) (\s@ListAlgorithms' {} a -> s {sortOrder = a} :: ListAlgorithms)

instance Core.AWSPager ListAlgorithms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAlgorithmsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listAlgorithmsResponse_algorithmSummaryList
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listAlgorithms_nextToken
              Lens..~ rs
              Lens.^? listAlgorithmsResponse_nextToken
              Prelude.. Lens._Just

instance Core.AWSRequest ListAlgorithms where
  type
    AWSResponse ListAlgorithms =
      ListAlgorithmsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAlgorithmsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "AlgorithmSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListAlgorithms where
  hashWithSalt _salt ListAlgorithms' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListAlgorithms where
  rnf ListAlgorithms' {..} =
    Prelude.rnf creationTimeAfter `Prelude.seq`
      Prelude.rnf creationTimeBefore `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf nameContains `Prelude.seq`
            Prelude.rnf nextToken `Prelude.seq`
              Prelude.rnf sortBy `Prelude.seq`
                Prelude.rnf sortOrder

instance Data.ToHeaders ListAlgorithms where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListAlgorithms" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListAlgorithms where
  toJSON ListAlgorithms' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListAlgorithms where
  toPath = Prelude.const "/"

instance Data.ToQuery ListAlgorithms where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListAlgorithmsResponse' smart constructor.
data ListAlgorithmsResponse = ListAlgorithmsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of algorithms, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | >An array of @AlgorithmSummary@ objects, each of which lists an
    -- algorithm.
    algorithmSummaryList :: [AlgorithmSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListAlgorithmsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAlgorithmsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of algorithms, use it in the subsequent request.
--
-- 'httpStatus', 'listAlgorithmsResponse_httpStatus' - The response's http status code.
--
-- 'algorithmSummaryList', 'listAlgorithmsResponse_algorithmSummaryList' - >An array of @AlgorithmSummary@ objects, each of which lists an
-- algorithm.
newListAlgorithmsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListAlgorithmsResponse
newListAlgorithmsResponse pHttpStatus_ =
  ListAlgorithmsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      algorithmSummaryList = Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of algorithms, use it in the subsequent request.
listAlgorithmsResponse_nextToken :: Lens.Lens' ListAlgorithmsResponse (Prelude.Maybe Prelude.Text)
listAlgorithmsResponse_nextToken = Lens.lens (\ListAlgorithmsResponse' {nextToken} -> nextToken) (\s@ListAlgorithmsResponse' {} a -> s {nextToken = a} :: ListAlgorithmsResponse)

-- | The response's http status code.
listAlgorithmsResponse_httpStatus :: Lens.Lens' ListAlgorithmsResponse Prelude.Int
listAlgorithmsResponse_httpStatus = Lens.lens (\ListAlgorithmsResponse' {httpStatus} -> httpStatus) (\s@ListAlgorithmsResponse' {} a -> s {httpStatus = a} :: ListAlgorithmsResponse)

-- | >An array of @AlgorithmSummary@ objects, each of which lists an
-- algorithm.
listAlgorithmsResponse_algorithmSummaryList :: Lens.Lens' ListAlgorithmsResponse [AlgorithmSummary]
listAlgorithmsResponse_algorithmSummaryList = Lens.lens (\ListAlgorithmsResponse' {algorithmSummaryList} -> algorithmSummaryList) (\s@ListAlgorithmsResponse' {} a -> s {algorithmSummaryList = a} :: ListAlgorithmsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListAlgorithmsResponse where
  rnf ListAlgorithmsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf algorithmSummaryList
