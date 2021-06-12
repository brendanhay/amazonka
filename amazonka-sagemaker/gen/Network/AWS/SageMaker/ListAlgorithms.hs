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
-- Module      : Network.AWS.SageMaker.ListAlgorithms
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the machine learning algorithms that have been created.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListAlgorithms
  ( -- * Creating a Request
    ListAlgorithms (..),
    newListAlgorithms,

    -- * Request Lenses
    listAlgorithms_sortOrder,
    listAlgorithms_nextToken,
    listAlgorithms_nameContains,
    listAlgorithms_maxResults,
    listAlgorithms_creationTimeBefore,
    listAlgorithms_sortBy,
    listAlgorithms_creationTimeAfter,

    -- * Destructuring the Response
    ListAlgorithmsResponse (..),
    newListAlgorithmsResponse,

    -- * Response Lenses
    listAlgorithmsResponse_nextToken,
    listAlgorithmsResponse_httpStatus,
    listAlgorithmsResponse_algorithmSummaryList,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListAlgorithms' smart constructor.
data ListAlgorithms = ListAlgorithms'
  { -- | The sort order for the results. The default is @Ascending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the response to a previous @ListAlgorithms@ request was truncated,
    -- the response includes a @NextToken@. To retrieve the next set of
    -- algorithms, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A string in the algorithm name. This filter returns only algorithms
    -- whose name contains the specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of algorithms to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only algorithms created before the specified time
    -- (timestamp).
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | The parameter by which to sort the results. The default is
    -- @CreationTime@.
    sortBy :: Core.Maybe AlgorithmSortBy,
    -- | A filter that returns only algorithms created after the specified time
    -- (timestamp).
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAlgorithms' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listAlgorithms_sortOrder' - The sort order for the results. The default is @Ascending@.
--
-- 'nextToken', 'listAlgorithms_nextToken' - If the response to a previous @ListAlgorithms@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- algorithms, use the token in the next request.
--
-- 'nameContains', 'listAlgorithms_nameContains' - A string in the algorithm name. This filter returns only algorithms
-- whose name contains the specified string.
--
-- 'maxResults', 'listAlgorithms_maxResults' - The maximum number of algorithms to return in the response.
--
-- 'creationTimeBefore', 'listAlgorithms_creationTimeBefore' - A filter that returns only algorithms created before the specified time
-- (timestamp).
--
-- 'sortBy', 'listAlgorithms_sortBy' - The parameter by which to sort the results. The default is
-- @CreationTime@.
--
-- 'creationTimeAfter', 'listAlgorithms_creationTimeAfter' - A filter that returns only algorithms created after the specified time
-- (timestamp).
newListAlgorithms ::
  ListAlgorithms
newListAlgorithms =
  ListAlgorithms'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      sortBy = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | The sort order for the results. The default is @Ascending@.
listAlgorithms_sortOrder :: Lens.Lens' ListAlgorithms (Core.Maybe SortOrder)
listAlgorithms_sortOrder = Lens.lens (\ListAlgorithms' {sortOrder} -> sortOrder) (\s@ListAlgorithms' {} a -> s {sortOrder = a} :: ListAlgorithms)

-- | If the response to a previous @ListAlgorithms@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of
-- algorithms, use the token in the next request.
listAlgorithms_nextToken :: Lens.Lens' ListAlgorithms (Core.Maybe Core.Text)
listAlgorithms_nextToken = Lens.lens (\ListAlgorithms' {nextToken} -> nextToken) (\s@ListAlgorithms' {} a -> s {nextToken = a} :: ListAlgorithms)

-- | A string in the algorithm name. This filter returns only algorithms
-- whose name contains the specified string.
listAlgorithms_nameContains :: Lens.Lens' ListAlgorithms (Core.Maybe Core.Text)
listAlgorithms_nameContains = Lens.lens (\ListAlgorithms' {nameContains} -> nameContains) (\s@ListAlgorithms' {} a -> s {nameContains = a} :: ListAlgorithms)

-- | The maximum number of algorithms to return in the response.
listAlgorithms_maxResults :: Lens.Lens' ListAlgorithms (Core.Maybe Core.Natural)
listAlgorithms_maxResults = Lens.lens (\ListAlgorithms' {maxResults} -> maxResults) (\s@ListAlgorithms' {} a -> s {maxResults = a} :: ListAlgorithms)

-- | A filter that returns only algorithms created before the specified time
-- (timestamp).
listAlgorithms_creationTimeBefore :: Lens.Lens' ListAlgorithms (Core.Maybe Core.UTCTime)
listAlgorithms_creationTimeBefore = Lens.lens (\ListAlgorithms' {creationTimeBefore} -> creationTimeBefore) (\s@ListAlgorithms' {} a -> s {creationTimeBefore = a} :: ListAlgorithms) Core.. Lens.mapping Core._Time

-- | The parameter by which to sort the results. The default is
-- @CreationTime@.
listAlgorithms_sortBy :: Lens.Lens' ListAlgorithms (Core.Maybe AlgorithmSortBy)
listAlgorithms_sortBy = Lens.lens (\ListAlgorithms' {sortBy} -> sortBy) (\s@ListAlgorithms' {} a -> s {sortBy = a} :: ListAlgorithms)

-- | A filter that returns only algorithms created after the specified time
-- (timestamp).
listAlgorithms_creationTimeAfter :: Lens.Lens' ListAlgorithms (Core.Maybe Core.UTCTime)
listAlgorithms_creationTimeAfter = Lens.lens (\ListAlgorithms' {creationTimeAfter} -> creationTimeAfter) (\s@ListAlgorithms' {} a -> s {creationTimeAfter = a} :: ListAlgorithms) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListAlgorithms where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listAlgorithmsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listAlgorithmsResponse_algorithmSummaryList
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listAlgorithms_nextToken
          Lens..~ rs
          Lens.^? listAlgorithmsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListAlgorithms where
  type
    AWSResponse ListAlgorithms =
      ListAlgorithmsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListAlgorithmsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "AlgorithmSummaryList"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListAlgorithms

instance Core.NFData ListAlgorithms

instance Core.ToHeaders ListAlgorithms where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListAlgorithms" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListAlgorithms where
  toJSON ListAlgorithms' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListAlgorithms where
  toPath = Core.const "/"

instance Core.ToQuery ListAlgorithms where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListAlgorithmsResponse' smart constructor.
data ListAlgorithmsResponse = ListAlgorithmsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of algorithms, use it in the subsequent request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | >An array of @AlgorithmSummary@ objects, each of which lists an
    -- algorithm.
    algorithmSummaryList :: [AlgorithmSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListAlgorithmsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listAlgorithmsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of algorithms, use it in the subsequent request.
--
-- 'httpStatus', 'listAlgorithmsResponse_httpStatus' - The response's http status code.
--
-- 'algorithmSummaryList', 'listAlgorithmsResponse_algorithmSummaryList' - >An array of @AlgorithmSummary@ objects, each of which lists an
-- algorithm.
newListAlgorithmsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListAlgorithmsResponse
newListAlgorithmsResponse pHttpStatus_ =
  ListAlgorithmsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      algorithmSummaryList = Core.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of algorithms, use it in the subsequent request.
listAlgorithmsResponse_nextToken :: Lens.Lens' ListAlgorithmsResponse (Core.Maybe Core.Text)
listAlgorithmsResponse_nextToken = Lens.lens (\ListAlgorithmsResponse' {nextToken} -> nextToken) (\s@ListAlgorithmsResponse' {} a -> s {nextToken = a} :: ListAlgorithmsResponse)

-- | The response's http status code.
listAlgorithmsResponse_httpStatus :: Lens.Lens' ListAlgorithmsResponse Core.Int
listAlgorithmsResponse_httpStatus = Lens.lens (\ListAlgorithmsResponse' {httpStatus} -> httpStatus) (\s@ListAlgorithmsResponse' {} a -> s {httpStatus = a} :: ListAlgorithmsResponse)

-- | >An array of @AlgorithmSummary@ objects, each of which lists an
-- algorithm.
listAlgorithmsResponse_algorithmSummaryList :: Lens.Lens' ListAlgorithmsResponse [AlgorithmSummary]
listAlgorithmsResponse_algorithmSummaryList = Lens.lens (\ListAlgorithmsResponse' {algorithmSummaryList} -> algorithmSummaryList) (\s@ListAlgorithmsResponse' {} a -> s {algorithmSummaryList = a} :: ListAlgorithmsResponse) Core.. Lens._Coerce

instance Core.NFData ListAlgorithmsResponse
