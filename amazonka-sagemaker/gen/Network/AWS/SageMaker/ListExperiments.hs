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
-- Module      : Network.AWS.SageMaker.ListExperiments
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the experiments in your account. The list can be filtered to
-- show only experiments that were created in a specific time range. The
-- list can be sorted by experiment name or creation time.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListExperiments
  ( -- * Creating a Request
    ListExperiments (..),
    newListExperiments,

    -- * Request Lenses
    listExperiments_createdAfter,
    listExperiments_sortOrder,
    listExperiments_nextToken,
    listExperiments_createdBefore,
    listExperiments_maxResults,
    listExperiments_sortBy,

    -- * Destructuring the Response
    ListExperimentsResponse (..),
    newListExperimentsResponse,

    -- * Response Lenses
    listExperimentsResponse_nextToken,
    listExperimentsResponse_experimentSummaries,
    listExperimentsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListExperiments' smart constructor.
data ListExperiments = ListExperiments'
  { -- | A filter that returns only experiments created after the specified time.
    createdAfter :: Core.Maybe Core.POSIX,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the previous call to @ListExperiments@ didn\'t return the full set of
    -- experiments, the call returns a token for getting the next set of
    -- experiments.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that returns only experiments created before the specified
    -- time.
    createdBefore :: Core.Maybe Core.POSIX,
    -- | The maximum number of experiments to return in the response. The default
    -- value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Core.Maybe SortExperimentsBy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListExperiments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listExperiments_createdAfter' - A filter that returns only experiments created after the specified time.
--
-- 'sortOrder', 'listExperiments_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'nextToken', 'listExperiments_nextToken' - If the previous call to @ListExperiments@ didn\'t return the full set of
-- experiments, the call returns a token for getting the next set of
-- experiments.
--
-- 'createdBefore', 'listExperiments_createdBefore' - A filter that returns only experiments created before the specified
-- time.
--
-- 'maxResults', 'listExperiments_maxResults' - The maximum number of experiments to return in the response. The default
-- value is 10.
--
-- 'sortBy', 'listExperiments_sortBy' - The property used to sort results. The default value is @CreationTime@.
newListExperiments ::
  ListExperiments
newListExperiments =
  ListExperiments'
    { createdAfter = Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      createdBefore = Core.Nothing,
      maxResults = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | A filter that returns only experiments created after the specified time.
listExperiments_createdAfter :: Lens.Lens' ListExperiments (Core.Maybe Core.UTCTime)
listExperiments_createdAfter = Lens.lens (\ListExperiments' {createdAfter} -> createdAfter) (\s@ListExperiments' {} a -> s {createdAfter = a} :: ListExperiments) Core.. Lens.mapping Core._Time

-- | The sort order. The default value is @Descending@.
listExperiments_sortOrder :: Lens.Lens' ListExperiments (Core.Maybe SortOrder)
listExperiments_sortOrder = Lens.lens (\ListExperiments' {sortOrder} -> sortOrder) (\s@ListExperiments' {} a -> s {sortOrder = a} :: ListExperiments)

-- | If the previous call to @ListExperiments@ didn\'t return the full set of
-- experiments, the call returns a token for getting the next set of
-- experiments.
listExperiments_nextToken :: Lens.Lens' ListExperiments (Core.Maybe Core.Text)
listExperiments_nextToken = Lens.lens (\ListExperiments' {nextToken} -> nextToken) (\s@ListExperiments' {} a -> s {nextToken = a} :: ListExperiments)

-- | A filter that returns only experiments created before the specified
-- time.
listExperiments_createdBefore :: Lens.Lens' ListExperiments (Core.Maybe Core.UTCTime)
listExperiments_createdBefore = Lens.lens (\ListExperiments' {createdBefore} -> createdBefore) (\s@ListExperiments' {} a -> s {createdBefore = a} :: ListExperiments) Core.. Lens.mapping Core._Time

-- | The maximum number of experiments to return in the response. The default
-- value is 10.
listExperiments_maxResults :: Lens.Lens' ListExperiments (Core.Maybe Core.Natural)
listExperiments_maxResults = Lens.lens (\ListExperiments' {maxResults} -> maxResults) (\s@ListExperiments' {} a -> s {maxResults = a} :: ListExperiments)

-- | The property used to sort results. The default value is @CreationTime@.
listExperiments_sortBy :: Lens.Lens' ListExperiments (Core.Maybe SortExperimentsBy)
listExperiments_sortBy = Lens.lens (\ListExperiments' {sortBy} -> sortBy) (\s@ListExperiments' {} a -> s {sortBy = a} :: ListExperiments)

instance Core.AWSPager ListExperiments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listExperimentsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listExperimentsResponse_experimentSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listExperiments_nextToken
          Lens..~ rs
          Lens.^? listExperimentsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListExperiments where
  type
    AWSResponse ListExperiments =
      ListExperimentsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExperimentsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> ( x Core..?> "ExperimentSummaries"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListExperiments

instance Core.NFData ListExperiments

instance Core.ToHeaders ListExperiments where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListExperiments" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListExperiments where
  toJSON ListExperiments' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.ToPath ListExperiments where
  toPath = Core.const "/"

instance Core.ToQuery ListExperiments where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListExperimentsResponse' smart constructor.
data ListExperimentsResponse = ListExperimentsResponse'
  { -- | A token for getting the next set of experiments, if there are any.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of the summaries of your experiments.
    experimentSummaries :: Core.Maybe [ExperimentSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListExperimentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listExperimentsResponse_nextToken' - A token for getting the next set of experiments, if there are any.
--
-- 'experimentSummaries', 'listExperimentsResponse_experimentSummaries' - A list of the summaries of your experiments.
--
-- 'httpStatus', 'listExperimentsResponse_httpStatus' - The response's http status code.
newListExperimentsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListExperimentsResponse
newListExperimentsResponse pHttpStatus_ =
  ListExperimentsResponse'
    { nextToken = Core.Nothing,
      experimentSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of experiments, if there are any.
listExperimentsResponse_nextToken :: Lens.Lens' ListExperimentsResponse (Core.Maybe Core.Text)
listExperimentsResponse_nextToken = Lens.lens (\ListExperimentsResponse' {nextToken} -> nextToken) (\s@ListExperimentsResponse' {} a -> s {nextToken = a} :: ListExperimentsResponse)

-- | A list of the summaries of your experiments.
listExperimentsResponse_experimentSummaries :: Lens.Lens' ListExperimentsResponse (Core.Maybe [ExperimentSummary])
listExperimentsResponse_experimentSummaries = Lens.lens (\ListExperimentsResponse' {experimentSummaries} -> experimentSummaries) (\s@ListExperimentsResponse' {} a -> s {experimentSummaries = a} :: ListExperimentsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listExperimentsResponse_httpStatus :: Lens.Lens' ListExperimentsResponse Core.Int
listExperimentsResponse_httpStatus = Lens.lens (\ListExperimentsResponse' {httpStatus} -> httpStatus) (\s@ListExperimentsResponse' {} a -> s {httpStatus = a} :: ListExperimentsResponse)

instance Core.NFData ListExperimentsResponse
