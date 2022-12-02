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
-- Module      : Amazonka.SageMaker.ListExperiments
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.SageMaker.ListExperiments
  ( -- * Creating a Request
    ListExperiments (..),
    newListExperiments,

    -- * Request Lenses
    listExperiments_sortOrder,
    listExperiments_nextToken,
    listExperiments_createdBefore,
    listExperiments_sortBy,
    listExperiments_maxResults,
    listExperiments_createdAfter,

    -- * Destructuring the Response
    ListExperimentsResponse (..),
    newListExperimentsResponse,

    -- * Response Lenses
    listExperimentsResponse_nextToken,
    listExperimentsResponse_experimentSummaries,
    listExperimentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListExperiments' smart constructor.
data ListExperiments = ListExperiments'
  { -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the previous call to @ListExperiments@ didn\'t return the full set of
    -- experiments, the call returns a token for getting the next set of
    -- experiments.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only experiments created before the specified
    -- time.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortExperimentsBy,
    -- | The maximum number of experiments to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only experiments created after the specified time.
    createdAfter :: Prelude.Maybe Data.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListExperiments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'sortBy', 'listExperiments_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'maxResults', 'listExperiments_maxResults' - The maximum number of experiments to return in the response. The default
-- value is 10.
--
-- 'createdAfter', 'listExperiments_createdAfter' - A filter that returns only experiments created after the specified time.
newListExperiments ::
  ListExperiments
newListExperiments =
  ListExperiments'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      createdAfter = Prelude.Nothing
    }

-- | The sort order. The default value is @Descending@.
listExperiments_sortOrder :: Lens.Lens' ListExperiments (Prelude.Maybe SortOrder)
listExperiments_sortOrder = Lens.lens (\ListExperiments' {sortOrder} -> sortOrder) (\s@ListExperiments' {} a -> s {sortOrder = a} :: ListExperiments)

-- | If the previous call to @ListExperiments@ didn\'t return the full set of
-- experiments, the call returns a token for getting the next set of
-- experiments.
listExperiments_nextToken :: Lens.Lens' ListExperiments (Prelude.Maybe Prelude.Text)
listExperiments_nextToken = Lens.lens (\ListExperiments' {nextToken} -> nextToken) (\s@ListExperiments' {} a -> s {nextToken = a} :: ListExperiments)

-- | A filter that returns only experiments created before the specified
-- time.
listExperiments_createdBefore :: Lens.Lens' ListExperiments (Prelude.Maybe Prelude.UTCTime)
listExperiments_createdBefore = Lens.lens (\ListExperiments' {createdBefore} -> createdBefore) (\s@ListExperiments' {} a -> s {createdBefore = a} :: ListExperiments) Prelude.. Lens.mapping Data._Time

-- | The property used to sort results. The default value is @CreationTime@.
listExperiments_sortBy :: Lens.Lens' ListExperiments (Prelude.Maybe SortExperimentsBy)
listExperiments_sortBy = Lens.lens (\ListExperiments' {sortBy} -> sortBy) (\s@ListExperiments' {} a -> s {sortBy = a} :: ListExperiments)

-- | The maximum number of experiments to return in the response. The default
-- value is 10.
listExperiments_maxResults :: Lens.Lens' ListExperiments (Prelude.Maybe Prelude.Natural)
listExperiments_maxResults = Lens.lens (\ListExperiments' {maxResults} -> maxResults) (\s@ListExperiments' {} a -> s {maxResults = a} :: ListExperiments)

-- | A filter that returns only experiments created after the specified time.
listExperiments_createdAfter :: Lens.Lens' ListExperiments (Prelude.Maybe Prelude.UTCTime)
listExperiments_createdAfter = Lens.lens (\ListExperiments' {createdAfter} -> createdAfter) (\s@ListExperiments' {} a -> s {createdAfter = a} :: ListExperiments) Prelude.. Lens.mapping Data._Time

instance Core.AWSPager ListExperiments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listExperimentsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listExperimentsResponse_experimentSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listExperiments_nextToken
          Lens..~ rs
          Lens.^? listExperimentsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListExperiments where
  type
    AWSResponse ListExperiments =
      ListExperimentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListExperimentsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> ( x Data..?> "ExperimentSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListExperiments where
  hashWithSalt _salt ListExperiments' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` createdAfter

instance Prelude.NFData ListExperiments where
  rnf ListExperiments' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf createdAfter

instance Data.ToHeaders ListExperiments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListExperiments" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListExperiments where
  toJSON ListExperiments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("CreatedAfter" Data..=) Prelude.<$> createdAfter
          ]
      )

instance Data.ToPath ListExperiments where
  toPath = Prelude.const "/"

instance Data.ToQuery ListExperiments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListExperimentsResponse' smart constructor.
data ListExperimentsResponse = ListExperimentsResponse'
  { -- | A token for getting the next set of experiments, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the summaries of your experiments.
    experimentSummaries :: Prelude.Maybe [ExperimentSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListExperimentsResponse
newListExperimentsResponse pHttpStatus_ =
  ListExperimentsResponse'
    { nextToken =
        Prelude.Nothing,
      experimentSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of experiments, if there are any.
listExperimentsResponse_nextToken :: Lens.Lens' ListExperimentsResponse (Prelude.Maybe Prelude.Text)
listExperimentsResponse_nextToken = Lens.lens (\ListExperimentsResponse' {nextToken} -> nextToken) (\s@ListExperimentsResponse' {} a -> s {nextToken = a} :: ListExperimentsResponse)

-- | A list of the summaries of your experiments.
listExperimentsResponse_experimentSummaries :: Lens.Lens' ListExperimentsResponse (Prelude.Maybe [ExperimentSummary])
listExperimentsResponse_experimentSummaries = Lens.lens (\ListExperimentsResponse' {experimentSummaries} -> experimentSummaries) (\s@ListExperimentsResponse' {} a -> s {experimentSummaries = a} :: ListExperimentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listExperimentsResponse_httpStatus :: Lens.Lens' ListExperimentsResponse Prelude.Int
listExperimentsResponse_httpStatus = Lens.lens (\ListExperimentsResponse' {httpStatus} -> httpStatus) (\s@ListExperimentsResponse' {} a -> s {httpStatus = a} :: ListExperimentsResponse)

instance Prelude.NFData ListExperimentsResponse where
  rnf ListExperimentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf experimentSummaries
      `Prelude.seq` Prelude.rnf httpStatus
