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
-- Module      : Amazonka.SageMaker.ListTrials
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the trials in your account. Specify an experiment name to limit
-- the list to the trials that are part of that experiment. Specify a trial
-- component name to limit the list to the trials that associated with that
-- trial component. The list can be filtered to show only trials that were
-- created in a specific time range. The list can be sorted by trial name
-- or creation time.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListTrials
  ( -- * Creating a Request
    ListTrials (..),
    newListTrials,

    -- * Request Lenses
    listTrials_createdAfter,
    listTrials_createdBefore,
    listTrials_experimentName,
    listTrials_maxResults,
    listTrials_nextToken,
    listTrials_sortBy,
    listTrials_sortOrder,
    listTrials_trialComponentName,

    -- * Destructuring the Response
    ListTrialsResponse (..),
    newListTrialsResponse,

    -- * Response Lenses
    listTrialsResponse_nextToken,
    listTrialsResponse_trialSummaries,
    listTrialsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListTrials' smart constructor.
data ListTrials = ListTrials'
  { -- | A filter that returns only trials created after the specified time.
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only trials created before the specified time.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only trials that are part of the specified
    -- experiment.
    experimentName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of trials to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the previous call to @ListTrials@ didn\'t return the full set of
    -- trials, the call returns a token for getting the next set of trials.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortTrialsBy,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only trials that are associated with the specified
    -- trial component.
    trialComponentName :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrials' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listTrials_createdAfter' - A filter that returns only trials created after the specified time.
--
-- 'createdBefore', 'listTrials_createdBefore' - A filter that returns only trials created before the specified time.
--
-- 'experimentName', 'listTrials_experimentName' - A filter that returns only trials that are part of the specified
-- experiment.
--
-- 'maxResults', 'listTrials_maxResults' - The maximum number of trials to return in the response. The default
-- value is 10.
--
-- 'nextToken', 'listTrials_nextToken' - If the previous call to @ListTrials@ didn\'t return the full set of
-- trials, the call returns a token for getting the next set of trials.
--
-- 'sortBy', 'listTrials_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'sortOrder', 'listTrials_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'trialComponentName', 'listTrials_trialComponentName' - A filter that returns only trials that are associated with the specified
-- trial component.
newListTrials ::
  ListTrials
newListTrials =
  ListTrials'
    { createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      trialComponentName = Prelude.Nothing
    }

-- | A filter that returns only trials created after the specified time.
listTrials_createdAfter :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.UTCTime)
listTrials_createdAfter = Lens.lens (\ListTrials' {createdAfter} -> createdAfter) (\s@ListTrials' {} a -> s {createdAfter = a} :: ListTrials) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only trials created before the specified time.
listTrials_createdBefore :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.UTCTime)
listTrials_createdBefore = Lens.lens (\ListTrials' {createdBefore} -> createdBefore) (\s@ListTrials' {} a -> s {createdBefore = a} :: ListTrials) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only trials that are part of the specified
-- experiment.
listTrials_experimentName :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.Text)
listTrials_experimentName = Lens.lens (\ListTrials' {experimentName} -> experimentName) (\s@ListTrials' {} a -> s {experimentName = a} :: ListTrials)

-- | The maximum number of trials to return in the response. The default
-- value is 10.
listTrials_maxResults :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.Natural)
listTrials_maxResults = Lens.lens (\ListTrials' {maxResults} -> maxResults) (\s@ListTrials' {} a -> s {maxResults = a} :: ListTrials)

-- | If the previous call to @ListTrials@ didn\'t return the full set of
-- trials, the call returns a token for getting the next set of trials.
listTrials_nextToken :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.Text)
listTrials_nextToken = Lens.lens (\ListTrials' {nextToken} -> nextToken) (\s@ListTrials' {} a -> s {nextToken = a} :: ListTrials)

-- | The property used to sort results. The default value is @CreationTime@.
listTrials_sortBy :: Lens.Lens' ListTrials (Prelude.Maybe SortTrialsBy)
listTrials_sortBy = Lens.lens (\ListTrials' {sortBy} -> sortBy) (\s@ListTrials' {} a -> s {sortBy = a} :: ListTrials)

-- | The sort order. The default value is @Descending@.
listTrials_sortOrder :: Lens.Lens' ListTrials (Prelude.Maybe SortOrder)
listTrials_sortOrder = Lens.lens (\ListTrials' {sortOrder} -> sortOrder) (\s@ListTrials' {} a -> s {sortOrder = a} :: ListTrials)

-- | A filter that returns only trials that are associated with the specified
-- trial component.
listTrials_trialComponentName :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.Text)
listTrials_trialComponentName = Lens.lens (\ListTrials' {trialComponentName} -> trialComponentName) (\s@ListTrials' {} a -> s {trialComponentName = a} :: ListTrials)

instance Core.AWSPager ListTrials where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listTrialsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listTrialsResponse_trialSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listTrials_nextToken
          Lens..~ rs
          Lens.^? listTrialsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListTrials where
  type AWSResponse ListTrials = ListTrialsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrialsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "TrialSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTrials where
  hashWithSalt _salt ListTrials' {..} =
    _salt `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` experimentName
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` trialComponentName

instance Prelude.NFData ListTrials where
  rnf ListTrials' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf experimentName
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf trialComponentName

instance Data.ToHeaders ListTrials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListTrials" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListTrials where
  toJSON ListTrials' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Data..=) Prelude.<$> createdAfter,
            ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("ExperimentName" Data..=)
              Prelude.<$> experimentName,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("TrialComponentName" Data..=)
              Prelude.<$> trialComponentName
          ]
      )

instance Data.ToPath ListTrials where
  toPath = Prelude.const "/"

instance Data.ToQuery ListTrials where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListTrialsResponse' smart constructor.
data ListTrialsResponse = ListTrialsResponse'
  { -- | A token for getting the next set of trials, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of the summaries of your trials.
    trialSummaries :: Prelude.Maybe [TrialSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListTrialsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listTrialsResponse_nextToken' - A token for getting the next set of trials, if there are any.
--
-- 'trialSummaries', 'listTrialsResponse_trialSummaries' - A list of the summaries of your trials.
--
-- 'httpStatus', 'listTrialsResponse_httpStatus' - The response's http status code.
newListTrialsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListTrialsResponse
newListTrialsResponse pHttpStatus_ =
  ListTrialsResponse'
    { nextToken = Prelude.Nothing,
      trialSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of trials, if there are any.
listTrialsResponse_nextToken :: Lens.Lens' ListTrialsResponse (Prelude.Maybe Prelude.Text)
listTrialsResponse_nextToken = Lens.lens (\ListTrialsResponse' {nextToken} -> nextToken) (\s@ListTrialsResponse' {} a -> s {nextToken = a} :: ListTrialsResponse)

-- | A list of the summaries of your trials.
listTrialsResponse_trialSummaries :: Lens.Lens' ListTrialsResponse (Prelude.Maybe [TrialSummary])
listTrialsResponse_trialSummaries = Lens.lens (\ListTrialsResponse' {trialSummaries} -> trialSummaries) (\s@ListTrialsResponse' {} a -> s {trialSummaries = a} :: ListTrialsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listTrialsResponse_httpStatus :: Lens.Lens' ListTrialsResponse Prelude.Int
listTrialsResponse_httpStatus = Lens.lens (\ListTrialsResponse' {httpStatus} -> httpStatus) (\s@ListTrialsResponse' {} a -> s {httpStatus = a} :: ListTrialsResponse)

instance Prelude.NFData ListTrialsResponse where
  rnf ListTrialsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf trialSummaries
      `Prelude.seq` Prelude.rnf httpStatus
