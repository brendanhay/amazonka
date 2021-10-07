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
-- Module      : Network.AWS.SageMaker.ListTrials
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.SageMaker.ListTrials
  ( -- * Creating a Request
    ListTrials (..),
    newListTrials,

    -- * Request Lenses
    listTrials_nextToken,
    listTrials_sortOrder,
    listTrials_createdAfter,
    listTrials_createdBefore,
    listTrials_maxResults,
    listTrials_sortBy,
    listTrials_experimentName,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListTrials' smart constructor.
data ListTrials = ListTrials'
  { -- | If the previous call to @ListTrials@ didn\'t return the full set of
    -- trials, the call returns a token for getting the next set of trials.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only trials created after the specified time.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only trials created before the specified time.
    createdBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of trials to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortTrialsBy,
    -- | A filter that returns only trials that are part of the specified
    -- experiment.
    experimentName :: Prelude.Maybe Prelude.Text,
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
-- 'nextToken', 'listTrials_nextToken' - If the previous call to @ListTrials@ didn\'t return the full set of
-- trials, the call returns a token for getting the next set of trials.
--
-- 'sortOrder', 'listTrials_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'createdAfter', 'listTrials_createdAfter' - A filter that returns only trials created after the specified time.
--
-- 'createdBefore', 'listTrials_createdBefore' - A filter that returns only trials created before the specified time.
--
-- 'maxResults', 'listTrials_maxResults' - The maximum number of trials to return in the response. The default
-- value is 10.
--
-- 'sortBy', 'listTrials_sortBy' - The property used to sort results. The default value is @CreationTime@.
--
-- 'experimentName', 'listTrials_experimentName' - A filter that returns only trials that are part of the specified
-- experiment.
--
-- 'trialComponentName', 'listTrials_trialComponentName' - A filter that returns only trials that are associated with the specified
-- trial component.
newListTrials ::
  ListTrials
newListTrials =
  ListTrials'
    { nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      experimentName = Prelude.Nothing,
      trialComponentName = Prelude.Nothing
    }

-- | If the previous call to @ListTrials@ didn\'t return the full set of
-- trials, the call returns a token for getting the next set of trials.
listTrials_nextToken :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.Text)
listTrials_nextToken = Lens.lens (\ListTrials' {nextToken} -> nextToken) (\s@ListTrials' {} a -> s {nextToken = a} :: ListTrials)

-- | The sort order. The default value is @Descending@.
listTrials_sortOrder :: Lens.Lens' ListTrials (Prelude.Maybe SortOrder)
listTrials_sortOrder = Lens.lens (\ListTrials' {sortOrder} -> sortOrder) (\s@ListTrials' {} a -> s {sortOrder = a} :: ListTrials)

-- | A filter that returns only trials created after the specified time.
listTrials_createdAfter :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.UTCTime)
listTrials_createdAfter = Lens.lens (\ListTrials' {createdAfter} -> createdAfter) (\s@ListTrials' {} a -> s {createdAfter = a} :: ListTrials) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only trials created before the specified time.
listTrials_createdBefore :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.UTCTime)
listTrials_createdBefore = Lens.lens (\ListTrials' {createdBefore} -> createdBefore) (\s@ListTrials' {} a -> s {createdBefore = a} :: ListTrials) Prelude.. Lens.mapping Core._Time

-- | The maximum number of trials to return in the response. The default
-- value is 10.
listTrials_maxResults :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.Natural)
listTrials_maxResults = Lens.lens (\ListTrials' {maxResults} -> maxResults) (\s@ListTrials' {} a -> s {maxResults = a} :: ListTrials)

-- | The property used to sort results. The default value is @CreationTime@.
listTrials_sortBy :: Lens.Lens' ListTrials (Prelude.Maybe SortTrialsBy)
listTrials_sortBy = Lens.lens (\ListTrials' {sortBy} -> sortBy) (\s@ListTrials' {} a -> s {sortBy = a} :: ListTrials)

-- | A filter that returns only trials that are part of the specified
-- experiment.
listTrials_experimentName :: Lens.Lens' ListTrials (Prelude.Maybe Prelude.Text)
listTrials_experimentName = Lens.lens (\ListTrials' {experimentName} -> experimentName) (\s@ListTrials' {} a -> s {experimentName = a} :: ListTrials)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListTrialsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (x Core..?> "TrialSummaries" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListTrials

instance Prelude.NFData ListTrials

instance Core.ToHeaders ListTrials where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListTrials" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListTrials where
  toJSON ListTrials' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("CreatedAfter" Core..=) Prelude.<$> createdAfter,
            ("CreatedBefore" Core..=) Prelude.<$> createdBefore,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("ExperimentName" Core..=)
              Prelude.<$> experimentName,
            ("TrialComponentName" Core..=)
              Prelude.<$> trialComponentName
          ]
      )

instance Core.ToPath ListTrials where
  toPath = Prelude.const "/"

instance Core.ToQuery ListTrials where
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
listTrialsResponse_trialSummaries = Lens.lens (\ListTrialsResponse' {trialSummaries} -> trialSummaries) (\s@ListTrialsResponse' {} a -> s {trialSummaries = a} :: ListTrialsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listTrialsResponse_httpStatus :: Lens.Lens' ListTrialsResponse Prelude.Int
listTrialsResponse_httpStatus = Lens.lens (\ListTrialsResponse' {httpStatus} -> httpStatus) (\s@ListTrialsResponse' {} a -> s {httpStatus = a} :: ListTrialsResponse)

instance Prelude.NFData ListTrialsResponse
