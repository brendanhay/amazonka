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
-- Module      : Amazonka.SageMaker.ListModelExplainabilityJobDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists model explainability job definitions that satisfy various filters.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListModelExplainabilityJobDefinitions
  ( -- * Creating a Request
    ListModelExplainabilityJobDefinitions (..),
    newListModelExplainabilityJobDefinitions,

    -- * Request Lenses
    listModelExplainabilityJobDefinitions_creationTimeAfter,
    listModelExplainabilityJobDefinitions_creationTimeBefore,
    listModelExplainabilityJobDefinitions_endpointName,
    listModelExplainabilityJobDefinitions_maxResults,
    listModelExplainabilityJobDefinitions_nameContains,
    listModelExplainabilityJobDefinitions_nextToken,
    listModelExplainabilityJobDefinitions_sortBy,
    listModelExplainabilityJobDefinitions_sortOrder,

    -- * Destructuring the Response
    ListModelExplainabilityJobDefinitionsResponse (..),
    newListModelExplainabilityJobDefinitionsResponse,

    -- * Response Lenses
    listModelExplainabilityJobDefinitionsResponse_nextToken,
    listModelExplainabilityJobDefinitionsResponse_httpStatus,
    listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListModelExplainabilityJobDefinitions' smart constructor.
data ListModelExplainabilityJobDefinitions = ListModelExplainabilityJobDefinitions'
  { -- | A filter that returns only model explainability jobs created after a
    -- specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only model explainability jobs created before a
    -- specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Name of the endpoint to monitor for model explainability.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of jobs to return in the response. The default value
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Filter for model explainability jobs whose name contains a specified
    -- string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Whether to sort results by the @Name@ or @CreationTime@ field. The
    -- default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringJobDefinitionSortKey,
    -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelExplainabilityJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listModelExplainabilityJobDefinitions_creationTimeAfter' - A filter that returns only model explainability jobs created after a
-- specified time.
--
-- 'creationTimeBefore', 'listModelExplainabilityJobDefinitions_creationTimeBefore' - A filter that returns only model explainability jobs created before a
-- specified time.
--
-- 'endpointName', 'listModelExplainabilityJobDefinitions_endpointName' - Name of the endpoint to monitor for model explainability.
--
-- 'maxResults', 'listModelExplainabilityJobDefinitions_maxResults' - The maximum number of jobs to return in the response. The default value
-- is 10.
--
-- 'nameContains', 'listModelExplainabilityJobDefinitions_nameContains' - Filter for model explainability jobs whose name contains a specified
-- string.
--
-- 'nextToken', 'listModelExplainabilityJobDefinitions_nextToken' - The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
--
-- 'sortBy', 'listModelExplainabilityJobDefinitions_sortBy' - Whether to sort results by the @Name@ or @CreationTime@ field. The
-- default is @CreationTime@.
--
-- 'sortOrder', 'listModelExplainabilityJobDefinitions_sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
newListModelExplainabilityJobDefinitions ::
  ListModelExplainabilityJobDefinitions
newListModelExplainabilityJobDefinitions =
  ListModelExplainabilityJobDefinitions'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      endpointName = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | A filter that returns only model explainability jobs created after a
-- specified time.
listModelExplainabilityJobDefinitions_creationTimeAfter :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.UTCTime)
listModelExplainabilityJobDefinitions_creationTimeAfter = Lens.lens (\ListModelExplainabilityJobDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {creationTimeAfter = a} :: ListModelExplainabilityJobDefinitions) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only model explainability jobs created before a
-- specified time.
listModelExplainabilityJobDefinitions_creationTimeBefore :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.UTCTime)
listModelExplainabilityJobDefinitions_creationTimeBefore = Lens.lens (\ListModelExplainabilityJobDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {creationTimeBefore = a} :: ListModelExplainabilityJobDefinitions) Prelude.. Lens.mapping Data._Time

-- | Name of the endpoint to monitor for model explainability.
listModelExplainabilityJobDefinitions_endpointName :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.Text)
listModelExplainabilityJobDefinitions_endpointName = Lens.lens (\ListModelExplainabilityJobDefinitions' {endpointName} -> endpointName) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {endpointName = a} :: ListModelExplainabilityJobDefinitions)

-- | The maximum number of jobs to return in the response. The default value
-- is 10.
listModelExplainabilityJobDefinitions_maxResults :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.Natural)
listModelExplainabilityJobDefinitions_maxResults = Lens.lens (\ListModelExplainabilityJobDefinitions' {maxResults} -> maxResults) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {maxResults = a} :: ListModelExplainabilityJobDefinitions)

-- | Filter for model explainability jobs whose name contains a specified
-- string.
listModelExplainabilityJobDefinitions_nameContains :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.Text)
listModelExplainabilityJobDefinitions_nameContains = Lens.lens (\ListModelExplainabilityJobDefinitions' {nameContains} -> nameContains) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {nameContains = a} :: ListModelExplainabilityJobDefinitions)

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listModelExplainabilityJobDefinitions_nextToken :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.Text)
listModelExplainabilityJobDefinitions_nextToken = Lens.lens (\ListModelExplainabilityJobDefinitions' {nextToken} -> nextToken) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {nextToken = a} :: ListModelExplainabilityJobDefinitions)

-- | Whether to sort results by the @Name@ or @CreationTime@ field. The
-- default is @CreationTime@.
listModelExplainabilityJobDefinitions_sortBy :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe MonitoringJobDefinitionSortKey)
listModelExplainabilityJobDefinitions_sortBy = Lens.lens (\ListModelExplainabilityJobDefinitions' {sortBy} -> sortBy) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {sortBy = a} :: ListModelExplainabilityJobDefinitions)

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
listModelExplainabilityJobDefinitions_sortOrder :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe SortOrder)
listModelExplainabilityJobDefinitions_sortOrder = Lens.lens (\ListModelExplainabilityJobDefinitions' {sortOrder} -> sortOrder) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {sortOrder = a} :: ListModelExplainabilityJobDefinitions)

instance
  Core.AWSPager
    ListModelExplainabilityJobDefinitions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelExplainabilityJobDefinitionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listModelExplainabilityJobDefinitions_nextToken
              Lens..~ rs
              Lens.^? listModelExplainabilityJobDefinitionsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListModelExplainabilityJobDefinitions
  where
  type
    AWSResponse
      ListModelExplainabilityJobDefinitions =
      ListModelExplainabilityJobDefinitionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelExplainabilityJobDefinitionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "JobDefinitionSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListModelExplainabilityJobDefinitions
  where
  hashWithSalt
    _salt
    ListModelExplainabilityJobDefinitions' {..} =
      _salt
        `Prelude.hashWithSalt` creationTimeAfter
        `Prelude.hashWithSalt` creationTimeBefore
        `Prelude.hashWithSalt` endpointName
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` nameContains
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` sortBy
        `Prelude.hashWithSalt` sortOrder

instance
  Prelude.NFData
    ListModelExplainabilityJobDefinitions
  where
  rnf ListModelExplainabilityJobDefinitions' {..} =
    Prelude.rnf creationTimeAfter `Prelude.seq`
      Prelude.rnf creationTimeBefore `Prelude.seq`
        Prelude.rnf endpointName `Prelude.seq`
          Prelude.rnf maxResults `Prelude.seq`
            Prelude.rnf nameContains `Prelude.seq`
              Prelude.rnf nextToken `Prelude.seq`
                Prelude.rnf sortBy `Prelude.seq`
                  Prelude.rnf sortOrder

instance
  Data.ToHeaders
    ListModelExplainabilityJobDefinitions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListModelExplainabilityJobDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    ListModelExplainabilityJobDefinitions
  where
  toJSON ListModelExplainabilityJobDefinitions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("EndpointName" Data..=) Prelude.<$> endpointName,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance
  Data.ToPath
    ListModelExplainabilityJobDefinitions
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListModelExplainabilityJobDefinitions
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelExplainabilityJobDefinitionsResponse' smart constructor.
data ListModelExplainabilityJobDefinitionsResponse = ListModelExplainabilityJobDefinitionsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of jobs, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A JSON array in which each element is a summary for a explainability
    -- bias jobs.
    jobDefinitionSummaries :: [MonitoringJobDefinitionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelExplainabilityJobDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelExplainabilityJobDefinitionsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent request.
--
-- 'httpStatus', 'listModelExplainabilityJobDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionSummaries', 'listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries' - A JSON array in which each element is a summary for a explainability
-- bias jobs.
newListModelExplainabilityJobDefinitionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelExplainabilityJobDefinitionsResponse
newListModelExplainabilityJobDefinitionsResponse
  pHttpStatus_ =
    ListModelExplainabilityJobDefinitionsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        jobDefinitionSummaries =
          Prelude.mempty
      }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent request.
listModelExplainabilityJobDefinitionsResponse_nextToken :: Lens.Lens' ListModelExplainabilityJobDefinitionsResponse (Prelude.Maybe Prelude.Text)
listModelExplainabilityJobDefinitionsResponse_nextToken = Lens.lens (\ListModelExplainabilityJobDefinitionsResponse' {nextToken} -> nextToken) (\s@ListModelExplainabilityJobDefinitionsResponse' {} a -> s {nextToken = a} :: ListModelExplainabilityJobDefinitionsResponse)

-- | The response's http status code.
listModelExplainabilityJobDefinitionsResponse_httpStatus :: Lens.Lens' ListModelExplainabilityJobDefinitionsResponse Prelude.Int
listModelExplainabilityJobDefinitionsResponse_httpStatus = Lens.lens (\ListModelExplainabilityJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListModelExplainabilityJobDefinitionsResponse' {} a -> s {httpStatus = a} :: ListModelExplainabilityJobDefinitionsResponse)

-- | A JSON array in which each element is a summary for a explainability
-- bias jobs.
listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries :: Lens.Lens' ListModelExplainabilityJobDefinitionsResponse [MonitoringJobDefinitionSummary]
listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries = Lens.lens (\ListModelExplainabilityJobDefinitionsResponse' {jobDefinitionSummaries} -> jobDefinitionSummaries) (\s@ListModelExplainabilityJobDefinitionsResponse' {} a -> s {jobDefinitionSummaries = a} :: ListModelExplainabilityJobDefinitionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListModelExplainabilityJobDefinitionsResponse
  where
  rnf
    ListModelExplainabilityJobDefinitionsResponse' {..} =
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus `Prelude.seq`
          Prelude.rnf jobDefinitionSummaries
