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
-- Module      : Network.AWS.SageMaker.ListModelExplainabilityJobDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists model explainability job definitions that satisfy various filters.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListModelExplainabilityJobDefinitions
  ( -- * Creating a Request
    ListModelExplainabilityJobDefinitions (..),
    newListModelExplainabilityJobDefinitions,

    -- * Request Lenses
    listModelExplainabilityJobDefinitions_nameContains,
    listModelExplainabilityJobDefinitions_endpointName,
    listModelExplainabilityJobDefinitions_creationTimeAfter,
    listModelExplainabilityJobDefinitions_nextToken,
    listModelExplainabilityJobDefinitions_sortOrder,
    listModelExplainabilityJobDefinitions_creationTimeBefore,
    listModelExplainabilityJobDefinitions_maxResults,
    listModelExplainabilityJobDefinitions_sortBy,

    -- * Destructuring the Response
    ListModelExplainabilityJobDefinitionsResponse (..),
    newListModelExplainabilityJobDefinitionsResponse,

    -- * Response Lenses
    listModelExplainabilityJobDefinitionsResponse_nextToken,
    listModelExplainabilityJobDefinitionsResponse_httpStatus,
    listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListModelExplainabilityJobDefinitions' smart constructor.
data ListModelExplainabilityJobDefinitions = ListModelExplainabilityJobDefinitions'
  { -- | Filter for model explainability jobs whose name contains a specified
    -- string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | Name of the endpoint to monitor for model explainability.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only model explainability jobs created after a
    -- specified time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only model explainability jobs created before a
    -- specified time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of jobs to return in the response. The default value
    -- is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Whether to sort results by the @Name@ or @CreationTime@ field. The
    -- default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringJobDefinitionSortKey
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
-- 'nameContains', 'listModelExplainabilityJobDefinitions_nameContains' - Filter for model explainability jobs whose name contains a specified
-- string.
--
-- 'endpointName', 'listModelExplainabilityJobDefinitions_endpointName' - Name of the endpoint to monitor for model explainability.
--
-- 'creationTimeAfter', 'listModelExplainabilityJobDefinitions_creationTimeAfter' - A filter that returns only model explainability jobs created after a
-- specified time.
--
-- 'nextToken', 'listModelExplainabilityJobDefinitions_nextToken' - The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
--
-- 'sortOrder', 'listModelExplainabilityJobDefinitions_sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
--
-- 'creationTimeBefore', 'listModelExplainabilityJobDefinitions_creationTimeBefore' - A filter that returns only model explainability jobs created before a
-- specified time.
--
-- 'maxResults', 'listModelExplainabilityJobDefinitions_maxResults' - The maximum number of jobs to return in the response. The default value
-- is 10.
--
-- 'sortBy', 'listModelExplainabilityJobDefinitions_sortBy' - Whether to sort results by the @Name@ or @CreationTime@ field. The
-- default is @CreationTime@.
newListModelExplainabilityJobDefinitions ::
  ListModelExplainabilityJobDefinitions
newListModelExplainabilityJobDefinitions =
  ListModelExplainabilityJobDefinitions'
    { nameContains =
        Prelude.Nothing,
      endpointName = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | Filter for model explainability jobs whose name contains a specified
-- string.
listModelExplainabilityJobDefinitions_nameContains :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.Text)
listModelExplainabilityJobDefinitions_nameContains = Lens.lens (\ListModelExplainabilityJobDefinitions' {nameContains} -> nameContains) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {nameContains = a} :: ListModelExplainabilityJobDefinitions)

-- | Name of the endpoint to monitor for model explainability.
listModelExplainabilityJobDefinitions_endpointName :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.Text)
listModelExplainabilityJobDefinitions_endpointName = Lens.lens (\ListModelExplainabilityJobDefinitions' {endpointName} -> endpointName) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {endpointName = a} :: ListModelExplainabilityJobDefinitions)

-- | A filter that returns only model explainability jobs created after a
-- specified time.
listModelExplainabilityJobDefinitions_creationTimeAfter :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.UTCTime)
listModelExplainabilityJobDefinitions_creationTimeAfter = Lens.lens (\ListModelExplainabilityJobDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {creationTimeAfter = a} :: ListModelExplainabilityJobDefinitions) Prelude.. Lens.mapping Core._Time

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listModelExplainabilityJobDefinitions_nextToken :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.Text)
listModelExplainabilityJobDefinitions_nextToken = Lens.lens (\ListModelExplainabilityJobDefinitions' {nextToken} -> nextToken) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {nextToken = a} :: ListModelExplainabilityJobDefinitions)

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
listModelExplainabilityJobDefinitions_sortOrder :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe SortOrder)
listModelExplainabilityJobDefinitions_sortOrder = Lens.lens (\ListModelExplainabilityJobDefinitions' {sortOrder} -> sortOrder) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {sortOrder = a} :: ListModelExplainabilityJobDefinitions)

-- | A filter that returns only model explainability jobs created before a
-- specified time.
listModelExplainabilityJobDefinitions_creationTimeBefore :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.UTCTime)
listModelExplainabilityJobDefinitions_creationTimeBefore = Lens.lens (\ListModelExplainabilityJobDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {creationTimeBefore = a} :: ListModelExplainabilityJobDefinitions) Prelude.. Lens.mapping Core._Time

-- | The maximum number of jobs to return in the response. The default value
-- is 10.
listModelExplainabilityJobDefinitions_maxResults :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe Prelude.Natural)
listModelExplainabilityJobDefinitions_maxResults = Lens.lens (\ListModelExplainabilityJobDefinitions' {maxResults} -> maxResults) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {maxResults = a} :: ListModelExplainabilityJobDefinitions)

-- | Whether to sort results by the @Name@ or @CreationTime@ field. The
-- default is @CreationTime@.
listModelExplainabilityJobDefinitions_sortBy :: Lens.Lens' ListModelExplainabilityJobDefinitions (Prelude.Maybe MonitoringJobDefinitionSortKey)
listModelExplainabilityJobDefinitions_sortBy = Lens.lens (\ListModelExplainabilityJobDefinitions' {sortBy} -> sortBy) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {sortBy = a} :: ListModelExplainabilityJobDefinitions)

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
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelExplainabilityJobDefinitionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
              Prelude.<*> ( x Core..?> "JobDefinitionSummaries"
                              Core..!@ Prelude.mempty
                          )
      )

instance
  Prelude.Hashable
    ListModelExplainabilityJobDefinitions

instance
  Prelude.NFData
    ListModelExplainabilityJobDefinitions

instance
  Core.ToHeaders
    ListModelExplainabilityJobDefinitions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListModelExplainabilityJobDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListModelExplainabilityJobDefinitions
  where
  toJSON ListModelExplainabilityJobDefinitions' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("EndpointName" Core..=) Prelude.<$> endpointName,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance
  Core.ToPath
    ListModelExplainabilityJobDefinitions
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
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
