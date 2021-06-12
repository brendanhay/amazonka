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
    listModelExplainabilityJobDefinitions_sortOrder,
    listModelExplainabilityJobDefinitions_nextToken,
    listModelExplainabilityJobDefinitions_endpointName,
    listModelExplainabilityJobDefinitions_nameContains,
    listModelExplainabilityJobDefinitions_maxResults,
    listModelExplainabilityJobDefinitions_creationTimeBefore,
    listModelExplainabilityJobDefinitions_sortBy,
    listModelExplainabilityJobDefinitions_creationTimeAfter,

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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListModelExplainabilityJobDefinitions' smart constructor.
data ListModelExplainabilityJobDefinitions = ListModelExplainabilityJobDefinitions'
  { -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | Name of the endpoint to monitor for model explainability.
    endpointName :: Core.Maybe Core.Text,
    -- | Filter for model explainability jobs whose name contains a specified
    -- string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of jobs to return in the response. The default value
    -- is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only model explainability jobs created before a
    -- specified time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | Whether to sort results by the @Name@ or @CreationTime@ field. The
    -- default is @CreationTime@.
    sortBy :: Core.Maybe MonitoringJobDefinitionSortKey,
    -- | A filter that returns only model explainability jobs created after a
    -- specified time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListModelExplainabilityJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listModelExplainabilityJobDefinitions_sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
--
-- 'nextToken', 'listModelExplainabilityJobDefinitions_nextToken' - The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
--
-- 'endpointName', 'listModelExplainabilityJobDefinitions_endpointName' - Name of the endpoint to monitor for model explainability.
--
-- 'nameContains', 'listModelExplainabilityJobDefinitions_nameContains' - Filter for model explainability jobs whose name contains a specified
-- string.
--
-- 'maxResults', 'listModelExplainabilityJobDefinitions_maxResults' - The maximum number of jobs to return in the response. The default value
-- is 10.
--
-- 'creationTimeBefore', 'listModelExplainabilityJobDefinitions_creationTimeBefore' - A filter that returns only model explainability jobs created before a
-- specified time.
--
-- 'sortBy', 'listModelExplainabilityJobDefinitions_sortBy' - Whether to sort results by the @Name@ or @CreationTime@ field. The
-- default is @CreationTime@.
--
-- 'creationTimeAfter', 'listModelExplainabilityJobDefinitions_creationTimeAfter' - A filter that returns only model explainability jobs created after a
-- specified time.
newListModelExplainabilityJobDefinitions ::
  ListModelExplainabilityJobDefinitions
newListModelExplainabilityJobDefinitions =
  ListModelExplainabilityJobDefinitions'
    { sortOrder =
        Core.Nothing,
      nextToken = Core.Nothing,
      endpointName = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      sortBy = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
listModelExplainabilityJobDefinitions_sortOrder :: Lens.Lens' ListModelExplainabilityJobDefinitions (Core.Maybe SortOrder)
listModelExplainabilityJobDefinitions_sortOrder = Lens.lens (\ListModelExplainabilityJobDefinitions' {sortOrder} -> sortOrder) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {sortOrder = a} :: ListModelExplainabilityJobDefinitions)

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listModelExplainabilityJobDefinitions_nextToken :: Lens.Lens' ListModelExplainabilityJobDefinitions (Core.Maybe Core.Text)
listModelExplainabilityJobDefinitions_nextToken = Lens.lens (\ListModelExplainabilityJobDefinitions' {nextToken} -> nextToken) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {nextToken = a} :: ListModelExplainabilityJobDefinitions)

-- | Name of the endpoint to monitor for model explainability.
listModelExplainabilityJobDefinitions_endpointName :: Lens.Lens' ListModelExplainabilityJobDefinitions (Core.Maybe Core.Text)
listModelExplainabilityJobDefinitions_endpointName = Lens.lens (\ListModelExplainabilityJobDefinitions' {endpointName} -> endpointName) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {endpointName = a} :: ListModelExplainabilityJobDefinitions)

-- | Filter for model explainability jobs whose name contains a specified
-- string.
listModelExplainabilityJobDefinitions_nameContains :: Lens.Lens' ListModelExplainabilityJobDefinitions (Core.Maybe Core.Text)
listModelExplainabilityJobDefinitions_nameContains = Lens.lens (\ListModelExplainabilityJobDefinitions' {nameContains} -> nameContains) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {nameContains = a} :: ListModelExplainabilityJobDefinitions)

-- | The maximum number of jobs to return in the response. The default value
-- is 10.
listModelExplainabilityJobDefinitions_maxResults :: Lens.Lens' ListModelExplainabilityJobDefinitions (Core.Maybe Core.Natural)
listModelExplainabilityJobDefinitions_maxResults = Lens.lens (\ListModelExplainabilityJobDefinitions' {maxResults} -> maxResults) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {maxResults = a} :: ListModelExplainabilityJobDefinitions)

-- | A filter that returns only model explainability jobs created before a
-- specified time.
listModelExplainabilityJobDefinitions_creationTimeBefore :: Lens.Lens' ListModelExplainabilityJobDefinitions (Core.Maybe Core.UTCTime)
listModelExplainabilityJobDefinitions_creationTimeBefore = Lens.lens (\ListModelExplainabilityJobDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {creationTimeBefore = a} :: ListModelExplainabilityJobDefinitions) Core.. Lens.mapping Core._Time

-- | Whether to sort results by the @Name@ or @CreationTime@ field. The
-- default is @CreationTime@.
listModelExplainabilityJobDefinitions_sortBy :: Lens.Lens' ListModelExplainabilityJobDefinitions (Core.Maybe MonitoringJobDefinitionSortKey)
listModelExplainabilityJobDefinitions_sortBy = Lens.lens (\ListModelExplainabilityJobDefinitions' {sortBy} -> sortBy) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {sortBy = a} :: ListModelExplainabilityJobDefinitions)

-- | A filter that returns only model explainability jobs created after a
-- specified time.
listModelExplainabilityJobDefinitions_creationTimeAfter :: Lens.Lens' ListModelExplainabilityJobDefinitions (Core.Maybe Core.UTCTime)
listModelExplainabilityJobDefinitions_creationTimeAfter = Lens.lens (\ListModelExplainabilityJobDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelExplainabilityJobDefinitions' {} a -> s {creationTimeAfter = a} :: ListModelExplainabilityJobDefinitions) Core.. Lens.mapping Core._Time

instance
  Core.AWSPager
    ListModelExplainabilityJobDefinitions
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelExplainabilityJobDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listModelExplainabilityJobDefinitions_nextToken
          Lens..~ rs
            Lens.^? listModelExplainabilityJobDefinitionsResponse_nextToken
              Core.. Lens._Just

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
            Core.<$> (x Core..?> "NextToken")
              Core.<*> (Core.pure (Core.fromEnum s))
              Core.<*> ( x Core..?> "JobDefinitionSummaries"
                           Core..!@ Core.mempty
                       )
      )

instance
  Core.Hashable
    ListModelExplainabilityJobDefinitions

instance
  Core.NFData
    ListModelExplainabilityJobDefinitions

instance
  Core.ToHeaders
    ListModelExplainabilityJobDefinitions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListModelExplainabilityJobDefinitions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    ListModelExplainabilityJobDefinitions
  where
  toJSON ListModelExplainabilityJobDefinitions' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("EndpointName" Core..=) Core.<$> endpointName,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance
  Core.ToPath
    ListModelExplainabilityJobDefinitions
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListModelExplainabilityJobDefinitions
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListModelExplainabilityJobDefinitionsResponse' smart constructor.
data ListModelExplainabilityJobDefinitionsResponse = ListModelExplainabilityJobDefinitionsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of jobs, use it in the subsequent request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A JSON array in which each element is a summary for a explainability
    -- bias jobs.
    jobDefinitionSummaries :: [MonitoringJobDefinitionSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  ListModelExplainabilityJobDefinitionsResponse
newListModelExplainabilityJobDefinitionsResponse
  pHttpStatus_ =
    ListModelExplainabilityJobDefinitionsResponse'
      { nextToken =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        jobDefinitionSummaries =
          Core.mempty
      }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent request.
listModelExplainabilityJobDefinitionsResponse_nextToken :: Lens.Lens' ListModelExplainabilityJobDefinitionsResponse (Core.Maybe Core.Text)
listModelExplainabilityJobDefinitionsResponse_nextToken = Lens.lens (\ListModelExplainabilityJobDefinitionsResponse' {nextToken} -> nextToken) (\s@ListModelExplainabilityJobDefinitionsResponse' {} a -> s {nextToken = a} :: ListModelExplainabilityJobDefinitionsResponse)

-- | The response's http status code.
listModelExplainabilityJobDefinitionsResponse_httpStatus :: Lens.Lens' ListModelExplainabilityJobDefinitionsResponse Core.Int
listModelExplainabilityJobDefinitionsResponse_httpStatus = Lens.lens (\ListModelExplainabilityJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListModelExplainabilityJobDefinitionsResponse' {} a -> s {httpStatus = a} :: ListModelExplainabilityJobDefinitionsResponse)

-- | A JSON array in which each element is a summary for a explainability
-- bias jobs.
listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries :: Lens.Lens' ListModelExplainabilityJobDefinitionsResponse [MonitoringJobDefinitionSummary]
listModelExplainabilityJobDefinitionsResponse_jobDefinitionSummaries = Lens.lens (\ListModelExplainabilityJobDefinitionsResponse' {jobDefinitionSummaries} -> jobDefinitionSummaries) (\s@ListModelExplainabilityJobDefinitionsResponse' {} a -> s {jobDefinitionSummaries = a} :: ListModelExplainabilityJobDefinitionsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    ListModelExplainabilityJobDefinitionsResponse
