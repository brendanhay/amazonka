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
-- Module      : Network.AWS.SageMaker.ListModelBiasJobDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists model bias jobs definitions that satisfy various filters.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListModelBiasJobDefinitions
  ( -- * Creating a Request
    ListModelBiasJobDefinitions (..),
    newListModelBiasJobDefinitions,

    -- * Request Lenses
    listModelBiasJobDefinitions_sortOrder,
    listModelBiasJobDefinitions_nextToken,
    listModelBiasJobDefinitions_endpointName,
    listModelBiasJobDefinitions_nameContains,
    listModelBiasJobDefinitions_maxResults,
    listModelBiasJobDefinitions_creationTimeBefore,
    listModelBiasJobDefinitions_sortBy,
    listModelBiasJobDefinitions_creationTimeAfter,

    -- * Destructuring the Response
    ListModelBiasJobDefinitionsResponse (..),
    newListModelBiasJobDefinitionsResponse,

    -- * Response Lenses
    listModelBiasJobDefinitionsResponse_nextToken,
    listModelBiasJobDefinitionsResponse_httpStatus,
    listModelBiasJobDefinitionsResponse_jobDefinitionSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListModelBiasJobDefinitions' smart constructor.
data ListModelBiasJobDefinitions = ListModelBiasJobDefinitions'
  { -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | Name of the endpoint to monitor for model bias.
    endpointName :: Core.Maybe Core.Text,
    -- | Filter for model bias jobs whose name contains a specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of model bias jobs to return in the response. The
    -- default value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only model bias jobs created before a specified
    -- time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | Whether to sort results by the @Name@ or @CreationTime@ field. The
    -- default is @CreationTime@.
    sortBy :: Core.Maybe MonitoringJobDefinitionSortKey,
    -- | A filter that returns only model bias jobs created after a specified
    -- time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListModelBiasJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listModelBiasJobDefinitions_sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
--
-- 'nextToken', 'listModelBiasJobDefinitions_nextToken' - The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
--
-- 'endpointName', 'listModelBiasJobDefinitions_endpointName' - Name of the endpoint to monitor for model bias.
--
-- 'nameContains', 'listModelBiasJobDefinitions_nameContains' - Filter for model bias jobs whose name contains a specified string.
--
-- 'maxResults', 'listModelBiasJobDefinitions_maxResults' - The maximum number of model bias jobs to return in the response. The
-- default value is 10.
--
-- 'creationTimeBefore', 'listModelBiasJobDefinitions_creationTimeBefore' - A filter that returns only model bias jobs created before a specified
-- time.
--
-- 'sortBy', 'listModelBiasJobDefinitions_sortBy' - Whether to sort results by the @Name@ or @CreationTime@ field. The
-- default is @CreationTime@.
--
-- 'creationTimeAfter', 'listModelBiasJobDefinitions_creationTimeAfter' - A filter that returns only model bias jobs created after a specified
-- time.
newListModelBiasJobDefinitions ::
  ListModelBiasJobDefinitions
newListModelBiasJobDefinitions =
  ListModelBiasJobDefinitions'
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
listModelBiasJobDefinitions_sortOrder :: Lens.Lens' ListModelBiasJobDefinitions (Core.Maybe SortOrder)
listModelBiasJobDefinitions_sortOrder = Lens.lens (\ListModelBiasJobDefinitions' {sortOrder} -> sortOrder) (\s@ListModelBiasJobDefinitions' {} a -> s {sortOrder = a} :: ListModelBiasJobDefinitions)

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listModelBiasJobDefinitions_nextToken :: Lens.Lens' ListModelBiasJobDefinitions (Core.Maybe Core.Text)
listModelBiasJobDefinitions_nextToken = Lens.lens (\ListModelBiasJobDefinitions' {nextToken} -> nextToken) (\s@ListModelBiasJobDefinitions' {} a -> s {nextToken = a} :: ListModelBiasJobDefinitions)

-- | Name of the endpoint to monitor for model bias.
listModelBiasJobDefinitions_endpointName :: Lens.Lens' ListModelBiasJobDefinitions (Core.Maybe Core.Text)
listModelBiasJobDefinitions_endpointName = Lens.lens (\ListModelBiasJobDefinitions' {endpointName} -> endpointName) (\s@ListModelBiasJobDefinitions' {} a -> s {endpointName = a} :: ListModelBiasJobDefinitions)

-- | Filter for model bias jobs whose name contains a specified string.
listModelBiasJobDefinitions_nameContains :: Lens.Lens' ListModelBiasJobDefinitions (Core.Maybe Core.Text)
listModelBiasJobDefinitions_nameContains = Lens.lens (\ListModelBiasJobDefinitions' {nameContains} -> nameContains) (\s@ListModelBiasJobDefinitions' {} a -> s {nameContains = a} :: ListModelBiasJobDefinitions)

-- | The maximum number of model bias jobs to return in the response. The
-- default value is 10.
listModelBiasJobDefinitions_maxResults :: Lens.Lens' ListModelBiasJobDefinitions (Core.Maybe Core.Natural)
listModelBiasJobDefinitions_maxResults = Lens.lens (\ListModelBiasJobDefinitions' {maxResults} -> maxResults) (\s@ListModelBiasJobDefinitions' {} a -> s {maxResults = a} :: ListModelBiasJobDefinitions)

-- | A filter that returns only model bias jobs created before a specified
-- time.
listModelBiasJobDefinitions_creationTimeBefore :: Lens.Lens' ListModelBiasJobDefinitions (Core.Maybe Core.UTCTime)
listModelBiasJobDefinitions_creationTimeBefore = Lens.lens (\ListModelBiasJobDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelBiasJobDefinitions' {} a -> s {creationTimeBefore = a} :: ListModelBiasJobDefinitions) Core.. Lens.mapping Core._Time

-- | Whether to sort results by the @Name@ or @CreationTime@ field. The
-- default is @CreationTime@.
listModelBiasJobDefinitions_sortBy :: Lens.Lens' ListModelBiasJobDefinitions (Core.Maybe MonitoringJobDefinitionSortKey)
listModelBiasJobDefinitions_sortBy = Lens.lens (\ListModelBiasJobDefinitions' {sortBy} -> sortBy) (\s@ListModelBiasJobDefinitions' {} a -> s {sortBy = a} :: ListModelBiasJobDefinitions)

-- | A filter that returns only model bias jobs created after a specified
-- time.
listModelBiasJobDefinitions_creationTimeAfter :: Lens.Lens' ListModelBiasJobDefinitions (Core.Maybe Core.UTCTime)
listModelBiasJobDefinitions_creationTimeAfter = Lens.lens (\ListModelBiasJobDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelBiasJobDefinitions' {} a -> s {creationTimeAfter = a} :: ListModelBiasJobDefinitions) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListModelBiasJobDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelBiasJobDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelBiasJobDefinitionsResponse_jobDefinitionSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listModelBiasJobDefinitions_nextToken
          Lens..~ rs
          Lens.^? listModelBiasJobDefinitionsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListModelBiasJobDefinitions where
  type
    AWSResponse ListModelBiasJobDefinitions =
      ListModelBiasJobDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelBiasJobDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "JobDefinitionSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListModelBiasJobDefinitions

instance Core.NFData ListModelBiasJobDefinitions

instance Core.ToHeaders ListModelBiasJobDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListModelBiasJobDefinitions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListModelBiasJobDefinitions where
  toJSON ListModelBiasJobDefinitions' {..} =
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

instance Core.ToPath ListModelBiasJobDefinitions where
  toPath = Core.const "/"

instance Core.ToQuery ListModelBiasJobDefinitions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListModelBiasJobDefinitionsResponse' smart constructor.
data ListModelBiasJobDefinitionsResponse = ListModelBiasJobDefinitionsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of jobs, use it in the subsequent request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A JSON array in which each element is a summary for a model bias jobs.
    jobDefinitionSummaries :: [MonitoringJobDefinitionSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListModelBiasJobDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelBiasJobDefinitionsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent request.
--
-- 'httpStatus', 'listModelBiasJobDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionSummaries', 'listModelBiasJobDefinitionsResponse_jobDefinitionSummaries' - A JSON array in which each element is a summary for a model bias jobs.
newListModelBiasJobDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListModelBiasJobDefinitionsResponse
newListModelBiasJobDefinitionsResponse pHttpStatus_ =
  ListModelBiasJobDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      jobDefinitionSummaries = Core.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent request.
listModelBiasJobDefinitionsResponse_nextToken :: Lens.Lens' ListModelBiasJobDefinitionsResponse (Core.Maybe Core.Text)
listModelBiasJobDefinitionsResponse_nextToken = Lens.lens (\ListModelBiasJobDefinitionsResponse' {nextToken} -> nextToken) (\s@ListModelBiasJobDefinitionsResponse' {} a -> s {nextToken = a} :: ListModelBiasJobDefinitionsResponse)

-- | The response's http status code.
listModelBiasJobDefinitionsResponse_httpStatus :: Lens.Lens' ListModelBiasJobDefinitionsResponse Core.Int
listModelBiasJobDefinitionsResponse_httpStatus = Lens.lens (\ListModelBiasJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListModelBiasJobDefinitionsResponse' {} a -> s {httpStatus = a} :: ListModelBiasJobDefinitionsResponse)

-- | A JSON array in which each element is a summary for a model bias jobs.
listModelBiasJobDefinitionsResponse_jobDefinitionSummaries :: Lens.Lens' ListModelBiasJobDefinitionsResponse [MonitoringJobDefinitionSummary]
listModelBiasJobDefinitionsResponse_jobDefinitionSummaries = Lens.lens (\ListModelBiasJobDefinitionsResponse' {jobDefinitionSummaries} -> jobDefinitionSummaries) (\s@ListModelBiasJobDefinitionsResponse' {} a -> s {jobDefinitionSummaries = a} :: ListModelBiasJobDefinitionsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    ListModelBiasJobDefinitionsResponse
