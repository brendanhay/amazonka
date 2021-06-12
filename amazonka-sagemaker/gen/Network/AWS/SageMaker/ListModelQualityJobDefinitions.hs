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
-- Module      : Network.AWS.SageMaker.ListModelQualityJobDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of model quality monitoring job definitions in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListModelQualityJobDefinitions
  ( -- * Creating a Request
    ListModelQualityJobDefinitions (..),
    newListModelQualityJobDefinitions,

    -- * Request Lenses
    listModelQualityJobDefinitions_sortOrder,
    listModelQualityJobDefinitions_nextToken,
    listModelQualityJobDefinitions_endpointName,
    listModelQualityJobDefinitions_nameContains,
    listModelQualityJobDefinitions_maxResults,
    listModelQualityJobDefinitions_creationTimeBefore,
    listModelQualityJobDefinitions_sortBy,
    listModelQualityJobDefinitions_creationTimeAfter,

    -- * Destructuring the Response
    ListModelQualityJobDefinitionsResponse (..),
    newListModelQualityJobDefinitionsResponse,

    -- * Response Lenses
    listModelQualityJobDefinitionsResponse_nextToken,
    listModelQualityJobDefinitionsResponse_httpStatus,
    listModelQualityJobDefinitionsResponse_jobDefinitionSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListModelQualityJobDefinitions' smart constructor.
data ListModelQualityJobDefinitions = ListModelQualityJobDefinitions'
  { -- | The sort order for results. The default is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the result of the previous @ListModelQualityJobDefinitions@ request
    -- was truncated, the response includes a @NextToken@. To retrieve the next
    -- set of model quality monitoring job definitions, use the token in the
    -- next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that returns only model quality monitoring job definitions that
    -- are associated with the specified endpoint.
    endpointName :: Core.Maybe Core.Text,
    -- | A string in the transform job name. This filter returns only model
    -- quality monitoring job definitions whose name contains the specified
    -- string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of results to return in a call to
    -- @ListModelQualityJobDefinitions@.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only model quality monitoring job definitions
    -- created before the specified time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Core.Maybe MonitoringJobDefinitionSortKey,
    -- | A filter that returns only model quality monitoring job definitions
    -- created after the specified time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListModelQualityJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listModelQualityJobDefinitions_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'nextToken', 'listModelQualityJobDefinitions_nextToken' - If the result of the previous @ListModelQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of model quality monitoring job definitions, use the token in the
-- next request.
--
-- 'endpointName', 'listModelQualityJobDefinitions_endpointName' - A filter that returns only model quality monitoring job definitions that
-- are associated with the specified endpoint.
--
-- 'nameContains', 'listModelQualityJobDefinitions_nameContains' - A string in the transform job name. This filter returns only model
-- quality monitoring job definitions whose name contains the specified
-- string.
--
-- 'maxResults', 'listModelQualityJobDefinitions_maxResults' - The maximum number of results to return in a call to
-- @ListModelQualityJobDefinitions@.
--
-- 'creationTimeBefore', 'listModelQualityJobDefinitions_creationTimeBefore' - A filter that returns only model quality monitoring job definitions
-- created before the specified time.
--
-- 'sortBy', 'listModelQualityJobDefinitions_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'creationTimeAfter', 'listModelQualityJobDefinitions_creationTimeAfter' - A filter that returns only model quality monitoring job definitions
-- created after the specified time.
newListModelQualityJobDefinitions ::
  ListModelQualityJobDefinitions
newListModelQualityJobDefinitions =
  ListModelQualityJobDefinitions'
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

-- | The sort order for results. The default is @Descending@.
listModelQualityJobDefinitions_sortOrder :: Lens.Lens' ListModelQualityJobDefinitions (Core.Maybe SortOrder)
listModelQualityJobDefinitions_sortOrder = Lens.lens (\ListModelQualityJobDefinitions' {sortOrder} -> sortOrder) (\s@ListModelQualityJobDefinitions' {} a -> s {sortOrder = a} :: ListModelQualityJobDefinitions)

-- | If the result of the previous @ListModelQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of model quality monitoring job definitions, use the token in the
-- next request.
listModelQualityJobDefinitions_nextToken :: Lens.Lens' ListModelQualityJobDefinitions (Core.Maybe Core.Text)
listModelQualityJobDefinitions_nextToken = Lens.lens (\ListModelQualityJobDefinitions' {nextToken} -> nextToken) (\s@ListModelQualityJobDefinitions' {} a -> s {nextToken = a} :: ListModelQualityJobDefinitions)

-- | A filter that returns only model quality monitoring job definitions that
-- are associated with the specified endpoint.
listModelQualityJobDefinitions_endpointName :: Lens.Lens' ListModelQualityJobDefinitions (Core.Maybe Core.Text)
listModelQualityJobDefinitions_endpointName = Lens.lens (\ListModelQualityJobDefinitions' {endpointName} -> endpointName) (\s@ListModelQualityJobDefinitions' {} a -> s {endpointName = a} :: ListModelQualityJobDefinitions)

-- | A string in the transform job name. This filter returns only model
-- quality monitoring job definitions whose name contains the specified
-- string.
listModelQualityJobDefinitions_nameContains :: Lens.Lens' ListModelQualityJobDefinitions (Core.Maybe Core.Text)
listModelQualityJobDefinitions_nameContains = Lens.lens (\ListModelQualityJobDefinitions' {nameContains} -> nameContains) (\s@ListModelQualityJobDefinitions' {} a -> s {nameContains = a} :: ListModelQualityJobDefinitions)

-- | The maximum number of results to return in a call to
-- @ListModelQualityJobDefinitions@.
listModelQualityJobDefinitions_maxResults :: Lens.Lens' ListModelQualityJobDefinitions (Core.Maybe Core.Natural)
listModelQualityJobDefinitions_maxResults = Lens.lens (\ListModelQualityJobDefinitions' {maxResults} -> maxResults) (\s@ListModelQualityJobDefinitions' {} a -> s {maxResults = a} :: ListModelQualityJobDefinitions)

-- | A filter that returns only model quality monitoring job definitions
-- created before the specified time.
listModelQualityJobDefinitions_creationTimeBefore :: Lens.Lens' ListModelQualityJobDefinitions (Core.Maybe Core.UTCTime)
listModelQualityJobDefinitions_creationTimeBefore = Lens.lens (\ListModelQualityJobDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelQualityJobDefinitions' {} a -> s {creationTimeBefore = a} :: ListModelQualityJobDefinitions) Core.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @CreationTime@.
listModelQualityJobDefinitions_sortBy :: Lens.Lens' ListModelQualityJobDefinitions (Core.Maybe MonitoringJobDefinitionSortKey)
listModelQualityJobDefinitions_sortBy = Lens.lens (\ListModelQualityJobDefinitions' {sortBy} -> sortBy) (\s@ListModelQualityJobDefinitions' {} a -> s {sortBy = a} :: ListModelQualityJobDefinitions)

-- | A filter that returns only model quality monitoring job definitions
-- created after the specified time.
listModelQualityJobDefinitions_creationTimeAfter :: Lens.Lens' ListModelQualityJobDefinitions (Core.Maybe Core.UTCTime)
listModelQualityJobDefinitions_creationTimeAfter = Lens.lens (\ListModelQualityJobDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelQualityJobDefinitions' {} a -> s {creationTimeAfter = a} :: ListModelQualityJobDefinitions) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListModelQualityJobDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelQualityJobDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelQualityJobDefinitionsResponse_jobDefinitionSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listModelQualityJobDefinitions_nextToken
          Lens..~ rs
          Lens.^? listModelQualityJobDefinitionsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListModelQualityJobDefinitions
  where
  type
    AWSResponse ListModelQualityJobDefinitions =
      ListModelQualityJobDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelQualityJobDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "JobDefinitionSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListModelQualityJobDefinitions

instance Core.NFData ListModelQualityJobDefinitions

instance
  Core.ToHeaders
    ListModelQualityJobDefinitions
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListModelQualityJobDefinitions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListModelQualityJobDefinitions where
  toJSON ListModelQualityJobDefinitions' {..} =
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

instance Core.ToPath ListModelQualityJobDefinitions where
  toPath = Core.const "/"

instance Core.ToQuery ListModelQualityJobDefinitions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListModelQualityJobDefinitionsResponse' smart constructor.
data ListModelQualityJobDefinitionsResponse = ListModelQualityJobDefinitionsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of model quality monitoring job definitions, use
    -- it in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of summaries of model quality monitoring job definitions.
    jobDefinitionSummaries :: [MonitoringJobDefinitionSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListModelQualityJobDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelQualityJobDefinitionsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of model quality monitoring job definitions, use
-- it in the next request.
--
-- 'httpStatus', 'listModelQualityJobDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionSummaries', 'listModelQualityJobDefinitionsResponse_jobDefinitionSummaries' - A list of summaries of model quality monitoring job definitions.
newListModelQualityJobDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListModelQualityJobDefinitionsResponse
newListModelQualityJobDefinitionsResponse
  pHttpStatus_ =
    ListModelQualityJobDefinitionsResponse'
      { nextToken =
          Core.Nothing,
        httpStatus = pHttpStatus_,
        jobDefinitionSummaries =
          Core.mempty
      }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of model quality monitoring job definitions, use
-- it in the next request.
listModelQualityJobDefinitionsResponse_nextToken :: Lens.Lens' ListModelQualityJobDefinitionsResponse (Core.Maybe Core.Text)
listModelQualityJobDefinitionsResponse_nextToken = Lens.lens (\ListModelQualityJobDefinitionsResponse' {nextToken} -> nextToken) (\s@ListModelQualityJobDefinitionsResponse' {} a -> s {nextToken = a} :: ListModelQualityJobDefinitionsResponse)

-- | The response's http status code.
listModelQualityJobDefinitionsResponse_httpStatus :: Lens.Lens' ListModelQualityJobDefinitionsResponse Core.Int
listModelQualityJobDefinitionsResponse_httpStatus = Lens.lens (\ListModelQualityJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListModelQualityJobDefinitionsResponse' {} a -> s {httpStatus = a} :: ListModelQualityJobDefinitionsResponse)

-- | A list of summaries of model quality monitoring job definitions.
listModelQualityJobDefinitionsResponse_jobDefinitionSummaries :: Lens.Lens' ListModelQualityJobDefinitionsResponse [MonitoringJobDefinitionSummary]
listModelQualityJobDefinitionsResponse_jobDefinitionSummaries = Lens.lens (\ListModelQualityJobDefinitionsResponse' {jobDefinitionSummaries} -> jobDefinitionSummaries) (\s@ListModelQualityJobDefinitionsResponse' {} a -> s {jobDefinitionSummaries = a} :: ListModelQualityJobDefinitionsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    ListModelQualityJobDefinitionsResponse
