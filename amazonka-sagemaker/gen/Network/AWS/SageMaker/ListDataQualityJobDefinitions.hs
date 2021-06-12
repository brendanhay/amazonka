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
-- Module      : Network.AWS.SageMaker.ListDataQualityJobDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the data quality job definitions in your account.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListDataQualityJobDefinitions
  ( -- * Creating a Request
    ListDataQualityJobDefinitions (..),
    newListDataQualityJobDefinitions,

    -- * Request Lenses
    listDataQualityJobDefinitions_sortOrder,
    listDataQualityJobDefinitions_nextToken,
    listDataQualityJobDefinitions_endpointName,
    listDataQualityJobDefinitions_nameContains,
    listDataQualityJobDefinitions_maxResults,
    listDataQualityJobDefinitions_creationTimeBefore,
    listDataQualityJobDefinitions_sortBy,
    listDataQualityJobDefinitions_creationTimeAfter,

    -- * Destructuring the Response
    ListDataQualityJobDefinitionsResponse (..),
    newListDataQualityJobDefinitionsResponse,

    -- * Response Lenses
    listDataQualityJobDefinitionsResponse_nextToken,
    listDataQualityJobDefinitionsResponse_httpStatus,
    listDataQualityJobDefinitionsResponse_jobDefinitionSummaries,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListDataQualityJobDefinitions' smart constructor.
data ListDataQualityJobDefinitions = ListDataQualityJobDefinitions'
  { -- | The sort order for results. The default is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the result of the previous @ListDataQualityJobDefinitions@ request
    -- was truncated, the response includes a @NextToken@. To retrieve the next
    -- set of transform jobs, use the token in the next request.>
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that lists the data quality job definitions associated with the
    -- specified endpoint.
    endpointName :: Core.Maybe Core.Text,
    -- | A string in the data quality monitoring job definition name. This filter
    -- returns only data quality monitoring job definitions whose name contains
    -- the specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of data quality monitoring job definitions to return
    -- in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only data quality monitoring job definitions
    -- created before the specified time.
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Core.Maybe MonitoringJobDefinitionSortKey,
    -- | A filter that returns only data quality monitoring job definitions
    -- created after the specified time.
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDataQualityJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listDataQualityJobDefinitions_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'nextToken', 'listDataQualityJobDefinitions_nextToken' - If the result of the previous @ListDataQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of transform jobs, use the token in the next request.>
--
-- 'endpointName', 'listDataQualityJobDefinitions_endpointName' - A filter that lists the data quality job definitions associated with the
-- specified endpoint.
--
-- 'nameContains', 'listDataQualityJobDefinitions_nameContains' - A string in the data quality monitoring job definition name. This filter
-- returns only data quality monitoring job definitions whose name contains
-- the specified string.
--
-- 'maxResults', 'listDataQualityJobDefinitions_maxResults' - The maximum number of data quality monitoring job definitions to return
-- in the response.
--
-- 'creationTimeBefore', 'listDataQualityJobDefinitions_creationTimeBefore' - A filter that returns only data quality monitoring job definitions
-- created before the specified time.
--
-- 'sortBy', 'listDataQualityJobDefinitions_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'creationTimeAfter', 'listDataQualityJobDefinitions_creationTimeAfter' - A filter that returns only data quality monitoring job definitions
-- created after the specified time.
newListDataQualityJobDefinitions ::
  ListDataQualityJobDefinitions
newListDataQualityJobDefinitions =
  ListDataQualityJobDefinitions'
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
listDataQualityJobDefinitions_sortOrder :: Lens.Lens' ListDataQualityJobDefinitions (Core.Maybe SortOrder)
listDataQualityJobDefinitions_sortOrder = Lens.lens (\ListDataQualityJobDefinitions' {sortOrder} -> sortOrder) (\s@ListDataQualityJobDefinitions' {} a -> s {sortOrder = a} :: ListDataQualityJobDefinitions)

-- | If the result of the previous @ListDataQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of transform jobs, use the token in the next request.>
listDataQualityJobDefinitions_nextToken :: Lens.Lens' ListDataQualityJobDefinitions (Core.Maybe Core.Text)
listDataQualityJobDefinitions_nextToken = Lens.lens (\ListDataQualityJobDefinitions' {nextToken} -> nextToken) (\s@ListDataQualityJobDefinitions' {} a -> s {nextToken = a} :: ListDataQualityJobDefinitions)

-- | A filter that lists the data quality job definitions associated with the
-- specified endpoint.
listDataQualityJobDefinitions_endpointName :: Lens.Lens' ListDataQualityJobDefinitions (Core.Maybe Core.Text)
listDataQualityJobDefinitions_endpointName = Lens.lens (\ListDataQualityJobDefinitions' {endpointName} -> endpointName) (\s@ListDataQualityJobDefinitions' {} a -> s {endpointName = a} :: ListDataQualityJobDefinitions)

-- | A string in the data quality monitoring job definition name. This filter
-- returns only data quality monitoring job definitions whose name contains
-- the specified string.
listDataQualityJobDefinitions_nameContains :: Lens.Lens' ListDataQualityJobDefinitions (Core.Maybe Core.Text)
listDataQualityJobDefinitions_nameContains = Lens.lens (\ListDataQualityJobDefinitions' {nameContains} -> nameContains) (\s@ListDataQualityJobDefinitions' {} a -> s {nameContains = a} :: ListDataQualityJobDefinitions)

-- | The maximum number of data quality monitoring job definitions to return
-- in the response.
listDataQualityJobDefinitions_maxResults :: Lens.Lens' ListDataQualityJobDefinitions (Core.Maybe Core.Natural)
listDataQualityJobDefinitions_maxResults = Lens.lens (\ListDataQualityJobDefinitions' {maxResults} -> maxResults) (\s@ListDataQualityJobDefinitions' {} a -> s {maxResults = a} :: ListDataQualityJobDefinitions)

-- | A filter that returns only data quality monitoring job definitions
-- created before the specified time.
listDataQualityJobDefinitions_creationTimeBefore :: Lens.Lens' ListDataQualityJobDefinitions (Core.Maybe Core.UTCTime)
listDataQualityJobDefinitions_creationTimeBefore = Lens.lens (\ListDataQualityJobDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListDataQualityJobDefinitions' {} a -> s {creationTimeBefore = a} :: ListDataQualityJobDefinitions) Core.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @CreationTime@.
listDataQualityJobDefinitions_sortBy :: Lens.Lens' ListDataQualityJobDefinitions (Core.Maybe MonitoringJobDefinitionSortKey)
listDataQualityJobDefinitions_sortBy = Lens.lens (\ListDataQualityJobDefinitions' {sortBy} -> sortBy) (\s@ListDataQualityJobDefinitions' {} a -> s {sortBy = a} :: ListDataQualityJobDefinitions)

-- | A filter that returns only data quality monitoring job definitions
-- created after the specified time.
listDataQualityJobDefinitions_creationTimeAfter :: Lens.Lens' ListDataQualityJobDefinitions (Core.Maybe Core.UTCTime)
listDataQualityJobDefinitions_creationTimeAfter = Lens.lens (\ListDataQualityJobDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListDataQualityJobDefinitions' {} a -> s {creationTimeAfter = a} :: ListDataQualityJobDefinitions) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListDataQualityJobDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataQualityJobDefinitionsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listDataQualityJobDefinitionsResponse_jobDefinitionSummaries
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listDataQualityJobDefinitions_nextToken
          Lens..~ rs
          Lens.^? listDataQualityJobDefinitionsResponse_nextToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    ListDataQualityJobDefinitions
  where
  type
    AWSResponse ListDataQualityJobDefinitions =
      ListDataQualityJobDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListDataQualityJobDefinitionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> ( x Core..?> "JobDefinitionSummaries"
                         Core..!@ Core.mempty
                     )
      )

instance Core.Hashable ListDataQualityJobDefinitions

instance Core.NFData ListDataQualityJobDefinitions

instance Core.ToHeaders ListDataQualityJobDefinitions where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListDataQualityJobDefinitions" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListDataQualityJobDefinitions where
  toJSON ListDataQualityJobDefinitions' {..} =
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

instance Core.ToPath ListDataQualityJobDefinitions where
  toPath = Core.const "/"

instance Core.ToQuery ListDataQualityJobDefinitions where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListDataQualityJobDefinitionsResponse' smart constructor.
data ListDataQualityJobDefinitionsResponse = ListDataQualityJobDefinitionsResponse'
  { -- | If the result of the previous @ListDataQualityJobDefinitions@ request
    -- was truncated, the response includes a @NextToken@. To retrieve the next
    -- set of data quality monitoring job definitions, use the token in the
    -- next request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | A list of data quality monitoring job definitions.
    jobDefinitionSummaries :: [MonitoringJobDefinitionSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListDataQualityJobDefinitionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listDataQualityJobDefinitionsResponse_nextToken' - If the result of the previous @ListDataQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of data quality monitoring job definitions, use the token in the
-- next request.
--
-- 'httpStatus', 'listDataQualityJobDefinitionsResponse_httpStatus' - The response's http status code.
--
-- 'jobDefinitionSummaries', 'listDataQualityJobDefinitionsResponse_jobDefinitionSummaries' - A list of data quality monitoring job definitions.
newListDataQualityJobDefinitionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListDataQualityJobDefinitionsResponse
newListDataQualityJobDefinitionsResponse pHttpStatus_ =
  ListDataQualityJobDefinitionsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      jobDefinitionSummaries = Core.mempty
    }

-- | If the result of the previous @ListDataQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of data quality monitoring job definitions, use the token in the
-- next request.
listDataQualityJobDefinitionsResponse_nextToken :: Lens.Lens' ListDataQualityJobDefinitionsResponse (Core.Maybe Core.Text)
listDataQualityJobDefinitionsResponse_nextToken = Lens.lens (\ListDataQualityJobDefinitionsResponse' {nextToken} -> nextToken) (\s@ListDataQualityJobDefinitionsResponse' {} a -> s {nextToken = a} :: ListDataQualityJobDefinitionsResponse)

-- | The response's http status code.
listDataQualityJobDefinitionsResponse_httpStatus :: Lens.Lens' ListDataQualityJobDefinitionsResponse Core.Int
listDataQualityJobDefinitionsResponse_httpStatus = Lens.lens (\ListDataQualityJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListDataQualityJobDefinitionsResponse' {} a -> s {httpStatus = a} :: ListDataQualityJobDefinitionsResponse)

-- | A list of data quality monitoring job definitions.
listDataQualityJobDefinitionsResponse_jobDefinitionSummaries :: Lens.Lens' ListDataQualityJobDefinitionsResponse [MonitoringJobDefinitionSummary]
listDataQualityJobDefinitionsResponse_jobDefinitionSummaries = Lens.lens (\ListDataQualityJobDefinitionsResponse' {jobDefinitionSummaries} -> jobDefinitionSummaries) (\s@ListDataQualityJobDefinitionsResponse' {} a -> s {jobDefinitionSummaries = a} :: ListDataQualityJobDefinitionsResponse) Core.. Lens._Coerce

instance
  Core.NFData
    ListDataQualityJobDefinitionsResponse
