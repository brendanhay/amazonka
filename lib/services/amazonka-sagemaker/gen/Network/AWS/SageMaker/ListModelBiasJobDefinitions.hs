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
    listModelBiasJobDefinitions_nameContains,
    listModelBiasJobDefinitions_endpointName,
    listModelBiasJobDefinitions_creationTimeAfter,
    listModelBiasJobDefinitions_nextToken,
    listModelBiasJobDefinitions_sortOrder,
    listModelBiasJobDefinitions_creationTimeBefore,
    listModelBiasJobDefinitions_maxResults,
    listModelBiasJobDefinitions_sortBy,

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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListModelBiasJobDefinitions' smart constructor.
data ListModelBiasJobDefinitions = ListModelBiasJobDefinitions'
  { -- | Filter for model bias jobs whose name contains a specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | Name of the endpoint to monitor for model bias.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only model bias jobs created after a specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | The token returned if the response is truncated. To retrieve the next
    -- set of job executions, use it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Whether to sort the results in @Ascending@ or @Descending@ order. The
    -- default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only model bias jobs created before a specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of model bias jobs to return in the response. The
    -- default value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Whether to sort results by the @Name@ or @CreationTime@ field. The
    -- default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringJobDefinitionSortKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelBiasJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameContains', 'listModelBiasJobDefinitions_nameContains' - Filter for model bias jobs whose name contains a specified string.
--
-- 'endpointName', 'listModelBiasJobDefinitions_endpointName' - Name of the endpoint to monitor for model bias.
--
-- 'creationTimeAfter', 'listModelBiasJobDefinitions_creationTimeAfter' - A filter that returns only model bias jobs created after a specified
-- time.
--
-- 'nextToken', 'listModelBiasJobDefinitions_nextToken' - The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
--
-- 'sortOrder', 'listModelBiasJobDefinitions_sortOrder' - Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
--
-- 'creationTimeBefore', 'listModelBiasJobDefinitions_creationTimeBefore' - A filter that returns only model bias jobs created before a specified
-- time.
--
-- 'maxResults', 'listModelBiasJobDefinitions_maxResults' - The maximum number of model bias jobs to return in the response. The
-- default value is 10.
--
-- 'sortBy', 'listModelBiasJobDefinitions_sortBy' - Whether to sort results by the @Name@ or @CreationTime@ field. The
-- default is @CreationTime@.
newListModelBiasJobDefinitions ::
  ListModelBiasJobDefinitions
newListModelBiasJobDefinitions =
  ListModelBiasJobDefinitions'
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

-- | Filter for model bias jobs whose name contains a specified string.
listModelBiasJobDefinitions_nameContains :: Lens.Lens' ListModelBiasJobDefinitions (Prelude.Maybe Prelude.Text)
listModelBiasJobDefinitions_nameContains = Lens.lens (\ListModelBiasJobDefinitions' {nameContains} -> nameContains) (\s@ListModelBiasJobDefinitions' {} a -> s {nameContains = a} :: ListModelBiasJobDefinitions)

-- | Name of the endpoint to monitor for model bias.
listModelBiasJobDefinitions_endpointName :: Lens.Lens' ListModelBiasJobDefinitions (Prelude.Maybe Prelude.Text)
listModelBiasJobDefinitions_endpointName = Lens.lens (\ListModelBiasJobDefinitions' {endpointName} -> endpointName) (\s@ListModelBiasJobDefinitions' {} a -> s {endpointName = a} :: ListModelBiasJobDefinitions)

-- | A filter that returns only model bias jobs created after a specified
-- time.
listModelBiasJobDefinitions_creationTimeAfter :: Lens.Lens' ListModelBiasJobDefinitions (Prelude.Maybe Prelude.UTCTime)
listModelBiasJobDefinitions_creationTimeAfter = Lens.lens (\ListModelBiasJobDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelBiasJobDefinitions' {} a -> s {creationTimeAfter = a} :: ListModelBiasJobDefinitions) Prelude.. Lens.mapping Core._Time

-- | The token returned if the response is truncated. To retrieve the next
-- set of job executions, use it in the next request.
listModelBiasJobDefinitions_nextToken :: Lens.Lens' ListModelBiasJobDefinitions (Prelude.Maybe Prelude.Text)
listModelBiasJobDefinitions_nextToken = Lens.lens (\ListModelBiasJobDefinitions' {nextToken} -> nextToken) (\s@ListModelBiasJobDefinitions' {} a -> s {nextToken = a} :: ListModelBiasJobDefinitions)

-- | Whether to sort the results in @Ascending@ or @Descending@ order. The
-- default is @Descending@.
listModelBiasJobDefinitions_sortOrder :: Lens.Lens' ListModelBiasJobDefinitions (Prelude.Maybe SortOrder)
listModelBiasJobDefinitions_sortOrder = Lens.lens (\ListModelBiasJobDefinitions' {sortOrder} -> sortOrder) (\s@ListModelBiasJobDefinitions' {} a -> s {sortOrder = a} :: ListModelBiasJobDefinitions)

-- | A filter that returns only model bias jobs created before a specified
-- time.
listModelBiasJobDefinitions_creationTimeBefore :: Lens.Lens' ListModelBiasJobDefinitions (Prelude.Maybe Prelude.UTCTime)
listModelBiasJobDefinitions_creationTimeBefore = Lens.lens (\ListModelBiasJobDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelBiasJobDefinitions' {} a -> s {creationTimeBefore = a} :: ListModelBiasJobDefinitions) Prelude.. Lens.mapping Core._Time

-- | The maximum number of model bias jobs to return in the response. The
-- default value is 10.
listModelBiasJobDefinitions_maxResults :: Lens.Lens' ListModelBiasJobDefinitions (Prelude.Maybe Prelude.Natural)
listModelBiasJobDefinitions_maxResults = Lens.lens (\ListModelBiasJobDefinitions' {maxResults} -> maxResults) (\s@ListModelBiasJobDefinitions' {} a -> s {maxResults = a} :: ListModelBiasJobDefinitions)

-- | Whether to sort results by the @Name@ or @CreationTime@ field. The
-- default is @CreationTime@.
listModelBiasJobDefinitions_sortBy :: Lens.Lens' ListModelBiasJobDefinitions (Prelude.Maybe MonitoringJobDefinitionSortKey)
listModelBiasJobDefinitions_sortBy = Lens.lens (\ListModelBiasJobDefinitions' {sortBy} -> sortBy) (\s@ListModelBiasJobDefinitions' {} a -> s {sortBy = a} :: ListModelBiasJobDefinitions)

instance Core.AWSPager ListModelBiasJobDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelBiasJobDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelBiasJobDefinitionsResponse_jobDefinitionSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listModelBiasJobDefinitions_nextToken
          Lens..~ rs
          Lens.^? listModelBiasJobDefinitionsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListModelBiasJobDefinitions where
  type
    AWSResponse ListModelBiasJobDefinitions =
      ListModelBiasJobDefinitionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelBiasJobDefinitionsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "JobDefinitionSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListModelBiasJobDefinitions

instance Prelude.NFData ListModelBiasJobDefinitions

instance Core.ToHeaders ListModelBiasJobDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListModelBiasJobDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListModelBiasJobDefinitions where
  toJSON ListModelBiasJobDefinitions' {..} =
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

instance Core.ToPath ListModelBiasJobDefinitions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListModelBiasJobDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelBiasJobDefinitionsResponse' smart constructor.
data ListModelBiasJobDefinitionsResponse = ListModelBiasJobDefinitionsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of jobs, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A JSON array in which each element is a summary for a model bias jobs.
    jobDefinitionSummaries :: [MonitoringJobDefinitionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListModelBiasJobDefinitionsResponse
newListModelBiasJobDefinitionsResponse pHttpStatus_ =
  ListModelBiasJobDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      jobDefinitionSummaries =
        Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of jobs, use it in the subsequent request.
listModelBiasJobDefinitionsResponse_nextToken :: Lens.Lens' ListModelBiasJobDefinitionsResponse (Prelude.Maybe Prelude.Text)
listModelBiasJobDefinitionsResponse_nextToken = Lens.lens (\ListModelBiasJobDefinitionsResponse' {nextToken} -> nextToken) (\s@ListModelBiasJobDefinitionsResponse' {} a -> s {nextToken = a} :: ListModelBiasJobDefinitionsResponse)

-- | The response's http status code.
listModelBiasJobDefinitionsResponse_httpStatus :: Lens.Lens' ListModelBiasJobDefinitionsResponse Prelude.Int
listModelBiasJobDefinitionsResponse_httpStatus = Lens.lens (\ListModelBiasJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListModelBiasJobDefinitionsResponse' {} a -> s {httpStatus = a} :: ListModelBiasJobDefinitionsResponse)

-- | A JSON array in which each element is a summary for a model bias jobs.
listModelBiasJobDefinitionsResponse_jobDefinitionSummaries :: Lens.Lens' ListModelBiasJobDefinitionsResponse [MonitoringJobDefinitionSummary]
listModelBiasJobDefinitionsResponse_jobDefinitionSummaries = Lens.lens (\ListModelBiasJobDefinitionsResponse' {jobDefinitionSummaries} -> jobDefinitionSummaries) (\s@ListModelBiasJobDefinitionsResponse' {} a -> s {jobDefinitionSummaries = a} :: ListModelBiasJobDefinitionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListModelBiasJobDefinitionsResponse
