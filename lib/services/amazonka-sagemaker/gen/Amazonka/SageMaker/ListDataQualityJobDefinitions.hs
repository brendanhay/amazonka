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
-- Module      : Amazonka.SageMaker.ListDataQualityJobDefinitions
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the data quality job definitions in your account.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListDataQualityJobDefinitions
  ( -- * Creating a Request
    ListDataQualityJobDefinitions (..),
    newListDataQualityJobDefinitions,

    -- * Request Lenses
    listDataQualityJobDefinitions_nameContains,
    listDataQualityJobDefinitions_endpointName,
    listDataQualityJobDefinitions_creationTimeAfter,
    listDataQualityJobDefinitions_nextToken,
    listDataQualityJobDefinitions_sortOrder,
    listDataQualityJobDefinitions_creationTimeBefore,
    listDataQualityJobDefinitions_maxResults,
    listDataQualityJobDefinitions_sortBy,

    -- * Destructuring the Response
    ListDataQualityJobDefinitionsResponse (..),
    newListDataQualityJobDefinitionsResponse,

    -- * Response Lenses
    listDataQualityJobDefinitionsResponse_nextToken,
    listDataQualityJobDefinitionsResponse_httpStatus,
    listDataQualityJobDefinitionsResponse_jobDefinitionSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListDataQualityJobDefinitions' smart constructor.
data ListDataQualityJobDefinitions = ListDataQualityJobDefinitions'
  { -- | A string in the data quality monitoring job definition name. This filter
    -- returns only data quality monitoring job definitions whose name contains
    -- the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that lists the data quality job definitions associated with the
    -- specified endpoint.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only data quality monitoring job definitions
    -- created after the specified time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | If the result of the previous @ListDataQualityJobDefinitions@ request
    -- was truncated, the response includes a @NextToken@. To retrieve the next
    -- set of transform jobs, use the token in the next request.>
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order for results. The default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only data quality monitoring job definitions
    -- created before the specified time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of data quality monitoring job definitions to return
    -- in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringJobDefinitionSortKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListDataQualityJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameContains', 'listDataQualityJobDefinitions_nameContains' - A string in the data quality monitoring job definition name. This filter
-- returns only data quality monitoring job definitions whose name contains
-- the specified string.
--
-- 'endpointName', 'listDataQualityJobDefinitions_endpointName' - A filter that lists the data quality job definitions associated with the
-- specified endpoint.
--
-- 'creationTimeAfter', 'listDataQualityJobDefinitions_creationTimeAfter' - A filter that returns only data quality monitoring job definitions
-- created after the specified time.
--
-- 'nextToken', 'listDataQualityJobDefinitions_nextToken' - If the result of the previous @ListDataQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of transform jobs, use the token in the next request.>
--
-- 'sortOrder', 'listDataQualityJobDefinitions_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'creationTimeBefore', 'listDataQualityJobDefinitions_creationTimeBefore' - A filter that returns only data quality monitoring job definitions
-- created before the specified time.
--
-- 'maxResults', 'listDataQualityJobDefinitions_maxResults' - The maximum number of data quality monitoring job definitions to return
-- in the response.
--
-- 'sortBy', 'listDataQualityJobDefinitions_sortBy' - The field to sort results by. The default is @CreationTime@.
newListDataQualityJobDefinitions ::
  ListDataQualityJobDefinitions
newListDataQualityJobDefinitions =
  ListDataQualityJobDefinitions'
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

-- | A string in the data quality monitoring job definition name. This filter
-- returns only data quality monitoring job definitions whose name contains
-- the specified string.
listDataQualityJobDefinitions_nameContains :: Lens.Lens' ListDataQualityJobDefinitions (Prelude.Maybe Prelude.Text)
listDataQualityJobDefinitions_nameContains = Lens.lens (\ListDataQualityJobDefinitions' {nameContains} -> nameContains) (\s@ListDataQualityJobDefinitions' {} a -> s {nameContains = a} :: ListDataQualityJobDefinitions)

-- | A filter that lists the data quality job definitions associated with the
-- specified endpoint.
listDataQualityJobDefinitions_endpointName :: Lens.Lens' ListDataQualityJobDefinitions (Prelude.Maybe Prelude.Text)
listDataQualityJobDefinitions_endpointName = Lens.lens (\ListDataQualityJobDefinitions' {endpointName} -> endpointName) (\s@ListDataQualityJobDefinitions' {} a -> s {endpointName = a} :: ListDataQualityJobDefinitions)

-- | A filter that returns only data quality monitoring job definitions
-- created after the specified time.
listDataQualityJobDefinitions_creationTimeAfter :: Lens.Lens' ListDataQualityJobDefinitions (Prelude.Maybe Prelude.UTCTime)
listDataQualityJobDefinitions_creationTimeAfter = Lens.lens (\ListDataQualityJobDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListDataQualityJobDefinitions' {} a -> s {creationTimeAfter = a} :: ListDataQualityJobDefinitions) Prelude.. Lens.mapping Core._Time

-- | If the result of the previous @ListDataQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of transform jobs, use the token in the next request.>
listDataQualityJobDefinitions_nextToken :: Lens.Lens' ListDataQualityJobDefinitions (Prelude.Maybe Prelude.Text)
listDataQualityJobDefinitions_nextToken = Lens.lens (\ListDataQualityJobDefinitions' {nextToken} -> nextToken) (\s@ListDataQualityJobDefinitions' {} a -> s {nextToken = a} :: ListDataQualityJobDefinitions)

-- | The sort order for results. The default is @Descending@.
listDataQualityJobDefinitions_sortOrder :: Lens.Lens' ListDataQualityJobDefinitions (Prelude.Maybe SortOrder)
listDataQualityJobDefinitions_sortOrder = Lens.lens (\ListDataQualityJobDefinitions' {sortOrder} -> sortOrder) (\s@ListDataQualityJobDefinitions' {} a -> s {sortOrder = a} :: ListDataQualityJobDefinitions)

-- | A filter that returns only data quality monitoring job definitions
-- created before the specified time.
listDataQualityJobDefinitions_creationTimeBefore :: Lens.Lens' ListDataQualityJobDefinitions (Prelude.Maybe Prelude.UTCTime)
listDataQualityJobDefinitions_creationTimeBefore = Lens.lens (\ListDataQualityJobDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListDataQualityJobDefinitions' {} a -> s {creationTimeBefore = a} :: ListDataQualityJobDefinitions) Prelude.. Lens.mapping Core._Time

-- | The maximum number of data quality monitoring job definitions to return
-- in the response.
listDataQualityJobDefinitions_maxResults :: Lens.Lens' ListDataQualityJobDefinitions (Prelude.Maybe Prelude.Natural)
listDataQualityJobDefinitions_maxResults = Lens.lens (\ListDataQualityJobDefinitions' {maxResults} -> maxResults) (\s@ListDataQualityJobDefinitions' {} a -> s {maxResults = a} :: ListDataQualityJobDefinitions)

-- | The field to sort results by. The default is @CreationTime@.
listDataQualityJobDefinitions_sortBy :: Lens.Lens' ListDataQualityJobDefinitions (Prelude.Maybe MonitoringJobDefinitionSortKey)
listDataQualityJobDefinitions_sortBy = Lens.lens (\ListDataQualityJobDefinitions' {sortBy} -> sortBy) (\s@ListDataQualityJobDefinitions' {} a -> s {sortBy = a} :: ListDataQualityJobDefinitions)

instance Core.AWSPager ListDataQualityJobDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listDataQualityJobDefinitionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listDataQualityJobDefinitionsResponse_jobDefinitionSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listDataQualityJobDefinitions_nextToken
          Lens..~ rs
          Lens.^? listDataQualityJobDefinitionsResponse_nextToken
            Prelude.. Lens._Just

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
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "JobDefinitionSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListDataQualityJobDefinitions

instance Prelude.NFData ListDataQualityJobDefinitions

instance Core.ToHeaders ListDataQualityJobDefinitions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListDataQualityJobDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListDataQualityJobDefinitions where
  toJSON ListDataQualityJobDefinitions' {..} =
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

instance Core.ToPath ListDataQualityJobDefinitions where
  toPath = Prelude.const "/"

instance Core.ToQuery ListDataQualityJobDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListDataQualityJobDefinitionsResponse' smart constructor.
data ListDataQualityJobDefinitionsResponse = ListDataQualityJobDefinitionsResponse'
  { -- | If the result of the previous @ListDataQualityJobDefinitions@ request
    -- was truncated, the response includes a @NextToken@. To retrieve the next
    -- set of data quality monitoring job definitions, use the token in the
    -- next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of data quality monitoring job definitions.
    jobDefinitionSummaries :: [MonitoringJobDefinitionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListDataQualityJobDefinitionsResponse
newListDataQualityJobDefinitionsResponse pHttpStatus_ =
  ListDataQualityJobDefinitionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      jobDefinitionSummaries =
        Prelude.mempty
    }

-- | If the result of the previous @ListDataQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of data quality monitoring job definitions, use the token in the
-- next request.
listDataQualityJobDefinitionsResponse_nextToken :: Lens.Lens' ListDataQualityJobDefinitionsResponse (Prelude.Maybe Prelude.Text)
listDataQualityJobDefinitionsResponse_nextToken = Lens.lens (\ListDataQualityJobDefinitionsResponse' {nextToken} -> nextToken) (\s@ListDataQualityJobDefinitionsResponse' {} a -> s {nextToken = a} :: ListDataQualityJobDefinitionsResponse)

-- | The response's http status code.
listDataQualityJobDefinitionsResponse_httpStatus :: Lens.Lens' ListDataQualityJobDefinitionsResponse Prelude.Int
listDataQualityJobDefinitionsResponse_httpStatus = Lens.lens (\ListDataQualityJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListDataQualityJobDefinitionsResponse' {} a -> s {httpStatus = a} :: ListDataQualityJobDefinitionsResponse)

-- | A list of data quality monitoring job definitions.
listDataQualityJobDefinitionsResponse_jobDefinitionSummaries :: Lens.Lens' ListDataQualityJobDefinitionsResponse [MonitoringJobDefinitionSummary]
listDataQualityJobDefinitionsResponse_jobDefinitionSummaries = Lens.lens (\ListDataQualityJobDefinitionsResponse' {jobDefinitionSummaries} -> jobDefinitionSummaries) (\s@ListDataQualityJobDefinitionsResponse' {} a -> s {jobDefinitionSummaries = a} :: ListDataQualityJobDefinitionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListDataQualityJobDefinitionsResponse
