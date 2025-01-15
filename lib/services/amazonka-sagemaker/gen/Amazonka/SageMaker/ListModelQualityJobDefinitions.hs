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
-- Module      : Amazonka.SageMaker.ListModelQualityJobDefinitions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of model quality monitoring job definitions in your account.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListModelQualityJobDefinitions
  ( -- * Creating a Request
    ListModelQualityJobDefinitions (..),
    newListModelQualityJobDefinitions,

    -- * Request Lenses
    listModelQualityJobDefinitions_creationTimeAfter,
    listModelQualityJobDefinitions_creationTimeBefore,
    listModelQualityJobDefinitions_endpointName,
    listModelQualityJobDefinitions_maxResults,
    listModelQualityJobDefinitions_nameContains,
    listModelQualityJobDefinitions_nextToken,
    listModelQualityJobDefinitions_sortBy,
    listModelQualityJobDefinitions_sortOrder,

    -- * Destructuring the Response
    ListModelQualityJobDefinitionsResponse (..),
    newListModelQualityJobDefinitionsResponse,

    -- * Response Lenses
    listModelQualityJobDefinitionsResponse_nextToken,
    listModelQualityJobDefinitionsResponse_httpStatus,
    listModelQualityJobDefinitionsResponse_jobDefinitionSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListModelQualityJobDefinitions' smart constructor.
data ListModelQualityJobDefinitions = ListModelQualityJobDefinitions'
  { -- | A filter that returns only model quality monitoring job definitions
    -- created after the specified time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only model quality monitoring job definitions
    -- created before the specified time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A filter that returns only model quality monitoring job definitions that
    -- are associated with the specified endpoint.
    endpointName :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of results to return in a call to
    -- @ListModelQualityJobDefinitions@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A string in the transform job name. This filter returns only model
    -- quality monitoring job definitions whose name contains the specified
    -- string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the result of the previous @ListModelQualityJobDefinitions@ request
    -- was truncated, the response includes a @NextToken@. To retrieve the next
    -- set of model quality monitoring job definitions, use the token in the
    -- next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe MonitoringJobDefinitionSortKey,
    -- | The sort order for results. The default is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelQualityJobDefinitions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listModelQualityJobDefinitions_creationTimeAfter' - A filter that returns only model quality monitoring job definitions
-- created after the specified time.
--
-- 'creationTimeBefore', 'listModelQualityJobDefinitions_creationTimeBefore' - A filter that returns only model quality monitoring job definitions
-- created before the specified time.
--
-- 'endpointName', 'listModelQualityJobDefinitions_endpointName' - A filter that returns only model quality monitoring job definitions that
-- are associated with the specified endpoint.
--
-- 'maxResults', 'listModelQualityJobDefinitions_maxResults' - The maximum number of results to return in a call to
-- @ListModelQualityJobDefinitions@.
--
-- 'nameContains', 'listModelQualityJobDefinitions_nameContains' - A string in the transform job name. This filter returns only model
-- quality monitoring job definitions whose name contains the specified
-- string.
--
-- 'nextToken', 'listModelQualityJobDefinitions_nextToken' - If the result of the previous @ListModelQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of model quality monitoring job definitions, use the token in the
-- next request.
--
-- 'sortBy', 'listModelQualityJobDefinitions_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'sortOrder', 'listModelQualityJobDefinitions_sortOrder' - The sort order for results. The default is @Descending@.
newListModelQualityJobDefinitions ::
  ListModelQualityJobDefinitions
newListModelQualityJobDefinitions =
  ListModelQualityJobDefinitions'
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

-- | A filter that returns only model quality monitoring job definitions
-- created after the specified time.
listModelQualityJobDefinitions_creationTimeAfter :: Lens.Lens' ListModelQualityJobDefinitions (Prelude.Maybe Prelude.UTCTime)
listModelQualityJobDefinitions_creationTimeAfter = Lens.lens (\ListModelQualityJobDefinitions' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelQualityJobDefinitions' {} a -> s {creationTimeAfter = a} :: ListModelQualityJobDefinitions) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only model quality monitoring job definitions
-- created before the specified time.
listModelQualityJobDefinitions_creationTimeBefore :: Lens.Lens' ListModelQualityJobDefinitions (Prelude.Maybe Prelude.UTCTime)
listModelQualityJobDefinitions_creationTimeBefore = Lens.lens (\ListModelQualityJobDefinitions' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelQualityJobDefinitions' {} a -> s {creationTimeBefore = a} :: ListModelQualityJobDefinitions) Prelude.. Lens.mapping Data._Time

-- | A filter that returns only model quality monitoring job definitions that
-- are associated with the specified endpoint.
listModelQualityJobDefinitions_endpointName :: Lens.Lens' ListModelQualityJobDefinitions (Prelude.Maybe Prelude.Text)
listModelQualityJobDefinitions_endpointName = Lens.lens (\ListModelQualityJobDefinitions' {endpointName} -> endpointName) (\s@ListModelQualityJobDefinitions' {} a -> s {endpointName = a} :: ListModelQualityJobDefinitions)

-- | The maximum number of results to return in a call to
-- @ListModelQualityJobDefinitions@.
listModelQualityJobDefinitions_maxResults :: Lens.Lens' ListModelQualityJobDefinitions (Prelude.Maybe Prelude.Natural)
listModelQualityJobDefinitions_maxResults = Lens.lens (\ListModelQualityJobDefinitions' {maxResults} -> maxResults) (\s@ListModelQualityJobDefinitions' {} a -> s {maxResults = a} :: ListModelQualityJobDefinitions)

-- | A string in the transform job name. This filter returns only model
-- quality monitoring job definitions whose name contains the specified
-- string.
listModelQualityJobDefinitions_nameContains :: Lens.Lens' ListModelQualityJobDefinitions (Prelude.Maybe Prelude.Text)
listModelQualityJobDefinitions_nameContains = Lens.lens (\ListModelQualityJobDefinitions' {nameContains} -> nameContains) (\s@ListModelQualityJobDefinitions' {} a -> s {nameContains = a} :: ListModelQualityJobDefinitions)

-- | If the result of the previous @ListModelQualityJobDefinitions@ request
-- was truncated, the response includes a @NextToken@. To retrieve the next
-- set of model quality monitoring job definitions, use the token in the
-- next request.
listModelQualityJobDefinitions_nextToken :: Lens.Lens' ListModelQualityJobDefinitions (Prelude.Maybe Prelude.Text)
listModelQualityJobDefinitions_nextToken = Lens.lens (\ListModelQualityJobDefinitions' {nextToken} -> nextToken) (\s@ListModelQualityJobDefinitions' {} a -> s {nextToken = a} :: ListModelQualityJobDefinitions)

-- | The field to sort results by. The default is @CreationTime@.
listModelQualityJobDefinitions_sortBy :: Lens.Lens' ListModelQualityJobDefinitions (Prelude.Maybe MonitoringJobDefinitionSortKey)
listModelQualityJobDefinitions_sortBy = Lens.lens (\ListModelQualityJobDefinitions' {sortBy} -> sortBy) (\s@ListModelQualityJobDefinitions' {} a -> s {sortBy = a} :: ListModelQualityJobDefinitions)

-- | The sort order for results. The default is @Descending@.
listModelQualityJobDefinitions_sortOrder :: Lens.Lens' ListModelQualityJobDefinitions (Prelude.Maybe SortOrder)
listModelQualityJobDefinitions_sortOrder = Lens.lens (\ListModelQualityJobDefinitions' {sortOrder} -> sortOrder) (\s@ListModelQualityJobDefinitions' {} a -> s {sortOrder = a} :: ListModelQualityJobDefinitions)

instance Core.AWSPager ListModelQualityJobDefinitions where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelQualityJobDefinitionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelQualityJobDefinitionsResponse_jobDefinitionSummaries
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just Prelude.$
          rq
            Prelude.& listModelQualityJobDefinitions_nextToken
              Lens..~ rs
              Lens.^? listModelQualityJobDefinitionsResponse_nextToken
              Prelude.. Lens._Just

instance
  Core.AWSRequest
    ListModelQualityJobDefinitions
  where
  type
    AWSResponse ListModelQualityJobDefinitions =
      ListModelQualityJobDefinitionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelQualityJobDefinitionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "JobDefinitionSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListModelQualityJobDefinitions
  where
  hashWithSalt
    _salt
    ListModelQualityJobDefinitions' {..} =
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
    ListModelQualityJobDefinitions
  where
  rnf ListModelQualityJobDefinitions' {..} =
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
    ListModelQualityJobDefinitions
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListModelQualityJobDefinitions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListModelQualityJobDefinitions where
  toJSON ListModelQualityJobDefinitions' {..} =
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

instance Data.ToPath ListModelQualityJobDefinitions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListModelQualityJobDefinitions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelQualityJobDefinitionsResponse' smart constructor.
data ListModelQualityJobDefinitionsResponse = ListModelQualityJobDefinitionsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of model quality monitoring job definitions, use
    -- it in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of summaries of model quality monitoring job definitions.
    jobDefinitionSummaries :: [MonitoringJobDefinitionSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListModelQualityJobDefinitionsResponse
newListModelQualityJobDefinitionsResponse
  pHttpStatus_ =
    ListModelQualityJobDefinitionsResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        jobDefinitionSummaries =
          Prelude.mempty
      }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of model quality monitoring job definitions, use
-- it in the next request.
listModelQualityJobDefinitionsResponse_nextToken :: Lens.Lens' ListModelQualityJobDefinitionsResponse (Prelude.Maybe Prelude.Text)
listModelQualityJobDefinitionsResponse_nextToken = Lens.lens (\ListModelQualityJobDefinitionsResponse' {nextToken} -> nextToken) (\s@ListModelQualityJobDefinitionsResponse' {} a -> s {nextToken = a} :: ListModelQualityJobDefinitionsResponse)

-- | The response's http status code.
listModelQualityJobDefinitionsResponse_httpStatus :: Lens.Lens' ListModelQualityJobDefinitionsResponse Prelude.Int
listModelQualityJobDefinitionsResponse_httpStatus = Lens.lens (\ListModelQualityJobDefinitionsResponse' {httpStatus} -> httpStatus) (\s@ListModelQualityJobDefinitionsResponse' {} a -> s {httpStatus = a} :: ListModelQualityJobDefinitionsResponse)

-- | A list of summaries of model quality monitoring job definitions.
listModelQualityJobDefinitionsResponse_jobDefinitionSummaries :: Lens.Lens' ListModelQualityJobDefinitionsResponse [MonitoringJobDefinitionSummary]
listModelQualityJobDefinitionsResponse_jobDefinitionSummaries = Lens.lens (\ListModelQualityJobDefinitionsResponse' {jobDefinitionSummaries} -> jobDefinitionSummaries) (\s@ListModelQualityJobDefinitionsResponse' {} a -> s {jobDefinitionSummaries = a} :: ListModelQualityJobDefinitionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListModelQualityJobDefinitionsResponse
  where
  rnf ListModelQualityJobDefinitionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf jobDefinitionSummaries
