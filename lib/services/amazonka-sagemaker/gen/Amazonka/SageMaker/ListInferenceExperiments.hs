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
-- Module      : Amazonka.SageMaker.ListInferenceExperiments
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the list of all inference experiments.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListInferenceExperiments
  ( -- * Creating a Request
    ListInferenceExperiments (..),
    newListInferenceExperiments,

    -- * Request Lenses
    listInferenceExperiments_creationTimeAfter,
    listInferenceExperiments_creationTimeBefore,
    listInferenceExperiments_lastModifiedTimeAfter,
    listInferenceExperiments_lastModifiedTimeBefore,
    listInferenceExperiments_maxResults,
    listInferenceExperiments_nameContains,
    listInferenceExperiments_nextToken,
    listInferenceExperiments_sortBy,
    listInferenceExperiments_sortOrder,
    listInferenceExperiments_statusEquals,
    listInferenceExperiments_type,

    -- * Destructuring the Response
    ListInferenceExperimentsResponse (..),
    newListInferenceExperimentsResponse,

    -- * Response Lenses
    listInferenceExperimentsResponse_inferenceExperiments,
    listInferenceExperimentsResponse_nextToken,
    listInferenceExperimentsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListInferenceExperiments' smart constructor.
data ListInferenceExperiments = ListInferenceExperiments'
  { -- | Selects inference experiments which were created after this timestamp.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Selects inference experiments which were created before this timestamp.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Selects inference experiments which were last modified after this
    -- timestamp.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Selects inference experiments which were last modified before this
    -- timestamp.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of results to select.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Selects inference experiments whose names contain this name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The response from the last list when returning a list large enough to
    -- need tokening.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The column by which to sort the listed inference experiments.
    sortBy :: Prelude.Maybe SortInferenceExperimentsBy,
    -- | The direction of sorting (ascending or descending).
    sortOrder :: Prelude.Maybe SortOrder,
    -- | Selects inference experiments which are in this status. For the possible
    -- statuses, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeInferenceExperiment.html DescribeInferenceExperiment>.
    statusEquals :: Prelude.Maybe InferenceExperimentStatus,
    -- | Selects inference experiments of this type. For the possible types of
    -- inference experiments, see
    -- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateInferenceExperiment.html CreateInferenceExperiment>.
    type' :: Prelude.Maybe InferenceExperimentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceExperiments' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listInferenceExperiments_creationTimeAfter' - Selects inference experiments which were created after this timestamp.
--
-- 'creationTimeBefore', 'listInferenceExperiments_creationTimeBefore' - Selects inference experiments which were created before this timestamp.
--
-- 'lastModifiedTimeAfter', 'listInferenceExperiments_lastModifiedTimeAfter' - Selects inference experiments which were last modified after this
-- timestamp.
--
-- 'lastModifiedTimeBefore', 'listInferenceExperiments_lastModifiedTimeBefore' - Selects inference experiments which were last modified before this
-- timestamp.
--
-- 'maxResults', 'listInferenceExperiments_maxResults' - The maximum number of results to select.
--
-- 'nameContains', 'listInferenceExperiments_nameContains' - Selects inference experiments whose names contain this name.
--
-- 'nextToken', 'listInferenceExperiments_nextToken' - The response from the last list when returning a list large enough to
-- need tokening.
--
-- 'sortBy', 'listInferenceExperiments_sortBy' - The column by which to sort the listed inference experiments.
--
-- 'sortOrder', 'listInferenceExperiments_sortOrder' - The direction of sorting (ascending or descending).
--
-- 'statusEquals', 'listInferenceExperiments_statusEquals' - Selects inference experiments which are in this status. For the possible
-- statuses, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeInferenceExperiment.html DescribeInferenceExperiment>.
--
-- 'type'', 'listInferenceExperiments_type' - Selects inference experiments of this type. For the possible types of
-- inference experiments, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateInferenceExperiment.html CreateInferenceExperiment>.
newListInferenceExperiments ::
  ListInferenceExperiments
newListInferenceExperiments =
  ListInferenceExperiments'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      type' = Prelude.Nothing
    }

-- | Selects inference experiments which were created after this timestamp.
listInferenceExperiments_creationTimeAfter :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe Prelude.UTCTime)
listInferenceExperiments_creationTimeAfter = Lens.lens (\ListInferenceExperiments' {creationTimeAfter} -> creationTimeAfter) (\s@ListInferenceExperiments' {} a -> s {creationTimeAfter = a} :: ListInferenceExperiments) Prelude.. Lens.mapping Data._Time

-- | Selects inference experiments which were created before this timestamp.
listInferenceExperiments_creationTimeBefore :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe Prelude.UTCTime)
listInferenceExperiments_creationTimeBefore = Lens.lens (\ListInferenceExperiments' {creationTimeBefore} -> creationTimeBefore) (\s@ListInferenceExperiments' {} a -> s {creationTimeBefore = a} :: ListInferenceExperiments) Prelude.. Lens.mapping Data._Time

-- | Selects inference experiments which were last modified after this
-- timestamp.
listInferenceExperiments_lastModifiedTimeAfter :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe Prelude.UTCTime)
listInferenceExperiments_lastModifiedTimeAfter = Lens.lens (\ListInferenceExperiments' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListInferenceExperiments' {} a -> s {lastModifiedTimeAfter = a} :: ListInferenceExperiments) Prelude.. Lens.mapping Data._Time

-- | Selects inference experiments which were last modified before this
-- timestamp.
listInferenceExperiments_lastModifiedTimeBefore :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe Prelude.UTCTime)
listInferenceExperiments_lastModifiedTimeBefore = Lens.lens (\ListInferenceExperiments' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListInferenceExperiments' {} a -> s {lastModifiedTimeBefore = a} :: ListInferenceExperiments) Prelude.. Lens.mapping Data._Time

-- | The maximum number of results to select.
listInferenceExperiments_maxResults :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe Prelude.Natural)
listInferenceExperiments_maxResults = Lens.lens (\ListInferenceExperiments' {maxResults} -> maxResults) (\s@ListInferenceExperiments' {} a -> s {maxResults = a} :: ListInferenceExperiments)

-- | Selects inference experiments whose names contain this name.
listInferenceExperiments_nameContains :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe Prelude.Text)
listInferenceExperiments_nameContains = Lens.lens (\ListInferenceExperiments' {nameContains} -> nameContains) (\s@ListInferenceExperiments' {} a -> s {nameContains = a} :: ListInferenceExperiments)

-- | The response from the last list when returning a list large enough to
-- need tokening.
listInferenceExperiments_nextToken :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe Prelude.Text)
listInferenceExperiments_nextToken = Lens.lens (\ListInferenceExperiments' {nextToken} -> nextToken) (\s@ListInferenceExperiments' {} a -> s {nextToken = a} :: ListInferenceExperiments)

-- | The column by which to sort the listed inference experiments.
listInferenceExperiments_sortBy :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe SortInferenceExperimentsBy)
listInferenceExperiments_sortBy = Lens.lens (\ListInferenceExperiments' {sortBy} -> sortBy) (\s@ListInferenceExperiments' {} a -> s {sortBy = a} :: ListInferenceExperiments)

-- | The direction of sorting (ascending or descending).
listInferenceExperiments_sortOrder :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe SortOrder)
listInferenceExperiments_sortOrder = Lens.lens (\ListInferenceExperiments' {sortOrder} -> sortOrder) (\s@ListInferenceExperiments' {} a -> s {sortOrder = a} :: ListInferenceExperiments)

-- | Selects inference experiments which are in this status. For the possible
-- statuses, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_DescribeInferenceExperiment.html DescribeInferenceExperiment>.
listInferenceExperiments_statusEquals :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe InferenceExperimentStatus)
listInferenceExperiments_statusEquals = Lens.lens (\ListInferenceExperiments' {statusEquals} -> statusEquals) (\s@ListInferenceExperiments' {} a -> s {statusEquals = a} :: ListInferenceExperiments)

-- | Selects inference experiments of this type. For the possible types of
-- inference experiments, see
-- <https://docs.aws.amazon.com/sagemaker/latest/APIReference/API_CreateInferenceExperiment.html CreateInferenceExperiment>.
listInferenceExperiments_type :: Lens.Lens' ListInferenceExperiments (Prelude.Maybe InferenceExperimentType)
listInferenceExperiments_type = Lens.lens (\ListInferenceExperiments' {type'} -> type') (\s@ListInferenceExperiments' {} a -> s {type' = a} :: ListInferenceExperiments)

instance Core.AWSPager ListInferenceExperiments where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listInferenceExperimentsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listInferenceExperimentsResponse_inferenceExperiments
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listInferenceExperiments_nextToken
          Lens..~ rs
          Lens.^? listInferenceExperimentsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListInferenceExperiments where
  type
    AWSResponse ListInferenceExperiments =
      ListInferenceExperimentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListInferenceExperimentsResponse'
            Prelude.<$> ( x
                            Data..?> "InferenceExperiments"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListInferenceExperiments where
  hashWithSalt _salt ListInferenceExperiments' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` statusEquals
      `Prelude.hashWithSalt` type'

instance Prelude.NFData ListInferenceExperiments where
  rnf ListInferenceExperiments' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf statusEquals
      `Prelude.seq` Prelude.rnf type'

instance Data.ToHeaders ListInferenceExperiments where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListInferenceExperiments" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListInferenceExperiments where
  toJSON ListInferenceExperiments' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Data..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("LastModifiedTimeBefore" Data..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("StatusEquals" Data..=) Prelude.<$> statusEquals,
            ("Type" Data..=) Prelude.<$> type'
          ]
      )

instance Data.ToPath ListInferenceExperiments where
  toPath = Prelude.const "/"

instance Data.ToQuery ListInferenceExperiments where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListInferenceExperimentsResponse' smart constructor.
data ListInferenceExperimentsResponse = ListInferenceExperimentsResponse'
  { -- | List of inference experiments.
    inferenceExperiments :: Prelude.Maybe [InferenceExperimentSummary],
    -- | The token to use when calling the next page of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListInferenceExperimentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'inferenceExperiments', 'listInferenceExperimentsResponse_inferenceExperiments' - List of inference experiments.
--
-- 'nextToken', 'listInferenceExperimentsResponse_nextToken' - The token to use when calling the next page of results.
--
-- 'httpStatus', 'listInferenceExperimentsResponse_httpStatus' - The response's http status code.
newListInferenceExperimentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListInferenceExperimentsResponse
newListInferenceExperimentsResponse pHttpStatus_ =
  ListInferenceExperimentsResponse'
    { inferenceExperiments =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of inference experiments.
listInferenceExperimentsResponse_inferenceExperiments :: Lens.Lens' ListInferenceExperimentsResponse (Prelude.Maybe [InferenceExperimentSummary])
listInferenceExperimentsResponse_inferenceExperiments = Lens.lens (\ListInferenceExperimentsResponse' {inferenceExperiments} -> inferenceExperiments) (\s@ListInferenceExperimentsResponse' {} a -> s {inferenceExperiments = a} :: ListInferenceExperimentsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token to use when calling the next page of results.
listInferenceExperimentsResponse_nextToken :: Lens.Lens' ListInferenceExperimentsResponse (Prelude.Maybe Prelude.Text)
listInferenceExperimentsResponse_nextToken = Lens.lens (\ListInferenceExperimentsResponse' {nextToken} -> nextToken) (\s@ListInferenceExperimentsResponse' {} a -> s {nextToken = a} :: ListInferenceExperimentsResponse)

-- | The response's http status code.
listInferenceExperimentsResponse_httpStatus :: Lens.Lens' ListInferenceExperimentsResponse Prelude.Int
listInferenceExperimentsResponse_httpStatus = Lens.lens (\ListInferenceExperimentsResponse' {httpStatus} -> httpStatus) (\s@ListInferenceExperimentsResponse' {} a -> s {httpStatus = a} :: ListInferenceExperimentsResponse)

instance
  Prelude.NFData
    ListInferenceExperimentsResponse
  where
  rnf ListInferenceExperimentsResponse' {..} =
    Prelude.rnf inferenceExperiments
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
