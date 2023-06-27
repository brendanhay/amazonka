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
-- Module      : Amazonka.Config.ListConformancePackComplianceScores
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of conformance pack compliance scores. A compliance score
-- is the percentage of the number of compliant rule-resource combinations
-- in a conformance pack compared to the number of total possible
-- rule-resource combinations in the conformance pack. This metric provides
-- you with a high-level view of the compliance state of your conformance
-- packs. You can use it to identify, investigate, and understand the level
-- of compliance in your conformance packs.
--
-- Conformance packs with no evaluation results will have a compliance
-- score of @INSUFFICIENT_DATA@.
module Amazonka.Config.ListConformancePackComplianceScores
  ( -- * Creating a Request
    ListConformancePackComplianceScores (..),
    newListConformancePackComplianceScores,

    -- * Request Lenses
    listConformancePackComplianceScores_filters,
    listConformancePackComplianceScores_limit,
    listConformancePackComplianceScores_nextToken,
    listConformancePackComplianceScores_sortBy,
    listConformancePackComplianceScores_sortOrder,

    -- * Destructuring the Response
    ListConformancePackComplianceScoresResponse (..),
    newListConformancePackComplianceScoresResponse,

    -- * Response Lenses
    listConformancePackComplianceScoresResponse_nextToken,
    listConformancePackComplianceScoresResponse_httpStatus,
    listConformancePackComplianceScoresResponse_conformancePackComplianceScores,
  )
where

import Amazonka.Config.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListConformancePackComplianceScores' smart constructor.
data ListConformancePackComplianceScores = ListConformancePackComplianceScores'
  { -- | Filters the results based on the
    -- @ConformancePackComplianceScoresFilters@.
    filters :: Prelude.Maybe ConformancePackComplianceScoresFilters,
    -- | The maximum number of conformance pack compliance scores returned on
    -- each page.
    limit :: Prelude.Maybe Prelude.Natural,
    -- | The @nextToken@ string in a prior request that you can use to get the
    -- paginated response for the next set of conformance pack compliance
    -- scores.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sorts your conformance pack compliance scores in either ascending or
    -- descending order, depending on @SortOrder@.
    --
    -- By default, conformance pack compliance scores are sorted in
    -- alphabetical order by name of the conformance pack. Enter @SCORE@, to
    -- sort conformance pack compliance scores by the numerical value of the
    -- compliance score.
    sortBy :: Prelude.Maybe SortBy,
    -- | Determines the order in which conformance pack compliance scores are
    -- sorted. Either in ascending or descending order.
    --
    -- By default, conformance pack compliance scores are sorted in
    -- alphabetical order by name of the conformance pack. Conformance pack
    -- compliance scores are sorted in reverse alphabetical order if you enter
    -- @DESCENDING@.
    --
    -- You can sort conformance pack compliance scores by the numerical value
    -- of the compliance score by entering @SCORE@ in the @SortBy@ action. When
    -- compliance scores are sorted by @SCORE@, conformance packs with a
    -- compliance score of @INSUFFICIENT_DATA@ will be last when sorting by
    -- ascending order and first when sorting by descending order.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConformancePackComplianceScores' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'listConformancePackComplianceScores_filters' - Filters the results based on the
-- @ConformancePackComplianceScoresFilters@.
--
-- 'limit', 'listConformancePackComplianceScores_limit' - The maximum number of conformance pack compliance scores returned on
-- each page.
--
-- 'nextToken', 'listConformancePackComplianceScores_nextToken' - The @nextToken@ string in a prior request that you can use to get the
-- paginated response for the next set of conformance pack compliance
-- scores.
--
-- 'sortBy', 'listConformancePackComplianceScores_sortBy' - Sorts your conformance pack compliance scores in either ascending or
-- descending order, depending on @SortOrder@.
--
-- By default, conformance pack compliance scores are sorted in
-- alphabetical order by name of the conformance pack. Enter @SCORE@, to
-- sort conformance pack compliance scores by the numerical value of the
-- compliance score.
--
-- 'sortOrder', 'listConformancePackComplianceScores_sortOrder' - Determines the order in which conformance pack compliance scores are
-- sorted. Either in ascending or descending order.
--
-- By default, conformance pack compliance scores are sorted in
-- alphabetical order by name of the conformance pack. Conformance pack
-- compliance scores are sorted in reverse alphabetical order if you enter
-- @DESCENDING@.
--
-- You can sort conformance pack compliance scores by the numerical value
-- of the compliance score by entering @SCORE@ in the @SortBy@ action. When
-- compliance scores are sorted by @SCORE@, conformance packs with a
-- compliance score of @INSUFFICIENT_DATA@ will be last when sorting by
-- ascending order and first when sorting by descending order.
newListConformancePackComplianceScores ::
  ListConformancePackComplianceScores
newListConformancePackComplianceScores =
  ListConformancePackComplianceScores'
    { filters =
        Prelude.Nothing,
      limit = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | Filters the results based on the
-- @ConformancePackComplianceScoresFilters@.
listConformancePackComplianceScores_filters :: Lens.Lens' ListConformancePackComplianceScores (Prelude.Maybe ConformancePackComplianceScoresFilters)
listConformancePackComplianceScores_filters = Lens.lens (\ListConformancePackComplianceScores' {filters} -> filters) (\s@ListConformancePackComplianceScores' {} a -> s {filters = a} :: ListConformancePackComplianceScores)

-- | The maximum number of conformance pack compliance scores returned on
-- each page.
listConformancePackComplianceScores_limit :: Lens.Lens' ListConformancePackComplianceScores (Prelude.Maybe Prelude.Natural)
listConformancePackComplianceScores_limit = Lens.lens (\ListConformancePackComplianceScores' {limit} -> limit) (\s@ListConformancePackComplianceScores' {} a -> s {limit = a} :: ListConformancePackComplianceScores)

-- | The @nextToken@ string in a prior request that you can use to get the
-- paginated response for the next set of conformance pack compliance
-- scores.
listConformancePackComplianceScores_nextToken :: Lens.Lens' ListConformancePackComplianceScores (Prelude.Maybe Prelude.Text)
listConformancePackComplianceScores_nextToken = Lens.lens (\ListConformancePackComplianceScores' {nextToken} -> nextToken) (\s@ListConformancePackComplianceScores' {} a -> s {nextToken = a} :: ListConformancePackComplianceScores)

-- | Sorts your conformance pack compliance scores in either ascending or
-- descending order, depending on @SortOrder@.
--
-- By default, conformance pack compliance scores are sorted in
-- alphabetical order by name of the conformance pack. Enter @SCORE@, to
-- sort conformance pack compliance scores by the numerical value of the
-- compliance score.
listConformancePackComplianceScores_sortBy :: Lens.Lens' ListConformancePackComplianceScores (Prelude.Maybe SortBy)
listConformancePackComplianceScores_sortBy = Lens.lens (\ListConformancePackComplianceScores' {sortBy} -> sortBy) (\s@ListConformancePackComplianceScores' {} a -> s {sortBy = a} :: ListConformancePackComplianceScores)

-- | Determines the order in which conformance pack compliance scores are
-- sorted. Either in ascending or descending order.
--
-- By default, conformance pack compliance scores are sorted in
-- alphabetical order by name of the conformance pack. Conformance pack
-- compliance scores are sorted in reverse alphabetical order if you enter
-- @DESCENDING@.
--
-- You can sort conformance pack compliance scores by the numerical value
-- of the compliance score by entering @SCORE@ in the @SortBy@ action. When
-- compliance scores are sorted by @SCORE@, conformance packs with a
-- compliance score of @INSUFFICIENT_DATA@ will be last when sorting by
-- ascending order and first when sorting by descending order.
listConformancePackComplianceScores_sortOrder :: Lens.Lens' ListConformancePackComplianceScores (Prelude.Maybe SortOrder)
listConformancePackComplianceScores_sortOrder = Lens.lens (\ListConformancePackComplianceScores' {sortOrder} -> sortOrder) (\s@ListConformancePackComplianceScores' {} a -> s {sortOrder = a} :: ListConformancePackComplianceScores)

instance
  Core.AWSRequest
    ListConformancePackComplianceScores
  where
  type
    AWSResponse ListConformancePackComplianceScores =
      ListConformancePackComplianceScoresResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListConformancePackComplianceScoresResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "ConformancePackComplianceScores"
                            Core..!@ Prelude.mempty
                        )
      )

instance
  Prelude.Hashable
    ListConformancePackComplianceScores
  where
  hashWithSalt
    _salt
    ListConformancePackComplianceScores' {..} =
      _salt
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` limit
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` sortBy
        `Prelude.hashWithSalt` sortOrder

instance
  Prelude.NFData
    ListConformancePackComplianceScores
  where
  rnf ListConformancePackComplianceScores' {..} =
    Prelude.rnf filters
      `Prelude.seq` Prelude.rnf limit
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance
  Data.ToHeaders
    ListConformancePackComplianceScores
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "StarlingDoveService.ListConformancePackComplianceScores" ::
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
    ListConformancePackComplianceScores
  where
  toJSON ListConformancePackComplianceScores' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("Limit" Data..=) Prelude.<$> limit,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance
  Data.ToPath
    ListConformancePackComplianceScores
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    ListConformancePackComplianceScores
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListConformancePackComplianceScoresResponse' smart constructor.
data ListConformancePackComplianceScoresResponse = ListConformancePackComplianceScoresResponse'
  { -- | The @nextToken@ string that you can use to get the next page of results
    -- in a paginated response.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of @ConformancePackComplianceScore@ objects.
    conformancePackComplianceScores :: [ConformancePackComplianceScore]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListConformancePackComplianceScoresResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listConformancePackComplianceScoresResponse_nextToken' - The @nextToken@ string that you can use to get the next page of results
-- in a paginated response.
--
-- 'httpStatus', 'listConformancePackComplianceScoresResponse_httpStatus' - The response's http status code.
--
-- 'conformancePackComplianceScores', 'listConformancePackComplianceScoresResponse_conformancePackComplianceScores' - A list of @ConformancePackComplianceScore@ objects.
newListConformancePackComplianceScoresResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListConformancePackComplianceScoresResponse
newListConformancePackComplianceScoresResponse
  pHttpStatus_ =
    ListConformancePackComplianceScoresResponse'
      { nextToken =
          Prelude.Nothing,
        httpStatus = pHttpStatus_,
        conformancePackComplianceScores =
          Prelude.mempty
      }

-- | The @nextToken@ string that you can use to get the next page of results
-- in a paginated response.
listConformancePackComplianceScoresResponse_nextToken :: Lens.Lens' ListConformancePackComplianceScoresResponse (Prelude.Maybe Prelude.Text)
listConformancePackComplianceScoresResponse_nextToken = Lens.lens (\ListConformancePackComplianceScoresResponse' {nextToken} -> nextToken) (\s@ListConformancePackComplianceScoresResponse' {} a -> s {nextToken = a} :: ListConformancePackComplianceScoresResponse)

-- | The response's http status code.
listConformancePackComplianceScoresResponse_httpStatus :: Lens.Lens' ListConformancePackComplianceScoresResponse Prelude.Int
listConformancePackComplianceScoresResponse_httpStatus = Lens.lens (\ListConformancePackComplianceScoresResponse' {httpStatus} -> httpStatus) (\s@ListConformancePackComplianceScoresResponse' {} a -> s {httpStatus = a} :: ListConformancePackComplianceScoresResponse)

-- | A list of @ConformancePackComplianceScore@ objects.
listConformancePackComplianceScoresResponse_conformancePackComplianceScores :: Lens.Lens' ListConformancePackComplianceScoresResponse [ConformancePackComplianceScore]
listConformancePackComplianceScoresResponse_conformancePackComplianceScores = Lens.lens (\ListConformancePackComplianceScoresResponse' {conformancePackComplianceScores} -> conformancePackComplianceScores) (\s@ListConformancePackComplianceScoresResponse' {} a -> s {conformancePackComplianceScores = a} :: ListConformancePackComplianceScoresResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListConformancePackComplianceScoresResponse
  where
  rnf ListConformancePackComplianceScoresResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf conformancePackComplianceScores
