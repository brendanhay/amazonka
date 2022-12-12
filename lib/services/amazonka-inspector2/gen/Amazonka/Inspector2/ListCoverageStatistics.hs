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
-- Module      : Amazonka.Inspector2.ListCoverageStatistics
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists Amazon Inspector coverage statistics for your environment.
--
-- This operation returns paginated results.
module Amazonka.Inspector2.ListCoverageStatistics
  ( -- * Creating a Request
    ListCoverageStatistics (..),
    newListCoverageStatistics,

    -- * Request Lenses
    listCoverageStatistics_filterCriteria,
    listCoverageStatistics_groupBy,
    listCoverageStatistics_nextToken,

    -- * Destructuring the Response
    ListCoverageStatisticsResponse (..),
    newListCoverageStatisticsResponse,

    -- * Response Lenses
    listCoverageStatisticsResponse_countsByGroup,
    listCoverageStatisticsResponse_nextToken,
    listCoverageStatisticsResponse_httpStatus,
    listCoverageStatisticsResponse_totalCounts,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Inspector2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListCoverageStatistics' smart constructor.
data ListCoverageStatistics = ListCoverageStatistics'
  { -- | An object that contains details on the filters to apply to the coverage
    -- data for your environment.
    filterCriteria :: Prelude.Maybe CoverageFilterCriteria,
    -- | The value to group the results by.
    groupBy :: Prelude.Maybe GroupKey,
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoverageStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filterCriteria', 'listCoverageStatistics_filterCriteria' - An object that contains details on the filters to apply to the coverage
-- data for your environment.
--
-- 'groupBy', 'listCoverageStatistics_groupBy' - The value to group the results by.
--
-- 'nextToken', 'listCoverageStatistics_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
newListCoverageStatistics ::
  ListCoverageStatistics
newListCoverageStatistics =
  ListCoverageStatistics'
    { filterCriteria =
        Prelude.Nothing,
      groupBy = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | An object that contains details on the filters to apply to the coverage
-- data for your environment.
listCoverageStatistics_filterCriteria :: Lens.Lens' ListCoverageStatistics (Prelude.Maybe CoverageFilterCriteria)
listCoverageStatistics_filterCriteria = Lens.lens (\ListCoverageStatistics' {filterCriteria} -> filterCriteria) (\s@ListCoverageStatistics' {} a -> s {filterCriteria = a} :: ListCoverageStatistics)

-- | The value to group the results by.
listCoverageStatistics_groupBy :: Lens.Lens' ListCoverageStatistics (Prelude.Maybe GroupKey)
listCoverageStatistics_groupBy = Lens.lens (\ListCoverageStatistics' {groupBy} -> groupBy) (\s@ListCoverageStatistics' {} a -> s {groupBy = a} :: ListCoverageStatistics)

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listCoverageStatistics_nextToken :: Lens.Lens' ListCoverageStatistics (Prelude.Maybe Prelude.Text)
listCoverageStatistics_nextToken = Lens.lens (\ListCoverageStatistics' {nextToken} -> nextToken) (\s@ListCoverageStatistics' {} a -> s {nextToken = a} :: ListCoverageStatistics)

instance Core.AWSPager ListCoverageStatistics where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listCoverageStatisticsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listCoverageStatisticsResponse_countsByGroup
              Prelude.. Lens._Just
              Prelude.. Lens.to Prelude.toList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listCoverageStatistics_nextToken
          Lens..~ rs
          Lens.^? listCoverageStatisticsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListCoverageStatistics where
  type
    AWSResponse ListCoverageStatistics =
      ListCoverageStatisticsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListCoverageStatisticsResponse'
            Prelude.<$> (x Data..?> "countsByGroup")
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..:> "totalCounts")
      )

instance Prelude.Hashable ListCoverageStatistics where
  hashWithSalt _salt ListCoverageStatistics' {..} =
    _salt `Prelude.hashWithSalt` filterCriteria
      `Prelude.hashWithSalt` groupBy
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListCoverageStatistics where
  rnf ListCoverageStatistics' {..} =
    Prelude.rnf filterCriteria
      `Prelude.seq` Prelude.rnf groupBy
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListCoverageStatistics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListCoverageStatistics where
  toJSON ListCoverageStatistics' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("filterCriteria" Data..=)
              Prelude.<$> filterCriteria,
            ("groupBy" Data..=) Prelude.<$> groupBy,
            ("nextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath ListCoverageStatistics where
  toPath = Prelude.const "/coverage/statistics/list"

instance Data.ToQuery ListCoverageStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListCoverageStatisticsResponse' smart constructor.
data ListCoverageStatisticsResponse = ListCoverageStatisticsResponse'
  { -- | An array with the number for each group.
    countsByGroup :: Prelude.Maybe (Prelude.NonEmpty Counts),
    -- | A token to use for paginating results that are returned in the response.
    -- Set the value of this parameter to null for the first request to a list
    -- action. For subsequent calls, use the @NextToken@ value returned from
    -- the previous request to continue listing results after the first page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The total number for all groups.
    totalCounts :: Prelude.Integer
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListCoverageStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'countsByGroup', 'listCoverageStatisticsResponse_countsByGroup' - An array with the number for each group.
--
-- 'nextToken', 'listCoverageStatisticsResponse_nextToken' - A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
--
-- 'httpStatus', 'listCoverageStatisticsResponse_httpStatus' - The response's http status code.
--
-- 'totalCounts', 'listCoverageStatisticsResponse_totalCounts' - The total number for all groups.
newListCoverageStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  -- | 'totalCounts'
  Prelude.Integer ->
  ListCoverageStatisticsResponse
newListCoverageStatisticsResponse
  pHttpStatus_
  pTotalCounts_ =
    ListCoverageStatisticsResponse'
      { countsByGroup =
          Prelude.Nothing,
        nextToken = Prelude.Nothing,
        httpStatus = pHttpStatus_,
        totalCounts = pTotalCounts_
      }

-- | An array with the number for each group.
listCoverageStatisticsResponse_countsByGroup :: Lens.Lens' ListCoverageStatisticsResponse (Prelude.Maybe (Prelude.NonEmpty Counts))
listCoverageStatisticsResponse_countsByGroup = Lens.lens (\ListCoverageStatisticsResponse' {countsByGroup} -> countsByGroup) (\s@ListCoverageStatisticsResponse' {} a -> s {countsByGroup = a} :: ListCoverageStatisticsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token to use for paginating results that are returned in the response.
-- Set the value of this parameter to null for the first request to a list
-- action. For subsequent calls, use the @NextToken@ value returned from
-- the previous request to continue listing results after the first page.
listCoverageStatisticsResponse_nextToken :: Lens.Lens' ListCoverageStatisticsResponse (Prelude.Maybe Prelude.Text)
listCoverageStatisticsResponse_nextToken = Lens.lens (\ListCoverageStatisticsResponse' {nextToken} -> nextToken) (\s@ListCoverageStatisticsResponse' {} a -> s {nextToken = a} :: ListCoverageStatisticsResponse)

-- | The response's http status code.
listCoverageStatisticsResponse_httpStatus :: Lens.Lens' ListCoverageStatisticsResponse Prelude.Int
listCoverageStatisticsResponse_httpStatus = Lens.lens (\ListCoverageStatisticsResponse' {httpStatus} -> httpStatus) (\s@ListCoverageStatisticsResponse' {} a -> s {httpStatus = a} :: ListCoverageStatisticsResponse)

-- | The total number for all groups.
listCoverageStatisticsResponse_totalCounts :: Lens.Lens' ListCoverageStatisticsResponse Prelude.Integer
listCoverageStatisticsResponse_totalCounts = Lens.lens (\ListCoverageStatisticsResponse' {totalCounts} -> totalCounts) (\s@ListCoverageStatisticsResponse' {} a -> s {totalCounts = a} :: ListCoverageStatisticsResponse)

instance
  Prelude.NFData
    ListCoverageStatisticsResponse
  where
  rnf ListCoverageStatisticsResponse' {..} =
    Prelude.rnf countsByGroup
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf totalCounts
