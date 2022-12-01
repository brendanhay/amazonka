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
-- Module      : Amazonka.SageMaker.ListLineageGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- A list of lineage groups shared with your Amazon Web Services account.
-- For more information, see
-- <https://docs.aws.amazon.com/sagemaker/latest/dg/xaccount-lineage-tracking.html Cross-Account Lineage Tracking>
-- in the /Amazon SageMaker Developer Guide/.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListLineageGroups
  ( -- * Creating a Request
    ListLineageGroups (..),
    newListLineageGroups,

    -- * Request Lenses
    listLineageGroups_sortOrder,
    listLineageGroups_nextToken,
    listLineageGroups_createdBefore,
    listLineageGroups_sortBy,
    listLineageGroups_maxResults,
    listLineageGroups_createdAfter,

    -- * Destructuring the Response
    ListLineageGroupsResponse (..),
    newListLineageGroupsResponse,

    -- * Response Lenses
    listLineageGroupsResponse_nextToken,
    listLineageGroupsResponse_lineageGroupSummaries,
    listLineageGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListLineageGroups' smart constructor.
data ListLineageGroups = ListLineageGroups'
  { -- | The sort order for the results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of algorithms, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A timestamp to filter against lineage groups created before a certain
    -- point in time.
    createdBefore :: Prelude.Maybe Core.POSIX,
    -- | The parameter by which to sort the results. The default is
    -- @CreationTime@.
    sortBy :: Prelude.Maybe SortLineageGroupsBy,
    -- | The maximum number of endpoints to return in the response. This value
    -- defaults to 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A timestamp to filter against lineage groups created after a certain
    -- point in time.
    createdAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLineageGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listLineageGroups_sortOrder' - The sort order for the results. The default is @Ascending@.
--
-- 'nextToken', 'listLineageGroups_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of algorithms, use it in the subsequent request.
--
-- 'createdBefore', 'listLineageGroups_createdBefore' - A timestamp to filter against lineage groups created before a certain
-- point in time.
--
-- 'sortBy', 'listLineageGroups_sortBy' - The parameter by which to sort the results. The default is
-- @CreationTime@.
--
-- 'maxResults', 'listLineageGroups_maxResults' - The maximum number of endpoints to return in the response. This value
-- defaults to 10.
--
-- 'createdAfter', 'listLineageGroups_createdAfter' - A timestamp to filter against lineage groups created after a certain
-- point in time.
newListLineageGroups ::
  ListLineageGroups
newListLineageGroups =
  ListLineageGroups'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      createdAfter = Prelude.Nothing
    }

-- | The sort order for the results. The default is @Ascending@.
listLineageGroups_sortOrder :: Lens.Lens' ListLineageGroups (Prelude.Maybe SortOrder)
listLineageGroups_sortOrder = Lens.lens (\ListLineageGroups' {sortOrder} -> sortOrder) (\s@ListLineageGroups' {} a -> s {sortOrder = a} :: ListLineageGroups)

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of algorithms, use it in the subsequent request.
listLineageGroups_nextToken :: Lens.Lens' ListLineageGroups (Prelude.Maybe Prelude.Text)
listLineageGroups_nextToken = Lens.lens (\ListLineageGroups' {nextToken} -> nextToken) (\s@ListLineageGroups' {} a -> s {nextToken = a} :: ListLineageGroups)

-- | A timestamp to filter against lineage groups created before a certain
-- point in time.
listLineageGroups_createdBefore :: Lens.Lens' ListLineageGroups (Prelude.Maybe Prelude.UTCTime)
listLineageGroups_createdBefore = Lens.lens (\ListLineageGroups' {createdBefore} -> createdBefore) (\s@ListLineageGroups' {} a -> s {createdBefore = a} :: ListLineageGroups) Prelude.. Lens.mapping Core._Time

-- | The parameter by which to sort the results. The default is
-- @CreationTime@.
listLineageGroups_sortBy :: Lens.Lens' ListLineageGroups (Prelude.Maybe SortLineageGroupsBy)
listLineageGroups_sortBy = Lens.lens (\ListLineageGroups' {sortBy} -> sortBy) (\s@ListLineageGroups' {} a -> s {sortBy = a} :: ListLineageGroups)

-- | The maximum number of endpoints to return in the response. This value
-- defaults to 10.
listLineageGroups_maxResults :: Lens.Lens' ListLineageGroups (Prelude.Maybe Prelude.Natural)
listLineageGroups_maxResults = Lens.lens (\ListLineageGroups' {maxResults} -> maxResults) (\s@ListLineageGroups' {} a -> s {maxResults = a} :: ListLineageGroups)

-- | A timestamp to filter against lineage groups created after a certain
-- point in time.
listLineageGroups_createdAfter :: Lens.Lens' ListLineageGroups (Prelude.Maybe Prelude.UTCTime)
listLineageGroups_createdAfter = Lens.lens (\ListLineageGroups' {createdAfter} -> createdAfter) (\s@ListLineageGroups' {} a -> s {createdAfter = a} :: ListLineageGroups) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListLineageGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listLineageGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listLineageGroupsResponse_lineageGroupSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listLineageGroups_nextToken
          Lens..~ rs
          Lens.^? listLineageGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListLineageGroups where
  type
    AWSResponse ListLineageGroups =
      ListLineageGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListLineageGroupsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "LineageGroupSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLineageGroups where
  hashWithSalt _salt ListLineageGroups' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` createdAfter

instance Prelude.NFData ListLineageGroups where
  rnf ListLineageGroups' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf createdAfter

instance Core.ToHeaders ListLineageGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListLineageGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListLineageGroups where
  toJSON ListLineageGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("CreatedBefore" Core..=) Prelude.<$> createdBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreatedAfter" Core..=) Prelude.<$> createdAfter
          ]
      )

instance Core.ToPath ListLineageGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery ListLineageGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLineageGroupsResponse' smart constructor.
data ListLineageGroupsResponse = ListLineageGroupsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of algorithms, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of lineage groups and their properties.
    lineageGroupSummaries :: Prelude.Maybe [LineageGroupSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListLineageGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listLineageGroupsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of algorithms, use it in the subsequent request.
--
-- 'lineageGroupSummaries', 'listLineageGroupsResponse_lineageGroupSummaries' - A list of lineage groups and their properties.
--
-- 'httpStatus', 'listLineageGroupsResponse_httpStatus' - The response's http status code.
newListLineageGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLineageGroupsResponse
newListLineageGroupsResponse pHttpStatus_ =
  ListLineageGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      lineageGroupSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of algorithms, use it in the subsequent request.
listLineageGroupsResponse_nextToken :: Lens.Lens' ListLineageGroupsResponse (Prelude.Maybe Prelude.Text)
listLineageGroupsResponse_nextToken = Lens.lens (\ListLineageGroupsResponse' {nextToken} -> nextToken) (\s@ListLineageGroupsResponse' {} a -> s {nextToken = a} :: ListLineageGroupsResponse)

-- | A list of lineage groups and their properties.
listLineageGroupsResponse_lineageGroupSummaries :: Lens.Lens' ListLineageGroupsResponse (Prelude.Maybe [LineageGroupSummary])
listLineageGroupsResponse_lineageGroupSummaries = Lens.lens (\ListLineageGroupsResponse' {lineageGroupSummaries} -> lineageGroupSummaries) (\s@ListLineageGroupsResponse' {} a -> s {lineageGroupSummaries = a} :: ListLineageGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listLineageGroupsResponse_httpStatus :: Lens.Lens' ListLineageGroupsResponse Prelude.Int
listLineageGroupsResponse_httpStatus = Lens.lens (\ListLineageGroupsResponse' {httpStatus} -> httpStatus) (\s@ListLineageGroupsResponse' {} a -> s {httpStatus = a} :: ListLineageGroupsResponse)

instance Prelude.NFData ListLineageGroupsResponse where
  rnf ListLineageGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lineageGroupSummaries
      `Prelude.seq` Prelude.rnf httpStatus
