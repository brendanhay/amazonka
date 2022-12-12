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
    listLineageGroups_createdAfter,
    listLineageGroups_createdBefore,
    listLineageGroups_maxResults,
    listLineageGroups_nextToken,
    listLineageGroups_sortBy,
    listLineageGroups_sortOrder,

    -- * Destructuring the Response
    ListLineageGroupsResponse (..),
    newListLineageGroupsResponse,

    -- * Response Lenses
    listLineageGroupsResponse_lineageGroupSummaries,
    listLineageGroupsResponse_nextToken,
    listLineageGroupsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListLineageGroups' smart constructor.
data ListLineageGroups = ListLineageGroups'
  { -- | A timestamp to filter against lineage groups created after a certain
    -- point in time.
    createdAfter :: Prelude.Maybe Data.POSIX,
    -- | A timestamp to filter against lineage groups created before a certain
    -- point in time.
    createdBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of endpoints to return in the response. This value
    -- defaults to 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of algorithms, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The parameter by which to sort the results. The default is
    -- @CreationTime@.
    sortBy :: Prelude.Maybe SortLineageGroupsBy,
    -- | The sort order for the results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder
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
-- 'createdAfter', 'listLineageGroups_createdAfter' - A timestamp to filter against lineage groups created after a certain
-- point in time.
--
-- 'createdBefore', 'listLineageGroups_createdBefore' - A timestamp to filter against lineage groups created before a certain
-- point in time.
--
-- 'maxResults', 'listLineageGroups_maxResults' - The maximum number of endpoints to return in the response. This value
-- defaults to 10.
--
-- 'nextToken', 'listLineageGroups_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of algorithms, use it in the subsequent request.
--
-- 'sortBy', 'listLineageGroups_sortBy' - The parameter by which to sort the results. The default is
-- @CreationTime@.
--
-- 'sortOrder', 'listLineageGroups_sortOrder' - The sort order for the results. The default is @Ascending@.
newListLineageGroups ::
  ListLineageGroups
newListLineageGroups =
  ListLineageGroups'
    { createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | A timestamp to filter against lineage groups created after a certain
-- point in time.
listLineageGroups_createdAfter :: Lens.Lens' ListLineageGroups (Prelude.Maybe Prelude.UTCTime)
listLineageGroups_createdAfter = Lens.lens (\ListLineageGroups' {createdAfter} -> createdAfter) (\s@ListLineageGroups' {} a -> s {createdAfter = a} :: ListLineageGroups) Prelude.. Lens.mapping Data._Time

-- | A timestamp to filter against lineage groups created before a certain
-- point in time.
listLineageGroups_createdBefore :: Lens.Lens' ListLineageGroups (Prelude.Maybe Prelude.UTCTime)
listLineageGroups_createdBefore = Lens.lens (\ListLineageGroups' {createdBefore} -> createdBefore) (\s@ListLineageGroups' {} a -> s {createdBefore = a} :: ListLineageGroups) Prelude.. Lens.mapping Data._Time

-- | The maximum number of endpoints to return in the response. This value
-- defaults to 10.
listLineageGroups_maxResults :: Lens.Lens' ListLineageGroups (Prelude.Maybe Prelude.Natural)
listLineageGroups_maxResults = Lens.lens (\ListLineageGroups' {maxResults} -> maxResults) (\s@ListLineageGroups' {} a -> s {maxResults = a} :: ListLineageGroups)

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of algorithms, use it in the subsequent request.
listLineageGroups_nextToken :: Lens.Lens' ListLineageGroups (Prelude.Maybe Prelude.Text)
listLineageGroups_nextToken = Lens.lens (\ListLineageGroups' {nextToken} -> nextToken) (\s@ListLineageGroups' {} a -> s {nextToken = a} :: ListLineageGroups)

-- | The parameter by which to sort the results. The default is
-- @CreationTime@.
listLineageGroups_sortBy :: Lens.Lens' ListLineageGroups (Prelude.Maybe SortLineageGroupsBy)
listLineageGroups_sortBy = Lens.lens (\ListLineageGroups' {sortBy} -> sortBy) (\s@ListLineageGroups' {} a -> s {sortBy = a} :: ListLineageGroups)

-- | The sort order for the results. The default is @Ascending@.
listLineageGroups_sortOrder :: Lens.Lens' ListLineageGroups (Prelude.Maybe SortOrder)
listLineageGroups_sortOrder = Lens.lens (\ListLineageGroups' {sortOrder} -> sortOrder) (\s@ListLineageGroups' {} a -> s {sortOrder = a} :: ListLineageGroups)

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
            Prelude.<$> ( x Data..?> "LineageGroupSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListLineageGroups where
  hashWithSalt _salt ListLineageGroups' {..} =
    _salt `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListLineageGroups where
  rnf ListLineageGroups' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListLineageGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListLineageGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListLineageGroups where
  toJSON ListLineageGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreatedAfter" Data..=) Prelude.<$> createdAfter,
            ("CreatedBefore" Data..=) Prelude.<$> createdBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListLineageGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListLineageGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListLineageGroupsResponse' smart constructor.
data ListLineageGroupsResponse = ListLineageGroupsResponse'
  { -- | A list of lineage groups and their properties.
    lineageGroupSummaries :: Prelude.Maybe [LineageGroupSummary],
    -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of algorithms, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'lineageGroupSummaries', 'listLineageGroupsResponse_lineageGroupSummaries' - A list of lineage groups and their properties.
--
-- 'nextToken', 'listLineageGroupsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of algorithms, use it in the subsequent request.
--
-- 'httpStatus', 'listLineageGroupsResponse_httpStatus' - The response's http status code.
newListLineageGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListLineageGroupsResponse
newListLineageGroupsResponse pHttpStatus_ =
  ListLineageGroupsResponse'
    { lineageGroupSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of lineage groups and their properties.
listLineageGroupsResponse_lineageGroupSummaries :: Lens.Lens' ListLineageGroupsResponse (Prelude.Maybe [LineageGroupSummary])
listLineageGroupsResponse_lineageGroupSummaries = Lens.lens (\ListLineageGroupsResponse' {lineageGroupSummaries} -> lineageGroupSummaries) (\s@ListLineageGroupsResponse' {} a -> s {lineageGroupSummaries = a} :: ListLineageGroupsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of algorithms, use it in the subsequent request.
listLineageGroupsResponse_nextToken :: Lens.Lens' ListLineageGroupsResponse (Prelude.Maybe Prelude.Text)
listLineageGroupsResponse_nextToken = Lens.lens (\ListLineageGroupsResponse' {nextToken} -> nextToken) (\s@ListLineageGroupsResponse' {} a -> s {nextToken = a} :: ListLineageGroupsResponse)

-- | The response's http status code.
listLineageGroupsResponse_httpStatus :: Lens.Lens' ListLineageGroupsResponse Prelude.Int
listLineageGroupsResponse_httpStatus = Lens.lens (\ListLineageGroupsResponse' {httpStatus} -> httpStatus) (\s@ListLineageGroupsResponse' {} a -> s {httpStatus = a} :: ListLineageGroupsResponse)

instance Prelude.NFData ListLineageGroupsResponse where
  rnf ListLineageGroupsResponse' {..} =
    Prelude.rnf lineageGroupSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
