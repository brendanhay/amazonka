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
-- Module      : Amazonka.SageMaker.ListModelPackageGroups
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a list of the model groups in your Amazon Web Services account.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListModelPackageGroups
  ( -- * Creating a Request
    ListModelPackageGroups (..),
    newListModelPackageGroups,

    -- * Request Lenses
    listModelPackageGroups_sortOrder,
    listModelPackageGroups_nextToken,
    listModelPackageGroups_nameContains,
    listModelPackageGroups_creationTimeBefore,
    listModelPackageGroups_sortBy,
    listModelPackageGroups_maxResults,
    listModelPackageGroups_creationTimeAfter,

    -- * Destructuring the Response
    ListModelPackageGroupsResponse (..),
    newListModelPackageGroupsResponse,

    -- * Response Lenses
    listModelPackageGroupsResponse_nextToken,
    listModelPackageGroupsResponse_httpStatus,
    listModelPackageGroupsResponse_modelPackageGroupSummaryList,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListModelPackageGroups' smart constructor.
data ListModelPackageGroups = ListModelPackageGroups'
  { -- | The sort order for results. The default is @Ascending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | If the result of the previous @ListModelPackageGroups@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of model groups, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string in the model group name. This filter returns only model groups
    -- whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only model groups created before the specified
    -- time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Prelude.Maybe ModelPackageGroupSortBy,
    -- | The maximum number of results to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only model groups created after the specified
    -- time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelPackageGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listModelPackageGroups_sortOrder' - The sort order for results. The default is @Ascending@.
--
-- 'nextToken', 'listModelPackageGroups_nextToken' - If the result of the previous @ListModelPackageGroups@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model groups, use the token in the next request.
--
-- 'nameContains', 'listModelPackageGroups_nameContains' - A string in the model group name. This filter returns only model groups
-- whose name contains the specified string.
--
-- 'creationTimeBefore', 'listModelPackageGroups_creationTimeBefore' - A filter that returns only model groups created before the specified
-- time.
--
-- 'sortBy', 'listModelPackageGroups_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'maxResults', 'listModelPackageGroups_maxResults' - The maximum number of results to return in the response.
--
-- 'creationTimeAfter', 'listModelPackageGroups_creationTimeAfter' - A filter that returns only model groups created after the specified
-- time.
newListModelPackageGroups ::
  ListModelPackageGroups
newListModelPackageGroups =
  ListModelPackageGroups'
    { sortOrder =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | The sort order for results. The default is @Ascending@.
listModelPackageGroups_sortOrder :: Lens.Lens' ListModelPackageGroups (Prelude.Maybe SortOrder)
listModelPackageGroups_sortOrder = Lens.lens (\ListModelPackageGroups' {sortOrder} -> sortOrder) (\s@ListModelPackageGroups' {} a -> s {sortOrder = a} :: ListModelPackageGroups)

-- | If the result of the previous @ListModelPackageGroups@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of model groups, use the token in the next request.
listModelPackageGroups_nextToken :: Lens.Lens' ListModelPackageGroups (Prelude.Maybe Prelude.Text)
listModelPackageGroups_nextToken = Lens.lens (\ListModelPackageGroups' {nextToken} -> nextToken) (\s@ListModelPackageGroups' {} a -> s {nextToken = a} :: ListModelPackageGroups)

-- | A string in the model group name. This filter returns only model groups
-- whose name contains the specified string.
listModelPackageGroups_nameContains :: Lens.Lens' ListModelPackageGroups (Prelude.Maybe Prelude.Text)
listModelPackageGroups_nameContains = Lens.lens (\ListModelPackageGroups' {nameContains} -> nameContains) (\s@ListModelPackageGroups' {} a -> s {nameContains = a} :: ListModelPackageGroups)

-- | A filter that returns only model groups created before the specified
-- time.
listModelPackageGroups_creationTimeBefore :: Lens.Lens' ListModelPackageGroups (Prelude.Maybe Prelude.UTCTime)
listModelPackageGroups_creationTimeBefore = Lens.lens (\ListModelPackageGroups' {creationTimeBefore} -> creationTimeBefore) (\s@ListModelPackageGroups' {} a -> s {creationTimeBefore = a} :: ListModelPackageGroups) Prelude.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @CreationTime@.
listModelPackageGroups_sortBy :: Lens.Lens' ListModelPackageGroups (Prelude.Maybe ModelPackageGroupSortBy)
listModelPackageGroups_sortBy = Lens.lens (\ListModelPackageGroups' {sortBy} -> sortBy) (\s@ListModelPackageGroups' {} a -> s {sortBy = a} :: ListModelPackageGroups)

-- | The maximum number of results to return in the response.
listModelPackageGroups_maxResults :: Lens.Lens' ListModelPackageGroups (Prelude.Maybe Prelude.Natural)
listModelPackageGroups_maxResults = Lens.lens (\ListModelPackageGroups' {maxResults} -> maxResults) (\s@ListModelPackageGroups' {} a -> s {maxResults = a} :: ListModelPackageGroups)

-- | A filter that returns only model groups created after the specified
-- time.
listModelPackageGroups_creationTimeAfter :: Lens.Lens' ListModelPackageGroups (Prelude.Maybe Prelude.UTCTime)
listModelPackageGroups_creationTimeAfter = Lens.lens (\ListModelPackageGroups' {creationTimeAfter} -> creationTimeAfter) (\s@ListModelPackageGroups' {} a -> s {creationTimeAfter = a} :: ListModelPackageGroups) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListModelPackageGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listModelPackageGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listModelPackageGroupsResponse_modelPackageGroupSummaryList
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listModelPackageGroups_nextToken
          Lens..~ rs
          Lens.^? listModelPackageGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListModelPackageGroups where
  type
    AWSResponse ListModelPackageGroups =
      ListModelPackageGroupsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListModelPackageGroupsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "ModelPackageGroupSummaryList"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListModelPackageGroups where
  hashWithSalt _salt ListModelPackageGroups' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` creationTimeAfter

instance Prelude.NFData ListModelPackageGroups where
  rnf ListModelPackageGroups' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf creationTimeAfter

instance Core.ToHeaders ListModelPackageGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListModelPackageGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListModelPackageGroups where
  toJSON ListModelPackageGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListModelPackageGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery ListModelPackageGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListModelPackageGroupsResponse' smart constructor.
data ListModelPackageGroupsResponse = ListModelPackageGroupsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of model groups, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A list of summaries of the model groups in your Amazon Web Services
    -- account.
    modelPackageGroupSummaryList :: [ModelPackageGroupSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListModelPackageGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listModelPackageGroupsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of model groups, use it in the subsequent request.
--
-- 'httpStatus', 'listModelPackageGroupsResponse_httpStatus' - The response's http status code.
--
-- 'modelPackageGroupSummaryList', 'listModelPackageGroupsResponse_modelPackageGroupSummaryList' - A list of summaries of the model groups in your Amazon Web Services
-- account.
newListModelPackageGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListModelPackageGroupsResponse
newListModelPackageGroupsResponse pHttpStatus_ =
  ListModelPackageGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      modelPackageGroupSummaryList =
        Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of model groups, use it in the subsequent request.
listModelPackageGroupsResponse_nextToken :: Lens.Lens' ListModelPackageGroupsResponse (Prelude.Maybe Prelude.Text)
listModelPackageGroupsResponse_nextToken = Lens.lens (\ListModelPackageGroupsResponse' {nextToken} -> nextToken) (\s@ListModelPackageGroupsResponse' {} a -> s {nextToken = a} :: ListModelPackageGroupsResponse)

-- | The response's http status code.
listModelPackageGroupsResponse_httpStatus :: Lens.Lens' ListModelPackageGroupsResponse Prelude.Int
listModelPackageGroupsResponse_httpStatus = Lens.lens (\ListModelPackageGroupsResponse' {httpStatus} -> httpStatus) (\s@ListModelPackageGroupsResponse' {} a -> s {httpStatus = a} :: ListModelPackageGroupsResponse)

-- | A list of summaries of the model groups in your Amazon Web Services
-- account.
listModelPackageGroupsResponse_modelPackageGroupSummaryList :: Lens.Lens' ListModelPackageGroupsResponse [ModelPackageGroupSummary]
listModelPackageGroupsResponse_modelPackageGroupSummaryList = Lens.lens (\ListModelPackageGroupsResponse' {modelPackageGroupSummaryList} -> modelPackageGroupSummaryList) (\s@ListModelPackageGroupsResponse' {} a -> s {modelPackageGroupSummaryList = a} :: ListModelPackageGroupsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListModelPackageGroupsResponse
  where
  rnf ListModelPackageGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf modelPackageGroupSummaryList
