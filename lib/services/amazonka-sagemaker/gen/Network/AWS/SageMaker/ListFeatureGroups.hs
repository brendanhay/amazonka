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
-- Module      : Amazonka.SageMaker.ListFeatureGroups
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List @FeatureGroup@s based on given filter and order.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListFeatureGroups
  ( -- * Creating a Request
    ListFeatureGroups (..),
    newListFeatureGroups,

    -- * Request Lenses
    listFeatureGroups_nameContains,
    listFeatureGroups_creationTimeAfter,
    listFeatureGroups_nextToken,
    listFeatureGroups_sortOrder,
    listFeatureGroups_creationTimeBefore,
    listFeatureGroups_offlineStoreStatusEquals,
    listFeatureGroups_featureGroupStatusEquals,
    listFeatureGroups_maxResults,
    listFeatureGroups_sortBy,

    -- * Destructuring the Response
    ListFeatureGroupsResponse (..),
    newListFeatureGroupsResponse,

    -- * Response Lenses
    listFeatureGroupsResponse_nextToken,
    listFeatureGroupsResponse_httpStatus,
    listFeatureGroupsResponse_featureGroupSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListFeatureGroups' smart constructor.
data ListFeatureGroups = ListFeatureGroups'
  { -- | A string that partially matches one or more @FeatureGroup@s names.
    -- Filters @FeatureGroup@s by name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | Use this parameter to search for @FeatureGroups@s created after a
    -- specific date and time.
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | A token to resume pagination of @ListFeatureGroups@ results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The order in which feature groups are listed.
    sortOrder :: Prelude.Maybe FeatureGroupSortOrder,
    -- | Use this parameter to search for @FeatureGroups@s created before a
    -- specific date and time.
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | An @OfflineStore@ status. Filters by @OfflineStore@ status.
    offlineStoreStatusEquals :: Prelude.Maybe OfflineStoreStatusValue,
    -- | A @FeatureGroup@ status. Filters by @FeatureGroup@ status.
    featureGroupStatusEquals :: Prelude.Maybe FeatureGroupStatus,
    -- | The maximum number of results returned by @ListFeatureGroups@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The value on which the feature group list is sorted.
    sortBy :: Prelude.Maybe FeatureGroupSortBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFeatureGroups' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameContains', 'listFeatureGroups_nameContains' - A string that partially matches one or more @FeatureGroup@s names.
-- Filters @FeatureGroup@s by name.
--
-- 'creationTimeAfter', 'listFeatureGroups_creationTimeAfter' - Use this parameter to search for @FeatureGroups@s created after a
-- specific date and time.
--
-- 'nextToken', 'listFeatureGroups_nextToken' - A token to resume pagination of @ListFeatureGroups@ results.
--
-- 'sortOrder', 'listFeatureGroups_sortOrder' - The order in which feature groups are listed.
--
-- 'creationTimeBefore', 'listFeatureGroups_creationTimeBefore' - Use this parameter to search for @FeatureGroups@s created before a
-- specific date and time.
--
-- 'offlineStoreStatusEquals', 'listFeatureGroups_offlineStoreStatusEquals' - An @OfflineStore@ status. Filters by @OfflineStore@ status.
--
-- 'featureGroupStatusEquals', 'listFeatureGroups_featureGroupStatusEquals' - A @FeatureGroup@ status. Filters by @FeatureGroup@ status.
--
-- 'maxResults', 'listFeatureGroups_maxResults' - The maximum number of results returned by @ListFeatureGroups@.
--
-- 'sortBy', 'listFeatureGroups_sortBy' - The value on which the feature group list is sorted.
newListFeatureGroups ::
  ListFeatureGroups
newListFeatureGroups =
  ListFeatureGroups'
    { nameContains = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      offlineStoreStatusEquals = Prelude.Nothing,
      featureGroupStatusEquals = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A string that partially matches one or more @FeatureGroup@s names.
-- Filters @FeatureGroup@s by name.
listFeatureGroups_nameContains :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.Text)
listFeatureGroups_nameContains = Lens.lens (\ListFeatureGroups' {nameContains} -> nameContains) (\s@ListFeatureGroups' {} a -> s {nameContains = a} :: ListFeatureGroups)

-- | Use this parameter to search for @FeatureGroups@s created after a
-- specific date and time.
listFeatureGroups_creationTimeAfter :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.UTCTime)
listFeatureGroups_creationTimeAfter = Lens.lens (\ListFeatureGroups' {creationTimeAfter} -> creationTimeAfter) (\s@ListFeatureGroups' {} a -> s {creationTimeAfter = a} :: ListFeatureGroups) Prelude.. Lens.mapping Core._Time

-- | A token to resume pagination of @ListFeatureGroups@ results.
listFeatureGroups_nextToken :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.Text)
listFeatureGroups_nextToken = Lens.lens (\ListFeatureGroups' {nextToken} -> nextToken) (\s@ListFeatureGroups' {} a -> s {nextToken = a} :: ListFeatureGroups)

-- | The order in which feature groups are listed.
listFeatureGroups_sortOrder :: Lens.Lens' ListFeatureGroups (Prelude.Maybe FeatureGroupSortOrder)
listFeatureGroups_sortOrder = Lens.lens (\ListFeatureGroups' {sortOrder} -> sortOrder) (\s@ListFeatureGroups' {} a -> s {sortOrder = a} :: ListFeatureGroups)

-- | Use this parameter to search for @FeatureGroups@s created before a
-- specific date and time.
listFeatureGroups_creationTimeBefore :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.UTCTime)
listFeatureGroups_creationTimeBefore = Lens.lens (\ListFeatureGroups' {creationTimeBefore} -> creationTimeBefore) (\s@ListFeatureGroups' {} a -> s {creationTimeBefore = a} :: ListFeatureGroups) Prelude.. Lens.mapping Core._Time

-- | An @OfflineStore@ status. Filters by @OfflineStore@ status.
listFeatureGroups_offlineStoreStatusEquals :: Lens.Lens' ListFeatureGroups (Prelude.Maybe OfflineStoreStatusValue)
listFeatureGroups_offlineStoreStatusEquals = Lens.lens (\ListFeatureGroups' {offlineStoreStatusEquals} -> offlineStoreStatusEquals) (\s@ListFeatureGroups' {} a -> s {offlineStoreStatusEquals = a} :: ListFeatureGroups)

-- | A @FeatureGroup@ status. Filters by @FeatureGroup@ status.
listFeatureGroups_featureGroupStatusEquals :: Lens.Lens' ListFeatureGroups (Prelude.Maybe FeatureGroupStatus)
listFeatureGroups_featureGroupStatusEquals = Lens.lens (\ListFeatureGroups' {featureGroupStatusEquals} -> featureGroupStatusEquals) (\s@ListFeatureGroups' {} a -> s {featureGroupStatusEquals = a} :: ListFeatureGroups)

-- | The maximum number of results returned by @ListFeatureGroups@.
listFeatureGroups_maxResults :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.Natural)
listFeatureGroups_maxResults = Lens.lens (\ListFeatureGroups' {maxResults} -> maxResults) (\s@ListFeatureGroups' {} a -> s {maxResults = a} :: ListFeatureGroups)

-- | The value on which the feature group list is sorted.
listFeatureGroups_sortBy :: Lens.Lens' ListFeatureGroups (Prelude.Maybe FeatureGroupSortBy)
listFeatureGroups_sortBy = Lens.lens (\ListFeatureGroups' {sortBy} -> sortBy) (\s@ListFeatureGroups' {} a -> s {sortBy = a} :: ListFeatureGroups)

instance Core.AWSPager ListFeatureGroups where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listFeatureGroupsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^. listFeatureGroupsResponse_featureGroupSummaries
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listFeatureGroups_nextToken
          Lens..~ rs
          Lens.^? listFeatureGroupsResponse_nextToken
            Prelude.. Lens._Just

instance Core.AWSRequest ListFeatureGroups where
  type
    AWSResponse ListFeatureGroups =
      ListFeatureGroupsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFeatureGroupsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Core..?> "FeatureGroupSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListFeatureGroups

instance Prelude.NFData ListFeatureGroups

instance Core.ToHeaders ListFeatureGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListFeatureGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListFeatureGroups where
  toJSON ListFeatureGroups' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("OfflineStoreStatusEquals" Core..=)
              Prelude.<$> offlineStoreStatusEquals,
            ("FeatureGroupStatusEquals" Core..=)
              Prelude.<$> featureGroupStatusEquals,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListFeatureGroups where
  toPath = Prelude.const "/"

instance Core.ToQuery ListFeatureGroups where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListFeatureGroupsResponse' smart constructor.
data ListFeatureGroupsResponse = ListFeatureGroupsResponse'
  { -- | A token to resume pagination of @ListFeatureGroups@ results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | A summary of feature groups.
    featureGroupSummaries :: [FeatureGroupSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListFeatureGroupsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listFeatureGroupsResponse_nextToken' - A token to resume pagination of @ListFeatureGroups@ results.
--
-- 'httpStatus', 'listFeatureGroupsResponse_httpStatus' - The response's http status code.
--
-- 'featureGroupSummaries', 'listFeatureGroupsResponse_featureGroupSummaries' - A summary of feature groups.
newListFeatureGroupsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListFeatureGroupsResponse
newListFeatureGroupsResponse pHttpStatus_ =
  ListFeatureGroupsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      featureGroupSummaries = Prelude.mempty
    }

-- | A token to resume pagination of @ListFeatureGroups@ results.
listFeatureGroupsResponse_nextToken :: Lens.Lens' ListFeatureGroupsResponse (Prelude.Maybe Prelude.Text)
listFeatureGroupsResponse_nextToken = Lens.lens (\ListFeatureGroupsResponse' {nextToken} -> nextToken) (\s@ListFeatureGroupsResponse' {} a -> s {nextToken = a} :: ListFeatureGroupsResponse)

-- | The response's http status code.
listFeatureGroupsResponse_httpStatus :: Lens.Lens' ListFeatureGroupsResponse Prelude.Int
listFeatureGroupsResponse_httpStatus = Lens.lens (\ListFeatureGroupsResponse' {httpStatus} -> httpStatus) (\s@ListFeatureGroupsResponse' {} a -> s {httpStatus = a} :: ListFeatureGroupsResponse)

-- | A summary of feature groups.
listFeatureGroupsResponse_featureGroupSummaries :: Lens.Lens' ListFeatureGroupsResponse [FeatureGroupSummary]
listFeatureGroupsResponse_featureGroupSummaries = Lens.lens (\ListFeatureGroupsResponse' {featureGroupSummaries} -> featureGroupSummaries) (\s@ListFeatureGroupsResponse' {} a -> s {featureGroupSummaries = a} :: ListFeatureGroupsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListFeatureGroupsResponse
