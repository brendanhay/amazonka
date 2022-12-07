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
-- Copyright   : (c) 2013-2022 Brendan Hay
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
    listFeatureGroups_sortOrder,
    listFeatureGroups_nextToken,
    listFeatureGroups_nameContains,
    listFeatureGroups_offlineStoreStatusEquals,
    listFeatureGroups_creationTimeBefore,
    listFeatureGroups_sortBy,
    listFeatureGroups_maxResults,
    listFeatureGroups_creationTimeAfter,
    listFeatureGroups_featureGroupStatusEquals,

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
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListFeatureGroups' smart constructor.
data ListFeatureGroups = ListFeatureGroups'
  { -- | The order in which feature groups are listed.
    sortOrder :: Prelude.Maybe FeatureGroupSortOrder,
    -- | A token to resume pagination of @ListFeatureGroups@ results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string that partially matches one or more @FeatureGroup@s names.
    -- Filters @FeatureGroup@s by name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | An @OfflineStore@ status. Filters by @OfflineStore@ status.
    offlineStoreStatusEquals :: Prelude.Maybe OfflineStoreStatusValue,
    -- | Use this parameter to search for @FeatureGroups@s created before a
    -- specific date and time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The value on which the feature group list is sorted.
    sortBy :: Prelude.Maybe FeatureGroupSortBy,
    -- | The maximum number of results returned by @ListFeatureGroups@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Use this parameter to search for @FeatureGroups@s created after a
    -- specific date and time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | A @FeatureGroup@ status. Filters by @FeatureGroup@ status.
    featureGroupStatusEquals :: Prelude.Maybe FeatureGroupStatus
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
-- 'sortOrder', 'listFeatureGroups_sortOrder' - The order in which feature groups are listed.
--
-- 'nextToken', 'listFeatureGroups_nextToken' - A token to resume pagination of @ListFeatureGroups@ results.
--
-- 'nameContains', 'listFeatureGroups_nameContains' - A string that partially matches one or more @FeatureGroup@s names.
-- Filters @FeatureGroup@s by name.
--
-- 'offlineStoreStatusEquals', 'listFeatureGroups_offlineStoreStatusEquals' - An @OfflineStore@ status. Filters by @OfflineStore@ status.
--
-- 'creationTimeBefore', 'listFeatureGroups_creationTimeBefore' - Use this parameter to search for @FeatureGroups@s created before a
-- specific date and time.
--
-- 'sortBy', 'listFeatureGroups_sortBy' - The value on which the feature group list is sorted.
--
-- 'maxResults', 'listFeatureGroups_maxResults' - The maximum number of results returned by @ListFeatureGroups@.
--
-- 'creationTimeAfter', 'listFeatureGroups_creationTimeAfter' - Use this parameter to search for @FeatureGroups@s created after a
-- specific date and time.
--
-- 'featureGroupStatusEquals', 'listFeatureGroups_featureGroupStatusEquals' - A @FeatureGroup@ status. Filters by @FeatureGroup@ status.
newListFeatureGroups ::
  ListFeatureGroups
newListFeatureGroups =
  ListFeatureGroups'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      offlineStoreStatusEquals = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      featureGroupStatusEquals = Prelude.Nothing
    }

-- | The order in which feature groups are listed.
listFeatureGroups_sortOrder :: Lens.Lens' ListFeatureGroups (Prelude.Maybe FeatureGroupSortOrder)
listFeatureGroups_sortOrder = Lens.lens (\ListFeatureGroups' {sortOrder} -> sortOrder) (\s@ListFeatureGroups' {} a -> s {sortOrder = a} :: ListFeatureGroups)

-- | A token to resume pagination of @ListFeatureGroups@ results.
listFeatureGroups_nextToken :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.Text)
listFeatureGroups_nextToken = Lens.lens (\ListFeatureGroups' {nextToken} -> nextToken) (\s@ListFeatureGroups' {} a -> s {nextToken = a} :: ListFeatureGroups)

-- | A string that partially matches one or more @FeatureGroup@s names.
-- Filters @FeatureGroup@s by name.
listFeatureGroups_nameContains :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.Text)
listFeatureGroups_nameContains = Lens.lens (\ListFeatureGroups' {nameContains} -> nameContains) (\s@ListFeatureGroups' {} a -> s {nameContains = a} :: ListFeatureGroups)

-- | An @OfflineStore@ status. Filters by @OfflineStore@ status.
listFeatureGroups_offlineStoreStatusEquals :: Lens.Lens' ListFeatureGroups (Prelude.Maybe OfflineStoreStatusValue)
listFeatureGroups_offlineStoreStatusEquals = Lens.lens (\ListFeatureGroups' {offlineStoreStatusEquals} -> offlineStoreStatusEquals) (\s@ListFeatureGroups' {} a -> s {offlineStoreStatusEquals = a} :: ListFeatureGroups)

-- | Use this parameter to search for @FeatureGroups@s created before a
-- specific date and time.
listFeatureGroups_creationTimeBefore :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.UTCTime)
listFeatureGroups_creationTimeBefore = Lens.lens (\ListFeatureGroups' {creationTimeBefore} -> creationTimeBefore) (\s@ListFeatureGroups' {} a -> s {creationTimeBefore = a} :: ListFeatureGroups) Prelude.. Lens.mapping Data._Time

-- | The value on which the feature group list is sorted.
listFeatureGroups_sortBy :: Lens.Lens' ListFeatureGroups (Prelude.Maybe FeatureGroupSortBy)
listFeatureGroups_sortBy = Lens.lens (\ListFeatureGroups' {sortBy} -> sortBy) (\s@ListFeatureGroups' {} a -> s {sortBy = a} :: ListFeatureGroups)

-- | The maximum number of results returned by @ListFeatureGroups@.
listFeatureGroups_maxResults :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.Natural)
listFeatureGroups_maxResults = Lens.lens (\ListFeatureGroups' {maxResults} -> maxResults) (\s@ListFeatureGroups' {} a -> s {maxResults = a} :: ListFeatureGroups)

-- | Use this parameter to search for @FeatureGroups@s created after a
-- specific date and time.
listFeatureGroups_creationTimeAfter :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.UTCTime)
listFeatureGroups_creationTimeAfter = Lens.lens (\ListFeatureGroups' {creationTimeAfter} -> creationTimeAfter) (\s@ListFeatureGroups' {} a -> s {creationTimeAfter = a} :: ListFeatureGroups) Prelude.. Lens.mapping Data._Time

-- | A @FeatureGroup@ status. Filters by @FeatureGroup@ status.
listFeatureGroups_featureGroupStatusEquals :: Lens.Lens' ListFeatureGroups (Prelude.Maybe FeatureGroupStatus)
listFeatureGroups_featureGroupStatusEquals = Lens.lens (\ListFeatureGroups' {featureGroupStatusEquals} -> featureGroupStatusEquals) (\s@ListFeatureGroups' {} a -> s {featureGroupStatusEquals = a} :: ListFeatureGroups)

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
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListFeatureGroupsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "FeatureGroupSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListFeatureGroups where
  hashWithSalt _salt ListFeatureGroups' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` offlineStoreStatusEquals
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` featureGroupStatusEquals

instance Prelude.NFData ListFeatureGroups where
  rnf ListFeatureGroups' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf offlineStoreStatusEquals
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf featureGroupStatusEquals

instance Data.ToHeaders ListFeatureGroups where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListFeatureGroups" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListFeatureGroups where
  toJSON ListFeatureGroups' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("SortOrder" Data..=) Prelude.<$> sortOrder,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("OfflineStoreStatusEquals" Data..=)
              Prelude.<$> offlineStoreStatusEquals,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("FeatureGroupStatusEquals" Data..=)
              Prelude.<$> featureGroupStatusEquals
          ]
      )

instance Data.ToPath ListFeatureGroups where
  toPath = Prelude.const "/"

instance Data.ToQuery ListFeatureGroups where
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

instance Prelude.NFData ListFeatureGroupsResponse where
  rnf ListFeatureGroupsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf featureGroupSummaries
