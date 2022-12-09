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
    listFeatureGroups_creationTimeAfter,
    listFeatureGroups_creationTimeBefore,
    listFeatureGroups_featureGroupStatusEquals,
    listFeatureGroups_maxResults,
    listFeatureGroups_nameContains,
    listFeatureGroups_nextToken,
    listFeatureGroups_offlineStoreStatusEquals,
    listFeatureGroups_sortBy,
    listFeatureGroups_sortOrder,

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
  { -- | Use this parameter to search for @FeatureGroups@s created after a
    -- specific date and time.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Use this parameter to search for @FeatureGroups@s created before a
    -- specific date and time.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | A @FeatureGroup@ status. Filters by @FeatureGroup@ status.
    featureGroupStatusEquals :: Prelude.Maybe FeatureGroupStatus,
    -- | The maximum number of results returned by @ListFeatureGroups@.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A string that partially matches one or more @FeatureGroup@s names.
    -- Filters @FeatureGroup@s by name.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A token to resume pagination of @ListFeatureGroups@ results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An @OfflineStore@ status. Filters by @OfflineStore@ status.
    offlineStoreStatusEquals :: Prelude.Maybe OfflineStoreStatusValue,
    -- | The value on which the feature group list is sorted.
    sortBy :: Prelude.Maybe FeatureGroupSortBy,
    -- | The order in which feature groups are listed.
    sortOrder :: Prelude.Maybe FeatureGroupSortOrder
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
-- 'creationTimeAfter', 'listFeatureGroups_creationTimeAfter' - Use this parameter to search for @FeatureGroups@s created after a
-- specific date and time.
--
-- 'creationTimeBefore', 'listFeatureGroups_creationTimeBefore' - Use this parameter to search for @FeatureGroups@s created before a
-- specific date and time.
--
-- 'featureGroupStatusEquals', 'listFeatureGroups_featureGroupStatusEquals' - A @FeatureGroup@ status. Filters by @FeatureGroup@ status.
--
-- 'maxResults', 'listFeatureGroups_maxResults' - The maximum number of results returned by @ListFeatureGroups@.
--
-- 'nameContains', 'listFeatureGroups_nameContains' - A string that partially matches one or more @FeatureGroup@s names.
-- Filters @FeatureGroup@s by name.
--
-- 'nextToken', 'listFeatureGroups_nextToken' - A token to resume pagination of @ListFeatureGroups@ results.
--
-- 'offlineStoreStatusEquals', 'listFeatureGroups_offlineStoreStatusEquals' - An @OfflineStore@ status. Filters by @OfflineStore@ status.
--
-- 'sortBy', 'listFeatureGroups_sortBy' - The value on which the feature group list is sorted.
--
-- 'sortOrder', 'listFeatureGroups_sortOrder' - The order in which feature groups are listed.
newListFeatureGroups ::
  ListFeatureGroups
newListFeatureGroups =
  ListFeatureGroups'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      featureGroupStatusEquals = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      offlineStoreStatusEquals = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | Use this parameter to search for @FeatureGroups@s created after a
-- specific date and time.
listFeatureGroups_creationTimeAfter :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.UTCTime)
listFeatureGroups_creationTimeAfter = Lens.lens (\ListFeatureGroups' {creationTimeAfter} -> creationTimeAfter) (\s@ListFeatureGroups' {} a -> s {creationTimeAfter = a} :: ListFeatureGroups) Prelude.. Lens.mapping Data._Time

-- | Use this parameter to search for @FeatureGroups@s created before a
-- specific date and time.
listFeatureGroups_creationTimeBefore :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.UTCTime)
listFeatureGroups_creationTimeBefore = Lens.lens (\ListFeatureGroups' {creationTimeBefore} -> creationTimeBefore) (\s@ListFeatureGroups' {} a -> s {creationTimeBefore = a} :: ListFeatureGroups) Prelude.. Lens.mapping Data._Time

-- | A @FeatureGroup@ status. Filters by @FeatureGroup@ status.
listFeatureGroups_featureGroupStatusEquals :: Lens.Lens' ListFeatureGroups (Prelude.Maybe FeatureGroupStatus)
listFeatureGroups_featureGroupStatusEquals = Lens.lens (\ListFeatureGroups' {featureGroupStatusEquals} -> featureGroupStatusEquals) (\s@ListFeatureGroups' {} a -> s {featureGroupStatusEquals = a} :: ListFeatureGroups)

-- | The maximum number of results returned by @ListFeatureGroups@.
listFeatureGroups_maxResults :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.Natural)
listFeatureGroups_maxResults = Lens.lens (\ListFeatureGroups' {maxResults} -> maxResults) (\s@ListFeatureGroups' {} a -> s {maxResults = a} :: ListFeatureGroups)

-- | A string that partially matches one or more @FeatureGroup@s names.
-- Filters @FeatureGroup@s by name.
listFeatureGroups_nameContains :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.Text)
listFeatureGroups_nameContains = Lens.lens (\ListFeatureGroups' {nameContains} -> nameContains) (\s@ListFeatureGroups' {} a -> s {nameContains = a} :: ListFeatureGroups)

-- | A token to resume pagination of @ListFeatureGroups@ results.
listFeatureGroups_nextToken :: Lens.Lens' ListFeatureGroups (Prelude.Maybe Prelude.Text)
listFeatureGroups_nextToken = Lens.lens (\ListFeatureGroups' {nextToken} -> nextToken) (\s@ListFeatureGroups' {} a -> s {nextToken = a} :: ListFeatureGroups)

-- | An @OfflineStore@ status. Filters by @OfflineStore@ status.
listFeatureGroups_offlineStoreStatusEquals :: Lens.Lens' ListFeatureGroups (Prelude.Maybe OfflineStoreStatusValue)
listFeatureGroups_offlineStoreStatusEquals = Lens.lens (\ListFeatureGroups' {offlineStoreStatusEquals} -> offlineStoreStatusEquals) (\s@ListFeatureGroups' {} a -> s {offlineStoreStatusEquals = a} :: ListFeatureGroups)

-- | The value on which the feature group list is sorted.
listFeatureGroups_sortBy :: Lens.Lens' ListFeatureGroups (Prelude.Maybe FeatureGroupSortBy)
listFeatureGroups_sortBy = Lens.lens (\ListFeatureGroups' {sortBy} -> sortBy) (\s@ListFeatureGroups' {} a -> s {sortBy = a} :: ListFeatureGroups)

-- | The order in which feature groups are listed.
listFeatureGroups_sortOrder :: Lens.Lens' ListFeatureGroups (Prelude.Maybe FeatureGroupSortOrder)
listFeatureGroups_sortOrder = Lens.lens (\ListFeatureGroups' {sortOrder} -> sortOrder) (\s@ListFeatureGroups' {} a -> s {sortOrder = a} :: ListFeatureGroups)

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
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` featureGroupStatusEquals
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` offlineStoreStatusEquals
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder

instance Prelude.NFData ListFeatureGroups where
  rnf ListFeatureGroups' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf featureGroupStatusEquals
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf offlineStoreStatusEquals
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

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
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("FeatureGroupStatusEquals" Data..=)
              Prelude.<$> featureGroupStatusEquals,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("OfflineStoreStatusEquals" Data..=)
              Prelude.<$> offlineStoreStatusEquals,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder
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
