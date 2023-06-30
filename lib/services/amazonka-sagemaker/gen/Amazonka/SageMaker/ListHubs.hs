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
-- Module      : Amazonka.SageMaker.ListHubs
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List all existing hubs.
module Amazonka.SageMaker.ListHubs
  ( -- * Creating a Request
    ListHubs (..),
    newListHubs,

    -- * Request Lenses
    listHubs_creationTimeAfter,
    listHubs_creationTimeBefore,
    listHubs_lastModifiedTimeAfter,
    listHubs_lastModifiedTimeBefore,
    listHubs_maxResults,
    listHubs_nameContains,
    listHubs_nextToken,
    listHubs_sortBy,
    listHubs_sortOrder,

    -- * Destructuring the Response
    ListHubsResponse (..),
    newListHubsResponse,

    -- * Response Lenses
    listHubsResponse_nextToken,
    listHubsResponse_httpStatus,
    listHubsResponse_hubSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListHubs' smart constructor.
data ListHubs = ListHubs'
  { -- | Only list hubs that were created after the time specified.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Only list hubs that were created before the time specified.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | Only list hubs that were last modified after the time specified.
    lastModifiedTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Only list hubs that were last modified before the time specified.
    lastModifiedTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of hubs to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Only list hubs with names that contain the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the response to a previous @ListHubs@ request was truncated, the
    -- response includes a @NextToken@. To retrieve the next set of hubs, use
    -- the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort hubs by either name or creation time.
    sortBy :: Prelude.Maybe HubSortBy,
    -- | Sort hubs by ascending or descending order.
    sortOrder :: Prelude.Maybe SortOrder
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHubs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listHubs_creationTimeAfter' - Only list hubs that were created after the time specified.
--
-- 'creationTimeBefore', 'listHubs_creationTimeBefore' - Only list hubs that were created before the time specified.
--
-- 'lastModifiedTimeAfter', 'listHubs_lastModifiedTimeAfter' - Only list hubs that were last modified after the time specified.
--
-- 'lastModifiedTimeBefore', 'listHubs_lastModifiedTimeBefore' - Only list hubs that were last modified before the time specified.
--
-- 'maxResults', 'listHubs_maxResults' - The maximum number of hubs to list.
--
-- 'nameContains', 'listHubs_nameContains' - Only list hubs with names that contain the specified string.
--
-- 'nextToken', 'listHubs_nextToken' - If the response to a previous @ListHubs@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of hubs, use
-- the token in the next request.
--
-- 'sortBy', 'listHubs_sortBy' - Sort hubs by either name or creation time.
--
-- 'sortOrder', 'listHubs_sortOrder' - Sort hubs by ascending or descending order.
newListHubs ::
  ListHubs
newListHubs =
  ListHubs'
    { creationTimeAfter = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing
    }

-- | Only list hubs that were created after the time specified.
listHubs_creationTimeAfter :: Lens.Lens' ListHubs (Prelude.Maybe Prelude.UTCTime)
listHubs_creationTimeAfter = Lens.lens (\ListHubs' {creationTimeAfter} -> creationTimeAfter) (\s@ListHubs' {} a -> s {creationTimeAfter = a} :: ListHubs) Prelude.. Lens.mapping Data._Time

-- | Only list hubs that were created before the time specified.
listHubs_creationTimeBefore :: Lens.Lens' ListHubs (Prelude.Maybe Prelude.UTCTime)
listHubs_creationTimeBefore = Lens.lens (\ListHubs' {creationTimeBefore} -> creationTimeBefore) (\s@ListHubs' {} a -> s {creationTimeBefore = a} :: ListHubs) Prelude.. Lens.mapping Data._Time

-- | Only list hubs that were last modified after the time specified.
listHubs_lastModifiedTimeAfter :: Lens.Lens' ListHubs (Prelude.Maybe Prelude.UTCTime)
listHubs_lastModifiedTimeAfter = Lens.lens (\ListHubs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListHubs' {} a -> s {lastModifiedTimeAfter = a} :: ListHubs) Prelude.. Lens.mapping Data._Time

-- | Only list hubs that were last modified before the time specified.
listHubs_lastModifiedTimeBefore :: Lens.Lens' ListHubs (Prelude.Maybe Prelude.UTCTime)
listHubs_lastModifiedTimeBefore = Lens.lens (\ListHubs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListHubs' {} a -> s {lastModifiedTimeBefore = a} :: ListHubs) Prelude.. Lens.mapping Data._Time

-- | The maximum number of hubs to list.
listHubs_maxResults :: Lens.Lens' ListHubs (Prelude.Maybe Prelude.Natural)
listHubs_maxResults = Lens.lens (\ListHubs' {maxResults} -> maxResults) (\s@ListHubs' {} a -> s {maxResults = a} :: ListHubs)

-- | Only list hubs with names that contain the specified string.
listHubs_nameContains :: Lens.Lens' ListHubs (Prelude.Maybe Prelude.Text)
listHubs_nameContains = Lens.lens (\ListHubs' {nameContains} -> nameContains) (\s@ListHubs' {} a -> s {nameContains = a} :: ListHubs)

-- | If the response to a previous @ListHubs@ request was truncated, the
-- response includes a @NextToken@. To retrieve the next set of hubs, use
-- the token in the next request.
listHubs_nextToken :: Lens.Lens' ListHubs (Prelude.Maybe Prelude.Text)
listHubs_nextToken = Lens.lens (\ListHubs' {nextToken} -> nextToken) (\s@ListHubs' {} a -> s {nextToken = a} :: ListHubs)

-- | Sort hubs by either name or creation time.
listHubs_sortBy :: Lens.Lens' ListHubs (Prelude.Maybe HubSortBy)
listHubs_sortBy = Lens.lens (\ListHubs' {sortBy} -> sortBy) (\s@ListHubs' {} a -> s {sortBy = a} :: ListHubs)

-- | Sort hubs by ascending or descending order.
listHubs_sortOrder :: Lens.Lens' ListHubs (Prelude.Maybe SortOrder)
listHubs_sortOrder = Lens.lens (\ListHubs' {sortOrder} -> sortOrder) (\s@ListHubs' {} a -> s {sortOrder = a} :: ListHubs)

instance Core.AWSRequest ListHubs where
  type AWSResponse ListHubs = ListHubsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHubsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Data..?> "HubSummaries" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListHubs where
  hashWithSalt _salt ListHubs' {..} =
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

instance Prelude.NFData ListHubs where
  rnf ListHubs' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder

instance Data.ToHeaders ListHubs where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListHubs" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListHubs where
  toJSON ListHubs' {..} =
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
            ("SortOrder" Data..=) Prelude.<$> sortOrder
          ]
      )

instance Data.ToPath ListHubs where
  toPath = Prelude.const "/"

instance Data.ToQuery ListHubs where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHubsResponse' smart constructor.
data ListHubsResponse = ListHubsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of hubs, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summaries of the listed hubs.
    hubSummaries :: [HubInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHubsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHubsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of hubs, use it in the subsequent request.
--
-- 'httpStatus', 'listHubsResponse_httpStatus' - The response's http status code.
--
-- 'hubSummaries', 'listHubsResponse_hubSummaries' - The summaries of the listed hubs.
newListHubsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHubsResponse
newListHubsResponse pHttpStatus_ =
  ListHubsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      hubSummaries = Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of hubs, use it in the subsequent request.
listHubsResponse_nextToken :: Lens.Lens' ListHubsResponse (Prelude.Maybe Prelude.Text)
listHubsResponse_nextToken = Lens.lens (\ListHubsResponse' {nextToken} -> nextToken) (\s@ListHubsResponse' {} a -> s {nextToken = a} :: ListHubsResponse)

-- | The response's http status code.
listHubsResponse_httpStatus :: Lens.Lens' ListHubsResponse Prelude.Int
listHubsResponse_httpStatus = Lens.lens (\ListHubsResponse' {httpStatus} -> httpStatus) (\s@ListHubsResponse' {} a -> s {httpStatus = a} :: ListHubsResponse)

-- | The summaries of the listed hubs.
listHubsResponse_hubSummaries :: Lens.Lens' ListHubsResponse [HubInfo]
listHubsResponse_hubSummaries = Lens.lens (\ListHubsResponse' {hubSummaries} -> hubSummaries) (\s@ListHubsResponse' {} a -> s {hubSummaries = a} :: ListHubsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListHubsResponse where
  rnf ListHubsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hubSummaries
