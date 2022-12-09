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
-- Module      : Amazonka.SageMaker.ListHubContents
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List the contents of a hub.
module Amazonka.SageMaker.ListHubContents
  ( -- * Creating a Request
    ListHubContents (..),
    newListHubContents,

    -- * Request Lenses
    listHubContents_creationTimeAfter,
    listHubContents_creationTimeBefore,
    listHubContents_maxResults,
    listHubContents_maxSchemaVersion,
    listHubContents_nameContains,
    listHubContents_nextToken,
    listHubContents_sortBy,
    listHubContents_sortOrder,
    listHubContents_hubName,
    listHubContents_hubContentType,

    -- * Destructuring the Response
    ListHubContentsResponse (..),
    newListHubContentsResponse,

    -- * Response Lenses
    listHubContentsResponse_nextToken,
    listHubContentsResponse_httpStatus,
    listHubContentsResponse_hubContentSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListHubContents' smart constructor.
data ListHubContents = ListHubContents'
  { -- | Only list hub content that was created after the time specified.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Only list hub content that was created before the time specified.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum amount of hub content to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The upper bound of the hub content schema verion.
    maxSchemaVersion :: Prelude.Maybe Prelude.Text,
    -- | Only list hub content if the name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | If the response to a previous @ListHubContents@ request was truncated,
    -- the response includes a @NextToken@. To retrieve the next set of hub
    -- content, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort hub content versions by either name or creation time.
    sortBy :: Prelude.Maybe HubContentSortBy,
    -- | Sort hubs by ascending or descending order.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The name of the hub to list the contents of.
    hubName :: Prelude.Text,
    -- | The type of hub content to list.
    hubContentType :: HubContentType
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHubContents' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listHubContents_creationTimeAfter' - Only list hub content that was created after the time specified.
--
-- 'creationTimeBefore', 'listHubContents_creationTimeBefore' - Only list hub content that was created before the time specified.
--
-- 'maxResults', 'listHubContents_maxResults' - The maximum amount of hub content to list.
--
-- 'maxSchemaVersion', 'listHubContents_maxSchemaVersion' - The upper bound of the hub content schema verion.
--
-- 'nameContains', 'listHubContents_nameContains' - Only list hub content if the name contains the specified string.
--
-- 'nextToken', 'listHubContents_nextToken' - If the response to a previous @ListHubContents@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of hub
-- content, use the token in the next request.
--
-- 'sortBy', 'listHubContents_sortBy' - Sort hub content versions by either name or creation time.
--
-- 'sortOrder', 'listHubContents_sortOrder' - Sort hubs by ascending or descending order.
--
-- 'hubName', 'listHubContents_hubName' - The name of the hub to list the contents of.
--
-- 'hubContentType', 'listHubContents_hubContentType' - The type of hub content to list.
newListHubContents ::
  -- | 'hubName'
  Prelude.Text ->
  -- | 'hubContentType'
  HubContentType ->
  ListHubContents
newListHubContents pHubName_ pHubContentType_ =
  ListHubContents'
    { creationTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      maxSchemaVersion = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      hubName = pHubName_,
      hubContentType = pHubContentType_
    }

-- | Only list hub content that was created after the time specified.
listHubContents_creationTimeAfter :: Lens.Lens' ListHubContents (Prelude.Maybe Prelude.UTCTime)
listHubContents_creationTimeAfter = Lens.lens (\ListHubContents' {creationTimeAfter} -> creationTimeAfter) (\s@ListHubContents' {} a -> s {creationTimeAfter = a} :: ListHubContents) Prelude.. Lens.mapping Data._Time

-- | Only list hub content that was created before the time specified.
listHubContents_creationTimeBefore :: Lens.Lens' ListHubContents (Prelude.Maybe Prelude.UTCTime)
listHubContents_creationTimeBefore = Lens.lens (\ListHubContents' {creationTimeBefore} -> creationTimeBefore) (\s@ListHubContents' {} a -> s {creationTimeBefore = a} :: ListHubContents) Prelude.. Lens.mapping Data._Time

-- | The maximum amount of hub content to list.
listHubContents_maxResults :: Lens.Lens' ListHubContents (Prelude.Maybe Prelude.Natural)
listHubContents_maxResults = Lens.lens (\ListHubContents' {maxResults} -> maxResults) (\s@ListHubContents' {} a -> s {maxResults = a} :: ListHubContents)

-- | The upper bound of the hub content schema verion.
listHubContents_maxSchemaVersion :: Lens.Lens' ListHubContents (Prelude.Maybe Prelude.Text)
listHubContents_maxSchemaVersion = Lens.lens (\ListHubContents' {maxSchemaVersion} -> maxSchemaVersion) (\s@ListHubContents' {} a -> s {maxSchemaVersion = a} :: ListHubContents)

-- | Only list hub content if the name contains the specified string.
listHubContents_nameContains :: Lens.Lens' ListHubContents (Prelude.Maybe Prelude.Text)
listHubContents_nameContains = Lens.lens (\ListHubContents' {nameContains} -> nameContains) (\s@ListHubContents' {} a -> s {nameContains = a} :: ListHubContents)

-- | If the response to a previous @ListHubContents@ request was truncated,
-- the response includes a @NextToken@. To retrieve the next set of hub
-- content, use the token in the next request.
listHubContents_nextToken :: Lens.Lens' ListHubContents (Prelude.Maybe Prelude.Text)
listHubContents_nextToken = Lens.lens (\ListHubContents' {nextToken} -> nextToken) (\s@ListHubContents' {} a -> s {nextToken = a} :: ListHubContents)

-- | Sort hub content versions by either name or creation time.
listHubContents_sortBy :: Lens.Lens' ListHubContents (Prelude.Maybe HubContentSortBy)
listHubContents_sortBy = Lens.lens (\ListHubContents' {sortBy} -> sortBy) (\s@ListHubContents' {} a -> s {sortBy = a} :: ListHubContents)

-- | Sort hubs by ascending or descending order.
listHubContents_sortOrder :: Lens.Lens' ListHubContents (Prelude.Maybe SortOrder)
listHubContents_sortOrder = Lens.lens (\ListHubContents' {sortOrder} -> sortOrder) (\s@ListHubContents' {} a -> s {sortOrder = a} :: ListHubContents)

-- | The name of the hub to list the contents of.
listHubContents_hubName :: Lens.Lens' ListHubContents Prelude.Text
listHubContents_hubName = Lens.lens (\ListHubContents' {hubName} -> hubName) (\s@ListHubContents' {} a -> s {hubName = a} :: ListHubContents)

-- | The type of hub content to list.
listHubContents_hubContentType :: Lens.Lens' ListHubContents HubContentType
listHubContents_hubContentType = Lens.lens (\ListHubContents' {hubContentType} -> hubContentType) (\s@ListHubContents' {} a -> s {hubContentType = a} :: ListHubContents)

instance Core.AWSRequest ListHubContents where
  type
    AWSResponse ListHubContents =
      ListHubContentsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHubContentsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Data..?> "HubContentSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListHubContents where
  hashWithSalt _salt ListHubContents' {..} =
    _salt `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` maxSchemaVersion
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` hubName
      `Prelude.hashWithSalt` hubContentType

instance Prelude.NFData ListHubContents where
  rnf ListHubContents' {..} =
    Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf maxSchemaVersion
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf hubName
      `Prelude.seq` Prelude.rnf hubContentType

instance Data.ToHeaders ListHubContents where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("SageMaker.ListHubContents" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListHubContents where
  toJSON ListHubContents' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("MaxSchemaVersion" Data..=)
              Prelude.<$> maxSchemaVersion,
            ("NameContains" Data..=) Prelude.<$> nameContains,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            Prelude.Just ("HubName" Data..= hubName),
            Prelude.Just
              ("HubContentType" Data..= hubContentType)
          ]
      )

instance Data.ToPath ListHubContents where
  toPath = Prelude.const "/"

instance Data.ToQuery ListHubContents where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHubContentsResponse' smart constructor.
data ListHubContentsResponse = ListHubContentsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of hub content, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summaries of the listed hub content.
    hubContentSummaries :: [HubContentInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHubContentsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHubContentsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of hub content, use it in the subsequent request.
--
-- 'httpStatus', 'listHubContentsResponse_httpStatus' - The response's http status code.
--
-- 'hubContentSummaries', 'listHubContentsResponse_hubContentSummaries' - The summaries of the listed hub content.
newListHubContentsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHubContentsResponse
newListHubContentsResponse pHttpStatus_ =
  ListHubContentsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      hubContentSummaries = Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of hub content, use it in the subsequent request.
listHubContentsResponse_nextToken :: Lens.Lens' ListHubContentsResponse (Prelude.Maybe Prelude.Text)
listHubContentsResponse_nextToken = Lens.lens (\ListHubContentsResponse' {nextToken} -> nextToken) (\s@ListHubContentsResponse' {} a -> s {nextToken = a} :: ListHubContentsResponse)

-- | The response's http status code.
listHubContentsResponse_httpStatus :: Lens.Lens' ListHubContentsResponse Prelude.Int
listHubContentsResponse_httpStatus = Lens.lens (\ListHubContentsResponse' {httpStatus} -> httpStatus) (\s@ListHubContentsResponse' {} a -> s {httpStatus = a} :: ListHubContentsResponse)

-- | The summaries of the listed hub content.
listHubContentsResponse_hubContentSummaries :: Lens.Lens' ListHubContentsResponse [HubContentInfo]
listHubContentsResponse_hubContentSummaries = Lens.lens (\ListHubContentsResponse' {hubContentSummaries} -> hubContentSummaries) (\s@ListHubContentsResponse' {} a -> s {hubContentSummaries = a} :: ListHubContentsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListHubContentsResponse where
  rnf ListHubContentsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf hubContentSummaries
