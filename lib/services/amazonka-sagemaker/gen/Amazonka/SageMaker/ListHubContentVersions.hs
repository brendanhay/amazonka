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
-- Module      : Amazonka.SageMaker.ListHubContentVersions
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- List hub content versions.
module Amazonka.SageMaker.ListHubContentVersions
  ( -- * Creating a Request
    ListHubContentVersions (..),
    newListHubContentVersions,

    -- * Request Lenses
    listHubContentVersions_creationTimeAfter,
    listHubContentVersions_creationTimeBefore,
    listHubContentVersions_maxResults,
    listHubContentVersions_maxSchemaVersion,
    listHubContentVersions_minVersion,
    listHubContentVersions_nextToken,
    listHubContentVersions_sortBy,
    listHubContentVersions_sortOrder,
    listHubContentVersions_hubName,
    listHubContentVersions_hubContentType,
    listHubContentVersions_hubContentName,

    -- * Destructuring the Response
    ListHubContentVersionsResponse (..),
    newListHubContentVersionsResponse,

    -- * Response Lenses
    listHubContentVersionsResponse_nextToken,
    listHubContentVersionsResponse_httpStatus,
    listHubContentVersionsResponse_hubContentSummaries,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListHubContentVersions' smart constructor.
data ListHubContentVersions = ListHubContentVersions'
  { -- | Only list hub content versions that were created after the time
    -- specified.
    creationTimeAfter :: Prelude.Maybe Data.POSIX,
    -- | Only list hub content versions that were created before the time
    -- specified.
    creationTimeBefore :: Prelude.Maybe Data.POSIX,
    -- | The maximum number of hub content versions to list.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The upper bound of the hub content schema version.
    maxSchemaVersion :: Prelude.Maybe Prelude.Text,
    -- | The lower bound of the hub content versions to list.
    minVersion :: Prelude.Maybe Prelude.Text,
    -- | If the response to a previous @ListHubContentVersions@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of hub content versions, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Sort hub content versions by either name or creation time.
    sortBy :: Prelude.Maybe HubContentSortBy,
    -- | Sort hub content versions by ascending or descending order.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | The name of the hub to list the content versions of.
    hubName :: Prelude.Text,
    -- | The type of hub content to list versions of.
    hubContentType :: HubContentType,
    -- | The name of the hub content.
    hubContentName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHubContentVersions' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'creationTimeAfter', 'listHubContentVersions_creationTimeAfter' - Only list hub content versions that were created after the time
-- specified.
--
-- 'creationTimeBefore', 'listHubContentVersions_creationTimeBefore' - Only list hub content versions that were created before the time
-- specified.
--
-- 'maxResults', 'listHubContentVersions_maxResults' - The maximum number of hub content versions to list.
--
-- 'maxSchemaVersion', 'listHubContentVersions_maxSchemaVersion' - The upper bound of the hub content schema version.
--
-- 'minVersion', 'listHubContentVersions_minVersion' - The lower bound of the hub content versions to list.
--
-- 'nextToken', 'listHubContentVersions_nextToken' - If the response to a previous @ListHubContentVersions@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of hub content versions, use the token in the next request.
--
-- 'sortBy', 'listHubContentVersions_sortBy' - Sort hub content versions by either name or creation time.
--
-- 'sortOrder', 'listHubContentVersions_sortOrder' - Sort hub content versions by ascending or descending order.
--
-- 'hubName', 'listHubContentVersions_hubName' - The name of the hub to list the content versions of.
--
-- 'hubContentType', 'listHubContentVersions_hubContentType' - The type of hub content to list versions of.
--
-- 'hubContentName', 'listHubContentVersions_hubContentName' - The name of the hub content.
newListHubContentVersions ::
  -- | 'hubName'
  Prelude.Text ->
  -- | 'hubContentType'
  HubContentType ->
  -- | 'hubContentName'
  Prelude.Text ->
  ListHubContentVersions
newListHubContentVersions
  pHubName_
  pHubContentType_
  pHubContentName_ =
    ListHubContentVersions'
      { creationTimeAfter =
          Prelude.Nothing,
        creationTimeBefore = Prelude.Nothing,
        maxResults = Prelude.Nothing,
        maxSchemaVersion = Prelude.Nothing,
        minVersion = Prelude.Nothing,
        nextToken = Prelude.Nothing,
        sortBy = Prelude.Nothing,
        sortOrder = Prelude.Nothing,
        hubName = pHubName_,
        hubContentType = pHubContentType_,
        hubContentName = pHubContentName_
      }

-- | Only list hub content versions that were created after the time
-- specified.
listHubContentVersions_creationTimeAfter :: Lens.Lens' ListHubContentVersions (Prelude.Maybe Prelude.UTCTime)
listHubContentVersions_creationTimeAfter = Lens.lens (\ListHubContentVersions' {creationTimeAfter} -> creationTimeAfter) (\s@ListHubContentVersions' {} a -> s {creationTimeAfter = a} :: ListHubContentVersions) Prelude.. Lens.mapping Data._Time

-- | Only list hub content versions that were created before the time
-- specified.
listHubContentVersions_creationTimeBefore :: Lens.Lens' ListHubContentVersions (Prelude.Maybe Prelude.UTCTime)
listHubContentVersions_creationTimeBefore = Lens.lens (\ListHubContentVersions' {creationTimeBefore} -> creationTimeBefore) (\s@ListHubContentVersions' {} a -> s {creationTimeBefore = a} :: ListHubContentVersions) Prelude.. Lens.mapping Data._Time

-- | The maximum number of hub content versions to list.
listHubContentVersions_maxResults :: Lens.Lens' ListHubContentVersions (Prelude.Maybe Prelude.Natural)
listHubContentVersions_maxResults = Lens.lens (\ListHubContentVersions' {maxResults} -> maxResults) (\s@ListHubContentVersions' {} a -> s {maxResults = a} :: ListHubContentVersions)

-- | The upper bound of the hub content schema version.
listHubContentVersions_maxSchemaVersion :: Lens.Lens' ListHubContentVersions (Prelude.Maybe Prelude.Text)
listHubContentVersions_maxSchemaVersion = Lens.lens (\ListHubContentVersions' {maxSchemaVersion} -> maxSchemaVersion) (\s@ListHubContentVersions' {} a -> s {maxSchemaVersion = a} :: ListHubContentVersions)

-- | The lower bound of the hub content versions to list.
listHubContentVersions_minVersion :: Lens.Lens' ListHubContentVersions (Prelude.Maybe Prelude.Text)
listHubContentVersions_minVersion = Lens.lens (\ListHubContentVersions' {minVersion} -> minVersion) (\s@ListHubContentVersions' {} a -> s {minVersion = a} :: ListHubContentVersions)

-- | If the response to a previous @ListHubContentVersions@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of hub content versions, use the token in the next request.
listHubContentVersions_nextToken :: Lens.Lens' ListHubContentVersions (Prelude.Maybe Prelude.Text)
listHubContentVersions_nextToken = Lens.lens (\ListHubContentVersions' {nextToken} -> nextToken) (\s@ListHubContentVersions' {} a -> s {nextToken = a} :: ListHubContentVersions)

-- | Sort hub content versions by either name or creation time.
listHubContentVersions_sortBy :: Lens.Lens' ListHubContentVersions (Prelude.Maybe HubContentSortBy)
listHubContentVersions_sortBy = Lens.lens (\ListHubContentVersions' {sortBy} -> sortBy) (\s@ListHubContentVersions' {} a -> s {sortBy = a} :: ListHubContentVersions)

-- | Sort hub content versions by ascending or descending order.
listHubContentVersions_sortOrder :: Lens.Lens' ListHubContentVersions (Prelude.Maybe SortOrder)
listHubContentVersions_sortOrder = Lens.lens (\ListHubContentVersions' {sortOrder} -> sortOrder) (\s@ListHubContentVersions' {} a -> s {sortOrder = a} :: ListHubContentVersions)

-- | The name of the hub to list the content versions of.
listHubContentVersions_hubName :: Lens.Lens' ListHubContentVersions Prelude.Text
listHubContentVersions_hubName = Lens.lens (\ListHubContentVersions' {hubName} -> hubName) (\s@ListHubContentVersions' {} a -> s {hubName = a} :: ListHubContentVersions)

-- | The type of hub content to list versions of.
listHubContentVersions_hubContentType :: Lens.Lens' ListHubContentVersions HubContentType
listHubContentVersions_hubContentType = Lens.lens (\ListHubContentVersions' {hubContentType} -> hubContentType) (\s@ListHubContentVersions' {} a -> s {hubContentType = a} :: ListHubContentVersions)

-- | The name of the hub content.
listHubContentVersions_hubContentName :: Lens.Lens' ListHubContentVersions Prelude.Text
listHubContentVersions_hubContentName = Lens.lens (\ListHubContentVersions' {hubContentName} -> hubContentName) (\s@ListHubContentVersions' {} a -> s {hubContentName = a} :: ListHubContentVersions)

instance Core.AWSRequest ListHubContentVersions where
  type
    AWSResponse ListHubContentVersions =
      ListHubContentVersionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListHubContentVersionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x
                            Data..?> "HubContentSummaries"
                            Core..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListHubContentVersions where
  hashWithSalt _salt ListHubContentVersions' {..} =
    _salt
      `Prelude.hashWithSalt` creationTimeAfter
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` maxSchemaVersion
      `Prelude.hashWithSalt` minVersion
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` hubName
      `Prelude.hashWithSalt` hubContentType
      `Prelude.hashWithSalt` hubContentName

instance Prelude.NFData ListHubContentVersions where
  rnf ListHubContentVersions' {..} =
    Prelude.rnf creationTimeAfter `Prelude.seq`
      Prelude.rnf creationTimeBefore `Prelude.seq`
        Prelude.rnf maxResults `Prelude.seq`
          Prelude.rnf maxSchemaVersion `Prelude.seq`
            Prelude.rnf minVersion `Prelude.seq`
              Prelude.rnf nextToken `Prelude.seq`
                Prelude.rnf sortBy `Prelude.seq`
                  Prelude.rnf sortOrder `Prelude.seq`
                    Prelude.rnf hubName `Prelude.seq`
                      Prelude.rnf hubContentType `Prelude.seq`
                        Prelude.rnf hubContentName

instance Data.ToHeaders ListHubContentVersions where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "SageMaker.ListHubContentVersions" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON ListHubContentVersions where
  toJSON ListHubContentVersions' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CreationTimeAfter" Data..=)
              Prelude.<$> creationTimeAfter,
            ("CreationTimeBefore" Data..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("MaxSchemaVersion" Data..=)
              Prelude.<$> maxSchemaVersion,
            ("MinVersion" Data..=) Prelude.<$> minVersion,
            ("NextToken" Data..=) Prelude.<$> nextToken,
            ("SortBy" Data..=) Prelude.<$> sortBy,
            ("SortOrder" Data..=) Prelude.<$> sortOrder,
            Prelude.Just ("HubName" Data..= hubName),
            Prelude.Just
              ("HubContentType" Data..= hubContentType),
            Prelude.Just
              ("HubContentName" Data..= hubContentName)
          ]
      )

instance Data.ToPath ListHubContentVersions where
  toPath = Prelude.const "/"

instance Data.ToQuery ListHubContentVersions where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListHubContentVersionsResponse' smart constructor.
data ListHubContentVersionsResponse = ListHubContentVersionsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of hub content versions, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | The summaries of the listed hub content versions.
    hubContentSummaries :: [HubContentInfo]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListHubContentVersionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listHubContentVersionsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of hub content versions, use it in the subsequent request.
--
-- 'httpStatus', 'listHubContentVersionsResponse_httpStatus' - The response's http status code.
--
-- 'hubContentSummaries', 'listHubContentVersionsResponse_hubContentSummaries' - The summaries of the listed hub content versions.
newListHubContentVersionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListHubContentVersionsResponse
newListHubContentVersionsResponse pHttpStatus_ =
  ListHubContentVersionsResponse'
    { nextToken =
        Prelude.Nothing,
      httpStatus = pHttpStatus_,
      hubContentSummaries = Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of hub content versions, use it in the subsequent request.
listHubContentVersionsResponse_nextToken :: Lens.Lens' ListHubContentVersionsResponse (Prelude.Maybe Prelude.Text)
listHubContentVersionsResponse_nextToken = Lens.lens (\ListHubContentVersionsResponse' {nextToken} -> nextToken) (\s@ListHubContentVersionsResponse' {} a -> s {nextToken = a} :: ListHubContentVersionsResponse)

-- | The response's http status code.
listHubContentVersionsResponse_httpStatus :: Lens.Lens' ListHubContentVersionsResponse Prelude.Int
listHubContentVersionsResponse_httpStatus = Lens.lens (\ListHubContentVersionsResponse' {httpStatus} -> httpStatus) (\s@ListHubContentVersionsResponse' {} a -> s {httpStatus = a} :: ListHubContentVersionsResponse)

-- | The summaries of the listed hub content versions.
listHubContentVersionsResponse_hubContentSummaries :: Lens.Lens' ListHubContentVersionsResponse [HubContentInfo]
listHubContentVersionsResponse_hubContentSummaries = Lens.lens (\ListHubContentVersionsResponse' {hubContentSummaries} -> hubContentSummaries) (\s@ListHubContentVersionsResponse' {} a -> s {hubContentSummaries = a} :: ListHubContentVersionsResponse) Prelude.. Lens.coerced

instance
  Prelude.NFData
    ListHubContentVersionsResponse
  where
  rnf ListHubContentVersionsResponse' {..} =
    Prelude.rnf nextToken `Prelude.seq`
      Prelude.rnf httpStatus `Prelude.seq`
        Prelude.rnf hubContentSummaries
