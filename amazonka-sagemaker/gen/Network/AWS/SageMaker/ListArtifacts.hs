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
-- Module      : Network.AWS.SageMaker.ListArtifacts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the artifacts in your account and their properties.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListArtifacts
  ( -- * Creating a Request
    ListArtifacts (..),
    newListArtifacts,

    -- * Request Lenses
    listArtifacts_createdAfter,
    listArtifacts_sortOrder,
    listArtifacts_nextToken,
    listArtifacts_createdBefore,
    listArtifacts_artifactType,
    listArtifacts_maxResults,
    listArtifacts_sourceUri,
    listArtifacts_sortBy,

    -- * Destructuring the Response
    ListArtifactsResponse (..),
    newListArtifactsResponse,

    -- * Response Lenses
    listArtifactsResponse_nextToken,
    listArtifactsResponse_artifactSummaries,
    listArtifactsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListArtifacts' smart constructor.
data ListArtifacts = ListArtifacts'
  { -- | A filter that returns only artifacts created on or after the specified
    -- time.
    createdAfter :: Core.Maybe Core.POSIX,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the previous call to @ListArtifacts@ didn\'t return the full set of
    -- artifacts, the call returns a token for getting the next set of
    -- artifacts.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that returns only artifacts created on or before the specified
    -- time.
    createdBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only artifacts of the specified type.
    artifactType :: Core.Maybe Core.Text,
    -- | The maximum number of artifacts to return in the response. The default
    -- value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only artifacts with the specified source URI.
    sourceUri :: Core.Maybe Core.Text,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Core.Maybe SortArtifactsBy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListArtifacts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'createdAfter', 'listArtifacts_createdAfter' - A filter that returns only artifacts created on or after the specified
-- time.
--
-- 'sortOrder', 'listArtifacts_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'nextToken', 'listArtifacts_nextToken' - If the previous call to @ListArtifacts@ didn\'t return the full set of
-- artifacts, the call returns a token for getting the next set of
-- artifacts.
--
-- 'createdBefore', 'listArtifacts_createdBefore' - A filter that returns only artifacts created on or before the specified
-- time.
--
-- 'artifactType', 'listArtifacts_artifactType' - A filter that returns only artifacts of the specified type.
--
-- 'maxResults', 'listArtifacts_maxResults' - The maximum number of artifacts to return in the response. The default
-- value is 10.
--
-- 'sourceUri', 'listArtifacts_sourceUri' - A filter that returns only artifacts with the specified source URI.
--
-- 'sortBy', 'listArtifacts_sortBy' - The property used to sort results. The default value is @CreationTime@.
newListArtifacts ::
  ListArtifacts
newListArtifacts =
  ListArtifacts'
    { createdAfter = Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      createdBefore = Core.Nothing,
      artifactType = Core.Nothing,
      maxResults = Core.Nothing,
      sourceUri = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | A filter that returns only artifacts created on or after the specified
-- time.
listArtifacts_createdAfter :: Lens.Lens' ListArtifacts (Core.Maybe Core.UTCTime)
listArtifacts_createdAfter = Lens.lens (\ListArtifacts' {createdAfter} -> createdAfter) (\s@ListArtifacts' {} a -> s {createdAfter = a} :: ListArtifacts) Core.. Lens.mapping Core._Time

-- | The sort order. The default value is @Descending@.
listArtifacts_sortOrder :: Lens.Lens' ListArtifacts (Core.Maybe SortOrder)
listArtifacts_sortOrder = Lens.lens (\ListArtifacts' {sortOrder} -> sortOrder) (\s@ListArtifacts' {} a -> s {sortOrder = a} :: ListArtifacts)

-- | If the previous call to @ListArtifacts@ didn\'t return the full set of
-- artifacts, the call returns a token for getting the next set of
-- artifacts.
listArtifacts_nextToken :: Lens.Lens' ListArtifacts (Core.Maybe Core.Text)
listArtifacts_nextToken = Lens.lens (\ListArtifacts' {nextToken} -> nextToken) (\s@ListArtifacts' {} a -> s {nextToken = a} :: ListArtifacts)

-- | A filter that returns only artifacts created on or before the specified
-- time.
listArtifacts_createdBefore :: Lens.Lens' ListArtifacts (Core.Maybe Core.UTCTime)
listArtifacts_createdBefore = Lens.lens (\ListArtifacts' {createdBefore} -> createdBefore) (\s@ListArtifacts' {} a -> s {createdBefore = a} :: ListArtifacts) Core.. Lens.mapping Core._Time

-- | A filter that returns only artifacts of the specified type.
listArtifacts_artifactType :: Lens.Lens' ListArtifacts (Core.Maybe Core.Text)
listArtifacts_artifactType = Lens.lens (\ListArtifacts' {artifactType} -> artifactType) (\s@ListArtifacts' {} a -> s {artifactType = a} :: ListArtifacts)

-- | The maximum number of artifacts to return in the response. The default
-- value is 10.
listArtifacts_maxResults :: Lens.Lens' ListArtifacts (Core.Maybe Core.Natural)
listArtifacts_maxResults = Lens.lens (\ListArtifacts' {maxResults} -> maxResults) (\s@ListArtifacts' {} a -> s {maxResults = a} :: ListArtifacts)

-- | A filter that returns only artifacts with the specified source URI.
listArtifacts_sourceUri :: Lens.Lens' ListArtifacts (Core.Maybe Core.Text)
listArtifacts_sourceUri = Lens.lens (\ListArtifacts' {sourceUri} -> sourceUri) (\s@ListArtifacts' {} a -> s {sourceUri = a} :: ListArtifacts)

-- | The property used to sort results. The default value is @CreationTime@.
listArtifacts_sortBy :: Lens.Lens' ListArtifacts (Core.Maybe SortArtifactsBy)
listArtifacts_sortBy = Lens.lens (\ListArtifacts' {sortBy} -> sortBy) (\s@ListArtifacts' {} a -> s {sortBy = a} :: ListArtifacts)

instance Core.AWSPager ListArtifacts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listArtifactsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listArtifactsResponse_artifactSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listArtifacts_nextToken
          Lens..~ rs
          Lens.^? listArtifactsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListArtifacts where
  type
    AWSResponse ListArtifacts =
      ListArtifactsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListArtifactsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ArtifactSummaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListArtifacts

instance Core.NFData ListArtifacts

instance Core.ToHeaders ListArtifacts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListArtifacts" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListArtifacts where
  toJSON ListArtifacts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("ArtifactType" Core..=) Core.<$> artifactType,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SourceUri" Core..=) Core.<$> sourceUri,
            ("SortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.ToPath ListArtifacts where
  toPath = Core.const "/"

instance Core.ToQuery ListArtifacts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListArtifactsResponse' smart constructor.
data ListArtifactsResponse = ListArtifactsResponse'
  { -- | A token for getting the next set of artifacts, if there are any.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of artifacts and their properties.
    artifactSummaries :: Core.Maybe [ArtifactSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListArtifactsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listArtifactsResponse_nextToken' - A token for getting the next set of artifacts, if there are any.
--
-- 'artifactSummaries', 'listArtifactsResponse_artifactSummaries' - A list of artifacts and their properties.
--
-- 'httpStatus', 'listArtifactsResponse_httpStatus' - The response's http status code.
newListArtifactsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListArtifactsResponse
newListArtifactsResponse pHttpStatus_ =
  ListArtifactsResponse'
    { nextToken = Core.Nothing,
      artifactSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of artifacts, if there are any.
listArtifactsResponse_nextToken :: Lens.Lens' ListArtifactsResponse (Core.Maybe Core.Text)
listArtifactsResponse_nextToken = Lens.lens (\ListArtifactsResponse' {nextToken} -> nextToken) (\s@ListArtifactsResponse' {} a -> s {nextToken = a} :: ListArtifactsResponse)

-- | A list of artifacts and their properties.
listArtifactsResponse_artifactSummaries :: Lens.Lens' ListArtifactsResponse (Core.Maybe [ArtifactSummary])
listArtifactsResponse_artifactSummaries = Lens.lens (\ListArtifactsResponse' {artifactSummaries} -> artifactSummaries) (\s@ListArtifactsResponse' {} a -> s {artifactSummaries = a} :: ListArtifactsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listArtifactsResponse_httpStatus :: Lens.Lens' ListArtifactsResponse Core.Int
listArtifactsResponse_httpStatus = Lens.lens (\ListArtifactsResponse' {httpStatus} -> httpStatus) (\s@ListArtifactsResponse' {} a -> s {httpStatus = a} :: ListArtifactsResponse)

instance Core.NFData ListArtifactsResponse
