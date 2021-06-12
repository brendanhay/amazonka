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
-- Module      : Network.AWS.SageMaker.ListContexts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the contexts in your account and their properties.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListContexts
  ( -- * Creating a Request
    ListContexts (..),
    newListContexts,

    -- * Request Lenses
    listContexts_contextType,
    listContexts_createdAfter,
    listContexts_sortOrder,
    listContexts_nextToken,
    listContexts_createdBefore,
    listContexts_maxResults,
    listContexts_sourceUri,
    listContexts_sortBy,

    -- * Destructuring the Response
    ListContextsResponse (..),
    newListContextsResponse,

    -- * Response Lenses
    listContextsResponse_nextToken,
    listContextsResponse_contextSummaries,
    listContextsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListContexts' smart constructor.
data ListContexts = ListContexts'
  { -- | A filter that returns only contexts of the specified type.
    contextType :: Core.Maybe Core.Text,
    -- | A filter that returns only contexts created on or after the specified
    -- time.
    createdAfter :: Core.Maybe Core.POSIX,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Core.Maybe SortOrder,
    -- | If the previous call to @ListContexts@ didn\'t return the full set of
    -- contexts, the call returns a token for getting the next set of contexts.
    nextToken :: Core.Maybe Core.Text,
    -- | A filter that returns only contexts created on or before the specified
    -- time.
    createdBefore :: Core.Maybe Core.POSIX,
    -- | The maximum number of contexts to return in the response. The default
    -- value is 10.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only contexts with the specified source URI.
    sourceUri :: Core.Maybe Core.Text,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Core.Maybe SortContextsBy
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListContexts' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'contextType', 'listContexts_contextType' - A filter that returns only contexts of the specified type.
--
-- 'createdAfter', 'listContexts_createdAfter' - A filter that returns only contexts created on or after the specified
-- time.
--
-- 'sortOrder', 'listContexts_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'nextToken', 'listContexts_nextToken' - If the previous call to @ListContexts@ didn\'t return the full set of
-- contexts, the call returns a token for getting the next set of contexts.
--
-- 'createdBefore', 'listContexts_createdBefore' - A filter that returns only contexts created on or before the specified
-- time.
--
-- 'maxResults', 'listContexts_maxResults' - The maximum number of contexts to return in the response. The default
-- value is 10.
--
-- 'sourceUri', 'listContexts_sourceUri' - A filter that returns only contexts with the specified source URI.
--
-- 'sortBy', 'listContexts_sortBy' - The property used to sort results. The default value is @CreationTime@.
newListContexts ::
  ListContexts
newListContexts =
  ListContexts'
    { contextType = Core.Nothing,
      createdAfter = Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      createdBefore = Core.Nothing,
      maxResults = Core.Nothing,
      sourceUri = Core.Nothing,
      sortBy = Core.Nothing
    }

-- | A filter that returns only contexts of the specified type.
listContexts_contextType :: Lens.Lens' ListContexts (Core.Maybe Core.Text)
listContexts_contextType = Lens.lens (\ListContexts' {contextType} -> contextType) (\s@ListContexts' {} a -> s {contextType = a} :: ListContexts)

-- | A filter that returns only contexts created on or after the specified
-- time.
listContexts_createdAfter :: Lens.Lens' ListContexts (Core.Maybe Core.UTCTime)
listContexts_createdAfter = Lens.lens (\ListContexts' {createdAfter} -> createdAfter) (\s@ListContexts' {} a -> s {createdAfter = a} :: ListContexts) Core.. Lens.mapping Core._Time

-- | The sort order. The default value is @Descending@.
listContexts_sortOrder :: Lens.Lens' ListContexts (Core.Maybe SortOrder)
listContexts_sortOrder = Lens.lens (\ListContexts' {sortOrder} -> sortOrder) (\s@ListContexts' {} a -> s {sortOrder = a} :: ListContexts)

-- | If the previous call to @ListContexts@ didn\'t return the full set of
-- contexts, the call returns a token for getting the next set of contexts.
listContexts_nextToken :: Lens.Lens' ListContexts (Core.Maybe Core.Text)
listContexts_nextToken = Lens.lens (\ListContexts' {nextToken} -> nextToken) (\s@ListContexts' {} a -> s {nextToken = a} :: ListContexts)

-- | A filter that returns only contexts created on or before the specified
-- time.
listContexts_createdBefore :: Lens.Lens' ListContexts (Core.Maybe Core.UTCTime)
listContexts_createdBefore = Lens.lens (\ListContexts' {createdBefore} -> createdBefore) (\s@ListContexts' {} a -> s {createdBefore = a} :: ListContexts) Core.. Lens.mapping Core._Time

-- | The maximum number of contexts to return in the response. The default
-- value is 10.
listContexts_maxResults :: Lens.Lens' ListContexts (Core.Maybe Core.Natural)
listContexts_maxResults = Lens.lens (\ListContexts' {maxResults} -> maxResults) (\s@ListContexts' {} a -> s {maxResults = a} :: ListContexts)

-- | A filter that returns only contexts with the specified source URI.
listContexts_sourceUri :: Lens.Lens' ListContexts (Core.Maybe Core.Text)
listContexts_sourceUri = Lens.lens (\ListContexts' {sourceUri} -> sourceUri) (\s@ListContexts' {} a -> s {sourceUri = a} :: ListContexts)

-- | The property used to sort results. The default value is @CreationTime@.
listContexts_sortBy :: Lens.Lens' ListContexts (Core.Maybe SortContextsBy)
listContexts_sortBy = Lens.lens (\ListContexts' {sortBy} -> sortBy) (\s@ListContexts' {} a -> s {sortBy = a} :: ListContexts)

instance Core.AWSPager ListContexts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContextsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listContextsResponse_contextSummaries
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listContexts_nextToken
          Lens..~ rs
          Lens.^? listContextsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListContexts where
  type AWSResponse ListContexts = ListContextsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContextsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ContextSummaries" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListContexts

instance Core.NFData ListContexts

instance Core.ToHeaders ListContexts where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListContexts" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListContexts where
  toJSON ListContexts' {..} =
    Core.object
      ( Core.catMaybes
          [ ("ContextType" Core..=) Core.<$> contextType,
            ("CreatedAfter" Core..=) Core.<$> createdAfter,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("CreatedBefore" Core..=) Core.<$> createdBefore,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("SourceUri" Core..=) Core.<$> sourceUri,
            ("SortBy" Core..=) Core.<$> sortBy
          ]
      )

instance Core.ToPath ListContexts where
  toPath = Core.const "/"

instance Core.ToQuery ListContexts where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListContextsResponse' smart constructor.
data ListContextsResponse = ListContextsResponse'
  { -- | A token for getting the next set of contexts, if there are any.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of contexts and their properties.
    contextSummaries :: Core.Maybe [ContextSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListContextsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listContextsResponse_nextToken' - A token for getting the next set of contexts, if there are any.
--
-- 'contextSummaries', 'listContextsResponse_contextSummaries' - A list of contexts and their properties.
--
-- 'httpStatus', 'listContextsResponse_httpStatus' - The response's http status code.
newListContextsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListContextsResponse
newListContextsResponse pHttpStatus_ =
  ListContextsResponse'
    { nextToken = Core.Nothing,
      contextSummaries = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of contexts, if there are any.
listContextsResponse_nextToken :: Lens.Lens' ListContextsResponse (Core.Maybe Core.Text)
listContextsResponse_nextToken = Lens.lens (\ListContextsResponse' {nextToken} -> nextToken) (\s@ListContextsResponse' {} a -> s {nextToken = a} :: ListContextsResponse)

-- | A list of contexts and their properties.
listContextsResponse_contextSummaries :: Lens.Lens' ListContextsResponse (Core.Maybe [ContextSummary])
listContextsResponse_contextSummaries = Lens.lens (\ListContextsResponse' {contextSummaries} -> contextSummaries) (\s@ListContextsResponse' {} a -> s {contextSummaries = a} :: ListContextsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listContextsResponse_httpStatus :: Lens.Lens' ListContextsResponse Core.Int
listContextsResponse_httpStatus = Lens.lens (\ListContextsResponse' {httpStatus} -> httpStatus) (\s@ListContextsResponse' {} a -> s {httpStatus = a} :: ListContextsResponse)

instance Core.NFData ListContextsResponse
