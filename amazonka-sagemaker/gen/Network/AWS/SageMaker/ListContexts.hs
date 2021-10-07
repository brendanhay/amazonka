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
    listContexts_nextToken,
    listContexts_sortOrder,
    listContexts_createdAfter,
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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListContexts' smart constructor.
data ListContexts = ListContexts'
  { -- | A filter that returns only contexts of the specified type.
    contextType :: Prelude.Maybe Prelude.Text,
    -- | If the previous call to @ListContexts@ didn\'t return the full set of
    -- contexts, the call returns a token for getting the next set of contexts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only contexts created on or after the specified
    -- time.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only contexts created on or before the specified
    -- time.
    createdBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of contexts to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only contexts with the specified source URI.
    sourceUri :: Prelude.Maybe Prelude.Text,
    -- | The property used to sort results. The default value is @CreationTime@.
    sortBy :: Prelude.Maybe SortContextsBy
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
-- 'nextToken', 'listContexts_nextToken' - If the previous call to @ListContexts@ didn\'t return the full set of
-- contexts, the call returns a token for getting the next set of contexts.
--
-- 'sortOrder', 'listContexts_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'createdAfter', 'listContexts_createdAfter' - A filter that returns only contexts created on or after the specified
-- time.
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
    { contextType = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      createdAfter = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sourceUri = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A filter that returns only contexts of the specified type.
listContexts_contextType :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.Text)
listContexts_contextType = Lens.lens (\ListContexts' {contextType} -> contextType) (\s@ListContexts' {} a -> s {contextType = a} :: ListContexts)

-- | If the previous call to @ListContexts@ didn\'t return the full set of
-- contexts, the call returns a token for getting the next set of contexts.
listContexts_nextToken :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.Text)
listContexts_nextToken = Lens.lens (\ListContexts' {nextToken} -> nextToken) (\s@ListContexts' {} a -> s {nextToken = a} :: ListContexts)

-- | The sort order. The default value is @Descending@.
listContexts_sortOrder :: Lens.Lens' ListContexts (Prelude.Maybe SortOrder)
listContexts_sortOrder = Lens.lens (\ListContexts' {sortOrder} -> sortOrder) (\s@ListContexts' {} a -> s {sortOrder = a} :: ListContexts)

-- | A filter that returns only contexts created on or after the specified
-- time.
listContexts_createdAfter :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.UTCTime)
listContexts_createdAfter = Lens.lens (\ListContexts' {createdAfter} -> createdAfter) (\s@ListContexts' {} a -> s {createdAfter = a} :: ListContexts) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only contexts created on or before the specified
-- time.
listContexts_createdBefore :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.UTCTime)
listContexts_createdBefore = Lens.lens (\ListContexts' {createdBefore} -> createdBefore) (\s@ListContexts' {} a -> s {createdBefore = a} :: ListContexts) Prelude.. Lens.mapping Core._Time

-- | The maximum number of contexts to return in the response. The default
-- value is 10.
listContexts_maxResults :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.Natural)
listContexts_maxResults = Lens.lens (\ListContexts' {maxResults} -> maxResults) (\s@ListContexts' {} a -> s {maxResults = a} :: ListContexts)

-- | A filter that returns only contexts with the specified source URI.
listContexts_sourceUri :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.Text)
listContexts_sourceUri = Lens.lens (\ListContexts' {sourceUri} -> sourceUri) (\s@ListContexts' {} a -> s {sourceUri = a} :: ListContexts)

-- | The property used to sort results. The default value is @CreationTime@.
listContexts_sortBy :: Lens.Lens' ListContexts (Prelude.Maybe SortContextsBy)
listContexts_sortBy = Lens.lens (\ListContexts' {sortBy} -> sortBy) (\s@ListContexts' {} a -> s {sortBy = a} :: ListContexts)

instance Core.AWSPager ListContexts where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listContextsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listContextsResponse_contextSummaries
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listContexts_nextToken
          Lens..~ rs
          Lens.^? listContextsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListContexts where
  type AWSResponse ListContexts = ListContextsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListContextsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> ( x Core..?> "ContextSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContexts

instance Prelude.NFData ListContexts

instance Core.ToHeaders ListContexts where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListContexts" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListContexts where
  toJSON ListContexts' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("ContextType" Core..=) Prelude.<$> contextType,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("CreatedAfter" Core..=) Prelude.<$> createdAfter,
            ("CreatedBefore" Core..=) Prelude.<$> createdBefore,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SourceUri" Core..=) Prelude.<$> sourceUri,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListContexts where
  toPath = Prelude.const "/"

instance Core.ToQuery ListContexts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListContextsResponse' smart constructor.
data ListContextsResponse = ListContextsResponse'
  { -- | A token for getting the next set of contexts, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of contexts and their properties.
    contextSummaries :: Prelude.Maybe [ContextSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListContextsResponse
newListContextsResponse pHttpStatus_ =
  ListContextsResponse'
    { nextToken = Prelude.Nothing,
      contextSummaries = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A token for getting the next set of contexts, if there are any.
listContextsResponse_nextToken :: Lens.Lens' ListContextsResponse (Prelude.Maybe Prelude.Text)
listContextsResponse_nextToken = Lens.lens (\ListContextsResponse' {nextToken} -> nextToken) (\s@ListContextsResponse' {} a -> s {nextToken = a} :: ListContextsResponse)

-- | A list of contexts and their properties.
listContextsResponse_contextSummaries :: Lens.Lens' ListContextsResponse (Prelude.Maybe [ContextSummary])
listContextsResponse_contextSummaries = Lens.lens (\ListContextsResponse' {contextSummaries} -> contextSummaries) (\s@ListContextsResponse' {} a -> s {contextSummaries = a} :: ListContextsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listContextsResponse_httpStatus :: Lens.Lens' ListContextsResponse Prelude.Int
listContextsResponse_httpStatus = Lens.lens (\ListContextsResponse' {httpStatus} -> httpStatus) (\s@ListContextsResponse' {} a -> s {httpStatus = a} :: ListContextsResponse)

instance Prelude.NFData ListContextsResponse
