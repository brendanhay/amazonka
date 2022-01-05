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
-- Module      : Amazonka.SageMaker.ListContexts
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists the contexts in your account and their properties.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListContexts
  ( -- * Creating a Request
    ListContexts (..),
    newListContexts,

    -- * Request Lenses
    listContexts_createdAfter,
    listContexts_nextToken,
    listContexts_sortOrder,
    listContexts_contextType,
    listContexts_sourceUri,
    listContexts_maxResults,
    listContexts_createdBefore,
    listContexts_sortBy,

    -- * Destructuring the Response
    ListContextsResponse (..),
    newListContextsResponse,

    -- * Response Lenses
    listContextsResponse_contextSummaries,
    listContextsResponse_nextToken,
    listContextsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListContexts' smart constructor.
data ListContexts = ListContexts'
  { -- | A filter that returns only contexts created on or after the specified
    -- time.
    createdAfter :: Prelude.Maybe Core.POSIX,
    -- | If the previous call to @ListContexts@ didn\'t return the full set of
    -- contexts, the call returns a token for getting the next set of contexts.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order. The default value is @Descending@.
    sortOrder :: Prelude.Maybe SortOrder,
    -- | A filter that returns only contexts of the specified type.
    contextType :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only contexts with the specified source URI.
    sourceUri :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of contexts to return in the response. The default
    -- value is 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only contexts created on or before the specified
    -- time.
    createdBefore :: Prelude.Maybe Core.POSIX,
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
-- 'createdAfter', 'listContexts_createdAfter' - A filter that returns only contexts created on or after the specified
-- time.
--
-- 'nextToken', 'listContexts_nextToken' - If the previous call to @ListContexts@ didn\'t return the full set of
-- contexts, the call returns a token for getting the next set of contexts.
--
-- 'sortOrder', 'listContexts_sortOrder' - The sort order. The default value is @Descending@.
--
-- 'contextType', 'listContexts_contextType' - A filter that returns only contexts of the specified type.
--
-- 'sourceUri', 'listContexts_sourceUri' - A filter that returns only contexts with the specified source URI.
--
-- 'maxResults', 'listContexts_maxResults' - The maximum number of contexts to return in the response. The default
-- value is 10.
--
-- 'createdBefore', 'listContexts_createdBefore' - A filter that returns only contexts created on or before the specified
-- time.
--
-- 'sortBy', 'listContexts_sortBy' - The property used to sort results. The default value is @CreationTime@.
newListContexts ::
  ListContexts
newListContexts =
  ListContexts'
    { createdAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      contextType = Prelude.Nothing,
      sourceUri = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      createdBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A filter that returns only contexts created on or after the specified
-- time.
listContexts_createdAfter :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.UTCTime)
listContexts_createdAfter = Lens.lens (\ListContexts' {createdAfter} -> createdAfter) (\s@ListContexts' {} a -> s {createdAfter = a} :: ListContexts) Prelude.. Lens.mapping Core._Time

-- | If the previous call to @ListContexts@ didn\'t return the full set of
-- contexts, the call returns a token for getting the next set of contexts.
listContexts_nextToken :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.Text)
listContexts_nextToken = Lens.lens (\ListContexts' {nextToken} -> nextToken) (\s@ListContexts' {} a -> s {nextToken = a} :: ListContexts)

-- | The sort order. The default value is @Descending@.
listContexts_sortOrder :: Lens.Lens' ListContexts (Prelude.Maybe SortOrder)
listContexts_sortOrder = Lens.lens (\ListContexts' {sortOrder} -> sortOrder) (\s@ListContexts' {} a -> s {sortOrder = a} :: ListContexts)

-- | A filter that returns only contexts of the specified type.
listContexts_contextType :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.Text)
listContexts_contextType = Lens.lens (\ListContexts' {contextType} -> contextType) (\s@ListContexts' {} a -> s {contextType = a} :: ListContexts)

-- | A filter that returns only contexts with the specified source URI.
listContexts_sourceUri :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.Text)
listContexts_sourceUri = Lens.lens (\ListContexts' {sourceUri} -> sourceUri) (\s@ListContexts' {} a -> s {sourceUri = a} :: ListContexts)

-- | The maximum number of contexts to return in the response. The default
-- value is 10.
listContexts_maxResults :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.Natural)
listContexts_maxResults = Lens.lens (\ListContexts' {maxResults} -> maxResults) (\s@ListContexts' {} a -> s {maxResults = a} :: ListContexts)

-- | A filter that returns only contexts created on or before the specified
-- time.
listContexts_createdBefore :: Lens.Lens' ListContexts (Prelude.Maybe Prelude.UTCTime)
listContexts_createdBefore = Lens.lens (\ListContexts' {createdBefore} -> createdBefore) (\s@ListContexts' {} a -> s {createdBefore = a} :: ListContexts) Prelude.. Lens.mapping Core._Time

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
            Prelude.<$> ( x Core..?> "ContextSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListContexts where
  hashWithSalt _salt ListContexts' {..} =
    _salt `Prelude.hashWithSalt` createdAfter
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` contextType
      `Prelude.hashWithSalt` sourceUri
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` createdBefore
      `Prelude.hashWithSalt` sortBy

instance Prelude.NFData ListContexts where
  rnf ListContexts' {..} =
    Prelude.rnf createdAfter
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf contextType
      `Prelude.seq` Prelude.rnf sourceUri
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf createdBefore
      `Prelude.seq` Prelude.rnf sortBy

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
          [ ("CreatedAfter" Core..=) Prelude.<$> createdAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("ContextType" Core..=) Prelude.<$> contextType,
            ("SourceUri" Core..=) Prelude.<$> sourceUri,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("CreatedBefore" Core..=) Prelude.<$> createdBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance Core.ToPath ListContexts where
  toPath = Prelude.const "/"

instance Core.ToQuery ListContexts where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListContextsResponse' smart constructor.
data ListContextsResponse = ListContextsResponse'
  { -- | A list of contexts and their properties.
    contextSummaries :: Prelude.Maybe [ContextSummary],
    -- | A token for getting the next set of contexts, if there are any.
    nextToken :: Prelude.Maybe Prelude.Text,
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
-- 'contextSummaries', 'listContextsResponse_contextSummaries' - A list of contexts and their properties.
--
-- 'nextToken', 'listContextsResponse_nextToken' - A token for getting the next set of contexts, if there are any.
--
-- 'httpStatus', 'listContextsResponse_httpStatus' - The response's http status code.
newListContextsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListContextsResponse
newListContextsResponse pHttpStatus_ =
  ListContextsResponse'
    { contextSummaries =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of contexts and their properties.
listContextsResponse_contextSummaries :: Lens.Lens' ListContextsResponse (Prelude.Maybe [ContextSummary])
listContextsResponse_contextSummaries = Lens.lens (\ListContextsResponse' {contextSummaries} -> contextSummaries) (\s@ListContextsResponse' {} a -> s {contextSummaries = a} :: ListContextsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A token for getting the next set of contexts, if there are any.
listContextsResponse_nextToken :: Lens.Lens' ListContextsResponse (Prelude.Maybe Prelude.Text)
listContextsResponse_nextToken = Lens.lens (\ListContextsResponse' {nextToken} -> nextToken) (\s@ListContextsResponse' {} a -> s {nextToken = a} :: ListContextsResponse)

-- | The response's http status code.
listContextsResponse_httpStatus :: Lens.Lens' ListContextsResponse Prelude.Int
listContextsResponse_httpStatus = Lens.lens (\ListContextsResponse' {httpStatus} -> httpStatus) (\s@ListContextsResponse' {} a -> s {httpStatus = a} :: ListContextsResponse)

instance Prelude.NFData ListContextsResponse where
  rnf ListContextsResponse' {..} =
    Prelude.rnf contextSummaries
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
