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
-- Module      : Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists notebook instance lifestyle configurations created with the
-- CreateNotebookInstanceLifecycleConfig API.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListNotebookInstanceLifecycleConfigs
  ( -- * Creating a Request
    ListNotebookInstanceLifecycleConfigs (..),
    newListNotebookInstanceLifecycleConfigs,

    -- * Request Lenses
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore,
    listNotebookInstanceLifecycleConfigs_sortOrder,
    listNotebookInstanceLifecycleConfigs_nextToken,
    listNotebookInstanceLifecycleConfigs_nameContains,
    listNotebookInstanceLifecycleConfigs_maxResults,
    listNotebookInstanceLifecycleConfigs_creationTimeBefore,
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter,
    listNotebookInstanceLifecycleConfigs_sortBy,
    listNotebookInstanceLifecycleConfigs_creationTimeAfter,

    -- * Destructuring the Response
    ListNotebookInstanceLifecycleConfigsResponse (..),
    newListNotebookInstanceLifecycleConfigsResponse,

    -- * Response Lenses
    listNotebookInstanceLifecycleConfigsResponse_nextToken,
    listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs,
    listNotebookInstanceLifecycleConfigsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListNotebookInstanceLifecycleConfigs' smart constructor.
data ListNotebookInstanceLifecycleConfigs = ListNotebookInstanceLifecycleConfigs'
  { -- | A filter that returns only lifecycle configurations that were modified
    -- before the specified time (timestamp).
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | The sort order for results.
    sortOrder :: Core.Maybe NotebookInstanceLifecycleConfigSortOrder,
    -- | If the result of a @ListNotebookInstanceLifecycleConfigs@ request was
    -- truncated, the response includes a @NextToken@. To get the next set of
    -- lifecycle configurations, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A string in the lifecycle configuration name. This filter returns only
    -- lifecycle configurations whose name contains the specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of lifecycle configurations to return in the
    -- response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only lifecycle configurations that were created
    -- before the specified time (timestamp).
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only lifecycle configurations that were modified
    -- after the specified time (timestamp).
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | Sorts the list of results. The default is @CreationTime@.
    sortBy :: Core.Maybe NotebookInstanceLifecycleConfigSortKey,
    -- | A filter that returns only lifecycle configurations that were created
    -- after the specified time (timestamp).
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListNotebookInstanceLifecycleConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore' - A filter that returns only lifecycle configurations that were modified
-- before the specified time (timestamp).
--
-- 'sortOrder', 'listNotebookInstanceLifecycleConfigs_sortOrder' - The sort order for results.
--
-- 'nextToken', 'listNotebookInstanceLifecycleConfigs_nextToken' - If the result of a @ListNotebookInstanceLifecycleConfigs@ request was
-- truncated, the response includes a @NextToken@. To get the next set of
-- lifecycle configurations, use the token in the next request.
--
-- 'nameContains', 'listNotebookInstanceLifecycleConfigs_nameContains' - A string in the lifecycle configuration name. This filter returns only
-- lifecycle configurations whose name contains the specified string.
--
-- 'maxResults', 'listNotebookInstanceLifecycleConfigs_maxResults' - The maximum number of lifecycle configurations to return in the
-- response.
--
-- 'creationTimeBefore', 'listNotebookInstanceLifecycleConfigs_creationTimeBefore' - A filter that returns only lifecycle configurations that were created
-- before the specified time (timestamp).
--
-- 'lastModifiedTimeAfter', 'listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter' - A filter that returns only lifecycle configurations that were modified
-- after the specified time (timestamp).
--
-- 'sortBy', 'listNotebookInstanceLifecycleConfigs_sortBy' - Sorts the list of results. The default is @CreationTime@.
--
-- 'creationTimeAfter', 'listNotebookInstanceLifecycleConfigs_creationTimeAfter' - A filter that returns only lifecycle configurations that were created
-- after the specified time (timestamp).
newListNotebookInstanceLifecycleConfigs ::
  ListNotebookInstanceLifecycleConfigs
newListNotebookInstanceLifecycleConfigs =
  ListNotebookInstanceLifecycleConfigs'
    { lastModifiedTimeBefore =
        Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      sortBy = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | A filter that returns only lifecycle configurations that were modified
-- before the specified time (timestamp).
listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.UTCTime)
listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {lastModifiedTimeBefore = a} :: ListNotebookInstanceLifecycleConfigs) Core.. Lens.mapping Core._Time

-- | The sort order for results.
listNotebookInstanceLifecycleConfigs_sortOrder :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe NotebookInstanceLifecycleConfigSortOrder)
listNotebookInstanceLifecycleConfigs_sortOrder = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {sortOrder} -> sortOrder) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {sortOrder = a} :: ListNotebookInstanceLifecycleConfigs)

-- | If the result of a @ListNotebookInstanceLifecycleConfigs@ request was
-- truncated, the response includes a @NextToken@. To get the next set of
-- lifecycle configurations, use the token in the next request.
listNotebookInstanceLifecycleConfigs_nextToken :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.Text)
listNotebookInstanceLifecycleConfigs_nextToken = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {nextToken} -> nextToken) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {nextToken = a} :: ListNotebookInstanceLifecycleConfigs)

-- | A string in the lifecycle configuration name. This filter returns only
-- lifecycle configurations whose name contains the specified string.
listNotebookInstanceLifecycleConfigs_nameContains :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.Text)
listNotebookInstanceLifecycleConfigs_nameContains = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {nameContains} -> nameContains) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {nameContains = a} :: ListNotebookInstanceLifecycleConfigs)

-- | The maximum number of lifecycle configurations to return in the
-- response.
listNotebookInstanceLifecycleConfigs_maxResults :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.Natural)
listNotebookInstanceLifecycleConfigs_maxResults = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {maxResults} -> maxResults) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {maxResults = a} :: ListNotebookInstanceLifecycleConfigs)

-- | A filter that returns only lifecycle configurations that were created
-- before the specified time (timestamp).
listNotebookInstanceLifecycleConfigs_creationTimeBefore :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.UTCTime)
listNotebookInstanceLifecycleConfigs_creationTimeBefore = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {creationTimeBefore} -> creationTimeBefore) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {creationTimeBefore = a} :: ListNotebookInstanceLifecycleConfigs) Core.. Lens.mapping Core._Time

-- | A filter that returns only lifecycle configurations that were modified
-- after the specified time (timestamp).
listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.UTCTime)
listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {lastModifiedTimeAfter = a} :: ListNotebookInstanceLifecycleConfigs) Core.. Lens.mapping Core._Time

-- | Sorts the list of results. The default is @CreationTime@.
listNotebookInstanceLifecycleConfigs_sortBy :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe NotebookInstanceLifecycleConfigSortKey)
listNotebookInstanceLifecycleConfigs_sortBy = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {sortBy} -> sortBy) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {sortBy = a} :: ListNotebookInstanceLifecycleConfigs)

-- | A filter that returns only lifecycle configurations that were created
-- after the specified time (timestamp).
listNotebookInstanceLifecycleConfigs_creationTimeAfter :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Core.Maybe Core.UTCTime)
listNotebookInstanceLifecycleConfigs_creationTimeAfter = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {creationTimeAfter} -> creationTimeAfter) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {creationTimeAfter = a} :: ListNotebookInstanceLifecycleConfigs) Core.. Lens.mapping Core._Time

instance
  Core.AWSPager
    ListNotebookInstanceLifecycleConfigs
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNotebookInstanceLifecycleConfigsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listNotebookInstanceLifecycleConfigs_nextToken
          Lens..~ rs
            Lens.^? listNotebookInstanceLifecycleConfigsResponse_nextToken
              Core.. Lens._Just

instance
  Core.AWSRequest
    ListNotebookInstanceLifecycleConfigs
  where
  type
    AWSResponse ListNotebookInstanceLifecycleConfigs =
      ListNotebookInstanceLifecycleConfigsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListNotebookInstanceLifecycleConfigsResponse'
            Core.<$> (x Core..?> "NextToken")
              Core.<*> ( x Core..?> "NotebookInstanceLifecycleConfigs"
                           Core..!@ Core.mempty
                       )
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    ListNotebookInstanceLifecycleConfigs

instance
  Core.NFData
    ListNotebookInstanceLifecycleConfigs

instance
  Core.ToHeaders
    ListNotebookInstanceLifecycleConfigs
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListNotebookInstanceLifecycleConfigs" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    ListNotebookInstanceLifecycleConfigs
  where
  toJSON ListNotebookInstanceLifecycleConfigs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("LastModifiedTimeBefore" Core..=)
              Core.<$> lastModifiedTimeBefore,
            ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Core..=)
              Core.<$> lastModifiedTimeAfter,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance
  Core.ToPath
    ListNotebookInstanceLifecycleConfigs
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    ListNotebookInstanceLifecycleConfigs
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListNotebookInstanceLifecycleConfigsResponse' smart constructor.
data ListNotebookInstanceLifecycleConfigsResponse = ListNotebookInstanceLifecycleConfigsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- get the next set of lifecycle configurations, use it in the next
    -- request.
    nextToken :: Core.Maybe Core.Text,
    -- | An array of @NotebookInstanceLifecycleConfiguration@ objects, each
    -- listing a lifecycle configuration.
    notebookInstanceLifecycleConfigs :: Core.Maybe [NotebookInstanceLifecycleConfigSummary],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListNotebookInstanceLifecycleConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listNotebookInstanceLifecycleConfigsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- get the next set of lifecycle configurations, use it in the next
-- request.
--
-- 'notebookInstanceLifecycleConfigs', 'listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs' - An array of @NotebookInstanceLifecycleConfiguration@ objects, each
-- listing a lifecycle configuration.
--
-- 'httpStatus', 'listNotebookInstanceLifecycleConfigsResponse_httpStatus' - The response's http status code.
newListNotebookInstanceLifecycleConfigsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListNotebookInstanceLifecycleConfigsResponse
newListNotebookInstanceLifecycleConfigsResponse
  pHttpStatus_ =
    ListNotebookInstanceLifecycleConfigsResponse'
      { nextToken =
          Core.Nothing,
        notebookInstanceLifecycleConfigs =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- get the next set of lifecycle configurations, use it in the next
-- request.
listNotebookInstanceLifecycleConfigsResponse_nextToken :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse (Core.Maybe Core.Text)
listNotebookInstanceLifecycleConfigsResponse_nextToken = Lens.lens (\ListNotebookInstanceLifecycleConfigsResponse' {nextToken} -> nextToken) (\s@ListNotebookInstanceLifecycleConfigsResponse' {} a -> s {nextToken = a} :: ListNotebookInstanceLifecycleConfigsResponse)

-- | An array of @NotebookInstanceLifecycleConfiguration@ objects, each
-- listing a lifecycle configuration.
listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse (Core.Maybe [NotebookInstanceLifecycleConfigSummary])
listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs = Lens.lens (\ListNotebookInstanceLifecycleConfigsResponse' {notebookInstanceLifecycleConfigs} -> notebookInstanceLifecycleConfigs) (\s@ListNotebookInstanceLifecycleConfigsResponse' {} a -> s {notebookInstanceLifecycleConfigs = a} :: ListNotebookInstanceLifecycleConfigsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
listNotebookInstanceLifecycleConfigsResponse_httpStatus :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse Core.Int
listNotebookInstanceLifecycleConfigsResponse_httpStatus = Lens.lens (\ListNotebookInstanceLifecycleConfigsResponse' {httpStatus} -> httpStatus) (\s@ListNotebookInstanceLifecycleConfigsResponse' {} a -> s {httpStatus = a} :: ListNotebookInstanceLifecycleConfigsResponse)

instance
  Core.NFData
    ListNotebookInstanceLifecycleConfigsResponse
