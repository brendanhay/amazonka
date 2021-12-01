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
-- Module      : Amazonka.SageMaker.ListNotebookInstanceLifecycleConfigs
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
module Amazonka.SageMaker.ListNotebookInstanceLifecycleConfigs
  ( -- * Creating a Request
    ListNotebookInstanceLifecycleConfigs (..),
    newListNotebookInstanceLifecycleConfigs,

    -- * Request Lenses
    listNotebookInstanceLifecycleConfigs_nameContains,
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore,
    listNotebookInstanceLifecycleConfigs_creationTimeAfter,
    listNotebookInstanceLifecycleConfigs_nextToken,
    listNotebookInstanceLifecycleConfigs_sortOrder,
    listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter,
    listNotebookInstanceLifecycleConfigs_creationTimeBefore,
    listNotebookInstanceLifecycleConfigs_maxResults,
    listNotebookInstanceLifecycleConfigs_sortBy,

    -- * Destructuring the Response
    ListNotebookInstanceLifecycleConfigsResponse (..),
    newListNotebookInstanceLifecycleConfigsResponse,

    -- * Response Lenses
    listNotebookInstanceLifecycleConfigsResponse_nextToken,
    listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs,
    listNotebookInstanceLifecycleConfigsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Lens as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListNotebookInstanceLifecycleConfigs' smart constructor.
data ListNotebookInstanceLifecycleConfigs = ListNotebookInstanceLifecycleConfigs'
  { -- | A string in the lifecycle configuration name. This filter returns only
    -- lifecycle configurations whose name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only lifecycle configurations that were modified
    -- before the specified time (timestamp).
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only lifecycle configurations that were created
    -- after the specified time (timestamp).
    creationTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | If the result of a @ListNotebookInstanceLifecycleConfigs@ request was
    -- truncated, the response includes a @NextToken@. To get the next set of
    -- lifecycle configurations, use the token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The sort order for results.
    sortOrder :: Prelude.Maybe NotebookInstanceLifecycleConfigSortOrder,
    -- | A filter that returns only lifecycle configurations that were modified
    -- after the specified time (timestamp).
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only lifecycle configurations that were created
    -- before the specified time (timestamp).
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | The maximum number of lifecycle configurations to return in the
    -- response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Sorts the list of results. The default is @CreationTime@.
    sortBy :: Prelude.Maybe NotebookInstanceLifecycleConfigSortKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListNotebookInstanceLifecycleConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nameContains', 'listNotebookInstanceLifecycleConfigs_nameContains' - A string in the lifecycle configuration name. This filter returns only
-- lifecycle configurations whose name contains the specified string.
--
-- 'lastModifiedTimeBefore', 'listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore' - A filter that returns only lifecycle configurations that were modified
-- before the specified time (timestamp).
--
-- 'creationTimeAfter', 'listNotebookInstanceLifecycleConfigs_creationTimeAfter' - A filter that returns only lifecycle configurations that were created
-- after the specified time (timestamp).
--
-- 'nextToken', 'listNotebookInstanceLifecycleConfigs_nextToken' - If the result of a @ListNotebookInstanceLifecycleConfigs@ request was
-- truncated, the response includes a @NextToken@. To get the next set of
-- lifecycle configurations, use the token in the next request.
--
-- 'sortOrder', 'listNotebookInstanceLifecycleConfigs_sortOrder' - The sort order for results.
--
-- 'lastModifiedTimeAfter', 'listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter' - A filter that returns only lifecycle configurations that were modified
-- after the specified time (timestamp).
--
-- 'creationTimeBefore', 'listNotebookInstanceLifecycleConfigs_creationTimeBefore' - A filter that returns only lifecycle configurations that were created
-- before the specified time (timestamp).
--
-- 'maxResults', 'listNotebookInstanceLifecycleConfigs_maxResults' - The maximum number of lifecycle configurations to return in the
-- response.
--
-- 'sortBy', 'listNotebookInstanceLifecycleConfigs_sortBy' - Sorts the list of results. The default is @CreationTime@.
newListNotebookInstanceLifecycleConfigs ::
  ListNotebookInstanceLifecycleConfigs
newListNotebookInstanceLifecycleConfigs =
  ListNotebookInstanceLifecycleConfigs'
    { nameContains =
        Prelude.Nothing,
      lastModifiedTimeBefore =
        Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      lastModifiedTimeAfter =
        Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      sortBy = Prelude.Nothing
    }

-- | A string in the lifecycle configuration name. This filter returns only
-- lifecycle configurations whose name contains the specified string.
listNotebookInstanceLifecycleConfigs_nameContains :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Prelude.Maybe Prelude.Text)
listNotebookInstanceLifecycleConfigs_nameContains = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {nameContains} -> nameContains) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {nameContains = a} :: ListNotebookInstanceLifecycleConfigs)

-- | A filter that returns only lifecycle configurations that were modified
-- before the specified time (timestamp).
listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listNotebookInstanceLifecycleConfigs_lastModifiedTimeBefore = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {lastModifiedTimeBefore = a} :: ListNotebookInstanceLifecycleConfigs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only lifecycle configurations that were created
-- after the specified time (timestamp).
listNotebookInstanceLifecycleConfigs_creationTimeAfter :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listNotebookInstanceLifecycleConfigs_creationTimeAfter = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {creationTimeAfter} -> creationTimeAfter) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {creationTimeAfter = a} :: ListNotebookInstanceLifecycleConfigs) Prelude.. Lens.mapping Core._Time

-- | If the result of a @ListNotebookInstanceLifecycleConfigs@ request was
-- truncated, the response includes a @NextToken@. To get the next set of
-- lifecycle configurations, use the token in the next request.
listNotebookInstanceLifecycleConfigs_nextToken :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Prelude.Maybe Prelude.Text)
listNotebookInstanceLifecycleConfigs_nextToken = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {nextToken} -> nextToken) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {nextToken = a} :: ListNotebookInstanceLifecycleConfigs)

-- | The sort order for results.
listNotebookInstanceLifecycleConfigs_sortOrder :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Prelude.Maybe NotebookInstanceLifecycleConfigSortOrder)
listNotebookInstanceLifecycleConfigs_sortOrder = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {sortOrder} -> sortOrder) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {sortOrder = a} :: ListNotebookInstanceLifecycleConfigs)

-- | A filter that returns only lifecycle configurations that were modified
-- after the specified time (timestamp).
listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listNotebookInstanceLifecycleConfigs_lastModifiedTimeAfter = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {lastModifiedTimeAfter = a} :: ListNotebookInstanceLifecycleConfigs) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only lifecycle configurations that were created
-- before the specified time (timestamp).
listNotebookInstanceLifecycleConfigs_creationTimeBefore :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Prelude.Maybe Prelude.UTCTime)
listNotebookInstanceLifecycleConfigs_creationTimeBefore = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {creationTimeBefore} -> creationTimeBefore) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {creationTimeBefore = a} :: ListNotebookInstanceLifecycleConfigs) Prelude.. Lens.mapping Core._Time

-- | The maximum number of lifecycle configurations to return in the
-- response.
listNotebookInstanceLifecycleConfigs_maxResults :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Prelude.Maybe Prelude.Natural)
listNotebookInstanceLifecycleConfigs_maxResults = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {maxResults} -> maxResults) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {maxResults = a} :: ListNotebookInstanceLifecycleConfigs)

-- | Sorts the list of results. The default is @CreationTime@.
listNotebookInstanceLifecycleConfigs_sortBy :: Lens.Lens' ListNotebookInstanceLifecycleConfigs (Prelude.Maybe NotebookInstanceLifecycleConfigSortKey)
listNotebookInstanceLifecycleConfigs_sortBy = Lens.lens (\ListNotebookInstanceLifecycleConfigs' {sortBy} -> sortBy) (\s@ListNotebookInstanceLifecycleConfigs' {} a -> s {sortBy = a} :: ListNotebookInstanceLifecycleConfigs)

instance
  Core.AWSPager
    ListNotebookInstanceLifecycleConfigs
  where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listNotebookInstanceLifecycleConfigsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listNotebookInstanceLifecycleConfigs_nextToken
          Lens..~ rs
            Lens.^? listNotebookInstanceLifecycleConfigsResponse_nextToken
              Prelude.. Lens._Just

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
            Prelude.<$> (x Core..?> "NextToken")
              Prelude.<*> ( x Core..?> "NotebookInstanceLifecycleConfigs"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    ListNotebookInstanceLifecycleConfigs
  where
  hashWithSalt
    salt'
    ListNotebookInstanceLifecycleConfigs' {..} =
      salt' `Prelude.hashWithSalt` sortBy
        `Prelude.hashWithSalt` maxResults
        `Prelude.hashWithSalt` creationTimeBefore
        `Prelude.hashWithSalt` lastModifiedTimeAfter
        `Prelude.hashWithSalt` sortOrder
        `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` creationTimeAfter
        `Prelude.hashWithSalt` lastModifiedTimeBefore
        `Prelude.hashWithSalt` nameContains

instance
  Prelude.NFData
    ListNotebookInstanceLifecycleConfigs
  where
  rnf ListNotebookInstanceLifecycleConfigs' {..} =
    Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf creationTimeAfter
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore

instance
  Core.ToHeaders
    ListNotebookInstanceLifecycleConfigs
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "SageMaker.ListNotebookInstanceLifecycleConfigs" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    ListNotebookInstanceLifecycleConfigs
  where
  toJSON ListNotebookInstanceLifecycleConfigs' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("NameContains" Core..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("SortBy" Core..=) Prelude.<$> sortBy
          ]
      )

instance
  Core.ToPath
    ListNotebookInstanceLifecycleConfigs
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    ListNotebookInstanceLifecycleConfigs
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListNotebookInstanceLifecycleConfigsResponse' smart constructor.
data ListNotebookInstanceLifecycleConfigsResponse = ListNotebookInstanceLifecycleConfigsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- get the next set of lifecycle configurations, use it in the next
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | An array of @NotebookInstanceLifecycleConfiguration@ objects, each
    -- listing a lifecycle configuration.
    notebookInstanceLifecycleConfigs :: Prelude.Maybe [NotebookInstanceLifecycleConfigSummary],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  ListNotebookInstanceLifecycleConfigsResponse
newListNotebookInstanceLifecycleConfigsResponse
  pHttpStatus_ =
    ListNotebookInstanceLifecycleConfigsResponse'
      { nextToken =
          Prelude.Nothing,
        notebookInstanceLifecycleConfigs =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- get the next set of lifecycle configurations, use it in the next
-- request.
listNotebookInstanceLifecycleConfigsResponse_nextToken :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse (Prelude.Maybe Prelude.Text)
listNotebookInstanceLifecycleConfigsResponse_nextToken = Lens.lens (\ListNotebookInstanceLifecycleConfigsResponse' {nextToken} -> nextToken) (\s@ListNotebookInstanceLifecycleConfigsResponse' {} a -> s {nextToken = a} :: ListNotebookInstanceLifecycleConfigsResponse)

-- | An array of @NotebookInstanceLifecycleConfiguration@ objects, each
-- listing a lifecycle configuration.
listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse (Prelude.Maybe [NotebookInstanceLifecycleConfigSummary])
listNotebookInstanceLifecycleConfigsResponse_notebookInstanceLifecycleConfigs = Lens.lens (\ListNotebookInstanceLifecycleConfigsResponse' {notebookInstanceLifecycleConfigs} -> notebookInstanceLifecycleConfigs) (\s@ListNotebookInstanceLifecycleConfigsResponse' {} a -> s {notebookInstanceLifecycleConfigs = a} :: ListNotebookInstanceLifecycleConfigsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listNotebookInstanceLifecycleConfigsResponse_httpStatus :: Lens.Lens' ListNotebookInstanceLifecycleConfigsResponse Prelude.Int
listNotebookInstanceLifecycleConfigsResponse_httpStatus = Lens.lens (\ListNotebookInstanceLifecycleConfigsResponse' {httpStatus} -> httpStatus) (\s@ListNotebookInstanceLifecycleConfigsResponse' {} a -> s {httpStatus = a} :: ListNotebookInstanceLifecycleConfigsResponse)

instance
  Prelude.NFData
    ListNotebookInstanceLifecycleConfigsResponse
  where
  rnf ListNotebookInstanceLifecycleConfigsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf notebookInstanceLifecycleConfigs
