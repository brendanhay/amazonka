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
-- Module      : Amazonka.SageMaker.ListEndpoints
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoints.
--
-- This operation returns paginated results.
module Amazonka.SageMaker.ListEndpoints
  ( -- * Creating a Request
    ListEndpoints (..),
    newListEndpoints,

    -- * Request Lenses
    listEndpoints_sortOrder,
    listEndpoints_nextToken,
    listEndpoints_lastModifiedTimeAfter,
    listEndpoints_nameContains,
    listEndpoints_lastModifiedTimeBefore,
    listEndpoints_creationTimeBefore,
    listEndpoints_sortBy,
    listEndpoints_maxResults,
    listEndpoints_statusEquals,
    listEndpoints_creationTimeAfter,

    -- * Destructuring the Response
    ListEndpointsResponse (..),
    newListEndpointsResponse,

    -- * Response Lenses
    listEndpointsResponse_nextToken,
    listEndpointsResponse_httpStatus,
    listEndpointsResponse_endpoints,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SageMaker.Types

-- | /See:/ 'newListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { -- | The sort order for results. The default is @Descending@.
    sortOrder :: Prelude.Maybe OrderKey,
    -- | If the result of a @ListEndpoints@ request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of endpoints, use the
    -- token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only endpoints that were modified after the
    -- specified timestamp.
    lastModifiedTimeAfter :: Prelude.Maybe Core.POSIX,
    -- | A string in endpoint names. This filter returns only endpoints whose
    -- name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | A filter that returns only endpoints that were modified before the
    -- specified timestamp.
    lastModifiedTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | A filter that returns only endpoints that were created before the
    -- specified time (timestamp).
    creationTimeBefore :: Prelude.Maybe Core.POSIX,
    -- | Sorts the list of results. The default is @CreationTime@.
    sortBy :: Prelude.Maybe EndpointSortKey,
    -- | The maximum number of endpoints to return in the response. This value
    -- defaults to 10.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only endpoints with the specified status.
    statusEquals :: Prelude.Maybe EndpointStatus,
    -- | A filter that returns only endpoints with a creation time greater than
    -- or equal to the specified time (timestamp).
    creationTimeAfter :: Prelude.Maybe Core.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listEndpoints_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'nextToken', 'listEndpoints_nextToken' - If the result of a @ListEndpoints@ request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of endpoints, use the
-- token in the next request.
--
-- 'lastModifiedTimeAfter', 'listEndpoints_lastModifiedTimeAfter' - A filter that returns only endpoints that were modified after the
-- specified timestamp.
--
-- 'nameContains', 'listEndpoints_nameContains' - A string in endpoint names. This filter returns only endpoints whose
-- name contains the specified string.
--
-- 'lastModifiedTimeBefore', 'listEndpoints_lastModifiedTimeBefore' - A filter that returns only endpoints that were modified before the
-- specified timestamp.
--
-- 'creationTimeBefore', 'listEndpoints_creationTimeBefore' - A filter that returns only endpoints that were created before the
-- specified time (timestamp).
--
-- 'sortBy', 'listEndpoints_sortBy' - Sorts the list of results. The default is @CreationTime@.
--
-- 'maxResults', 'listEndpoints_maxResults' - The maximum number of endpoints to return in the response. This value
-- defaults to 10.
--
-- 'statusEquals', 'listEndpoints_statusEquals' - A filter that returns only endpoints with the specified status.
--
-- 'creationTimeAfter', 'listEndpoints_creationTimeAfter' - A filter that returns only endpoints with a creation time greater than
-- or equal to the specified time (timestamp).
newListEndpoints ::
  ListEndpoints
newListEndpoints =
  ListEndpoints'
    { sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      lastModifiedTimeBefore = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | The sort order for results. The default is @Descending@.
listEndpoints_sortOrder :: Lens.Lens' ListEndpoints (Prelude.Maybe OrderKey)
listEndpoints_sortOrder = Lens.lens (\ListEndpoints' {sortOrder} -> sortOrder) (\s@ListEndpoints' {} a -> s {sortOrder = a} :: ListEndpoints)

-- | If the result of a @ListEndpoints@ request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of endpoints, use the
-- token in the next request.
listEndpoints_nextToken :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Text)
listEndpoints_nextToken = Lens.lens (\ListEndpoints' {nextToken} -> nextToken) (\s@ListEndpoints' {} a -> s {nextToken = a} :: ListEndpoints)

-- | A filter that returns only endpoints that were modified after the
-- specified timestamp.
listEndpoints_lastModifiedTimeAfter :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.UTCTime)
listEndpoints_lastModifiedTimeAfter = Lens.lens (\ListEndpoints' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListEndpoints' {} a -> s {lastModifiedTimeAfter = a} :: ListEndpoints) Prelude.. Lens.mapping Core._Time

-- | A string in endpoint names. This filter returns only endpoints whose
-- name contains the specified string.
listEndpoints_nameContains :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Text)
listEndpoints_nameContains = Lens.lens (\ListEndpoints' {nameContains} -> nameContains) (\s@ListEndpoints' {} a -> s {nameContains = a} :: ListEndpoints)

-- | A filter that returns only endpoints that were modified before the
-- specified timestamp.
listEndpoints_lastModifiedTimeBefore :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.UTCTime)
listEndpoints_lastModifiedTimeBefore = Lens.lens (\ListEndpoints' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListEndpoints' {} a -> s {lastModifiedTimeBefore = a} :: ListEndpoints) Prelude.. Lens.mapping Core._Time

-- | A filter that returns only endpoints that were created before the
-- specified time (timestamp).
listEndpoints_creationTimeBefore :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.UTCTime)
listEndpoints_creationTimeBefore = Lens.lens (\ListEndpoints' {creationTimeBefore} -> creationTimeBefore) (\s@ListEndpoints' {} a -> s {creationTimeBefore = a} :: ListEndpoints) Prelude.. Lens.mapping Core._Time

-- | Sorts the list of results. The default is @CreationTime@.
listEndpoints_sortBy :: Lens.Lens' ListEndpoints (Prelude.Maybe EndpointSortKey)
listEndpoints_sortBy = Lens.lens (\ListEndpoints' {sortBy} -> sortBy) (\s@ListEndpoints' {} a -> s {sortBy = a} :: ListEndpoints)

-- | The maximum number of endpoints to return in the response. This value
-- defaults to 10.
listEndpoints_maxResults :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Natural)
listEndpoints_maxResults = Lens.lens (\ListEndpoints' {maxResults} -> maxResults) (\s@ListEndpoints' {} a -> s {maxResults = a} :: ListEndpoints)

-- | A filter that returns only endpoints with the specified status.
listEndpoints_statusEquals :: Lens.Lens' ListEndpoints (Prelude.Maybe EndpointStatus)
listEndpoints_statusEquals = Lens.lens (\ListEndpoints' {statusEquals} -> statusEquals) (\s@ListEndpoints' {} a -> s {statusEquals = a} :: ListEndpoints)

-- | A filter that returns only endpoints with a creation time greater than
-- or equal to the specified time (timestamp).
listEndpoints_creationTimeAfter :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.UTCTime)
listEndpoints_creationTimeAfter = Lens.lens (\ListEndpoints' {creationTimeAfter} -> creationTimeAfter) (\s@ListEndpoints' {} a -> s {creationTimeAfter = a} :: ListEndpoints) Prelude.. Lens.mapping Core._Time

instance Core.AWSPager ListEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEndpointsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        (rs Lens.^. listEndpointsResponse_endpoints) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& listEndpoints_nextToken
          Lens..~ rs
          Lens.^? listEndpointsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest ListEndpoints where
  type
    AWSResponse ListEndpoints =
      ListEndpointsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            Prelude.<$> (x Core..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> (x Core..?> "Endpoints" Core..!@ Prelude.mempty)
      )

instance Prelude.Hashable ListEndpoints where
  hashWithSalt _salt ListEndpoints' {..} =
    _salt `Prelude.hashWithSalt` sortOrder
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` lastModifiedTimeAfter
      `Prelude.hashWithSalt` nameContains
      `Prelude.hashWithSalt` lastModifiedTimeBefore
      `Prelude.hashWithSalt` creationTimeBefore
      `Prelude.hashWithSalt` sortBy
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` statusEquals
      `Prelude.hashWithSalt` creationTimeAfter

instance Prelude.NFData ListEndpoints where
  rnf ListEndpoints' {..} =
    Prelude.rnf sortOrder
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lastModifiedTimeAfter
      `Prelude.seq` Prelude.rnf nameContains
      `Prelude.seq` Prelude.rnf lastModifiedTimeBefore
      `Prelude.seq` Prelude.rnf creationTimeBefore
      `Prelude.seq` Prelude.rnf sortBy
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf statusEquals
      `Prelude.seq` Prelude.rnf creationTimeAfter

instance Core.ToHeaders ListEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListEndpoints" :: Prelude.ByteString),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON ListEndpoints where
  toJSON ListEndpoints' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("SortOrder" Core..=) Prelude.<$> sortOrder,
            ("NextToken" Core..=) Prelude.<$> nextToken,
            ("LastModifiedTimeAfter" Core..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("NameContains" Core..=) Prelude.<$> nameContains,
            ("LastModifiedTimeBefore" Core..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("CreationTimeBefore" Core..=)
              Prelude.<$> creationTimeBefore,
            ("SortBy" Core..=) Prelude.<$> sortBy,
            ("MaxResults" Core..=) Prelude.<$> maxResults,
            ("StatusEquals" Core..=) Prelude.<$> statusEquals,
            ("CreationTimeAfter" Core..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListEndpoints where
  toPath = Prelude.const "/"

instance Core.ToQuery ListEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { -- | If the response is truncated, SageMaker returns this token. To retrieve
    -- the next set of training jobs, use it in the subsequent request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array or endpoint objects.
    endpoints :: [EndpointSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEndpointsResponse_nextToken' - If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of training jobs, use it in the subsequent request.
--
-- 'httpStatus', 'listEndpointsResponse_httpStatus' - The response's http status code.
--
-- 'endpoints', 'listEndpointsResponse_endpoints' - An array or endpoint objects.
newListEndpointsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListEndpointsResponse
newListEndpointsResponse pHttpStatus_ =
  ListEndpointsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      endpoints = Prelude.mempty
    }

-- | If the response is truncated, SageMaker returns this token. To retrieve
-- the next set of training jobs, use it in the subsequent request.
listEndpointsResponse_nextToken :: Lens.Lens' ListEndpointsResponse (Prelude.Maybe Prelude.Text)
listEndpointsResponse_nextToken = Lens.lens (\ListEndpointsResponse' {nextToken} -> nextToken) (\s@ListEndpointsResponse' {} a -> s {nextToken = a} :: ListEndpointsResponse)

-- | The response's http status code.
listEndpointsResponse_httpStatus :: Lens.Lens' ListEndpointsResponse Prelude.Int
listEndpointsResponse_httpStatus = Lens.lens (\ListEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListEndpointsResponse' {} a -> s {httpStatus = a} :: ListEndpointsResponse)

-- | An array or endpoint objects.
listEndpointsResponse_endpoints :: Lens.Lens' ListEndpointsResponse [EndpointSummary]
listEndpointsResponse_endpoints = Lens.lens (\ListEndpointsResponse' {endpoints} -> endpoints) (\s@ListEndpointsResponse' {} a -> s {endpoints = a} :: ListEndpointsResponse) Prelude.. Lens.coerced

instance Prelude.NFData ListEndpointsResponse where
  rnf ListEndpointsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
      `Prelude.seq` Prelude.rnf endpoints
