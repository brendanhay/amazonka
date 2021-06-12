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
-- Module      : Network.AWS.SageMaker.ListEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoints.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListEndpoints
  ( -- * Creating a Request
    ListEndpoints (..),
    newListEndpoints,

    -- * Request Lenses
    listEndpoints_lastModifiedTimeBefore,
    listEndpoints_sortOrder,
    listEndpoints_nextToken,
    listEndpoints_nameContains,
    listEndpoints_maxResults,
    listEndpoints_creationTimeBefore,
    listEndpoints_lastModifiedTimeAfter,
    listEndpoints_sortBy,
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

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { -- | A filter that returns only endpoints that were modified before the
    -- specified timestamp.
    lastModifiedTimeBefore :: Core.Maybe Core.POSIX,
    -- | The sort order for results. The default is @Descending@.
    sortOrder :: Core.Maybe OrderKey,
    -- | If the result of a @ListEndpoints@ request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of endpoints, use the
    -- token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A string in endpoint names. This filter returns only endpoints whose
    -- name contains the specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of endpoints to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only endpoints that were created before the
    -- specified time (timestamp).
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | A filter that returns only endpoints that were modified after the
    -- specified timestamp.
    lastModifiedTimeAfter :: Core.Maybe Core.POSIX,
    -- | Sorts the list of results. The default is @CreationTime@.
    sortBy :: Core.Maybe EndpointSortKey,
    -- | A filter that returns only endpoints with the specified status.
    statusEquals :: Core.Maybe EndpointStatus,
    -- | A filter that returns only endpoints with a creation time greater than
    -- or equal to the specified time (timestamp).
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'lastModifiedTimeBefore', 'listEndpoints_lastModifiedTimeBefore' - A filter that returns only endpoints that were modified before the
-- specified timestamp.
--
-- 'sortOrder', 'listEndpoints_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'nextToken', 'listEndpoints_nextToken' - If the result of a @ListEndpoints@ request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of endpoints, use the
-- token in the next request.
--
-- 'nameContains', 'listEndpoints_nameContains' - A string in endpoint names. This filter returns only endpoints whose
-- name contains the specified string.
--
-- 'maxResults', 'listEndpoints_maxResults' - The maximum number of endpoints to return in the response.
--
-- 'creationTimeBefore', 'listEndpoints_creationTimeBefore' - A filter that returns only endpoints that were created before the
-- specified time (timestamp).
--
-- 'lastModifiedTimeAfter', 'listEndpoints_lastModifiedTimeAfter' - A filter that returns only endpoints that were modified after the
-- specified timestamp.
--
-- 'sortBy', 'listEndpoints_sortBy' - Sorts the list of results. The default is @CreationTime@.
--
-- 'statusEquals', 'listEndpoints_statusEquals' - A filter that returns only endpoints with the specified status.
--
-- 'creationTimeAfter', 'listEndpoints_creationTimeAfter' - A filter that returns only endpoints with a creation time greater than
-- or equal to the specified time (timestamp).
newListEndpoints ::
  ListEndpoints
newListEndpoints =
  ListEndpoints'
    { lastModifiedTimeBefore =
        Core.Nothing,
      sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      lastModifiedTimeAfter = Core.Nothing,
      sortBy = Core.Nothing,
      statusEquals = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | A filter that returns only endpoints that were modified before the
-- specified timestamp.
listEndpoints_lastModifiedTimeBefore :: Lens.Lens' ListEndpoints (Core.Maybe Core.UTCTime)
listEndpoints_lastModifiedTimeBefore = Lens.lens (\ListEndpoints' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListEndpoints' {} a -> s {lastModifiedTimeBefore = a} :: ListEndpoints) Core.. Lens.mapping Core._Time

-- | The sort order for results. The default is @Descending@.
listEndpoints_sortOrder :: Lens.Lens' ListEndpoints (Core.Maybe OrderKey)
listEndpoints_sortOrder = Lens.lens (\ListEndpoints' {sortOrder} -> sortOrder) (\s@ListEndpoints' {} a -> s {sortOrder = a} :: ListEndpoints)

-- | If the result of a @ListEndpoints@ request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of endpoints, use the
-- token in the next request.
listEndpoints_nextToken :: Lens.Lens' ListEndpoints (Core.Maybe Core.Text)
listEndpoints_nextToken = Lens.lens (\ListEndpoints' {nextToken} -> nextToken) (\s@ListEndpoints' {} a -> s {nextToken = a} :: ListEndpoints)

-- | A string in endpoint names. This filter returns only endpoints whose
-- name contains the specified string.
listEndpoints_nameContains :: Lens.Lens' ListEndpoints (Core.Maybe Core.Text)
listEndpoints_nameContains = Lens.lens (\ListEndpoints' {nameContains} -> nameContains) (\s@ListEndpoints' {} a -> s {nameContains = a} :: ListEndpoints)

-- | The maximum number of endpoints to return in the response.
listEndpoints_maxResults :: Lens.Lens' ListEndpoints (Core.Maybe Core.Natural)
listEndpoints_maxResults = Lens.lens (\ListEndpoints' {maxResults} -> maxResults) (\s@ListEndpoints' {} a -> s {maxResults = a} :: ListEndpoints)

-- | A filter that returns only endpoints that were created before the
-- specified time (timestamp).
listEndpoints_creationTimeBefore :: Lens.Lens' ListEndpoints (Core.Maybe Core.UTCTime)
listEndpoints_creationTimeBefore = Lens.lens (\ListEndpoints' {creationTimeBefore} -> creationTimeBefore) (\s@ListEndpoints' {} a -> s {creationTimeBefore = a} :: ListEndpoints) Core.. Lens.mapping Core._Time

-- | A filter that returns only endpoints that were modified after the
-- specified timestamp.
listEndpoints_lastModifiedTimeAfter :: Lens.Lens' ListEndpoints (Core.Maybe Core.UTCTime)
listEndpoints_lastModifiedTimeAfter = Lens.lens (\ListEndpoints' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListEndpoints' {} a -> s {lastModifiedTimeAfter = a} :: ListEndpoints) Core.. Lens.mapping Core._Time

-- | Sorts the list of results. The default is @CreationTime@.
listEndpoints_sortBy :: Lens.Lens' ListEndpoints (Core.Maybe EndpointSortKey)
listEndpoints_sortBy = Lens.lens (\ListEndpoints' {sortBy} -> sortBy) (\s@ListEndpoints' {} a -> s {sortBy = a} :: ListEndpoints)

-- | A filter that returns only endpoints with the specified status.
listEndpoints_statusEquals :: Lens.Lens' ListEndpoints (Core.Maybe EndpointStatus)
listEndpoints_statusEquals = Lens.lens (\ListEndpoints' {statusEquals} -> statusEquals) (\s@ListEndpoints' {} a -> s {statusEquals = a} :: ListEndpoints)

-- | A filter that returns only endpoints with a creation time greater than
-- or equal to the specified time (timestamp).
listEndpoints_creationTimeAfter :: Lens.Lens' ListEndpoints (Core.Maybe Core.UTCTime)
listEndpoints_creationTimeAfter = Lens.lens (\ListEndpoints' {creationTimeAfter} -> creationTimeAfter) (\s@ListEndpoints' {} a -> s {creationTimeAfter = a} :: ListEndpoints) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEndpointsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^. listEndpointsResponse_endpoints) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listEndpoints_nextToken
          Lens..~ rs
          Lens.^? listEndpointsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest ListEndpoints where
  type
    AWSResponse ListEndpoints =
      ListEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "Endpoints" Core..!@ Core.mempty)
      )

instance Core.Hashable ListEndpoints

instance Core.NFData ListEndpoints

instance Core.ToHeaders ListEndpoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListEndpoints" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListEndpoints where
  toJSON ListEndpoints' {..} =
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
            ("StatusEquals" Core..=) Core.<$> statusEquals,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListEndpoints where
  toPath = Core.const "/"

instance Core.ToQuery ListEndpoints where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of training jobs, use it in the subsequent
    -- request.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array or endpoint objects.
    endpoints :: [EndpointSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEndpointsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of training jobs, use it in the subsequent
-- request.
--
-- 'httpStatus', 'listEndpointsResponse_httpStatus' - The response's http status code.
--
-- 'endpoints', 'listEndpointsResponse_endpoints' - An array or endpoint objects.
newListEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListEndpointsResponse
newListEndpointsResponse pHttpStatus_ =
  ListEndpointsResponse'
    { nextToken = Core.Nothing,
      httpStatus = pHttpStatus_,
      endpoints = Core.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of training jobs, use it in the subsequent
-- request.
listEndpointsResponse_nextToken :: Lens.Lens' ListEndpointsResponse (Core.Maybe Core.Text)
listEndpointsResponse_nextToken = Lens.lens (\ListEndpointsResponse' {nextToken} -> nextToken) (\s@ListEndpointsResponse' {} a -> s {nextToken = a} :: ListEndpointsResponse)

-- | The response's http status code.
listEndpointsResponse_httpStatus :: Lens.Lens' ListEndpointsResponse Core.Int
listEndpointsResponse_httpStatus = Lens.lens (\ListEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListEndpointsResponse' {} a -> s {httpStatus = a} :: ListEndpointsResponse)

-- | An array or endpoint objects.
listEndpointsResponse_endpoints :: Lens.Lens' ListEndpointsResponse [EndpointSummary]
listEndpointsResponse_endpoints = Lens.lens (\ListEndpointsResponse' {endpoints} -> endpoints) (\s@ListEndpointsResponse' {} a -> s {endpoints = a} :: ListEndpointsResponse) Core.. Lens._Coerce

instance Core.NFData ListEndpointsResponse
