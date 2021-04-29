{-# LANGUAGE DeriveDataTypeable #-}
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListEndpoints' smart constructor.
data ListEndpoints = ListEndpoints'
  { -- | A filter that returns only endpoints that were modified before the
    -- specified timestamp.
    lastModifiedTimeBefore :: Prelude.Maybe Prelude.POSIX,
    -- | The sort order for results. The default is @Descending@.
    sortOrder :: Prelude.Maybe OrderKey,
    -- | If the result of a @ListEndpoints@ request was truncated, the response
    -- includes a @NextToken@. To retrieve the next set of endpoints, use the
    -- token in the next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A string in endpoint names. This filter returns only endpoints whose
    -- name contains the specified string.
    nameContains :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of endpoints to return in the response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A filter that returns only endpoints that were created before the
    -- specified time (timestamp).
    creationTimeBefore :: Prelude.Maybe Prelude.POSIX,
    -- | A filter that returns only endpoints that were modified after the
    -- specified timestamp.
    lastModifiedTimeAfter :: Prelude.Maybe Prelude.POSIX,
    -- | Sorts the list of results. The default is @CreationTime@.
    sortBy :: Prelude.Maybe EndpointSortKey,
    -- | A filter that returns only endpoints with the specified status.
    statusEquals :: Prelude.Maybe EndpointStatus,
    -- | A filter that returns only endpoints with a creation time greater than
    -- or equal to the specified time (timestamp).
    creationTimeAfter :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
        Prelude.Nothing,
      sortOrder = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      nameContains = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      creationTimeBefore = Prelude.Nothing,
      lastModifiedTimeAfter = Prelude.Nothing,
      sortBy = Prelude.Nothing,
      statusEquals = Prelude.Nothing,
      creationTimeAfter = Prelude.Nothing
    }

-- | A filter that returns only endpoints that were modified before the
-- specified timestamp.
listEndpoints_lastModifiedTimeBefore :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.UTCTime)
listEndpoints_lastModifiedTimeBefore = Lens.lens (\ListEndpoints' {lastModifiedTimeBefore} -> lastModifiedTimeBefore) (\s@ListEndpoints' {} a -> s {lastModifiedTimeBefore = a} :: ListEndpoints) Prelude.. Lens.mapping Prelude._Time

-- | The sort order for results. The default is @Descending@.
listEndpoints_sortOrder :: Lens.Lens' ListEndpoints (Prelude.Maybe OrderKey)
listEndpoints_sortOrder = Lens.lens (\ListEndpoints' {sortOrder} -> sortOrder) (\s@ListEndpoints' {} a -> s {sortOrder = a} :: ListEndpoints)

-- | If the result of a @ListEndpoints@ request was truncated, the response
-- includes a @NextToken@. To retrieve the next set of endpoints, use the
-- token in the next request.
listEndpoints_nextToken :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Text)
listEndpoints_nextToken = Lens.lens (\ListEndpoints' {nextToken} -> nextToken) (\s@ListEndpoints' {} a -> s {nextToken = a} :: ListEndpoints)

-- | A string in endpoint names. This filter returns only endpoints whose
-- name contains the specified string.
listEndpoints_nameContains :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Text)
listEndpoints_nameContains = Lens.lens (\ListEndpoints' {nameContains} -> nameContains) (\s@ListEndpoints' {} a -> s {nameContains = a} :: ListEndpoints)

-- | The maximum number of endpoints to return in the response.
listEndpoints_maxResults :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.Natural)
listEndpoints_maxResults = Lens.lens (\ListEndpoints' {maxResults} -> maxResults) (\s@ListEndpoints' {} a -> s {maxResults = a} :: ListEndpoints)

-- | A filter that returns only endpoints that were created before the
-- specified time (timestamp).
listEndpoints_creationTimeBefore :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.UTCTime)
listEndpoints_creationTimeBefore = Lens.lens (\ListEndpoints' {creationTimeBefore} -> creationTimeBefore) (\s@ListEndpoints' {} a -> s {creationTimeBefore = a} :: ListEndpoints) Prelude.. Lens.mapping Prelude._Time

-- | A filter that returns only endpoints that were modified after the
-- specified timestamp.
listEndpoints_lastModifiedTimeAfter :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.UTCTime)
listEndpoints_lastModifiedTimeAfter = Lens.lens (\ListEndpoints' {lastModifiedTimeAfter} -> lastModifiedTimeAfter) (\s@ListEndpoints' {} a -> s {lastModifiedTimeAfter = a} :: ListEndpoints) Prelude.. Lens.mapping Prelude._Time

-- | Sorts the list of results. The default is @CreationTime@.
listEndpoints_sortBy :: Lens.Lens' ListEndpoints (Prelude.Maybe EndpointSortKey)
listEndpoints_sortBy = Lens.lens (\ListEndpoints' {sortBy} -> sortBy) (\s@ListEndpoints' {} a -> s {sortBy = a} :: ListEndpoints)

-- | A filter that returns only endpoints with the specified status.
listEndpoints_statusEquals :: Lens.Lens' ListEndpoints (Prelude.Maybe EndpointStatus)
listEndpoints_statusEquals = Lens.lens (\ListEndpoints' {statusEquals} -> statusEquals) (\s@ListEndpoints' {} a -> s {statusEquals = a} :: ListEndpoints)

-- | A filter that returns only endpoints with a creation time greater than
-- or equal to the specified time (timestamp).
listEndpoints_creationTimeAfter :: Lens.Lens' ListEndpoints (Prelude.Maybe Prelude.UTCTime)
listEndpoints_creationTimeAfter = Lens.lens (\ListEndpoints' {creationTimeAfter} -> creationTimeAfter) (\s@ListEndpoints' {} a -> s {creationTimeAfter = a} :: ListEndpoints) Prelude.. Lens.mapping Prelude._Time

instance Pager.AWSPager ListEndpoints where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? listEndpointsResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        (rs Lens.^. listEndpointsResponse_endpoints) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& listEndpoints_nextToken
          Lens..~ rs
          Lens.^? listEndpointsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest ListEndpoints where
  type Rs ListEndpoints = ListEndpointsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
            Prelude.<*> ( x Prelude..?> "Endpoints"
                            Prelude..!@ Prelude.mempty
                        )
      )

instance Prelude.Hashable ListEndpoints

instance Prelude.NFData ListEndpoints

instance Prelude.ToHeaders ListEndpoints where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("SageMaker.ListEndpoints" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON ListEndpoints where
  toJSON ListEndpoints' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("LastModifiedTimeBefore" Prelude..=)
              Prelude.<$> lastModifiedTimeBefore,
            ("SortOrder" Prelude..=) Prelude.<$> sortOrder,
            ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("NameContains" Prelude..=) Prelude.<$> nameContains,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("CreationTimeBefore" Prelude..=)
              Prelude.<$> creationTimeBefore,
            ("LastModifiedTimeAfter" Prelude..=)
              Prelude.<$> lastModifiedTimeAfter,
            ("SortBy" Prelude..=) Prelude.<$> sortBy,
            ("StatusEquals" Prelude..=) Prelude.<$> statusEquals,
            ("CreationTimeAfter" Prelude..=)
              Prelude.<$> creationTimeAfter
          ]
      )

instance Prelude.ToPath ListEndpoints where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ListEndpoints where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newListEndpointsResponse' smart constructor.
data ListEndpointsResponse = ListEndpointsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of training jobs, use it in the subsequent
    -- request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int,
    -- | An array or endpoint objects.
    endpoints :: [EndpointSummary]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  ListEndpointsResponse
newListEndpointsResponse pHttpStatus_ =
  ListEndpointsResponse'
    { nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_,
      endpoints = Prelude.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of training jobs, use it in the subsequent
-- request.
listEndpointsResponse_nextToken :: Lens.Lens' ListEndpointsResponse (Prelude.Maybe Prelude.Text)
listEndpointsResponse_nextToken = Lens.lens (\ListEndpointsResponse' {nextToken} -> nextToken) (\s@ListEndpointsResponse' {} a -> s {nextToken = a} :: ListEndpointsResponse)

-- | The response's http status code.
listEndpointsResponse_httpStatus :: Lens.Lens' ListEndpointsResponse Prelude.Int
listEndpointsResponse_httpStatus = Lens.lens (\ListEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListEndpointsResponse' {} a -> s {httpStatus = a} :: ListEndpointsResponse)

-- | An array or endpoint objects.
listEndpointsResponse_endpoints :: Lens.Lens' ListEndpointsResponse [EndpointSummary]
listEndpointsResponse_endpoints = Lens.lens (\ListEndpointsResponse' {endpoints} -> endpoints) (\s@ListEndpointsResponse' {} a -> s {endpoints = a} :: ListEndpointsResponse) Prelude.. Prelude._Coerce

instance Prelude.NFData ListEndpointsResponse
