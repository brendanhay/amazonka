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
-- Module      : Network.AWS.SageMaker.ListEndpointConfigs
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists endpoint configurations.
--
-- This operation returns paginated results.
module Network.AWS.SageMaker.ListEndpointConfigs
  ( -- * Creating a Request
    ListEndpointConfigs (..),
    newListEndpointConfigs,

    -- * Request Lenses
    listEndpointConfigs_sortOrder,
    listEndpointConfigs_nextToken,
    listEndpointConfigs_nameContains,
    listEndpointConfigs_maxResults,
    listEndpointConfigs_creationTimeBefore,
    listEndpointConfigs_sortBy,
    listEndpointConfigs_creationTimeAfter,

    -- * Destructuring the Response
    ListEndpointConfigsResponse (..),
    newListEndpointConfigsResponse,

    -- * Response Lenses
    listEndpointConfigsResponse_nextToken,
    listEndpointConfigsResponse_httpStatus,
    listEndpointConfigsResponse_endpointConfigs,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.SageMaker.Types

-- | /See:/ 'newListEndpointConfigs' smart constructor.
data ListEndpointConfigs = ListEndpointConfigs'
  { -- | The sort order for results. The default is @Descending@.
    sortOrder :: Core.Maybe OrderKey,
    -- | If the result of the previous @ListEndpointConfig@ request was
    -- truncated, the response includes a @NextToken@. To retrieve the next set
    -- of endpoint configurations, use the token in the next request.
    nextToken :: Core.Maybe Core.Text,
    -- | A string in the endpoint configuration name. This filter returns only
    -- endpoint configurations whose name contains the specified string.
    nameContains :: Core.Maybe Core.Text,
    -- | The maximum number of training jobs to return in the response.
    maxResults :: Core.Maybe Core.Natural,
    -- | A filter that returns only endpoint configurations created before the
    -- specified time (timestamp).
    creationTimeBefore :: Core.Maybe Core.POSIX,
    -- | The field to sort results by. The default is @CreationTime@.
    sortBy :: Core.Maybe EndpointConfigSortKey,
    -- | A filter that returns only endpoint configurations with a creation time
    -- greater than or equal to the specified time (timestamp).
    creationTimeAfter :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEndpointConfigs' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'sortOrder', 'listEndpointConfigs_sortOrder' - The sort order for results. The default is @Descending@.
--
-- 'nextToken', 'listEndpointConfigs_nextToken' - If the result of the previous @ListEndpointConfig@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of endpoint configurations, use the token in the next request.
--
-- 'nameContains', 'listEndpointConfigs_nameContains' - A string in the endpoint configuration name. This filter returns only
-- endpoint configurations whose name contains the specified string.
--
-- 'maxResults', 'listEndpointConfigs_maxResults' - The maximum number of training jobs to return in the response.
--
-- 'creationTimeBefore', 'listEndpointConfigs_creationTimeBefore' - A filter that returns only endpoint configurations created before the
-- specified time (timestamp).
--
-- 'sortBy', 'listEndpointConfigs_sortBy' - The field to sort results by. The default is @CreationTime@.
--
-- 'creationTimeAfter', 'listEndpointConfigs_creationTimeAfter' - A filter that returns only endpoint configurations with a creation time
-- greater than or equal to the specified time (timestamp).
newListEndpointConfigs ::
  ListEndpointConfigs
newListEndpointConfigs =
  ListEndpointConfigs'
    { sortOrder = Core.Nothing,
      nextToken = Core.Nothing,
      nameContains = Core.Nothing,
      maxResults = Core.Nothing,
      creationTimeBefore = Core.Nothing,
      sortBy = Core.Nothing,
      creationTimeAfter = Core.Nothing
    }

-- | The sort order for results. The default is @Descending@.
listEndpointConfigs_sortOrder :: Lens.Lens' ListEndpointConfigs (Core.Maybe OrderKey)
listEndpointConfigs_sortOrder = Lens.lens (\ListEndpointConfigs' {sortOrder} -> sortOrder) (\s@ListEndpointConfigs' {} a -> s {sortOrder = a} :: ListEndpointConfigs)

-- | If the result of the previous @ListEndpointConfig@ request was
-- truncated, the response includes a @NextToken@. To retrieve the next set
-- of endpoint configurations, use the token in the next request.
listEndpointConfigs_nextToken :: Lens.Lens' ListEndpointConfigs (Core.Maybe Core.Text)
listEndpointConfigs_nextToken = Lens.lens (\ListEndpointConfigs' {nextToken} -> nextToken) (\s@ListEndpointConfigs' {} a -> s {nextToken = a} :: ListEndpointConfigs)

-- | A string in the endpoint configuration name. This filter returns only
-- endpoint configurations whose name contains the specified string.
listEndpointConfigs_nameContains :: Lens.Lens' ListEndpointConfigs (Core.Maybe Core.Text)
listEndpointConfigs_nameContains = Lens.lens (\ListEndpointConfigs' {nameContains} -> nameContains) (\s@ListEndpointConfigs' {} a -> s {nameContains = a} :: ListEndpointConfigs)

-- | The maximum number of training jobs to return in the response.
listEndpointConfigs_maxResults :: Lens.Lens' ListEndpointConfigs (Core.Maybe Core.Natural)
listEndpointConfigs_maxResults = Lens.lens (\ListEndpointConfigs' {maxResults} -> maxResults) (\s@ListEndpointConfigs' {} a -> s {maxResults = a} :: ListEndpointConfigs)

-- | A filter that returns only endpoint configurations created before the
-- specified time (timestamp).
listEndpointConfigs_creationTimeBefore :: Lens.Lens' ListEndpointConfigs (Core.Maybe Core.UTCTime)
listEndpointConfigs_creationTimeBefore = Lens.lens (\ListEndpointConfigs' {creationTimeBefore} -> creationTimeBefore) (\s@ListEndpointConfigs' {} a -> s {creationTimeBefore = a} :: ListEndpointConfigs) Core.. Lens.mapping Core._Time

-- | The field to sort results by. The default is @CreationTime@.
listEndpointConfigs_sortBy :: Lens.Lens' ListEndpointConfigs (Core.Maybe EndpointConfigSortKey)
listEndpointConfigs_sortBy = Lens.lens (\ListEndpointConfigs' {sortBy} -> sortBy) (\s@ListEndpointConfigs' {} a -> s {sortBy = a} :: ListEndpointConfigs)

-- | A filter that returns only endpoint configurations with a creation time
-- greater than or equal to the specified time (timestamp).
listEndpointConfigs_creationTimeAfter :: Lens.Lens' ListEndpointConfigs (Core.Maybe Core.UTCTime)
listEndpointConfigs_creationTimeAfter = Lens.lens (\ListEndpointConfigs' {creationTimeAfter} -> creationTimeAfter) (\s@ListEndpointConfigs' {} a -> s {creationTimeAfter = a} :: ListEndpointConfigs) Core.. Lens.mapping Core._Time

instance Core.AWSPager ListEndpointConfigs where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listEndpointConfigsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^. listEndpointConfigsResponse_endpointConfigs
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listEndpointConfigs_nextToken
          Lens..~ rs
          Lens.^? listEndpointConfigsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListEndpointConfigs where
  type
    AWSResponse ListEndpointConfigs =
      ListEndpointConfigsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListEndpointConfigsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
            Core.<*> (x Core..?> "EndpointConfigs" Core..!@ Core.mempty)
      )

instance Core.Hashable ListEndpointConfigs

instance Core.NFData ListEndpointConfigs

instance Core.ToHeaders ListEndpointConfigs where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("SageMaker.ListEndpointConfigs" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON ListEndpointConfigs where
  toJSON ListEndpointConfigs' {..} =
    Core.object
      ( Core.catMaybes
          [ ("SortOrder" Core..=) Core.<$> sortOrder,
            ("NextToken" Core..=) Core.<$> nextToken,
            ("NameContains" Core..=) Core.<$> nameContains,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("CreationTimeBefore" Core..=)
              Core.<$> creationTimeBefore,
            ("SortBy" Core..=) Core.<$> sortBy,
            ("CreationTimeAfter" Core..=)
              Core.<$> creationTimeAfter
          ]
      )

instance Core.ToPath ListEndpointConfigs where
  toPath = Core.const "/"

instance Core.ToQuery ListEndpointConfigs where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newListEndpointConfigsResponse' smart constructor.
data ListEndpointConfigsResponse = ListEndpointConfigsResponse'
  { -- | If the response is truncated, Amazon SageMaker returns this token. To
    -- retrieve the next set of endpoint configurations, use it in the
    -- subsequent request
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int,
    -- | An array of endpoint configurations.
    endpointConfigs :: [EndpointConfigSummary]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListEndpointConfigsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listEndpointConfigsResponse_nextToken' - If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of endpoint configurations, use it in the
-- subsequent request
--
-- 'httpStatus', 'listEndpointConfigsResponse_httpStatus' - The response's http status code.
--
-- 'endpointConfigs', 'listEndpointConfigsResponse_endpointConfigs' - An array of endpoint configurations.
newListEndpointConfigsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListEndpointConfigsResponse
newListEndpointConfigsResponse pHttpStatus_ =
  ListEndpointConfigsResponse'
    { nextToken =
        Core.Nothing,
      httpStatus = pHttpStatus_,
      endpointConfigs = Core.mempty
    }

-- | If the response is truncated, Amazon SageMaker returns this token. To
-- retrieve the next set of endpoint configurations, use it in the
-- subsequent request
listEndpointConfigsResponse_nextToken :: Lens.Lens' ListEndpointConfigsResponse (Core.Maybe Core.Text)
listEndpointConfigsResponse_nextToken = Lens.lens (\ListEndpointConfigsResponse' {nextToken} -> nextToken) (\s@ListEndpointConfigsResponse' {} a -> s {nextToken = a} :: ListEndpointConfigsResponse)

-- | The response's http status code.
listEndpointConfigsResponse_httpStatus :: Lens.Lens' ListEndpointConfigsResponse Core.Int
listEndpointConfigsResponse_httpStatus = Lens.lens (\ListEndpointConfigsResponse' {httpStatus} -> httpStatus) (\s@ListEndpointConfigsResponse' {} a -> s {httpStatus = a} :: ListEndpointConfigsResponse)

-- | An array of endpoint configurations.
listEndpointConfigsResponse_endpointConfigs :: Lens.Lens' ListEndpointConfigsResponse [EndpointConfigSummary]
listEndpointConfigsResponse_endpointConfigs = Lens.lens (\ListEndpointConfigsResponse' {endpointConfigs} -> endpointConfigs) (\s@ListEndpointConfigsResponse' {} a -> s {endpointConfigs = a} :: ListEndpointConfigsResponse) Core.. Lens._Coerce

instance Core.NFData ListEndpointConfigsResponse
