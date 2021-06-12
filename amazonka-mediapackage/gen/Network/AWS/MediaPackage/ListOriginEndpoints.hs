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
-- Module      : Network.AWS.MediaPackage.ListOriginEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a collection of OriginEndpoint records.
--
-- This operation returns paginated results.
module Network.AWS.MediaPackage.ListOriginEndpoints
  ( -- * Creating a Request
    ListOriginEndpoints (..),
    newListOriginEndpoints,

    -- * Request Lenses
    listOriginEndpoints_nextToken,
    listOriginEndpoints_channelId,
    listOriginEndpoints_maxResults,

    -- * Destructuring the Response
    ListOriginEndpointsResponse (..),
    newListOriginEndpointsResponse,

    -- * Response Lenses
    listOriginEndpointsResponse_originEndpoints,
    listOriginEndpointsResponse_nextToken,
    listOriginEndpointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.MediaPackage.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListOriginEndpoints' smart constructor.
data ListOriginEndpoints = ListOriginEndpoints'
  { -- | A token used to resume pagination from the end of a previous request.
    nextToken :: Core.Maybe Core.Text,
    -- | When specified, the request will return only OriginEndpoints associated
    -- with the given Channel ID.
    channelId :: Core.Maybe Core.Text,
    -- | The upper bound on the number of records to return.
    maxResults :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOriginEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listOriginEndpoints_nextToken' - A token used to resume pagination from the end of a previous request.
--
-- 'channelId', 'listOriginEndpoints_channelId' - When specified, the request will return only OriginEndpoints associated
-- with the given Channel ID.
--
-- 'maxResults', 'listOriginEndpoints_maxResults' - The upper bound on the number of records to return.
newListOriginEndpoints ::
  ListOriginEndpoints
newListOriginEndpoints =
  ListOriginEndpoints'
    { nextToken = Core.Nothing,
      channelId = Core.Nothing,
      maxResults = Core.Nothing
    }

-- | A token used to resume pagination from the end of a previous request.
listOriginEndpoints_nextToken :: Lens.Lens' ListOriginEndpoints (Core.Maybe Core.Text)
listOriginEndpoints_nextToken = Lens.lens (\ListOriginEndpoints' {nextToken} -> nextToken) (\s@ListOriginEndpoints' {} a -> s {nextToken = a} :: ListOriginEndpoints)

-- | When specified, the request will return only OriginEndpoints associated
-- with the given Channel ID.
listOriginEndpoints_channelId :: Lens.Lens' ListOriginEndpoints (Core.Maybe Core.Text)
listOriginEndpoints_channelId = Lens.lens (\ListOriginEndpoints' {channelId} -> channelId) (\s@ListOriginEndpoints' {} a -> s {channelId = a} :: ListOriginEndpoints)

-- | The upper bound on the number of records to return.
listOriginEndpoints_maxResults :: Lens.Lens' ListOriginEndpoints (Core.Maybe Core.Natural)
listOriginEndpoints_maxResults = Lens.lens (\ListOriginEndpoints' {maxResults} -> maxResults) (\s@ListOriginEndpoints' {} a -> s {maxResults = a} :: ListOriginEndpoints)

instance Core.AWSPager ListOriginEndpoints where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listOriginEndpointsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listOriginEndpointsResponse_originEndpoints
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listOriginEndpoints_nextToken
          Lens..~ rs
          Lens.^? listOriginEndpointsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListOriginEndpoints where
  type
    AWSResponse ListOriginEndpoints =
      ListOriginEndpointsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListOriginEndpointsResponse'
            Core.<$> (x Core..?> "originEndpoints" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListOriginEndpoints

instance Core.NFData ListOriginEndpoints

instance Core.ToHeaders ListOriginEndpoints where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListOriginEndpoints where
  toPath = Core.const "/origin_endpoints"

instance Core.ToQuery ListOriginEndpoints where
  toQuery ListOriginEndpoints' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "channelId" Core.=: channelId,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListOriginEndpointsResponse' smart constructor.
data ListOriginEndpointsResponse = ListOriginEndpointsResponse'
  { -- | A list of OriginEndpoint records.
    originEndpoints :: Core.Maybe [OriginEndpoint],
    -- | A token that can be used to resume pagination from the end of the
    -- collection.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListOriginEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'originEndpoints', 'listOriginEndpointsResponse_originEndpoints' - A list of OriginEndpoint records.
--
-- 'nextToken', 'listOriginEndpointsResponse_nextToken' - A token that can be used to resume pagination from the end of the
-- collection.
--
-- 'httpStatus', 'listOriginEndpointsResponse_httpStatus' - The response's http status code.
newListOriginEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListOriginEndpointsResponse
newListOriginEndpointsResponse pHttpStatus_ =
  ListOriginEndpointsResponse'
    { originEndpoints =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of OriginEndpoint records.
listOriginEndpointsResponse_originEndpoints :: Lens.Lens' ListOriginEndpointsResponse (Core.Maybe [OriginEndpoint])
listOriginEndpointsResponse_originEndpoints = Lens.lens (\ListOriginEndpointsResponse' {originEndpoints} -> originEndpoints) (\s@ListOriginEndpointsResponse' {} a -> s {originEndpoints = a} :: ListOriginEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | A token that can be used to resume pagination from the end of the
-- collection.
listOriginEndpointsResponse_nextToken :: Lens.Lens' ListOriginEndpointsResponse (Core.Maybe Core.Text)
listOriginEndpointsResponse_nextToken = Lens.lens (\ListOriginEndpointsResponse' {nextToken} -> nextToken) (\s@ListOriginEndpointsResponse' {} a -> s {nextToken = a} :: ListOriginEndpointsResponse)

-- | The response's http status code.
listOriginEndpointsResponse_httpStatus :: Lens.Lens' ListOriginEndpointsResponse Core.Int
listOriginEndpointsResponse_httpStatus = Lens.lens (\ListOriginEndpointsResponse' {httpStatus} -> httpStatus) (\s@ListOriginEndpointsResponse' {} a -> s {httpStatus = a} :: ListOriginEndpointsResponse)

instance Core.NFData ListOriginEndpointsResponse
