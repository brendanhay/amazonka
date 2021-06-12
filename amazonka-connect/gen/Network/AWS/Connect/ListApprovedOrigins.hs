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
-- Module      : Network.AWS.Connect.ListApprovedOrigins
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This API is in preview release for Amazon Connect and is subject to
-- change.
--
-- Returns a paginated list of all approved origins associated with the
-- instance.
--
-- This operation returns paginated results.
module Network.AWS.Connect.ListApprovedOrigins
  ( -- * Creating a Request
    ListApprovedOrigins (..),
    newListApprovedOrigins,

    -- * Request Lenses
    listApprovedOrigins_nextToken,
    listApprovedOrigins_maxResults,
    listApprovedOrigins_instanceId,

    -- * Destructuring the Response
    ListApprovedOriginsResponse (..),
    newListApprovedOriginsResponse,

    -- * Response Lenses
    listApprovedOriginsResponse_origins,
    listApprovedOriginsResponse_nextToken,
    listApprovedOriginsResponse_httpStatus,
  )
where

import Network.AWS.Connect.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newListApprovedOrigins' smart constructor.
data ListApprovedOrigins = ListApprovedOrigins'
  { -- | The token for the next set of results. Use the value returned in the
    -- previous response in the next request to retrieve the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The maximum number of results to return per page.
    maxResults :: Core.Maybe Core.Natural,
    -- | The identifier of the Amazon Connect instance.
    instanceId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApprovedOrigins' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listApprovedOrigins_nextToken' - The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
--
-- 'maxResults', 'listApprovedOrigins_maxResults' - The maximum number of results to return per page.
--
-- 'instanceId', 'listApprovedOrigins_instanceId' - The identifier of the Amazon Connect instance.
newListApprovedOrigins ::
  -- | 'instanceId'
  Core.Text ->
  ListApprovedOrigins
newListApprovedOrigins pInstanceId_ =
  ListApprovedOrigins'
    { nextToken = Core.Nothing,
      maxResults = Core.Nothing,
      instanceId = pInstanceId_
    }

-- | The token for the next set of results. Use the value returned in the
-- previous response in the next request to retrieve the next set of
-- results.
listApprovedOrigins_nextToken :: Lens.Lens' ListApprovedOrigins (Core.Maybe Core.Text)
listApprovedOrigins_nextToken = Lens.lens (\ListApprovedOrigins' {nextToken} -> nextToken) (\s@ListApprovedOrigins' {} a -> s {nextToken = a} :: ListApprovedOrigins)

-- | The maximum number of results to return per page.
listApprovedOrigins_maxResults :: Lens.Lens' ListApprovedOrigins (Core.Maybe Core.Natural)
listApprovedOrigins_maxResults = Lens.lens (\ListApprovedOrigins' {maxResults} -> maxResults) (\s@ListApprovedOrigins' {} a -> s {maxResults = a} :: ListApprovedOrigins)

-- | The identifier of the Amazon Connect instance.
listApprovedOrigins_instanceId :: Lens.Lens' ListApprovedOrigins Core.Text
listApprovedOrigins_instanceId = Lens.lens (\ListApprovedOrigins' {instanceId} -> instanceId) (\s@ListApprovedOrigins' {} a -> s {instanceId = a} :: ListApprovedOrigins)

instance Core.AWSPager ListApprovedOrigins where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listApprovedOriginsResponse_nextToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? listApprovedOriginsResponse_origins
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& listApprovedOrigins_nextToken
          Lens..~ rs
          Lens.^? listApprovedOriginsResponse_nextToken
            Core.. Lens._Just

instance Core.AWSRequest ListApprovedOrigins where
  type
    AWSResponse ListApprovedOrigins =
      ListApprovedOriginsResponse
  request = Request.get defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          ListApprovedOriginsResponse'
            Core.<$> (x Core..?> "Origins" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "NextToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable ListApprovedOrigins

instance Core.NFData ListApprovedOrigins

instance Core.ToHeaders ListApprovedOrigins where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToPath ListApprovedOrigins where
  toPath ListApprovedOrigins' {..} =
    Core.mconcat
      [ "/instance/",
        Core.toBS instanceId,
        "/approved-origins"
      ]

instance Core.ToQuery ListApprovedOrigins where
  toQuery ListApprovedOrigins' {..} =
    Core.mconcat
      [ "nextToken" Core.=: nextToken,
        "maxResults" Core.=: maxResults
      ]

-- | /See:/ 'newListApprovedOriginsResponse' smart constructor.
data ListApprovedOriginsResponse = ListApprovedOriginsResponse'
  { -- | The approved origins.
    origins :: Core.Maybe [Core.Text],
    -- | If there are additional results, this is the token for the next set of
    -- results.
    nextToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ListApprovedOriginsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'origins', 'listApprovedOriginsResponse_origins' - The approved origins.
--
-- 'nextToken', 'listApprovedOriginsResponse_nextToken' - If there are additional results, this is the token for the next set of
-- results.
--
-- 'httpStatus', 'listApprovedOriginsResponse_httpStatus' - The response's http status code.
newListApprovedOriginsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  ListApprovedOriginsResponse
newListApprovedOriginsResponse pHttpStatus_ =
  ListApprovedOriginsResponse'
    { origins =
        Core.Nothing,
      nextToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The approved origins.
listApprovedOriginsResponse_origins :: Lens.Lens' ListApprovedOriginsResponse (Core.Maybe [Core.Text])
listApprovedOriginsResponse_origins = Lens.lens (\ListApprovedOriginsResponse' {origins} -> origins) (\s@ListApprovedOriginsResponse' {} a -> s {origins = a} :: ListApprovedOriginsResponse) Core.. Lens.mapping Lens._Coerce

-- | If there are additional results, this is the token for the next set of
-- results.
listApprovedOriginsResponse_nextToken :: Lens.Lens' ListApprovedOriginsResponse (Core.Maybe Core.Text)
listApprovedOriginsResponse_nextToken = Lens.lens (\ListApprovedOriginsResponse' {nextToken} -> nextToken) (\s@ListApprovedOriginsResponse' {} a -> s {nextToken = a} :: ListApprovedOriginsResponse)

-- | The response's http status code.
listApprovedOriginsResponse_httpStatus :: Lens.Lens' ListApprovedOriginsResponse Core.Int
listApprovedOriginsResponse_httpStatus = Lens.lens (\ListApprovedOriginsResponse' {httpStatus} -> httpStatus) (\s@ListApprovedOriginsResponse' {} a -> s {httpStatus = a} :: ListApprovedOriginsResponse)

instance Core.NFData ListApprovedOriginsResponse
