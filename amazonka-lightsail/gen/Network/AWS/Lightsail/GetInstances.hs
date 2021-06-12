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
-- Module      : Network.AWS.Lightsail.GetInstances
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all Amazon Lightsail virtual private servers,
-- or /instances/.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetInstances
  ( -- * Creating a Request
    GetInstances (..),
    newGetInstances,

    -- * Request Lenses
    getInstances_pageToken,

    -- * Destructuring the Response
    GetInstancesResponse (..),
    newGetInstancesResponse,

    -- * Response Lenses
    getInstancesResponse_instances,
    getInstancesResponse_nextPageToken,
    getInstancesResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInstances' smart constructor.
data GetInstances = GetInstances'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetInstances@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInstances' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getInstances_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstances@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
newGetInstances ::
  GetInstances
newGetInstances =
  GetInstances' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstances@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getInstances_pageToken :: Lens.Lens' GetInstances (Core.Maybe Core.Text)
getInstances_pageToken = Lens.lens (\GetInstances' {pageToken} -> pageToken) (\s@GetInstances' {} a -> s {pageToken = a} :: GetInstances)

instance Core.AWSPager GetInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInstancesResponse_nextPageToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getInstancesResponse_instances Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getInstances_pageToken
          Lens..~ rs
          Lens.^? getInstancesResponse_nextPageToken Core.. Lens._Just

instance Core.AWSRequest GetInstances where
  type AWSResponse GetInstances = GetInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstancesResponse'
            Core.<$> (x Core..?> "instances" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInstances

instance Core.NFData GetInstances

instance Core.ToHeaders GetInstances where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetInstances" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetInstances where
  toJSON GetInstances' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetInstances where
  toPath = Core.const "/"

instance Core.ToQuery GetInstances where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInstancesResponse' smart constructor.
data GetInstancesResponse = GetInstancesResponse'
  { -- | An array of key-value pairs containing information about your instances.
    instances :: Core.Maybe [Instance],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetInstances@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInstancesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instances', 'getInstancesResponse_instances' - An array of key-value pairs containing information about your instances.
--
-- 'nextPageToken', 'getInstancesResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetInstances@ request
-- and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getInstancesResponse_httpStatus' - The response's http status code.
newGetInstancesResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetInstancesResponse
newGetInstancesResponse pHttpStatus_ =
  GetInstancesResponse'
    { instances = Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about your instances.
getInstancesResponse_instances :: Lens.Lens' GetInstancesResponse (Core.Maybe [Instance])
getInstancesResponse_instances = Lens.lens (\GetInstancesResponse' {instances} -> instances) (\s@GetInstancesResponse' {} a -> s {instances = a} :: GetInstancesResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetInstances@ request
-- and specify the next page token using the @pageToken@ parameter.
getInstancesResponse_nextPageToken :: Lens.Lens' GetInstancesResponse (Core.Maybe Core.Text)
getInstancesResponse_nextPageToken = Lens.lens (\GetInstancesResponse' {nextPageToken} -> nextPageToken) (\s@GetInstancesResponse' {} a -> s {nextPageToken = a} :: GetInstancesResponse)

-- | The response's http status code.
getInstancesResponse_httpStatus :: Lens.Lens' GetInstancesResponse Core.Int
getInstancesResponse_httpStatus = Lens.lens (\GetInstancesResponse' {httpStatus} -> httpStatus) (\s@GetInstancesResponse' {} a -> s {httpStatus = a} :: GetInstancesResponse)

instance Core.NFData GetInstancesResponse
