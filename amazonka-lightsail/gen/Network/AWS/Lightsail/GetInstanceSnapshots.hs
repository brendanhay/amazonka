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
-- Module      : Network.AWS.Lightsail.GetInstanceSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns all instance snapshots for the user\'s account.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetInstanceSnapshots
  ( -- * Creating a Request
    GetInstanceSnapshots (..),
    newGetInstanceSnapshots,

    -- * Request Lenses
    getInstanceSnapshots_pageToken,

    -- * Destructuring the Response
    GetInstanceSnapshotsResponse (..),
    newGetInstanceSnapshotsResponse,

    -- * Response Lenses
    getInstanceSnapshotsResponse_instanceSnapshots,
    getInstanceSnapshotsResponse_nextPageToken,
    getInstanceSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInstanceSnapshots' smart constructor.
data GetInstanceSnapshots = GetInstanceSnapshots'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetInstanceSnapshots@ request.
    -- If your results are paginated, the response will return a next page
    -- token that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInstanceSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getInstanceSnapshots_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstanceSnapshots@ request.
-- If your results are paginated, the response will return a next page
-- token that you can specify as the page token in a subsequent request.
newGetInstanceSnapshots ::
  GetInstanceSnapshots
newGetInstanceSnapshots =
  GetInstanceSnapshots' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstanceSnapshots@ request.
-- If your results are paginated, the response will return a next page
-- token that you can specify as the page token in a subsequent request.
getInstanceSnapshots_pageToken :: Lens.Lens' GetInstanceSnapshots (Core.Maybe Core.Text)
getInstanceSnapshots_pageToken = Lens.lens (\GetInstanceSnapshots' {pageToken} -> pageToken) (\s@GetInstanceSnapshots' {} a -> s {pageToken = a} :: GetInstanceSnapshots)

instance Core.AWSPager GetInstanceSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInstanceSnapshotsResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getInstanceSnapshotsResponse_instanceSnapshots
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getInstanceSnapshots_pageToken
          Lens..~ rs
          Lens.^? getInstanceSnapshotsResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest GetInstanceSnapshots where
  type
    AWSResponse GetInstanceSnapshots =
      GetInstanceSnapshotsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceSnapshotsResponse'
            Core.<$> (x Core..?> "instanceSnapshots" Core..!@ Core.mempty)
            Core.<*> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetInstanceSnapshots

instance Core.NFData GetInstanceSnapshots

instance Core.ToHeaders GetInstanceSnapshots where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetInstanceSnapshots" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetInstanceSnapshots where
  toJSON GetInstanceSnapshots' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetInstanceSnapshots where
  toPath = Core.const "/"

instance Core.ToQuery GetInstanceSnapshots where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetInstanceSnapshotsResponse' smart constructor.
data GetInstanceSnapshotsResponse = GetInstanceSnapshotsResponse'
  { -- | An array of key-value pairs containing information about the results of
    -- your get instance snapshots request.
    instanceSnapshots :: Core.Maybe [InstanceSnapshot],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetInstanceSnapshots@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetInstanceSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'instanceSnapshots', 'getInstanceSnapshotsResponse_instanceSnapshots' - An array of key-value pairs containing information about the results of
-- your get instance snapshots request.
--
-- 'nextPageToken', 'getInstanceSnapshotsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetInstanceSnapshots@
-- request and specify the next page token using the @pageToken@ parameter.
--
-- 'httpStatus', 'getInstanceSnapshotsResponse_httpStatus' - The response's http status code.
newGetInstanceSnapshotsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetInstanceSnapshotsResponse
newGetInstanceSnapshotsResponse pHttpStatus_ =
  GetInstanceSnapshotsResponse'
    { instanceSnapshots =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the results of
-- your get instance snapshots request.
getInstanceSnapshotsResponse_instanceSnapshots :: Lens.Lens' GetInstanceSnapshotsResponse (Core.Maybe [InstanceSnapshot])
getInstanceSnapshotsResponse_instanceSnapshots = Lens.lens (\GetInstanceSnapshotsResponse' {instanceSnapshots} -> instanceSnapshots) (\s@GetInstanceSnapshotsResponse' {} a -> s {instanceSnapshots = a} :: GetInstanceSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetInstanceSnapshots@
-- request and specify the next page token using the @pageToken@ parameter.
getInstanceSnapshotsResponse_nextPageToken :: Lens.Lens' GetInstanceSnapshotsResponse (Core.Maybe Core.Text)
getInstanceSnapshotsResponse_nextPageToken = Lens.lens (\GetInstanceSnapshotsResponse' {nextPageToken} -> nextPageToken) (\s@GetInstanceSnapshotsResponse' {} a -> s {nextPageToken = a} :: GetInstanceSnapshotsResponse)

-- | The response's http status code.
getInstanceSnapshotsResponse_httpStatus :: Lens.Lens' GetInstanceSnapshotsResponse Core.Int
getInstanceSnapshotsResponse_httpStatus = Lens.lens (\GetInstanceSnapshotsResponse' {httpStatus} -> httpStatus) (\s@GetInstanceSnapshotsResponse' {} a -> s {httpStatus = a} :: GetInstanceSnapshotsResponse)

instance Core.NFData GetInstanceSnapshotsResponse
