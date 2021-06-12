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
-- Module      : Network.AWS.Lightsail.GetDiskSnapshots
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all block storage disk snapshots in your AWS
-- account and region.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDiskSnapshots
  ( -- * Creating a Request
    GetDiskSnapshots (..),
    newGetDiskSnapshots,

    -- * Request Lenses
    getDiskSnapshots_pageToken,

    -- * Destructuring the Response
    GetDiskSnapshotsResponse (..),
    newGetDiskSnapshotsResponse,

    -- * Response Lenses
    getDiskSnapshotsResponse_nextPageToken,
    getDiskSnapshotsResponse_diskSnapshots,
    getDiskSnapshotsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDiskSnapshots' smart constructor.
data GetDiskSnapshots = GetDiskSnapshots'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDiskSnapshots@ request. If
    -- your results are paginated, the response will return a next page token
    -- that you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDiskSnapshots' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getDiskSnapshots_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDiskSnapshots@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
newGetDiskSnapshots ::
  GetDiskSnapshots
newGetDiskSnapshots =
  GetDiskSnapshots' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDiskSnapshots@ request. If
-- your results are paginated, the response will return a next page token
-- that you can specify as the page token in a subsequent request.
getDiskSnapshots_pageToken :: Lens.Lens' GetDiskSnapshots (Core.Maybe Core.Text)
getDiskSnapshots_pageToken = Lens.lens (\GetDiskSnapshots' {pageToken} -> pageToken) (\s@GetDiskSnapshots' {} a -> s {pageToken = a} :: GetDiskSnapshots)

instance Core.AWSPager GetDiskSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDiskSnapshotsResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getDiskSnapshotsResponse_diskSnapshots
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getDiskSnapshots_pageToken
          Lens..~ rs
          Lens.^? getDiskSnapshotsResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest GetDiskSnapshots where
  type
    AWSResponse GetDiskSnapshots =
      GetDiskSnapshotsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDiskSnapshotsResponse'
            Core.<$> (x Core..?> "nextPageToken")
            Core.<*> (x Core..?> "diskSnapshots" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDiskSnapshots

instance Core.NFData GetDiskSnapshots

instance Core.ToHeaders GetDiskSnapshots where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetDiskSnapshots" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDiskSnapshots where
  toJSON GetDiskSnapshots' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetDiskSnapshots where
  toPath = Core.const "/"

instance Core.ToQuery GetDiskSnapshots where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDiskSnapshotsResponse' smart constructor.
data GetDiskSnapshotsResponse = GetDiskSnapshotsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetDiskSnapshots@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | An array of objects containing information about all block storage disk
    -- snapshots.
    diskSnapshots :: Core.Maybe [DiskSnapshot],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDiskSnapshotsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getDiskSnapshotsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDiskSnapshots@
-- request and specify the next page token using the @pageToken@ parameter.
--
-- 'diskSnapshots', 'getDiskSnapshotsResponse_diskSnapshots' - An array of objects containing information about all block storage disk
-- snapshots.
--
-- 'httpStatus', 'getDiskSnapshotsResponse_httpStatus' - The response's http status code.
newGetDiskSnapshotsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDiskSnapshotsResponse
newGetDiskSnapshotsResponse pHttpStatus_ =
  GetDiskSnapshotsResponse'
    { nextPageToken =
        Core.Nothing,
      diskSnapshots = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDiskSnapshots@
-- request and specify the next page token using the @pageToken@ parameter.
getDiskSnapshotsResponse_nextPageToken :: Lens.Lens' GetDiskSnapshotsResponse (Core.Maybe Core.Text)
getDiskSnapshotsResponse_nextPageToken = Lens.lens (\GetDiskSnapshotsResponse' {nextPageToken} -> nextPageToken) (\s@GetDiskSnapshotsResponse' {} a -> s {nextPageToken = a} :: GetDiskSnapshotsResponse)

-- | An array of objects containing information about all block storage disk
-- snapshots.
getDiskSnapshotsResponse_diskSnapshots :: Lens.Lens' GetDiskSnapshotsResponse (Core.Maybe [DiskSnapshot])
getDiskSnapshotsResponse_diskSnapshots = Lens.lens (\GetDiskSnapshotsResponse' {diskSnapshots} -> diskSnapshots) (\s@GetDiskSnapshotsResponse' {} a -> s {diskSnapshots = a} :: GetDiskSnapshotsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDiskSnapshotsResponse_httpStatus :: Lens.Lens' GetDiskSnapshotsResponse Core.Int
getDiskSnapshotsResponse_httpStatus = Lens.lens (\GetDiskSnapshotsResponse' {httpStatus} -> httpStatus) (\s@GetDiskSnapshotsResponse' {} a -> s {httpStatus = a} :: GetDiskSnapshotsResponse)

instance Core.NFData GetDiskSnapshotsResponse
