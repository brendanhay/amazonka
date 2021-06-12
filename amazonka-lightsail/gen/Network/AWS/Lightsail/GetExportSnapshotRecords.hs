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
-- Module      : Network.AWS.Lightsail.GetExportSnapshotRecords
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the export snapshot record created as a result of the
-- @export snapshot@ operation.
--
-- An export snapshot record can be used to create a new Amazon EC2
-- instance and its related resources with the
-- @create cloud formation stack@ operation.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetExportSnapshotRecords
  ( -- * Creating a Request
    GetExportSnapshotRecords (..),
    newGetExportSnapshotRecords,

    -- * Request Lenses
    getExportSnapshotRecords_pageToken,

    -- * Destructuring the Response
    GetExportSnapshotRecordsResponse (..),
    newGetExportSnapshotRecordsResponse,

    -- * Response Lenses
    getExportSnapshotRecordsResponse_exportSnapshotRecords,
    getExportSnapshotRecordsResponse_nextPageToken,
    getExportSnapshotRecordsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetExportSnapshotRecords' smart constructor.
data GetExportSnapshotRecords = GetExportSnapshotRecords'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetExportSnapshotRecords@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetExportSnapshotRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getExportSnapshotRecords_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetExportSnapshotRecords@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
newGetExportSnapshotRecords ::
  GetExportSnapshotRecords
newGetExportSnapshotRecords =
  GetExportSnapshotRecords' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetExportSnapshotRecords@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getExportSnapshotRecords_pageToken :: Lens.Lens' GetExportSnapshotRecords (Core.Maybe Core.Text)
getExportSnapshotRecords_pageToken = Lens.lens (\GetExportSnapshotRecords' {pageToken} -> pageToken) (\s@GetExportSnapshotRecords' {} a -> s {pageToken = a} :: GetExportSnapshotRecords)

instance Core.AWSPager GetExportSnapshotRecords where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getExportSnapshotRecordsResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getExportSnapshotRecordsResponse_exportSnapshotRecords
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getExportSnapshotRecords_pageToken
          Lens..~ rs
          Lens.^? getExportSnapshotRecordsResponse_nextPageToken
            Core.. Lens._Just

instance Core.AWSRequest GetExportSnapshotRecords where
  type
    AWSResponse GetExportSnapshotRecords =
      GetExportSnapshotRecordsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExportSnapshotRecordsResponse'
            Core.<$> ( x Core..?> "exportSnapshotRecords"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetExportSnapshotRecords

instance Core.NFData GetExportSnapshotRecords

instance Core.ToHeaders GetExportSnapshotRecords where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetExportSnapshotRecords" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetExportSnapshotRecords where
  toJSON GetExportSnapshotRecords' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetExportSnapshotRecords where
  toPath = Core.const "/"

instance Core.ToQuery GetExportSnapshotRecords where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetExportSnapshotRecordsResponse' smart constructor.
data GetExportSnapshotRecordsResponse = GetExportSnapshotRecordsResponse'
  { -- | A list of objects describing the export snapshot records.
    exportSnapshotRecords :: Core.Maybe [ExportSnapshotRecord],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetExportSnapshotRecords@ request and specify the next page token using
    -- the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetExportSnapshotRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'exportSnapshotRecords', 'getExportSnapshotRecordsResponse_exportSnapshotRecords' - A list of objects describing the export snapshot records.
--
-- 'nextPageToken', 'getExportSnapshotRecordsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetExportSnapshotRecords@ request and specify the next page token using
-- the @pageToken@ parameter.
--
-- 'httpStatus', 'getExportSnapshotRecordsResponse_httpStatus' - The response's http status code.
newGetExportSnapshotRecordsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetExportSnapshotRecordsResponse
newGetExportSnapshotRecordsResponse pHttpStatus_ =
  GetExportSnapshotRecordsResponse'
    { exportSnapshotRecords =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of objects describing the export snapshot records.
getExportSnapshotRecordsResponse_exportSnapshotRecords :: Lens.Lens' GetExportSnapshotRecordsResponse (Core.Maybe [ExportSnapshotRecord])
getExportSnapshotRecordsResponse_exportSnapshotRecords = Lens.lens (\GetExportSnapshotRecordsResponse' {exportSnapshotRecords} -> exportSnapshotRecords) (\s@GetExportSnapshotRecordsResponse' {} a -> s {exportSnapshotRecords = a} :: GetExportSnapshotRecordsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetExportSnapshotRecords@ request and specify the next page token using
-- the @pageToken@ parameter.
getExportSnapshotRecordsResponse_nextPageToken :: Lens.Lens' GetExportSnapshotRecordsResponse (Core.Maybe Core.Text)
getExportSnapshotRecordsResponse_nextPageToken = Lens.lens (\GetExportSnapshotRecordsResponse' {nextPageToken} -> nextPageToken) (\s@GetExportSnapshotRecordsResponse' {} a -> s {nextPageToken = a} :: GetExportSnapshotRecordsResponse)

-- | The response's http status code.
getExportSnapshotRecordsResponse_httpStatus :: Lens.Lens' GetExportSnapshotRecordsResponse Core.Int
getExportSnapshotRecordsResponse_httpStatus = Lens.lens (\GetExportSnapshotRecordsResponse' {httpStatus} -> httpStatus) (\s@GetExportSnapshotRecordsResponse' {} a -> s {httpStatus = a} :: GetExportSnapshotRecordsResponse)

instance Core.NFData GetExportSnapshotRecordsResponse
