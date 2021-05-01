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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
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
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  GetExportSnapshotRecords'
    { pageToken =
        Prelude.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetExportSnapshotRecords@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getExportSnapshotRecords_pageToken :: Lens.Lens' GetExportSnapshotRecords (Prelude.Maybe Prelude.Text)
getExportSnapshotRecords_pageToken = Lens.lens (\GetExportSnapshotRecords' {pageToken} -> pageToken) (\s@GetExportSnapshotRecords' {} a -> s {pageToken = a} :: GetExportSnapshotRecords)

instance Pager.AWSPager GetExportSnapshotRecords where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getExportSnapshotRecordsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getExportSnapshotRecordsResponse_exportSnapshotRecords
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getExportSnapshotRecords_pageToken
          Lens..~ rs
          Lens.^? getExportSnapshotRecordsResponse_nextPageToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetExportSnapshotRecords where
  type
    Rs GetExportSnapshotRecords =
      GetExportSnapshotRecordsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetExportSnapshotRecordsResponse'
            Prelude.<$> ( x Prelude..?> "exportSnapshotRecords"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetExportSnapshotRecords

instance Prelude.NFData GetExportSnapshotRecords

instance Prelude.ToHeaders GetExportSnapshotRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetExportSnapshotRecords" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetExportSnapshotRecords where
  toJSON GetExportSnapshotRecords' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("pageToken" Prelude..=) Prelude.<$> pageToken]
      )

instance Prelude.ToPath GetExportSnapshotRecords where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetExportSnapshotRecords where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetExportSnapshotRecordsResponse' smart constructor.
data GetExportSnapshotRecordsResponse = GetExportSnapshotRecordsResponse'
  { -- | A list of objects describing the export snapshot records.
    exportSnapshotRecords :: Prelude.Maybe [ExportSnapshotRecord],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetExportSnapshotRecords@ request and specify the next page token using
    -- the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetExportSnapshotRecordsResponse
newGetExportSnapshotRecordsResponse pHttpStatus_ =
  GetExportSnapshotRecordsResponse'
    { exportSnapshotRecords =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of objects describing the export snapshot records.
getExportSnapshotRecordsResponse_exportSnapshotRecords :: Lens.Lens' GetExportSnapshotRecordsResponse (Prelude.Maybe [ExportSnapshotRecord])
getExportSnapshotRecordsResponse_exportSnapshotRecords = Lens.lens (\GetExportSnapshotRecordsResponse' {exportSnapshotRecords} -> exportSnapshotRecords) (\s@GetExportSnapshotRecordsResponse' {} a -> s {exportSnapshotRecords = a} :: GetExportSnapshotRecordsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetExportSnapshotRecords@ request and specify the next page token using
-- the @pageToken@ parameter.
getExportSnapshotRecordsResponse_nextPageToken :: Lens.Lens' GetExportSnapshotRecordsResponse (Prelude.Maybe Prelude.Text)
getExportSnapshotRecordsResponse_nextPageToken = Lens.lens (\GetExportSnapshotRecordsResponse' {nextPageToken} -> nextPageToken) (\s@GetExportSnapshotRecordsResponse' {} a -> s {nextPageToken = a} :: GetExportSnapshotRecordsResponse)

-- | The response's http status code.
getExportSnapshotRecordsResponse_httpStatus :: Lens.Lens' GetExportSnapshotRecordsResponse Prelude.Int
getExportSnapshotRecordsResponse_httpStatus = Lens.lens (\GetExportSnapshotRecordsResponse' {httpStatus} -> httpStatus) (\s@GetExportSnapshotRecordsResponse' {} a -> s {httpStatus = a} :: GetExportSnapshotRecordsResponse)

instance
  Prelude.NFData
    GetExportSnapshotRecordsResponse
