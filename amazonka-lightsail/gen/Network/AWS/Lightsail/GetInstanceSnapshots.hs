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
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInstanceSnapshots' smart constructor.
data GetInstanceSnapshots = GetInstanceSnapshots'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetInstanceSnapshots@ request.
    -- If your results are paginated, the response will return a next page
    -- token that you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  GetInstanceSnapshots' {pageToken = Prelude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstanceSnapshots@ request.
-- If your results are paginated, the response will return a next page
-- token that you can specify as the page token in a subsequent request.
getInstanceSnapshots_pageToken :: Lens.Lens' GetInstanceSnapshots (Prelude.Maybe Prelude.Text)
getInstanceSnapshots_pageToken = Lens.lens (\GetInstanceSnapshots' {pageToken} -> pageToken) (\s@GetInstanceSnapshots' {} a -> s {pageToken = a} :: GetInstanceSnapshots)

instance Core.AWSPager GetInstanceSnapshots where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInstanceSnapshotsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getInstanceSnapshotsResponse_instanceSnapshots
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getInstanceSnapshots_pageToken
          Lens..~ rs
          Lens.^? getInstanceSnapshotsResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetInstanceSnapshots where
  type
    AWSResponse GetInstanceSnapshots =
      GetInstanceSnapshotsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstanceSnapshotsResponse'
            Prelude.<$> ( x Core..?> "instanceSnapshots"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Core..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstanceSnapshots

instance Prelude.NFData GetInstanceSnapshots

instance Core.ToHeaders GetInstanceSnapshots where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetInstanceSnapshots" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetInstanceSnapshots where
  toJSON GetInstanceSnapshots' {..} =
    Core.object
      ( Prelude.catMaybes
          [("pageToken" Core..=) Prelude.<$> pageToken]
      )

instance Core.ToPath GetInstanceSnapshots where
  toPath = Prelude.const "/"

instance Core.ToQuery GetInstanceSnapshots where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstanceSnapshotsResponse' smart constructor.
data GetInstanceSnapshotsResponse = GetInstanceSnapshotsResponse'
  { -- | An array of key-value pairs containing information about the results of
    -- your get instance snapshots request.
    instanceSnapshots :: Prelude.Maybe [InstanceSnapshot],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetInstanceSnapshots@
    -- request and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetInstanceSnapshotsResponse
newGetInstanceSnapshotsResponse pHttpStatus_ =
  GetInstanceSnapshotsResponse'
    { instanceSnapshots =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about the results of
-- your get instance snapshots request.
getInstanceSnapshotsResponse_instanceSnapshots :: Lens.Lens' GetInstanceSnapshotsResponse (Prelude.Maybe [InstanceSnapshot])
getInstanceSnapshotsResponse_instanceSnapshots = Lens.lens (\GetInstanceSnapshotsResponse' {instanceSnapshots} -> instanceSnapshots) (\s@GetInstanceSnapshotsResponse' {} a -> s {instanceSnapshots = a} :: GetInstanceSnapshotsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetInstanceSnapshots@
-- request and specify the next page token using the @pageToken@ parameter.
getInstanceSnapshotsResponse_nextPageToken :: Lens.Lens' GetInstanceSnapshotsResponse (Prelude.Maybe Prelude.Text)
getInstanceSnapshotsResponse_nextPageToken = Lens.lens (\GetInstanceSnapshotsResponse' {nextPageToken} -> nextPageToken) (\s@GetInstanceSnapshotsResponse' {} a -> s {nextPageToken = a} :: GetInstanceSnapshotsResponse)

-- | The response's http status code.
getInstanceSnapshotsResponse_httpStatus :: Lens.Lens' GetInstanceSnapshotsResponse Prelude.Int
getInstanceSnapshotsResponse_httpStatus = Lens.lens (\GetInstanceSnapshotsResponse' {httpStatus} -> httpStatus) (\s@GetInstanceSnapshotsResponse' {} a -> s {httpStatus = a} :: GetInstanceSnapshotsResponse)

instance Prelude.NFData GetInstanceSnapshotsResponse
