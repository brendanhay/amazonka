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
-- Module      : Network.AWS.Lightsail.GetDisks
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all block storage disks in your AWS account
-- and region.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetDisks
  ( -- * Creating a Request
    GetDisks (..),
    newGetDisks,

    -- * Request Lenses
    getDisks_pageToken,

    -- * Destructuring the Response
    GetDisksResponse (..),
    newGetDisksResponse,

    -- * Response Lenses
    getDisksResponse_nextPageToken,
    getDisksResponse_disks,
    getDisksResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetDisks' smart constructor.
data GetDisks = GetDisks'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetDisks@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDisks' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getDisks_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDisks@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
newGetDisks ::
  GetDisks
newGetDisks = GetDisks' {pageToken = Core.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetDisks@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getDisks_pageToken :: Lens.Lens' GetDisks (Core.Maybe Core.Text)
getDisks_pageToken = Lens.lens (\GetDisks' {pageToken} -> pageToken) (\s@GetDisks' {} a -> s {pageToken = a} :: GetDisks)

instance Core.AWSPager GetDisks where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getDisksResponse_nextPageToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        (rs Lens.^? getDisksResponse_disks Core.. Lens._Just) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getDisks_pageToken
          Lens..~ rs
          Lens.^? getDisksResponse_nextPageToken Core.. Lens._Just

instance Core.AWSRequest GetDisks where
  type AWSResponse GetDisks = GetDisksResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetDisksResponse'
            Core.<$> (x Core..?> "nextPageToken")
            Core.<*> (x Core..?> "disks" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetDisks

instance Core.NFData GetDisks

instance Core.ToHeaders GetDisks where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("Lightsail_20161128.GetDisks" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetDisks where
  toJSON GetDisks' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetDisks where
  toPath = Core.const "/"

instance Core.ToQuery GetDisks where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetDisksResponse' smart constructor.
data GetDisksResponse = GetDisksResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetDisks@ request and
    -- specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | An array of objects containing information about all block storage
    -- disks.
    disks :: Core.Maybe [Disk],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetDisksResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageToken', 'getDisksResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDisks@ request and
-- specify the next page token using the @pageToken@ parameter.
--
-- 'disks', 'getDisksResponse_disks' - An array of objects containing information about all block storage
-- disks.
--
-- 'httpStatus', 'getDisksResponse_httpStatus' - The response's http status code.
newGetDisksResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetDisksResponse
newGetDisksResponse pHttpStatus_ =
  GetDisksResponse'
    { nextPageToken = Core.Nothing,
      disks = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetDisks@ request and
-- specify the next page token using the @pageToken@ parameter.
getDisksResponse_nextPageToken :: Lens.Lens' GetDisksResponse (Core.Maybe Core.Text)
getDisksResponse_nextPageToken = Lens.lens (\GetDisksResponse' {nextPageToken} -> nextPageToken) (\s@GetDisksResponse' {} a -> s {nextPageToken = a} :: GetDisksResponse)

-- | An array of objects containing information about all block storage
-- disks.
getDisksResponse_disks :: Lens.Lens' GetDisksResponse (Core.Maybe [Disk])
getDisksResponse_disks = Lens.lens (\GetDisksResponse' {disks} -> disks) (\s@GetDisksResponse' {} a -> s {disks = a} :: GetDisksResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getDisksResponse_httpStatus :: Lens.Lens' GetDisksResponse Core.Int
getDisksResponse_httpStatus = Lens.lens (\GetDisksResponse' {httpStatus} -> httpStatus) (\s@GetDisksResponse' {} a -> s {httpStatus = a} :: GetDisksResponse)

instance Core.NFData GetDisksResponse
