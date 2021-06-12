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
-- Module      : Network.AWS.Lightsail.GetCloudFormationStackRecords
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the CloudFormation stack record created as a result of the
-- @create cloud formation stack@ operation.
--
-- An AWS CloudFormation stack is used to create a new Amazon EC2 instance
-- from an exported Lightsail snapshot.
--
-- This operation returns paginated results.
module Network.AWS.Lightsail.GetCloudFormationStackRecords
  ( -- * Creating a Request
    GetCloudFormationStackRecords (..),
    newGetCloudFormationStackRecords,

    -- * Request Lenses
    getCloudFormationStackRecords_pageToken,

    -- * Destructuring the Response
    GetCloudFormationStackRecordsResponse (..),
    newGetCloudFormationStackRecordsResponse,

    -- * Response Lenses
    getCloudFormationStackRecordsResponse_cloudFormationStackRecords,
    getCloudFormationStackRecordsResponse_nextPageToken,
    getCloudFormationStackRecordsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCloudFormationStackRecords' smart constructor.
data GetCloudFormationStackRecords = GetCloudFormationStackRecords'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetClouFormationStackRecords@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCloudFormationStackRecords' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getCloudFormationStackRecords_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetClouFormationStackRecords@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
newGetCloudFormationStackRecords ::
  GetCloudFormationStackRecords
newGetCloudFormationStackRecords =
  GetCloudFormationStackRecords'
    { pageToken =
        Core.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetClouFormationStackRecords@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getCloudFormationStackRecords_pageToken :: Lens.Lens' GetCloudFormationStackRecords (Core.Maybe Core.Text)
getCloudFormationStackRecords_pageToken = Lens.lens (\GetCloudFormationStackRecords' {pageToken} -> pageToken) (\s@GetCloudFormationStackRecords' {} a -> s {pageToken = a} :: GetCloudFormationStackRecords)

instance Core.AWSPager GetCloudFormationStackRecords where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCloudFormationStackRecordsResponse_nextPageToken
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getCloudFormationStackRecordsResponse_cloudFormationStackRecords
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getCloudFormationStackRecords_pageToken
          Lens..~ rs
          Lens.^? getCloudFormationStackRecordsResponse_nextPageToken
            Core.. Lens._Just

instance
  Core.AWSRequest
    GetCloudFormationStackRecords
  where
  type
    AWSResponse GetCloudFormationStackRecords =
      GetCloudFormationStackRecordsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCloudFormationStackRecordsResponse'
            Core.<$> ( x Core..?> "cloudFormationStackRecords"
                         Core..!@ Core.mempty
                     )
            Core.<*> (x Core..?> "nextPageToken")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCloudFormationStackRecords

instance Core.NFData GetCloudFormationStackRecords

instance Core.ToHeaders GetCloudFormationStackRecords where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetCloudFormationStackRecords" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetCloudFormationStackRecords where
  toJSON GetCloudFormationStackRecords' {..} =
    Core.object
      ( Core.catMaybes
          [("pageToken" Core..=) Core.<$> pageToken]
      )

instance Core.ToPath GetCloudFormationStackRecords where
  toPath = Core.const "/"

instance Core.ToQuery GetCloudFormationStackRecords where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCloudFormationStackRecordsResponse' smart constructor.
data GetCloudFormationStackRecordsResponse = GetCloudFormationStackRecordsResponse'
  { -- | A list of objects describing the CloudFormation stack records.
    cloudFormationStackRecords :: Core.Maybe [CloudFormationStackRecord],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetCloudFormationStackRecords@ request and specify the next page token
    -- using the @pageToken@ parameter.
    nextPageToken :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCloudFormationStackRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cloudFormationStackRecords', 'getCloudFormationStackRecordsResponse_cloudFormationStackRecords' - A list of objects describing the CloudFormation stack records.
--
-- 'nextPageToken', 'getCloudFormationStackRecordsResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetCloudFormationStackRecords@ request and specify the next page token
-- using the @pageToken@ parameter.
--
-- 'httpStatus', 'getCloudFormationStackRecordsResponse_httpStatus' - The response's http status code.
newGetCloudFormationStackRecordsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCloudFormationStackRecordsResponse
newGetCloudFormationStackRecordsResponse pHttpStatus_ =
  GetCloudFormationStackRecordsResponse'
    { cloudFormationStackRecords =
        Core.Nothing,
      nextPageToken = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of objects describing the CloudFormation stack records.
getCloudFormationStackRecordsResponse_cloudFormationStackRecords :: Lens.Lens' GetCloudFormationStackRecordsResponse (Core.Maybe [CloudFormationStackRecord])
getCloudFormationStackRecordsResponse_cloudFormationStackRecords = Lens.lens (\GetCloudFormationStackRecordsResponse' {cloudFormationStackRecords} -> cloudFormationStackRecords) (\s@GetCloudFormationStackRecordsResponse' {} a -> s {cloudFormationStackRecords = a} :: GetCloudFormationStackRecordsResponse) Core.. Lens.mapping Lens._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetCloudFormationStackRecords@ request and specify the next page token
-- using the @pageToken@ parameter.
getCloudFormationStackRecordsResponse_nextPageToken :: Lens.Lens' GetCloudFormationStackRecordsResponse (Core.Maybe Core.Text)
getCloudFormationStackRecordsResponse_nextPageToken = Lens.lens (\GetCloudFormationStackRecordsResponse' {nextPageToken} -> nextPageToken) (\s@GetCloudFormationStackRecordsResponse' {} a -> s {nextPageToken = a} :: GetCloudFormationStackRecordsResponse)

-- | The response's http status code.
getCloudFormationStackRecordsResponse_httpStatus :: Lens.Lens' GetCloudFormationStackRecordsResponse Core.Int
getCloudFormationStackRecordsResponse_httpStatus = Lens.lens (\GetCloudFormationStackRecordsResponse' {httpStatus} -> httpStatus) (\s@GetCloudFormationStackRecordsResponse' {} a -> s {httpStatus = a} :: GetCloudFormationStackRecordsResponse)

instance
  Core.NFData
    GetCloudFormationStackRecordsResponse
