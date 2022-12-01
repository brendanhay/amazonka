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
-- Module      : Amazonka.Lightsail.GetCloudFormationStackRecords
-- Copyright   : (c) 2013-2022 Brendan Hay
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
module Amazonka.Lightsail.GetCloudFormationStackRecords
  ( -- * Creating a Request
    GetCloudFormationStackRecords (..),
    newGetCloudFormationStackRecords,

    -- * Request Lenses
    getCloudFormationStackRecords_pageToken,

    -- * Destructuring the Response
    GetCloudFormationStackRecordsResponse (..),
    newGetCloudFormationStackRecordsResponse,

    -- * Response Lenses
    getCloudFormationStackRecordsResponse_nextPageToken,
    getCloudFormationStackRecordsResponse_cloudFormationStackRecords,
    getCloudFormationStackRecordsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCloudFormationStackRecords' smart constructor.
data GetCloudFormationStackRecords = GetCloudFormationStackRecords'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetClouFormationStackRecords@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
        Prelude.Nothing
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetClouFormationStackRecords@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getCloudFormationStackRecords_pageToken :: Lens.Lens' GetCloudFormationStackRecords (Prelude.Maybe Prelude.Text)
getCloudFormationStackRecords_pageToken = Lens.lens (\GetCloudFormationStackRecords' {pageToken} -> pageToken) (\s@GetCloudFormationStackRecords' {} a -> s {pageToken = a} :: GetCloudFormationStackRecords)

instance Core.AWSPager GetCloudFormationStackRecords where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getCloudFormationStackRecordsResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getCloudFormationStackRecordsResponse_cloudFormationStackRecords
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getCloudFormationStackRecords_pageToken
          Lens..~ rs
          Lens.^? getCloudFormationStackRecordsResponse_nextPageToken
            Prelude.. Lens._Just

instance
  Core.AWSRequest
    GetCloudFormationStackRecords
  where
  type
    AWSResponse GetCloudFormationStackRecords =
      GetCloudFormationStackRecordsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetCloudFormationStackRecordsResponse'
            Prelude.<$> (x Core..?> "nextPageToken")
            Prelude.<*> ( x Core..?> "cloudFormationStackRecords"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetCloudFormationStackRecords
  where
  hashWithSalt _salt GetCloudFormationStackRecords' {..} =
    _salt `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetCloudFormationStackRecords where
  rnf GetCloudFormationStackRecords' {..} =
    Prelude.rnf pageToken

instance Core.ToHeaders GetCloudFormationStackRecords where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetCloudFormationStackRecords" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetCloudFormationStackRecords where
  toJSON GetCloudFormationStackRecords' {..} =
    Core.object
      ( Prelude.catMaybes
          [("pageToken" Core..=) Prelude.<$> pageToken]
      )

instance Core.ToPath GetCloudFormationStackRecords where
  toPath = Prelude.const "/"

instance Core.ToQuery GetCloudFormationStackRecords where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCloudFormationStackRecordsResponse' smart constructor.
data GetCloudFormationStackRecordsResponse = GetCloudFormationStackRecordsResponse'
  { -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetCloudFormationStackRecords@ request and specify the next page token
    -- using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | A list of objects describing the CloudFormation stack records.
    cloudFormationStackRecords :: Prelude.Maybe [CloudFormationStackRecord],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCloudFormationStackRecordsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
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
-- 'cloudFormationStackRecords', 'getCloudFormationStackRecordsResponse_cloudFormationStackRecords' - A list of objects describing the CloudFormation stack records.
--
-- 'httpStatus', 'getCloudFormationStackRecordsResponse_httpStatus' - The response's http status code.
newGetCloudFormationStackRecordsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCloudFormationStackRecordsResponse
newGetCloudFormationStackRecordsResponse pHttpStatus_ =
  GetCloudFormationStackRecordsResponse'
    { nextPageToken =
        Prelude.Nothing,
      cloudFormationStackRecords =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetCloudFormationStackRecords@ request and specify the next page token
-- using the @pageToken@ parameter.
getCloudFormationStackRecordsResponse_nextPageToken :: Lens.Lens' GetCloudFormationStackRecordsResponse (Prelude.Maybe Prelude.Text)
getCloudFormationStackRecordsResponse_nextPageToken = Lens.lens (\GetCloudFormationStackRecordsResponse' {nextPageToken} -> nextPageToken) (\s@GetCloudFormationStackRecordsResponse' {} a -> s {nextPageToken = a} :: GetCloudFormationStackRecordsResponse)

-- | A list of objects describing the CloudFormation stack records.
getCloudFormationStackRecordsResponse_cloudFormationStackRecords :: Lens.Lens' GetCloudFormationStackRecordsResponse (Prelude.Maybe [CloudFormationStackRecord])
getCloudFormationStackRecordsResponse_cloudFormationStackRecords = Lens.lens (\GetCloudFormationStackRecordsResponse' {cloudFormationStackRecords} -> cloudFormationStackRecords) (\s@GetCloudFormationStackRecordsResponse' {} a -> s {cloudFormationStackRecords = a} :: GetCloudFormationStackRecordsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getCloudFormationStackRecordsResponse_httpStatus :: Lens.Lens' GetCloudFormationStackRecordsResponse Prelude.Int
getCloudFormationStackRecordsResponse_httpStatus = Lens.lens (\GetCloudFormationStackRecordsResponse' {httpStatus} -> httpStatus) (\s@GetCloudFormationStackRecordsResponse' {} a -> s {httpStatus = a} :: GetCloudFormationStackRecordsResponse)

instance
  Prelude.NFData
    GetCloudFormationStackRecordsResponse
  where
  rnf GetCloudFormationStackRecordsResponse' {..} =
    Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf cloudFormationStackRecords
      `Prelude.seq` Prelude.rnf httpStatus
