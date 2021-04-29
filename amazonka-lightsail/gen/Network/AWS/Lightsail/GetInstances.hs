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

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetInstances' smart constructor.
data GetInstances = GetInstances'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetInstances@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  GetInstances' {pageToken = Prelude.Nothing}

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetInstances@ request. If your
-- results are paginated, the response will return a next page token that
-- you can specify as the page token in a subsequent request.
getInstances_pageToken :: Lens.Lens' GetInstances (Prelude.Maybe Prelude.Text)
getInstances_pageToken = Lens.lens (\GetInstances' {pageToken} -> pageToken) (\s@GetInstances' {} a -> s {pageToken = a} :: GetInstances)

instance Pager.AWSPager GetInstances where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getInstancesResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getInstancesResponse_instances Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getInstances_pageToken
          Lens..~ rs
          Lens.^? getInstancesResponse_nextPageToken
            Prelude.. Lens._Just

instance Prelude.AWSRequest GetInstances where
  type Rs GetInstances = GetInstancesResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstancesResponse'
            Prelude.<$> ( x Prelude..?> "instances"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstances

instance Prelude.NFData GetInstances

instance Prelude.ToHeaders GetInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetInstances" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetInstances where
  toJSON GetInstances' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [("pageToken" Prelude..=) Prelude.<$> pageToken]
      )

instance Prelude.ToPath GetInstances where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetInstances where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetInstancesResponse' smart constructor.
data GetInstancesResponse = GetInstancesResponse'
  { -- | An array of key-value pairs containing information about your instances.
    instances :: Prelude.Maybe [Instance],
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another @GetInstances@ request
    -- and specify the next page token using the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetInstancesResponse
newGetInstancesResponse pHttpStatus_ =
  GetInstancesResponse'
    { instances = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of key-value pairs containing information about your instances.
getInstancesResponse_instances :: Lens.Lens' GetInstancesResponse (Prelude.Maybe [Instance])
getInstancesResponse_instances = Lens.lens (\GetInstancesResponse' {instances} -> instances) (\s@GetInstancesResponse' {} a -> s {instances = a} :: GetInstancesResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another @GetInstances@ request
-- and specify the next page token using the @pageToken@ parameter.
getInstancesResponse_nextPageToken :: Lens.Lens' GetInstancesResponse (Prelude.Maybe Prelude.Text)
getInstancesResponse_nextPageToken = Lens.lens (\GetInstancesResponse' {nextPageToken} -> nextPageToken) (\s@GetInstancesResponse' {} a -> s {nextPageToken = a} :: GetInstancesResponse)

-- | The response's http status code.
getInstancesResponse_httpStatus :: Lens.Lens' GetInstancesResponse Prelude.Int
getInstancesResponse_httpStatus = Lens.lens (\GetInstancesResponse' {httpStatus} -> httpStatus) (\s@GetInstancesResponse' {} a -> s {httpStatus = a} :: GetInstancesResponse)

instance Prelude.NFData GetInstancesResponse
