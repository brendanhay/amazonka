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
-- Module      : Amazonka.Lightsail.GetInstances
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about all Amazon Lightsail virtual private servers,
-- or /instances/.
--
-- This operation returns paginated results.
module Amazonka.Lightsail.GetInstances
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetInstances' smart constructor.
data GetInstances = GetInstances'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetInstances@ request. If your
    -- results are paginated, the response will return a next page token that
    -- you can specify as the page token in a subsequent request.
    pageToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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

instance Core.AWSPager GetInstances where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getInstancesResponse_nextPageToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getInstancesResponse_instances Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getInstances_pageToken
          Lens..~ rs
          Lens.^? getInstancesResponse_nextPageToken
            Prelude.. Lens._Just

instance Core.AWSRequest GetInstances where
  type AWSResponse GetInstances = GetInstancesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetInstancesResponse'
            Prelude.<$> (x Data..?> "instances" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetInstances where
  hashWithSalt _salt GetInstances' {..} =
    _salt `Prelude.hashWithSalt` pageToken

instance Prelude.NFData GetInstances where
  rnf GetInstances' {..} = Prelude.rnf pageToken

instance Data.ToHeaders GetInstances where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetInstances" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetInstances where
  toJSON GetInstances' {..} =
    Data.object
      ( Prelude.catMaybes
          [("pageToken" Data..=) Prelude.<$> pageToken]
      )

instance Data.ToPath GetInstances where
  toPath = Prelude.const "/"

instance Data.ToQuery GetInstances where
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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
getInstancesResponse_instances = Lens.lens (\GetInstancesResponse' {instances} -> instances) (\s@GetInstancesResponse' {} a -> s {instances = a} :: GetInstancesResponse) Prelude.. Lens.mapping Lens.coerced

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

instance Prelude.NFData GetInstancesResponse where
  rnf GetInstancesResponse' {..} =
    Prelude.rnf instances
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf httpStatus
