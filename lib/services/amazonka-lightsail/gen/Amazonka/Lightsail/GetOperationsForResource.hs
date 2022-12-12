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
-- Module      : Amazonka.Lightsail.GetOperationsForResource
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets operations for a specific resource (e.g., an instance or a static
-- IP).
module Amazonka.Lightsail.GetOperationsForResource
  ( -- * Creating a Request
    GetOperationsForResource (..),
    newGetOperationsForResource,

    -- * Request Lenses
    getOperationsForResource_pageToken,
    getOperationsForResource_resourceName,

    -- * Destructuring the Response
    GetOperationsForResourceResponse (..),
    newGetOperationsForResourceResponse,

    -- * Response Lenses
    getOperationsForResourceResponse_nextPageCount,
    getOperationsForResourceResponse_nextPageToken,
    getOperationsForResourceResponse_operations,
    getOperationsForResourceResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOperationsForResource' smart constructor.
data GetOperationsForResource = GetOperationsForResource'
  { -- | The token to advance to the next page of results from your request.
    --
    -- To get a page token, perform an initial @GetOperationsForResource@
    -- request. If your results are paginated, the response will return a next
    -- page token that you can specify as the page token in a subsequent
    -- request.
    pageToken :: Prelude.Maybe Prelude.Text,
    -- | The name of the resource for which you are requesting information.
    resourceName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOperationsForResource' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'pageToken', 'getOperationsForResource_pageToken' - The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperationsForResource@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
--
-- 'resourceName', 'getOperationsForResource_resourceName' - The name of the resource for which you are requesting information.
newGetOperationsForResource ::
  -- | 'resourceName'
  Prelude.Text ->
  GetOperationsForResource
newGetOperationsForResource pResourceName_ =
  GetOperationsForResource'
    { pageToken =
        Prelude.Nothing,
      resourceName = pResourceName_
    }

-- | The token to advance to the next page of results from your request.
--
-- To get a page token, perform an initial @GetOperationsForResource@
-- request. If your results are paginated, the response will return a next
-- page token that you can specify as the page token in a subsequent
-- request.
getOperationsForResource_pageToken :: Lens.Lens' GetOperationsForResource (Prelude.Maybe Prelude.Text)
getOperationsForResource_pageToken = Lens.lens (\GetOperationsForResource' {pageToken} -> pageToken) (\s@GetOperationsForResource' {} a -> s {pageToken = a} :: GetOperationsForResource)

-- | The name of the resource for which you are requesting information.
getOperationsForResource_resourceName :: Lens.Lens' GetOperationsForResource Prelude.Text
getOperationsForResource_resourceName = Lens.lens (\GetOperationsForResource' {resourceName} -> resourceName) (\s@GetOperationsForResource' {} a -> s {resourceName = a} :: GetOperationsForResource)

instance Core.AWSRequest GetOperationsForResource where
  type
    AWSResponse GetOperationsForResource =
      GetOperationsForResourceResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationsForResourceResponse'
            Prelude.<$> (x Data..?> "nextPageCount")
            Prelude.<*> (x Data..?> "nextPageToken")
            Prelude.<*> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOperationsForResource where
  hashWithSalt _salt GetOperationsForResource' {..} =
    _salt `Prelude.hashWithSalt` pageToken
      `Prelude.hashWithSalt` resourceName

instance Prelude.NFData GetOperationsForResource where
  rnf GetOperationsForResource' {..} =
    Prelude.rnf pageToken
      `Prelude.seq` Prelude.rnf resourceName

instance Data.ToHeaders GetOperationsForResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.GetOperationsForResource" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetOperationsForResource where
  toJSON GetOperationsForResource' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("pageToken" Data..=) Prelude.<$> pageToken,
            Prelude.Just ("resourceName" Data..= resourceName)
          ]
      )

instance Data.ToPath GetOperationsForResource where
  toPath = Prelude.const "/"

instance Data.ToQuery GetOperationsForResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOperationsForResourceResponse' smart constructor.
data GetOperationsForResourceResponse = GetOperationsForResourceResponse'
  { -- | (Deprecated) Returns the number of pages of results that remain.
    --
    -- In releases prior to June 12, 2017, this parameter returned @null@ by
    -- the API. It is now deprecated, and the API returns the @next page token@
    -- parameter instead.
    nextPageCount :: Prelude.Maybe Prelude.Text,
    -- | The token to advance to the next page of results from your request.
    --
    -- A next page token is not returned if there are no more results to
    -- display.
    --
    -- To get the next page of results, perform another
    -- @GetOperationsForResource@ request and specify the next page token using
    -- the @pageToken@ parameter.
    nextPageToken :: Prelude.Maybe Prelude.Text,
    -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOperationsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextPageCount', 'getOperationsForResourceResponse_nextPageCount' - (Deprecated) Returns the number of pages of results that remain.
--
-- In releases prior to June 12, 2017, this parameter returned @null@ by
-- the API. It is now deprecated, and the API returns the @next page token@
-- parameter instead.
--
-- 'nextPageToken', 'getOperationsForResourceResponse_nextPageToken' - The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetOperationsForResource@ request and specify the next page token using
-- the @pageToken@ parameter.
--
-- 'operations', 'getOperationsForResourceResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'getOperationsForResourceResponse_httpStatus' - The response's http status code.
newGetOperationsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOperationsForResourceResponse
newGetOperationsForResourceResponse pHttpStatus_ =
  GetOperationsForResourceResponse'
    { nextPageCount =
        Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | (Deprecated) Returns the number of pages of results that remain.
--
-- In releases prior to June 12, 2017, this parameter returned @null@ by
-- the API. It is now deprecated, and the API returns the @next page token@
-- parameter instead.
getOperationsForResourceResponse_nextPageCount :: Lens.Lens' GetOperationsForResourceResponse (Prelude.Maybe Prelude.Text)
getOperationsForResourceResponse_nextPageCount = Lens.lens (\GetOperationsForResourceResponse' {nextPageCount} -> nextPageCount) (\s@GetOperationsForResourceResponse' {} a -> s {nextPageCount = a} :: GetOperationsForResourceResponse)

-- | The token to advance to the next page of results from your request.
--
-- A next page token is not returned if there are no more results to
-- display.
--
-- To get the next page of results, perform another
-- @GetOperationsForResource@ request and specify the next page token using
-- the @pageToken@ parameter.
getOperationsForResourceResponse_nextPageToken :: Lens.Lens' GetOperationsForResourceResponse (Prelude.Maybe Prelude.Text)
getOperationsForResourceResponse_nextPageToken = Lens.lens (\GetOperationsForResourceResponse' {nextPageToken} -> nextPageToken) (\s@GetOperationsForResourceResponse' {} a -> s {nextPageToken = a} :: GetOperationsForResourceResponse)

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
getOperationsForResourceResponse_operations :: Lens.Lens' GetOperationsForResourceResponse (Prelude.Maybe [Operation])
getOperationsForResourceResponse_operations = Lens.lens (\GetOperationsForResourceResponse' {operations} -> operations) (\s@GetOperationsForResourceResponse' {} a -> s {operations = a} :: GetOperationsForResourceResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getOperationsForResourceResponse_httpStatus :: Lens.Lens' GetOperationsForResourceResponse Prelude.Int
getOperationsForResourceResponse_httpStatus = Lens.lens (\GetOperationsForResourceResponse' {httpStatus} -> httpStatus) (\s@GetOperationsForResourceResponse' {} a -> s {httpStatus = a} :: GetOperationsForResourceResponse)

instance
  Prelude.NFData
    GetOperationsForResourceResponse
  where
  rnf GetOperationsForResourceResponse' {..} =
    Prelude.rnf nextPageCount
      `Prelude.seq` Prelude.rnf nextPageToken
      `Prelude.seq` Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
