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
-- Module      : Network.AWS.Lightsail.GetOperationsForResource
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets operations for a specific resource (e.g., an instance or a static
-- IP).
module Network.AWS.Lightsail.GetOperationsForResource
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
    getOperationsForResourceResponse_operations,
    getOperationsForResourceResponse_nextPageCount,
    getOperationsForResourceResponse_nextPageToken,
    getOperationsForResourceResponse_httpStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

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
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.AWSRequest GetOperationsForResource where
  type
    Rs GetOperationsForResource =
      GetOperationsForResourceResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetOperationsForResourceResponse'
            Prelude.<$> ( x Prelude..?> "operations"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (x Prelude..?> "nextPageCount")
            Prelude.<*> (x Prelude..?> "nextPageToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetOperationsForResource

instance Prelude.NFData GetOperationsForResource

instance Prelude.ToHeaders GetOperationsForResource where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ( "Lightsail_20161128.GetOperationsForResource" ::
                             Prelude.ByteString
                         ),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetOperationsForResource where
  toJSON GetOperationsForResource' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("pageToken" Prelude..=) Prelude.<$> pageToken,
            Prelude.Just
              ("resourceName" Prelude..= resourceName)
          ]
      )

instance Prelude.ToPath GetOperationsForResource where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetOperationsForResource where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOperationsForResourceResponse' smart constructor.
data GetOperationsForResourceResponse = GetOperationsForResourceResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | (Deprecated) Returns the number of pages of results that remain.
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
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'GetOperationsForResourceResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'getOperationsForResourceResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
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
-- 'httpStatus', 'getOperationsForResourceResponse_httpStatus' - The response's http status code.
newGetOperationsForResourceResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetOperationsForResourceResponse
newGetOperationsForResourceResponse pHttpStatus_ =
  GetOperationsForResourceResponse'
    { operations =
        Prelude.Nothing,
      nextPageCount = Prelude.Nothing,
      nextPageToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
getOperationsForResourceResponse_operations :: Lens.Lens' GetOperationsForResourceResponse (Prelude.Maybe [Operation])
getOperationsForResourceResponse_operations = Lens.lens (\GetOperationsForResourceResponse' {operations} -> operations) (\s@GetOperationsForResourceResponse' {} a -> s {operations = a} :: GetOperationsForResourceResponse) Prelude.. Lens.mapping Prelude._Coerce

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

-- | The response's http status code.
getOperationsForResourceResponse_httpStatus :: Lens.Lens' GetOperationsForResourceResponse Prelude.Int
getOperationsForResourceResponse_httpStatus = Lens.lens (\GetOperationsForResourceResponse' {httpStatus} -> httpStatus) (\s@GetOperationsForResourceResponse' {} a -> s {httpStatus = a} :: GetOperationsForResourceResponse)

instance
  Prelude.NFData
    GetOperationsForResourceResponse
