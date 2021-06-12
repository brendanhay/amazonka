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
-- Module      : Network.AWS.Config.GetStoredQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the details of a specific stored query.
module Network.AWS.Config.GetStoredQuery
  ( -- * Creating a Request
    GetStoredQuery (..),
    newGetStoredQuery,

    -- * Request Lenses
    getStoredQuery_queryName,

    -- * Destructuring the Response
    GetStoredQueryResponse (..),
    newGetStoredQueryResponse,

    -- * Response Lenses
    getStoredQueryResponse_storedQuery,
    getStoredQueryResponse_httpStatus,
  )
where

import Network.AWS.Config.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetStoredQuery' smart constructor.
data GetStoredQuery = GetStoredQuery'
  { -- | The name of the query.
    queryName :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetStoredQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'queryName', 'getStoredQuery_queryName' - The name of the query.
newGetStoredQuery ::
  -- | 'queryName'
  Core.Text ->
  GetStoredQuery
newGetStoredQuery pQueryName_ =
  GetStoredQuery' {queryName = pQueryName_}

-- | The name of the query.
getStoredQuery_queryName :: Lens.Lens' GetStoredQuery Core.Text
getStoredQuery_queryName = Lens.lens (\GetStoredQuery' {queryName} -> queryName) (\s@GetStoredQuery' {} a -> s {queryName = a} :: GetStoredQuery)

instance Core.AWSRequest GetStoredQuery where
  type
    AWSResponse GetStoredQuery =
      GetStoredQueryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetStoredQueryResponse'
            Core.<$> (x Core..?> "StoredQuery")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetStoredQuery

instance Core.NFData GetStoredQuery

instance Core.ToHeaders GetStoredQuery where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "StarlingDoveService.GetStoredQuery" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetStoredQuery where
  toJSON GetStoredQuery' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("QueryName" Core..= queryName)]
      )

instance Core.ToPath GetStoredQuery where
  toPath = Core.const "/"

instance Core.ToQuery GetStoredQuery where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetStoredQueryResponse' smart constructor.
data GetStoredQueryResponse = GetStoredQueryResponse'
  { -- | Returns a @StoredQuery@ object.
    storedQuery :: Core.Maybe StoredQuery,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetStoredQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'storedQuery', 'getStoredQueryResponse_storedQuery' - Returns a @StoredQuery@ object.
--
-- 'httpStatus', 'getStoredQueryResponse_httpStatus' - The response's http status code.
newGetStoredQueryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetStoredQueryResponse
newGetStoredQueryResponse pHttpStatus_ =
  GetStoredQueryResponse'
    { storedQuery = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns a @StoredQuery@ object.
getStoredQueryResponse_storedQuery :: Lens.Lens' GetStoredQueryResponse (Core.Maybe StoredQuery)
getStoredQueryResponse_storedQuery = Lens.lens (\GetStoredQueryResponse' {storedQuery} -> storedQuery) (\s@GetStoredQueryResponse' {} a -> s {storedQuery = a} :: GetStoredQueryResponse)

-- | The response's http status code.
getStoredQueryResponse_httpStatus :: Lens.Lens' GetStoredQueryResponse Core.Int
getStoredQueryResponse_httpStatus = Lens.lens (\GetStoredQueryResponse' {httpStatus} -> httpStatus) (\s@GetStoredQueryResponse' {} a -> s {httpStatus = a} :: GetStoredQueryResponse)

instance Core.NFData GetStoredQueryResponse
