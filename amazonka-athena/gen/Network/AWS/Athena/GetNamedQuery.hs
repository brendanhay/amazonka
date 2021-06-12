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
-- Module      : Network.AWS.Athena.GetNamedQuery
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a single query. Requires that you have access
-- to the workgroup in which the query was saved.
module Network.AWS.Athena.GetNamedQuery
  ( -- * Creating a Request
    GetNamedQuery (..),
    newGetNamedQuery,

    -- * Request Lenses
    getNamedQuery_namedQueryId,

    -- * Destructuring the Response
    GetNamedQueryResponse (..),
    newGetNamedQueryResponse,

    -- * Response Lenses
    getNamedQueryResponse_namedQuery,
    getNamedQueryResponse_httpStatus,
  )
where

import Network.AWS.Athena.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetNamedQuery' smart constructor.
data GetNamedQuery = GetNamedQuery'
  { -- | The unique ID of the query. Use ListNamedQueries to get query IDs.
    namedQueryId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetNamedQuery' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namedQueryId', 'getNamedQuery_namedQueryId' - The unique ID of the query. Use ListNamedQueries to get query IDs.
newGetNamedQuery ::
  -- | 'namedQueryId'
  Core.Text ->
  GetNamedQuery
newGetNamedQuery pNamedQueryId_ =
  GetNamedQuery' {namedQueryId = pNamedQueryId_}

-- | The unique ID of the query. Use ListNamedQueries to get query IDs.
getNamedQuery_namedQueryId :: Lens.Lens' GetNamedQuery Core.Text
getNamedQuery_namedQueryId = Lens.lens (\GetNamedQuery' {namedQueryId} -> namedQueryId) (\s@GetNamedQuery' {} a -> s {namedQueryId = a} :: GetNamedQuery)

instance Core.AWSRequest GetNamedQuery where
  type
    AWSResponse GetNamedQuery =
      GetNamedQueryResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetNamedQueryResponse'
            Core.<$> (x Core..?> "NamedQuery")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetNamedQuery

instance Core.NFData GetNamedQuery

instance Core.ToHeaders GetNamedQuery where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AmazonAthena.GetNamedQuery" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetNamedQuery where
  toJSON GetNamedQuery' {..} =
    Core.object
      ( Core.catMaybes
          [Core.Just ("NamedQueryId" Core..= namedQueryId)]
      )

instance Core.ToPath GetNamedQuery where
  toPath = Core.const "/"

instance Core.ToQuery GetNamedQuery where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetNamedQueryResponse' smart constructor.
data GetNamedQueryResponse = GetNamedQueryResponse'
  { -- | Information about the query.
    namedQuery :: Core.Maybe NamedQuery,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetNamedQueryResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'namedQuery', 'getNamedQueryResponse_namedQuery' - Information about the query.
--
-- 'httpStatus', 'getNamedQueryResponse_httpStatus' - The response's http status code.
newGetNamedQueryResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetNamedQueryResponse
newGetNamedQueryResponse pHttpStatus_ =
  GetNamedQueryResponse'
    { namedQuery = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the query.
getNamedQueryResponse_namedQuery :: Lens.Lens' GetNamedQueryResponse (Core.Maybe NamedQuery)
getNamedQueryResponse_namedQuery = Lens.lens (\GetNamedQueryResponse' {namedQuery} -> namedQuery) (\s@GetNamedQueryResponse' {} a -> s {namedQuery = a} :: GetNamedQueryResponse)

-- | The response's http status code.
getNamedQueryResponse_httpStatus :: Lens.Lens' GetNamedQueryResponse Core.Int
getNamedQueryResponse_httpStatus = Lens.lens (\GetNamedQueryResponse' {httpStatus} -> httpStatus) (\s@GetNamedQueryResponse' {} a -> s {httpStatus = a} :: GetNamedQueryResponse)

instance Core.NFData GetNamedQueryResponse
