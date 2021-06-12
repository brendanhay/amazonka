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
-- Module      : Network.AWS.Glue.GetConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of connection definitions from the Data Catalog.
--
-- This operation returns paginated results.
module Network.AWS.Glue.GetConnections
  ( -- * Creating a Request
    GetConnections (..),
    newGetConnections,

    -- * Request Lenses
    getConnections_nextToken,
    getConnections_catalogId,
    getConnections_maxResults,
    getConnections_hidePassword,
    getConnections_filter,

    -- * Destructuring the Response
    GetConnectionsResponse (..),
    newGetConnectionsResponse,

    -- * Response Lenses
    getConnectionsResponse_nextToken,
    getConnectionsResponse_connectionList,
    getConnectionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConnections' smart constructor.
data GetConnections = GetConnections'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Core.Maybe Core.Text,
    -- | The ID of the Data Catalog in which the connections reside. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Core.Maybe Core.Text,
    -- | The maximum number of connections to return in one response.
    maxResults :: Core.Maybe Core.Natural,
    -- | Allows you to retrieve the connection metadata without returning the
    -- password. For instance, the AWS Glue console uses this flag to retrieve
    -- the connection, and does not display the password. Set this parameter
    -- when the caller might not have permission to use the AWS KMS key to
    -- decrypt the password, but it does have permission to access the rest of
    -- the connection properties.
    hidePassword :: Core.Maybe Core.Bool,
    -- | A filter that controls which connections are returned.
    filter' :: Core.Maybe GetConnectionsFilter
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getConnections_nextToken' - A continuation token, if this is a continuation call.
--
-- 'catalogId', 'getConnections_catalogId' - The ID of the Data Catalog in which the connections reside. If none is
-- provided, the AWS account ID is used by default.
--
-- 'maxResults', 'getConnections_maxResults' - The maximum number of connections to return in one response.
--
-- 'hidePassword', 'getConnections_hidePassword' - Allows you to retrieve the connection metadata without returning the
-- password. For instance, the AWS Glue console uses this flag to retrieve
-- the connection, and does not display the password. Set this parameter
-- when the caller might not have permission to use the AWS KMS key to
-- decrypt the password, but it does have permission to access the rest of
-- the connection properties.
--
-- 'filter'', 'getConnections_filter' - A filter that controls which connections are returned.
newGetConnections ::
  GetConnections
newGetConnections =
  GetConnections'
    { nextToken = Core.Nothing,
      catalogId = Core.Nothing,
      maxResults = Core.Nothing,
      hidePassword = Core.Nothing,
      filter' = Core.Nothing
    }

-- | A continuation token, if this is a continuation call.
getConnections_nextToken :: Lens.Lens' GetConnections (Core.Maybe Core.Text)
getConnections_nextToken = Lens.lens (\GetConnections' {nextToken} -> nextToken) (\s@GetConnections' {} a -> s {nextToken = a} :: GetConnections)

-- | The ID of the Data Catalog in which the connections reside. If none is
-- provided, the AWS account ID is used by default.
getConnections_catalogId :: Lens.Lens' GetConnections (Core.Maybe Core.Text)
getConnections_catalogId = Lens.lens (\GetConnections' {catalogId} -> catalogId) (\s@GetConnections' {} a -> s {catalogId = a} :: GetConnections)

-- | The maximum number of connections to return in one response.
getConnections_maxResults :: Lens.Lens' GetConnections (Core.Maybe Core.Natural)
getConnections_maxResults = Lens.lens (\GetConnections' {maxResults} -> maxResults) (\s@GetConnections' {} a -> s {maxResults = a} :: GetConnections)

-- | Allows you to retrieve the connection metadata without returning the
-- password. For instance, the AWS Glue console uses this flag to retrieve
-- the connection, and does not display the password. Set this parameter
-- when the caller might not have permission to use the AWS KMS key to
-- decrypt the password, but it does have permission to access the rest of
-- the connection properties.
getConnections_hidePassword :: Lens.Lens' GetConnections (Core.Maybe Core.Bool)
getConnections_hidePassword = Lens.lens (\GetConnections' {hidePassword} -> hidePassword) (\s@GetConnections' {} a -> s {hidePassword = a} :: GetConnections)

-- | A filter that controls which connections are returned.
getConnections_filter :: Lens.Lens' GetConnections (Core.Maybe GetConnectionsFilter)
getConnections_filter = Lens.lens (\GetConnections' {filter'} -> filter') (\s@GetConnections' {} a -> s {filter' = a} :: GetConnections)

instance Core.AWSPager GetConnections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getConnectionsResponse_nextToken Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.stop
        ( rs
            Lens.^? getConnectionsResponse_connectionList
              Core.. Lens._Just
        ) =
      Core.Nothing
    | Core.otherwise =
      Core.Just Core.$
        rq
          Lens.& getConnections_nextToken
          Lens..~ rs
          Lens.^? getConnectionsResponse_nextToken Core.. Lens._Just

instance Core.AWSRequest GetConnections where
  type
    AWSResponse GetConnections =
      GetConnectionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionsResponse'
            Core.<$> (x Core..?> "NextToken")
            Core.<*> (x Core..?> "ConnectionList" Core..!@ Core.mempty)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetConnections

instance Core.NFData GetConnections

instance Core.ToHeaders GetConnections where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ("AWSGlue.GetConnections" :: Core.ByteString),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON GetConnections where
  toJSON GetConnections' {..} =
    Core.object
      ( Core.catMaybes
          [ ("NextToken" Core..=) Core.<$> nextToken,
            ("CatalogId" Core..=) Core.<$> catalogId,
            ("MaxResults" Core..=) Core.<$> maxResults,
            ("HidePassword" Core..=) Core.<$> hidePassword,
            ("Filter" Core..=) Core.<$> filter'
          ]
      )

instance Core.ToPath GetConnections where
  toPath = Core.const "/"

instance Core.ToQuery GetConnections where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetConnectionsResponse' smart constructor.
data GetConnectionsResponse = GetConnectionsResponse'
  { -- | A continuation token, if the list of connections returned does not
    -- include the last of the filtered connections.
    nextToken :: Core.Maybe Core.Text,
    -- | A list of requested connection definitions.
    connectionList :: Core.Maybe [Connection],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getConnectionsResponse_nextToken' - A continuation token, if the list of connections returned does not
-- include the last of the filtered connections.
--
-- 'connectionList', 'getConnectionsResponse_connectionList' - A list of requested connection definitions.
--
-- 'httpStatus', 'getConnectionsResponse_httpStatus' - The response's http status code.
newGetConnectionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetConnectionsResponse
newGetConnectionsResponse pHttpStatus_ =
  GetConnectionsResponse'
    { nextToken = Core.Nothing,
      connectionList = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the list of connections returned does not
-- include the last of the filtered connections.
getConnectionsResponse_nextToken :: Lens.Lens' GetConnectionsResponse (Core.Maybe Core.Text)
getConnectionsResponse_nextToken = Lens.lens (\GetConnectionsResponse' {nextToken} -> nextToken) (\s@GetConnectionsResponse' {} a -> s {nextToken = a} :: GetConnectionsResponse)

-- | A list of requested connection definitions.
getConnectionsResponse_connectionList :: Lens.Lens' GetConnectionsResponse (Core.Maybe [Connection])
getConnectionsResponse_connectionList = Lens.lens (\GetConnectionsResponse' {connectionList} -> connectionList) (\s@GetConnectionsResponse' {} a -> s {connectionList = a} :: GetConnectionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
getConnectionsResponse_httpStatus :: Lens.Lens' GetConnectionsResponse Core.Int
getConnectionsResponse_httpStatus = Lens.lens (\GetConnectionsResponse' {httpStatus} -> httpStatus) (\s@GetConnectionsResponse' {} a -> s {httpStatus = a} :: GetConnectionsResponse)

instance Core.NFData GetConnectionsResponse
