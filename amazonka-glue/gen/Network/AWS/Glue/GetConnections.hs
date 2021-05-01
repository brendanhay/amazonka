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

import Network.AWS.Glue.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Pager as Pager
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetConnections' smart constructor.
data GetConnections = GetConnections'
  { -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Data Catalog in which the connections reside. If none is
    -- provided, the AWS account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | The maximum number of connections to return in one response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | Allows you to retrieve the connection metadata without returning the
    -- password. For instance, the AWS Glue console uses this flag to retrieve
    -- the connection, and does not display the password. Set this parameter
    -- when the caller might not have permission to use the AWS KMS key to
    -- decrypt the password, but it does have permission to access the rest of
    -- the connection properties.
    hidePassword :: Prelude.Maybe Prelude.Bool,
    -- | A filter that controls which connections are returned.
    filter' :: Prelude.Maybe GetConnectionsFilter
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { nextToken = Prelude.Nothing,
      catalogId = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      hidePassword = Prelude.Nothing,
      filter' = Prelude.Nothing
    }

-- | A continuation token, if this is a continuation call.
getConnections_nextToken :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Text)
getConnections_nextToken = Lens.lens (\GetConnections' {nextToken} -> nextToken) (\s@GetConnections' {} a -> s {nextToken = a} :: GetConnections)

-- | The ID of the Data Catalog in which the connections reside. If none is
-- provided, the AWS account ID is used by default.
getConnections_catalogId :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Text)
getConnections_catalogId = Lens.lens (\GetConnections' {catalogId} -> catalogId) (\s@GetConnections' {} a -> s {catalogId = a} :: GetConnections)

-- | The maximum number of connections to return in one response.
getConnections_maxResults :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Natural)
getConnections_maxResults = Lens.lens (\GetConnections' {maxResults} -> maxResults) (\s@GetConnections' {} a -> s {maxResults = a} :: GetConnections)

-- | Allows you to retrieve the connection metadata without returning the
-- password. For instance, the AWS Glue console uses this flag to retrieve
-- the connection, and does not display the password. Set this parameter
-- when the caller might not have permission to use the AWS KMS key to
-- decrypt the password, but it does have permission to access the rest of
-- the connection properties.
getConnections_hidePassword :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Bool)
getConnections_hidePassword = Lens.lens (\GetConnections' {hidePassword} -> hidePassword) (\s@GetConnections' {} a -> s {hidePassword = a} :: GetConnections)

-- | A filter that controls which connections are returned.
getConnections_filter :: Lens.Lens' GetConnections (Prelude.Maybe GetConnectionsFilter)
getConnections_filter = Lens.lens (\GetConnections' {filter'} -> filter') (\s@GetConnections' {} a -> s {filter' = a} :: GetConnections)

instance Pager.AWSPager GetConnections where
  page rq rs
    | Pager.stop
        ( rs
            Lens.^? getConnectionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Pager.stop
        ( rs
            Lens.^? getConnectionsResponse_connectionList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Lens.& getConnections_nextToken
          Lens..~ rs
          Lens.^? getConnectionsResponse_nextToken Prelude.. Lens._Just

instance Prelude.AWSRequest GetConnections where
  type Rs GetConnections = GetConnectionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionsResponse'
            Prelude.<$> (x Prelude..?> "NextToken")
            Prelude.<*> ( x Prelude..?> "ConnectionList"
                            Prelude..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnections

instance Prelude.NFData GetConnections

instance Prelude.ToHeaders GetConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Prelude.=# ("AWSGlue.GetConnections" :: Prelude.ByteString),
            "Content-Type"
              Prelude.=# ( "application/x-amz-json-1.1" ::
                             Prelude.ByteString
                         )
          ]
      )

instance Prelude.ToJSON GetConnections where
  toJSON GetConnections' {..} =
    Prelude.object
      ( Prelude.catMaybes
          [ ("NextToken" Prelude..=) Prelude.<$> nextToken,
            ("CatalogId" Prelude..=) Prelude.<$> catalogId,
            ("MaxResults" Prelude..=) Prelude.<$> maxResults,
            ("HidePassword" Prelude..=) Prelude.<$> hidePassword,
            ("Filter" Prelude..=) Prelude.<$> filter'
          ]
      )

instance Prelude.ToPath GetConnections where
  toPath = Prelude.const "/"

instance Prelude.ToQuery GetConnections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectionsResponse' smart constructor.
data GetConnectionsResponse = GetConnectionsResponse'
  { -- | A continuation token, if the list of connections returned does not
    -- include the last of the filtered connections.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of requested connection definitions.
    connectionList :: Prelude.Maybe [Connection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
  Prelude.Int ->
  GetConnectionsResponse
newGetConnectionsResponse pHttpStatus_ =
  GetConnectionsResponse'
    { nextToken =
        Prelude.Nothing,
      connectionList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A continuation token, if the list of connections returned does not
-- include the last of the filtered connections.
getConnectionsResponse_nextToken :: Lens.Lens' GetConnectionsResponse (Prelude.Maybe Prelude.Text)
getConnectionsResponse_nextToken = Lens.lens (\GetConnectionsResponse' {nextToken} -> nextToken) (\s@GetConnectionsResponse' {} a -> s {nextToken = a} :: GetConnectionsResponse)

-- | A list of requested connection definitions.
getConnectionsResponse_connectionList :: Lens.Lens' GetConnectionsResponse (Prelude.Maybe [Connection])
getConnectionsResponse_connectionList = Lens.lens (\GetConnectionsResponse' {connectionList} -> connectionList) (\s@GetConnectionsResponse' {} a -> s {connectionList = a} :: GetConnectionsResponse) Prelude.. Lens.mapping Prelude._Coerce

-- | The response's http status code.
getConnectionsResponse_httpStatus :: Lens.Lens' GetConnectionsResponse Prelude.Int
getConnectionsResponse_httpStatus = Lens.lens (\GetConnectionsResponse' {httpStatus} -> httpStatus) (\s@GetConnectionsResponse' {} a -> s {httpStatus = a} :: GetConnectionsResponse)

instance Prelude.NFData GetConnectionsResponse
