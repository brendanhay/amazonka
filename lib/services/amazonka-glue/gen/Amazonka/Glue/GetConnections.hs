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
-- Module      : Amazonka.Glue.GetConnections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves a list of connection definitions from the Data Catalog.
--
-- This operation returns paginated results.
module Amazonka.Glue.GetConnections
  ( -- * Creating a Request
    GetConnections (..),
    newGetConnections,

    -- * Request Lenses
    getConnections_catalogId,
    getConnections_filter,
    getConnections_hidePassword,
    getConnections_maxResults,
    getConnections_nextToken,

    -- * Destructuring the Response
    GetConnectionsResponse (..),
    newGetConnectionsResponse,

    -- * Response Lenses
    getConnectionsResponse_connectionList,
    getConnectionsResponse_nextToken,
    getConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Glue.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetConnections' smart constructor.
data GetConnections = GetConnections'
  { -- | The ID of the Data Catalog in which the connections reside. If none is
    -- provided, the Amazon Web Services account ID is used by default.
    catalogId :: Prelude.Maybe Prelude.Text,
    -- | A filter that controls which connections are returned.
    filter' :: Prelude.Maybe GetConnectionsFilter,
    -- | Allows you to retrieve the connection metadata without returning the
    -- password. For instance, the Glue console uses this flag to retrieve the
    -- connection, and does not display the password. Set this parameter when
    -- the caller might not have permission to use the KMS key to decrypt the
    -- password, but it does have permission to access the rest of the
    -- connection properties.
    hidePassword :: Prelude.Maybe Prelude.Bool,
    -- | The maximum number of connections to return in one response.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | A continuation token, if this is a continuation call.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'catalogId', 'getConnections_catalogId' - The ID of the Data Catalog in which the connections reside. If none is
-- provided, the Amazon Web Services account ID is used by default.
--
-- 'filter'', 'getConnections_filter' - A filter that controls which connections are returned.
--
-- 'hidePassword', 'getConnections_hidePassword' - Allows you to retrieve the connection metadata without returning the
-- password. For instance, the Glue console uses this flag to retrieve the
-- connection, and does not display the password. Set this parameter when
-- the caller might not have permission to use the KMS key to decrypt the
-- password, but it does have permission to access the rest of the
-- connection properties.
--
-- 'maxResults', 'getConnections_maxResults' - The maximum number of connections to return in one response.
--
-- 'nextToken', 'getConnections_nextToken' - A continuation token, if this is a continuation call.
newGetConnections ::
  GetConnections
newGetConnections =
  GetConnections'
    { catalogId = Prelude.Nothing,
      filter' = Prelude.Nothing,
      hidePassword = Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The ID of the Data Catalog in which the connections reside. If none is
-- provided, the Amazon Web Services account ID is used by default.
getConnections_catalogId :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Text)
getConnections_catalogId = Lens.lens (\GetConnections' {catalogId} -> catalogId) (\s@GetConnections' {} a -> s {catalogId = a} :: GetConnections)

-- | A filter that controls which connections are returned.
getConnections_filter :: Lens.Lens' GetConnections (Prelude.Maybe GetConnectionsFilter)
getConnections_filter = Lens.lens (\GetConnections' {filter'} -> filter') (\s@GetConnections' {} a -> s {filter' = a} :: GetConnections)

-- | Allows you to retrieve the connection metadata without returning the
-- password. For instance, the Glue console uses this flag to retrieve the
-- connection, and does not display the password. Set this parameter when
-- the caller might not have permission to use the KMS key to decrypt the
-- password, but it does have permission to access the rest of the
-- connection properties.
getConnections_hidePassword :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Bool)
getConnections_hidePassword = Lens.lens (\GetConnections' {hidePassword} -> hidePassword) (\s@GetConnections' {} a -> s {hidePassword = a} :: GetConnections)

-- | The maximum number of connections to return in one response.
getConnections_maxResults :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Natural)
getConnections_maxResults = Lens.lens (\GetConnections' {maxResults} -> maxResults) (\s@GetConnections' {} a -> s {maxResults = a} :: GetConnections)

-- | A continuation token, if this is a continuation call.
getConnections_nextToken :: Lens.Lens' GetConnections (Prelude.Maybe Prelude.Text)
getConnections_nextToken = Lens.lens (\GetConnections' {nextToken} -> nextToken) (\s@GetConnections' {} a -> s {nextToken = a} :: GetConnections)

instance Core.AWSPager GetConnections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getConnectionsResponse_nextToken
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getConnectionsResponse_connectionList
              Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getConnections_nextToken
          Lens..~ rs
          Lens.^? getConnectionsResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetConnections where
  type
    AWSResponse GetConnections =
      GetConnectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetConnectionsResponse'
            Prelude.<$> (x Data..?> "ConnectionList" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetConnections where
  hashWithSalt _salt GetConnections' {..} =
    _salt `Prelude.hashWithSalt` catalogId
      `Prelude.hashWithSalt` filter'
      `Prelude.hashWithSalt` hidePassword
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData GetConnections where
  rnf GetConnections' {..} =
    Prelude.rnf catalogId
      `Prelude.seq` Prelude.rnf filter'
      `Prelude.seq` Prelude.rnf hidePassword
      `Prelude.seq` Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders GetConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ("AWSGlue.GetConnections" :: Prelude.ByteString),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetConnections where
  toJSON GetConnections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("CatalogId" Data..=) Prelude.<$> catalogId,
            ("Filter" Data..=) Prelude.<$> filter',
            ("HidePassword" Data..=) Prelude.<$> hidePassword,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath GetConnections where
  toPath = Prelude.const "/"

instance Data.ToQuery GetConnections where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetConnectionsResponse' smart constructor.
data GetConnectionsResponse = GetConnectionsResponse'
  { -- | A list of requested connection definitions.
    connectionList :: Prelude.Maybe [Connection],
    -- | A continuation token, if the list of connections returned does not
    -- include the last of the filtered connections.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connectionList', 'getConnectionsResponse_connectionList' - A list of requested connection definitions.
--
-- 'nextToken', 'getConnectionsResponse_nextToken' - A continuation token, if the list of connections returned does not
-- include the last of the filtered connections.
--
-- 'httpStatus', 'getConnectionsResponse_httpStatus' - The response's http status code.
newGetConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetConnectionsResponse
newGetConnectionsResponse pHttpStatus_ =
  GetConnectionsResponse'
    { connectionList =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A list of requested connection definitions.
getConnectionsResponse_connectionList :: Lens.Lens' GetConnectionsResponse (Prelude.Maybe [Connection])
getConnectionsResponse_connectionList = Lens.lens (\GetConnectionsResponse' {connectionList} -> connectionList) (\s@GetConnectionsResponse' {} a -> s {connectionList = a} :: GetConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | A continuation token, if the list of connections returned does not
-- include the last of the filtered connections.
getConnectionsResponse_nextToken :: Lens.Lens' GetConnectionsResponse (Prelude.Maybe Prelude.Text)
getConnectionsResponse_nextToken = Lens.lens (\GetConnectionsResponse' {nextToken} -> nextToken) (\s@GetConnectionsResponse' {} a -> s {nextToken = a} :: GetConnectionsResponse)

-- | The response's http status code.
getConnectionsResponse_httpStatus :: Lens.Lens' GetConnectionsResponse Prelude.Int
getConnectionsResponse_httpStatus = Lens.lens (\GetConnectionsResponse' {httpStatus} -> httpStatus) (\s@GetConnectionsResponse' {} a -> s {httpStatus = a} :: GetConnectionsResponse)

instance Prelude.NFData GetConnectionsResponse where
  rnf GetConnectionsResponse' {..} =
    Prelude.rnf connectionList
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
