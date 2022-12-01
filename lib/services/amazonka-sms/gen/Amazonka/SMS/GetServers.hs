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
-- Module      : Amazonka.SMS.GetServers
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the servers in your server catalog.
--
-- Before you can describe your servers, you must import them using
-- ImportServerCatalog.
--
-- This operation returns paginated results.
module Amazonka.SMS.GetServers
  ( -- * Creating a Request
    GetServers (..),
    newGetServers,

    -- * Request Lenses
    getServers_nextToken,
    getServers_vmServerAddressList,
    getServers_maxResults,

    -- * Destructuring the Response
    GetServersResponse (..),
    newGetServersResponse,

    -- * Response Lenses
    getServersResponse_nextToken,
    getServersResponse_lastModifiedOn,
    getServersResponse_serverCatalogStatus,
    getServersResponse_serverList,
    getServersResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.SMS.Types

-- | /See:/ 'newGetServers' smart constructor.
data GetServers = GetServers'
  { -- | The token for the next set of results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The server addresses.
    vmServerAddressList :: Prelude.Maybe [VmServerAddress],
    -- | The maximum number of results to return in a single call. The default
    -- value is 50. To retrieve the remaining results, make another call with
    -- the returned @NextToken@ value.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServers' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getServers_nextToken' - The token for the next set of results.
--
-- 'vmServerAddressList', 'getServers_vmServerAddressList' - The server addresses.
--
-- 'maxResults', 'getServers_maxResults' - The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
newGetServers ::
  GetServers
newGetServers =
  GetServers'
    { nextToken = Prelude.Nothing,
      vmServerAddressList = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | The token for the next set of results.
getServers_nextToken :: Lens.Lens' GetServers (Prelude.Maybe Prelude.Text)
getServers_nextToken = Lens.lens (\GetServers' {nextToken} -> nextToken) (\s@GetServers' {} a -> s {nextToken = a} :: GetServers)

-- | The server addresses.
getServers_vmServerAddressList :: Lens.Lens' GetServers (Prelude.Maybe [VmServerAddress])
getServers_vmServerAddressList = Lens.lens (\GetServers' {vmServerAddressList} -> vmServerAddressList) (\s@GetServers' {} a -> s {vmServerAddressList = a} :: GetServers) Prelude.. Lens.mapping Lens.coerced

-- | The maximum number of results to return in a single call. The default
-- value is 50. To retrieve the remaining results, make another call with
-- the returned @NextToken@ value.
getServers_maxResults :: Lens.Lens' GetServers (Prelude.Maybe Prelude.Int)
getServers_maxResults = Lens.lens (\GetServers' {maxResults} -> maxResults) (\s@GetServers' {} a -> s {maxResults = a} :: GetServers)

instance Core.AWSPager GetServers where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getServersResponse_nextToken Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getServersResponse_serverList Prelude.. Lens._Just
        ) =
      Prelude.Nothing
    | Prelude.otherwise =
      Prelude.Just Prelude.$
        rq
          Prelude.& getServers_nextToken
          Lens..~ rs
          Lens.^? getServersResponse_nextToken Prelude.. Lens._Just

instance Core.AWSRequest GetServers where
  type AWSResponse GetServers = GetServersResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServersResponse'
            Prelude.<$> (x Core..?> "nextToken")
            Prelude.<*> (x Core..?> "lastModifiedOn")
            Prelude.<*> (x Core..?> "serverCatalogStatus")
            Prelude.<*> (x Core..?> "serverList" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServers where
  hashWithSalt _salt GetServers' {..} =
    _salt `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` vmServerAddressList
      `Prelude.hashWithSalt` maxResults

instance Prelude.NFData GetServers where
  rnf GetServers' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vmServerAddressList
      `Prelude.seq` Prelude.rnf maxResults

instance Core.ToHeaders GetServers where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "AWSServerMigrationService_V2016_10_24.GetServers" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetServers where
  toJSON GetServers' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("nextToken" Core..=) Prelude.<$> nextToken,
            ("vmServerAddressList" Core..=)
              Prelude.<$> vmServerAddressList,
            ("maxResults" Core..=) Prelude.<$> maxResults
          ]
      )

instance Core.ToPath GetServers where
  toPath = Prelude.const "/"

instance Core.ToQuery GetServers where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetServersResponse' smart constructor.
data GetServersResponse = GetServersResponse'
  { -- | The token required to retrieve the next set of results. This value is
    -- null when there are no more results to return.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The time when the server was last modified.
    lastModifiedOn :: Prelude.Maybe Core.POSIX,
    -- | The status of the server catalog.
    serverCatalogStatus :: Prelude.Maybe ServerCatalogStatus,
    -- | Information about the servers.
    serverList :: Prelude.Maybe [Server],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServersResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'getServersResponse_nextToken' - The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
--
-- 'lastModifiedOn', 'getServersResponse_lastModifiedOn' - The time when the server was last modified.
--
-- 'serverCatalogStatus', 'getServersResponse_serverCatalogStatus' - The status of the server catalog.
--
-- 'serverList', 'getServersResponse_serverList' - Information about the servers.
--
-- 'httpStatus', 'getServersResponse_httpStatus' - The response's http status code.
newGetServersResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServersResponse
newGetServersResponse pHttpStatus_ =
  GetServersResponse'
    { nextToken = Prelude.Nothing,
      lastModifiedOn = Prelude.Nothing,
      serverCatalogStatus = Prelude.Nothing,
      serverList = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The token required to retrieve the next set of results. This value is
-- null when there are no more results to return.
getServersResponse_nextToken :: Lens.Lens' GetServersResponse (Prelude.Maybe Prelude.Text)
getServersResponse_nextToken = Lens.lens (\GetServersResponse' {nextToken} -> nextToken) (\s@GetServersResponse' {} a -> s {nextToken = a} :: GetServersResponse)

-- | The time when the server was last modified.
getServersResponse_lastModifiedOn :: Lens.Lens' GetServersResponse (Prelude.Maybe Prelude.UTCTime)
getServersResponse_lastModifiedOn = Lens.lens (\GetServersResponse' {lastModifiedOn} -> lastModifiedOn) (\s@GetServersResponse' {} a -> s {lastModifiedOn = a} :: GetServersResponse) Prelude.. Lens.mapping Core._Time

-- | The status of the server catalog.
getServersResponse_serverCatalogStatus :: Lens.Lens' GetServersResponse (Prelude.Maybe ServerCatalogStatus)
getServersResponse_serverCatalogStatus = Lens.lens (\GetServersResponse' {serverCatalogStatus} -> serverCatalogStatus) (\s@GetServersResponse' {} a -> s {serverCatalogStatus = a} :: GetServersResponse)

-- | Information about the servers.
getServersResponse_serverList :: Lens.Lens' GetServersResponse (Prelude.Maybe [Server])
getServersResponse_serverList = Lens.lens (\GetServersResponse' {serverList} -> serverList) (\s@GetServersResponse' {} a -> s {serverList = a} :: GetServersResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getServersResponse_httpStatus :: Lens.Lens' GetServersResponse Prelude.Int
getServersResponse_httpStatus = Lens.lens (\GetServersResponse' {httpStatus} -> httpStatus) (\s@GetServersResponse' {} a -> s {httpStatus = a} :: GetServersResponse)

instance Prelude.NFData GetServersResponse where
  rnf GetServersResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf lastModifiedOn
      `Prelude.seq` Prelude.rnf serverCatalogStatus
      `Prelude.seq` Prelude.rnf serverList
      `Prelude.seq` Prelude.rnf httpStatus
