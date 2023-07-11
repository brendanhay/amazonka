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
-- Module      : Amazonka.MigrationHubStrategy.GetServerDetails
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves detailed information about a specified server.
--
-- This operation returns paginated results.
module Amazonka.MigrationHubStrategy.GetServerDetails
  ( -- * Creating a Request
    GetServerDetails (..),
    newGetServerDetails,

    -- * Request Lenses
    getServerDetails_maxResults,
    getServerDetails_nextToken,
    getServerDetails_serverId,

    -- * Destructuring the Response
    GetServerDetailsResponse (..),
    newGetServerDetailsResponse,

    -- * Response Lenses
    getServerDetailsResponse_associatedApplications,
    getServerDetailsResponse_nextToken,
    getServerDetailsResponse_serverDetail,
    getServerDetailsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MigrationHubStrategy.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetServerDetails' smart constructor.
data GetServerDetails = GetServerDetails'
  { -- | The maximum number of items to include in the response. The maximum
    -- value is 100.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | The token from a previous call that you use to retrieve the next set of
    -- results. For example, if a previous call to this action returned 100
    -- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
    -- results along with a token. You then use the returned token to retrieve
    -- the next set of 10.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of the server.
    serverId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServerDetails' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'getServerDetails_maxResults' - The maximum number of items to include in the response. The maximum
-- value is 100.
--
-- 'nextToken', 'getServerDetails_nextToken' - The token from a previous call that you use to retrieve the next set of
-- results. For example, if a previous call to this action returned 100
-- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
-- results along with a token. You then use the returned token to retrieve
-- the next set of 10.
--
-- 'serverId', 'getServerDetails_serverId' - The ID of the server.
newGetServerDetails ::
  -- | 'serverId'
  Prelude.Text ->
  GetServerDetails
newGetServerDetails pServerId_ =
  GetServerDetails'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serverId = pServerId_
    }

-- | The maximum number of items to include in the response. The maximum
-- value is 100.
getServerDetails_maxResults :: Lens.Lens' GetServerDetails (Prelude.Maybe Prelude.Int)
getServerDetails_maxResults = Lens.lens (\GetServerDetails' {maxResults} -> maxResults) (\s@GetServerDetails' {} a -> s {maxResults = a} :: GetServerDetails)

-- | The token from a previous call that you use to retrieve the next set of
-- results. For example, if a previous call to this action returned 100
-- items, but you set @maxResults@ to 10. You\'ll receive a set of 10
-- results along with a token. You then use the returned token to retrieve
-- the next set of 10.
getServerDetails_nextToken :: Lens.Lens' GetServerDetails (Prelude.Maybe Prelude.Text)
getServerDetails_nextToken = Lens.lens (\GetServerDetails' {nextToken} -> nextToken) (\s@GetServerDetails' {} a -> s {nextToken = a} :: GetServerDetails)

-- | The ID of the server.
getServerDetails_serverId :: Lens.Lens' GetServerDetails Prelude.Text
getServerDetails_serverId = Lens.lens (\GetServerDetails' {serverId} -> serverId) (\s@GetServerDetails' {} a -> s {serverId = a} :: GetServerDetails)

instance Core.AWSPager GetServerDetails where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? getServerDetailsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? getServerDetailsResponse_associatedApplications
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& getServerDetails_nextToken
          Lens..~ rs
          Lens.^? getServerDetailsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest GetServerDetails where
  type
    AWSResponse GetServerDetails =
      GetServerDetailsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetServerDetailsResponse'
            Prelude.<$> ( x
                            Data..?> "associatedApplications"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "serverDetail")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetServerDetails where
  hashWithSalt _salt GetServerDetails' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` serverId

instance Prelude.NFData GetServerDetails where
  rnf GetServerDetails' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serverId

instance Data.ToHeaders GetServerDetails where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath GetServerDetails where
  toPath GetServerDetails' {..} =
    Prelude.mconcat
      ["/get-server-details/", Data.toBS serverId]

instance Data.ToQuery GetServerDetails where
  toQuery GetServerDetails' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newGetServerDetailsResponse' smart constructor.
data GetServerDetailsResponse = GetServerDetailsResponse'
  { -- | The associated application group the server belongs to, as defined in
    -- AWS Application Discovery Service.
    associatedApplications :: Prelude.Maybe [AssociatedApplication],
    -- | The token you use to retrieve the next set of results, or null if there
    -- are no more results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Detailed information about the server.
    serverDetail :: Prelude.Maybe ServerDetail,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetServerDetailsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'associatedApplications', 'getServerDetailsResponse_associatedApplications' - The associated application group the server belongs to, as defined in
-- AWS Application Discovery Service.
--
-- 'nextToken', 'getServerDetailsResponse_nextToken' - The token you use to retrieve the next set of results, or null if there
-- are no more results.
--
-- 'serverDetail', 'getServerDetailsResponse_serverDetail' - Detailed information about the server.
--
-- 'httpStatus', 'getServerDetailsResponse_httpStatus' - The response's http status code.
newGetServerDetailsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetServerDetailsResponse
newGetServerDetailsResponse pHttpStatus_ =
  GetServerDetailsResponse'
    { associatedApplications =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      serverDetail = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The associated application group the server belongs to, as defined in
-- AWS Application Discovery Service.
getServerDetailsResponse_associatedApplications :: Lens.Lens' GetServerDetailsResponse (Prelude.Maybe [AssociatedApplication])
getServerDetailsResponse_associatedApplications = Lens.lens (\GetServerDetailsResponse' {associatedApplications} -> associatedApplications) (\s@GetServerDetailsResponse' {} a -> s {associatedApplications = a} :: GetServerDetailsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The token you use to retrieve the next set of results, or null if there
-- are no more results.
getServerDetailsResponse_nextToken :: Lens.Lens' GetServerDetailsResponse (Prelude.Maybe Prelude.Text)
getServerDetailsResponse_nextToken = Lens.lens (\GetServerDetailsResponse' {nextToken} -> nextToken) (\s@GetServerDetailsResponse' {} a -> s {nextToken = a} :: GetServerDetailsResponse)

-- | Detailed information about the server.
getServerDetailsResponse_serverDetail :: Lens.Lens' GetServerDetailsResponse (Prelude.Maybe ServerDetail)
getServerDetailsResponse_serverDetail = Lens.lens (\GetServerDetailsResponse' {serverDetail} -> serverDetail) (\s@GetServerDetailsResponse' {} a -> s {serverDetail = a} :: GetServerDetailsResponse)

-- | The response's http status code.
getServerDetailsResponse_httpStatus :: Lens.Lens' GetServerDetailsResponse Prelude.Int
getServerDetailsResponse_httpStatus = Lens.lens (\GetServerDetailsResponse' {httpStatus} -> httpStatus) (\s@GetServerDetailsResponse' {} a -> s {httpStatus = a} :: GetServerDetailsResponse)

instance Prelude.NFData GetServerDetailsResponse where
  rnf GetServerDetailsResponse' {..} =
    Prelude.rnf associatedApplications
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf serverDetail
      `Prelude.seq` Prelude.rnf httpStatus
