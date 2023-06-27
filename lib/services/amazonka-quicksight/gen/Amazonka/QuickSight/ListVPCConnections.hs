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
-- Module      : Amazonka.QuickSight.ListVPCConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all of the VPC connections in the current set Amazon Web Services
-- Region of an Amazon Web Services account.
module Amazonka.QuickSight.ListVPCConnections
  ( -- * Creating a Request
    ListVPCConnections (..),
    newListVPCConnections,

    -- * Request Lenses
    listVPCConnections_maxResults,
    listVPCConnections_nextToken,
    listVPCConnections_awsAccountId,

    -- * Destructuring the Response
    ListVPCConnectionsResponse (..),
    newListVPCConnectionsResponse,

    -- * Response Lenses
    listVPCConnectionsResponse_nextToken,
    listVPCConnectionsResponse_requestId,
    listVPCConnectionsResponse_vPCConnectionSummaries,
    listVPCConnectionsResponse_status,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.QuickSight.Types
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVPCConnections' smart constructor.
data ListVPCConnections = ListVPCConnections'
  { -- | The maximum number of results to be returned per request.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services account ID of the account that contains the VPC
    -- connections that you want to list.
    awsAccountId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVPCConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVPCConnections_maxResults' - The maximum number of results to be returned per request.
--
-- 'nextToken', 'listVPCConnections_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'awsAccountId', 'listVPCConnections_awsAccountId' - The Amazon Web Services account ID of the account that contains the VPC
-- connections that you want to list.
newListVPCConnections ::
  -- | 'awsAccountId'
  Prelude.Text ->
  ListVPCConnections
newListVPCConnections pAwsAccountId_ =
  ListVPCConnections'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing,
      awsAccountId = pAwsAccountId_
    }

-- | The maximum number of results to be returned per request.
listVPCConnections_maxResults :: Lens.Lens' ListVPCConnections (Prelude.Maybe Prelude.Natural)
listVPCConnections_maxResults = Lens.lens (\ListVPCConnections' {maxResults} -> maxResults) (\s@ListVPCConnections' {} a -> s {maxResults = a} :: ListVPCConnections)

-- | The token for the next set of results, or null if there are no more
-- results.
listVPCConnections_nextToken :: Lens.Lens' ListVPCConnections (Prelude.Maybe Prelude.Text)
listVPCConnections_nextToken = Lens.lens (\ListVPCConnections' {nextToken} -> nextToken) (\s@ListVPCConnections' {} a -> s {nextToken = a} :: ListVPCConnections)

-- | The Amazon Web Services account ID of the account that contains the VPC
-- connections that you want to list.
listVPCConnections_awsAccountId :: Lens.Lens' ListVPCConnections Prelude.Text
listVPCConnections_awsAccountId = Lens.lens (\ListVPCConnections' {awsAccountId} -> awsAccountId) (\s@ListVPCConnections' {} a -> s {awsAccountId = a} :: ListVPCConnections)

instance Core.AWSRequest ListVPCConnections where
  type
    AWSResponse ListVPCConnections =
      ListVPCConnectionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVPCConnectionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
            Prelude.<*> (x Data..?> "RequestId")
            Prelude.<*> ( x
                            Data..?> "VPCConnectionSummaries"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVPCConnections where
  hashWithSalt _salt ListVPCConnections' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` awsAccountId

instance Prelude.NFData ListVPCConnections where
  rnf ListVPCConnections' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf awsAccountId

instance Data.ToHeaders ListVPCConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.0" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListVPCConnections where
  toPath ListVPCConnections' {..} =
    Prelude.mconcat
      [ "/accounts/",
        Data.toBS awsAccountId,
        "/vpc-connections"
      ]

instance Data.ToQuery ListVPCConnections where
  toQuery ListVPCConnections' {..} =
    Prelude.mconcat
      [ "max-results" Data.=: maxResults,
        "next-token" Data.=: nextToken
      ]

-- | /See:/ 'newListVPCConnectionsResponse' smart constructor.
data ListVPCConnectionsResponse = ListVPCConnectionsResponse'
  { -- | The token for the next set of results, or null if there are no more
    -- results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Web Services request ID for this operation.
    requestId :: Prelude.Maybe Prelude.Text,
    -- | A @VPCConnectionSummaries@ object that returns a summary of VPC
    -- connection objects.
    vPCConnectionSummaries :: Prelude.Maybe [VPCConnectionSummary],
    -- | The HTTP status of the request.
    status :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVPCConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVPCConnectionsResponse_nextToken' - The token for the next set of results, or null if there are no more
-- results.
--
-- 'requestId', 'listVPCConnectionsResponse_requestId' - The Amazon Web Services request ID for this operation.
--
-- 'vPCConnectionSummaries', 'listVPCConnectionsResponse_vPCConnectionSummaries' - A @VPCConnectionSummaries@ object that returns a summary of VPC
-- connection objects.
--
-- 'status', 'listVPCConnectionsResponse_status' - The HTTP status of the request.
newListVPCConnectionsResponse ::
  -- | 'status'
  Prelude.Int ->
  ListVPCConnectionsResponse
newListVPCConnectionsResponse pStatus_ =
  ListVPCConnectionsResponse'
    { nextToken =
        Prelude.Nothing,
      requestId = Prelude.Nothing,
      vPCConnectionSummaries = Prelude.Nothing,
      status = pStatus_
    }

-- | The token for the next set of results, or null if there are no more
-- results.
listVPCConnectionsResponse_nextToken :: Lens.Lens' ListVPCConnectionsResponse (Prelude.Maybe Prelude.Text)
listVPCConnectionsResponse_nextToken = Lens.lens (\ListVPCConnectionsResponse' {nextToken} -> nextToken) (\s@ListVPCConnectionsResponse' {} a -> s {nextToken = a} :: ListVPCConnectionsResponse)

-- | The Amazon Web Services request ID for this operation.
listVPCConnectionsResponse_requestId :: Lens.Lens' ListVPCConnectionsResponse (Prelude.Maybe Prelude.Text)
listVPCConnectionsResponse_requestId = Lens.lens (\ListVPCConnectionsResponse' {requestId} -> requestId) (\s@ListVPCConnectionsResponse' {} a -> s {requestId = a} :: ListVPCConnectionsResponse)

-- | A @VPCConnectionSummaries@ object that returns a summary of VPC
-- connection objects.
listVPCConnectionsResponse_vPCConnectionSummaries :: Lens.Lens' ListVPCConnectionsResponse (Prelude.Maybe [VPCConnectionSummary])
listVPCConnectionsResponse_vPCConnectionSummaries = Lens.lens (\ListVPCConnectionsResponse' {vPCConnectionSummaries} -> vPCConnectionSummaries) (\s@ListVPCConnectionsResponse' {} a -> s {vPCConnectionSummaries = a} :: ListVPCConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The HTTP status of the request.
listVPCConnectionsResponse_status :: Lens.Lens' ListVPCConnectionsResponse Prelude.Int
listVPCConnectionsResponse_status = Lens.lens (\ListVPCConnectionsResponse' {status} -> status) (\s@ListVPCConnectionsResponse' {} a -> s {status = a} :: ListVPCConnectionsResponse)

instance Prelude.NFData ListVPCConnectionsResponse where
  rnf ListVPCConnectionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf requestId
      `Prelude.seq` Prelude.rnf vPCConnectionSummaries
      `Prelude.seq` Prelude.rnf status
