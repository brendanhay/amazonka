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
-- Module      : Amazonka.Kafka.ListClientVpcConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the VPC connections in this Region.
--
-- This operation returns paginated results.
module Amazonka.Kafka.ListClientVpcConnections
  ( -- * Creating a Request
    ListClientVpcConnections (..),
    newListClientVpcConnections,

    -- * Request Lenses
    listClientVpcConnections_maxResults,
    listClientVpcConnections_nextToken,
    listClientVpcConnections_clusterArn,

    -- * Destructuring the Response
    ListClientVpcConnectionsResponse (..),
    newListClientVpcConnectionsResponse,

    -- * Response Lenses
    listClientVpcConnectionsResponse_clientVpcConnections,
    listClientVpcConnectionsResponse_nextToken,
    listClientVpcConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListClientVpcConnections' smart constructor.
data ListClientVpcConnections = ListClientVpcConnections'
  { -- | The maximum number of results to return in the response. If there are
    -- more results, the response includes a NextToken parameter.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The paginated results marker. When the result of the operation is
    -- truncated, the call returns NextToken in the response. To get the next
    -- batch, provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the cluster.
    clusterArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClientVpcConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listClientVpcConnections_maxResults' - The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
--
-- 'nextToken', 'listClientVpcConnections_nextToken' - The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
--
-- 'clusterArn', 'listClientVpcConnections_clusterArn' - The Amazon Resource Name (ARN) of the cluster.
newListClientVpcConnections ::
  -- | 'clusterArn'
  Prelude.Text ->
  ListClientVpcConnections
newListClientVpcConnections pClusterArn_ =
  ListClientVpcConnections'
    { maxResults =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      clusterArn = pClusterArn_
    }

-- | The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
listClientVpcConnections_maxResults :: Lens.Lens' ListClientVpcConnections (Prelude.Maybe Prelude.Natural)
listClientVpcConnections_maxResults = Lens.lens (\ListClientVpcConnections' {maxResults} -> maxResults) (\s@ListClientVpcConnections' {} a -> s {maxResults = a} :: ListClientVpcConnections)

-- | The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
listClientVpcConnections_nextToken :: Lens.Lens' ListClientVpcConnections (Prelude.Maybe Prelude.Text)
listClientVpcConnections_nextToken = Lens.lens (\ListClientVpcConnections' {nextToken} -> nextToken) (\s@ListClientVpcConnections' {} a -> s {nextToken = a} :: ListClientVpcConnections)

-- | The Amazon Resource Name (ARN) of the cluster.
listClientVpcConnections_clusterArn :: Lens.Lens' ListClientVpcConnections Prelude.Text
listClientVpcConnections_clusterArn = Lens.lens (\ListClientVpcConnections' {clusterArn} -> clusterArn) (\s@ListClientVpcConnections' {} a -> s {clusterArn = a} :: ListClientVpcConnections)

instance Core.AWSPager ListClientVpcConnections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listClientVpcConnectionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listClientVpcConnectionsResponse_clientVpcConnections
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listClientVpcConnections_nextToken
          Lens..~ rs
          Lens.^? listClientVpcConnectionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListClientVpcConnections where
  type
    AWSResponse ListClientVpcConnections =
      ListClientVpcConnectionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListClientVpcConnectionsResponse'
            Prelude.<$> ( x
                            Data..?> "clientVpcConnections"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (x Data..?> "nextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListClientVpcConnections where
  hashWithSalt _salt ListClientVpcConnections' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken
      `Prelude.hashWithSalt` clusterArn

instance Prelude.NFData ListClientVpcConnections where
  rnf ListClientVpcConnections' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf clusterArn

instance Data.ToHeaders ListClientVpcConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListClientVpcConnections where
  toPath ListClientVpcConnections' {..} =
    Prelude.mconcat
      [ "/v1/clusters/",
        Data.toBS clusterArn,
        "/client-vpc-connections"
      ]

instance Data.ToQuery ListClientVpcConnections where
  toQuery ListClientVpcConnections' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListClientVpcConnectionsResponse' smart constructor.
data ListClientVpcConnectionsResponse = ListClientVpcConnectionsResponse'
  { -- | List of client VPC connections.
    clientVpcConnections :: Prelude.Maybe [ClientVpcConnection],
    -- | The paginated results marker. When the result of a
    -- ListClientVpcConnections operation is truncated, the call returns
    -- NextToken in the response. To get another batch of configurations,
    -- provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListClientVpcConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'clientVpcConnections', 'listClientVpcConnectionsResponse_clientVpcConnections' - List of client VPC connections.
--
-- 'nextToken', 'listClientVpcConnectionsResponse_nextToken' - The paginated results marker. When the result of a
-- ListClientVpcConnections operation is truncated, the call returns
-- NextToken in the response. To get another batch of configurations,
-- provide this token in your next request.
--
-- 'httpStatus', 'listClientVpcConnectionsResponse_httpStatus' - The response's http status code.
newListClientVpcConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListClientVpcConnectionsResponse
newListClientVpcConnectionsResponse pHttpStatus_ =
  ListClientVpcConnectionsResponse'
    { clientVpcConnections =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of client VPC connections.
listClientVpcConnectionsResponse_clientVpcConnections :: Lens.Lens' ListClientVpcConnectionsResponse (Prelude.Maybe [ClientVpcConnection])
listClientVpcConnectionsResponse_clientVpcConnections = Lens.lens (\ListClientVpcConnectionsResponse' {clientVpcConnections} -> clientVpcConnections) (\s@ListClientVpcConnectionsResponse' {} a -> s {clientVpcConnections = a} :: ListClientVpcConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The paginated results marker. When the result of a
-- ListClientVpcConnections operation is truncated, the call returns
-- NextToken in the response. To get another batch of configurations,
-- provide this token in your next request.
listClientVpcConnectionsResponse_nextToken :: Lens.Lens' ListClientVpcConnectionsResponse (Prelude.Maybe Prelude.Text)
listClientVpcConnectionsResponse_nextToken = Lens.lens (\ListClientVpcConnectionsResponse' {nextToken} -> nextToken) (\s@ListClientVpcConnectionsResponse' {} a -> s {nextToken = a} :: ListClientVpcConnectionsResponse)

-- | The response's http status code.
listClientVpcConnectionsResponse_httpStatus :: Lens.Lens' ListClientVpcConnectionsResponse Prelude.Int
listClientVpcConnectionsResponse_httpStatus = Lens.lens (\ListClientVpcConnectionsResponse' {httpStatus} -> httpStatus) (\s@ListClientVpcConnectionsResponse' {} a -> s {httpStatus = a} :: ListClientVpcConnectionsResponse)

instance
  Prelude.NFData
    ListClientVpcConnectionsResponse
  where
  rnf ListClientVpcConnectionsResponse' {..} =
    Prelude.rnf clientVpcConnections
      `Prelude.seq` Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf httpStatus
