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
-- Module      : Amazonka.Kafka.ListVpcConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of all the VPC connections in this Region.
--
-- This operation returns paginated results.
module Amazonka.Kafka.ListVpcConnections
  ( -- * Creating a Request
    ListVpcConnections (..),
    newListVpcConnections,

    -- * Request Lenses
    listVpcConnections_maxResults,
    listVpcConnections_nextToken,

    -- * Destructuring the Response
    ListVpcConnectionsResponse (..),
    newListVpcConnectionsResponse,

    -- * Response Lenses
    listVpcConnectionsResponse_nextToken,
    listVpcConnectionsResponse_vpcConnections,
    listVpcConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Kafka.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newListVpcConnections' smart constructor.
data ListVpcConnections = ListVpcConnections'
  { -- | The maximum number of results to return in the response. If there are
    -- more results, the response includes a NextToken parameter.
    maxResults :: Prelude.Maybe Prelude.Natural,
    -- | The paginated results marker. When the result of the operation is
    -- truncated, the call returns NextToken in the response. To get the next
    -- batch, provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxResults', 'listVpcConnections_maxResults' - The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
--
-- 'nextToken', 'listVpcConnections_nextToken' - The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
newListVpcConnections ::
  ListVpcConnections
newListVpcConnections =
  ListVpcConnections'
    { maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | The maximum number of results to return in the response. If there are
-- more results, the response includes a NextToken parameter.
listVpcConnections_maxResults :: Lens.Lens' ListVpcConnections (Prelude.Maybe Prelude.Natural)
listVpcConnections_maxResults = Lens.lens (\ListVpcConnections' {maxResults} -> maxResults) (\s@ListVpcConnections' {} a -> s {maxResults = a} :: ListVpcConnections)

-- | The paginated results marker. When the result of the operation is
-- truncated, the call returns NextToken in the response. To get the next
-- batch, provide this token in your next request.
listVpcConnections_nextToken :: Lens.Lens' ListVpcConnections (Prelude.Maybe Prelude.Text)
listVpcConnections_nextToken = Lens.lens (\ListVpcConnections' {nextToken} -> nextToken) (\s@ListVpcConnections' {} a -> s {nextToken = a} :: ListVpcConnections)

instance Core.AWSPager ListVpcConnections where
  page rq rs
    | Core.stop
        ( rs
            Lens.^? listVpcConnectionsResponse_nextToken
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Core.stop
        ( rs
            Lens.^? listVpcConnectionsResponse_vpcConnections
            Prelude.. Lens._Just
        ) =
        Prelude.Nothing
    | Prelude.otherwise =
        Prelude.Just
          Prelude.$ rq
          Prelude.& listVpcConnections_nextToken
          Lens..~ rs
          Lens.^? listVpcConnectionsResponse_nextToken
          Prelude.. Lens._Just

instance Core.AWSRequest ListVpcConnections where
  type
    AWSResponse ListVpcConnections =
      ListVpcConnectionsResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          ListVpcConnectionsResponse'
            Prelude.<$> (x Data..?> "nextToken")
            Prelude.<*> (x Data..?> "vpcConnections" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable ListVpcConnections where
  hashWithSalt _salt ListVpcConnections' {..} =
    _salt
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData ListVpcConnections where
  rnf ListVpcConnections' {..} =
    Prelude.rnf maxResults
      `Prelude.seq` Prelude.rnf nextToken

instance Data.ToHeaders ListVpcConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToPath ListVpcConnections where
  toPath = Prelude.const "/v1/vpc-connections"

instance Data.ToQuery ListVpcConnections where
  toQuery ListVpcConnections' {..} =
    Prelude.mconcat
      [ "maxResults" Data.=: maxResults,
        "nextToken" Data.=: nextToken
      ]

-- | /See:/ 'newListVpcConnectionsResponse' smart constructor.
data ListVpcConnectionsResponse = ListVpcConnectionsResponse'
  { -- | The paginated results marker. When the result of a
    -- ListClientVpcConnections operation is truncated, the call returns
    -- NextToken in the response. To get another batch of configurations,
    -- provide this token in your next request.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | List of VPC connections.
    vpcConnections :: Prelude.Maybe [VpcConnection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ListVpcConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'listVpcConnectionsResponse_nextToken' - The paginated results marker. When the result of a
-- ListClientVpcConnections operation is truncated, the call returns
-- NextToken in the response. To get another batch of configurations,
-- provide this token in your next request.
--
-- 'vpcConnections', 'listVpcConnectionsResponse_vpcConnections' - List of VPC connections.
--
-- 'httpStatus', 'listVpcConnectionsResponse_httpStatus' - The response's http status code.
newListVpcConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  ListVpcConnectionsResponse
newListVpcConnectionsResponse pHttpStatus_ =
  ListVpcConnectionsResponse'
    { nextToken =
        Prelude.Nothing,
      vpcConnections = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The paginated results marker. When the result of a
-- ListClientVpcConnections operation is truncated, the call returns
-- NextToken in the response. To get another batch of configurations,
-- provide this token in your next request.
listVpcConnectionsResponse_nextToken :: Lens.Lens' ListVpcConnectionsResponse (Prelude.Maybe Prelude.Text)
listVpcConnectionsResponse_nextToken = Lens.lens (\ListVpcConnectionsResponse' {nextToken} -> nextToken) (\s@ListVpcConnectionsResponse' {} a -> s {nextToken = a} :: ListVpcConnectionsResponse)

-- | List of VPC connections.
listVpcConnectionsResponse_vpcConnections :: Lens.Lens' ListVpcConnectionsResponse (Prelude.Maybe [VpcConnection])
listVpcConnectionsResponse_vpcConnections = Lens.lens (\ListVpcConnectionsResponse' {vpcConnections} -> vpcConnections) (\s@ListVpcConnectionsResponse' {} a -> s {vpcConnections = a} :: ListVpcConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
listVpcConnectionsResponse_httpStatus :: Lens.Lens' ListVpcConnectionsResponse Prelude.Int
listVpcConnectionsResponse_httpStatus = Lens.lens (\ListVpcConnectionsResponse' {httpStatus} -> httpStatus) (\s@ListVpcConnectionsResponse' {} a -> s {httpStatus = a} :: ListVpcConnectionsResponse)

instance Prelude.NFData ListVpcConnectionsResponse where
  rnf ListVpcConnectionsResponse' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf vpcConnections
      `Prelude.seq` Prelude.rnf httpStatus
