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
-- Module      : Amazonka.OpenSearch.DescribeInboundConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the inbound cross-cluster search connections for a destination
-- (remote) Amazon OpenSearch Service domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cross-cluster-search.html Cross-cluster search for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DescribeInboundConnections
  ( -- * Creating a Request
    DescribeInboundConnections (..),
    newDescribeInboundConnections,

    -- * Request Lenses
    describeInboundConnections_filters,
    describeInboundConnections_maxResults,
    describeInboundConnections_nextToken,

    -- * Destructuring the Response
    DescribeInboundConnectionsResponse (..),
    newDescribeInboundConnectionsResponse,

    -- * Response Lenses
    describeInboundConnectionsResponse_connections,
    describeInboundConnectionsResponse_nextToken,
    describeInboundConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeInboundConnections@
-- operation.
--
-- /See:/ 'newDescribeInboundConnections' smart constructor.
data DescribeInboundConnections = DescribeInboundConnections'
  { -- | A list of filters used to match properties for inbound cross-cluster
    -- connections.
    filters :: Prelude.Maybe [Filter],
    -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If your initial @DescribeInboundConnections@ operation returns a
    -- @nextToken@, you can include the returned @nextToken@ in subsequent
    -- @DescribeInboundConnections@ operations, which returns results in the
    -- next page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInboundConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeInboundConnections_filters' - A list of filters used to match properties for inbound cross-cluster
-- connections.
--
-- 'maxResults', 'describeInboundConnections_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'nextToken', 'describeInboundConnections_nextToken' - If your initial @DescribeInboundConnections@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribeInboundConnections@ operations, which returns results in the
-- next page.
newDescribeInboundConnections ::
  DescribeInboundConnections
newDescribeInboundConnections =
  DescribeInboundConnections'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | A list of filters used to match properties for inbound cross-cluster
-- connections.
describeInboundConnections_filters :: Lens.Lens' DescribeInboundConnections (Prelude.Maybe [Filter])
describeInboundConnections_filters = Lens.lens (\DescribeInboundConnections' {filters} -> filters) (\s@DescribeInboundConnections' {} a -> s {filters = a} :: DescribeInboundConnections) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
describeInboundConnections_maxResults :: Lens.Lens' DescribeInboundConnections (Prelude.Maybe Prelude.Int)
describeInboundConnections_maxResults = Lens.lens (\DescribeInboundConnections' {maxResults} -> maxResults) (\s@DescribeInboundConnections' {} a -> s {maxResults = a} :: DescribeInboundConnections)

-- | If your initial @DescribeInboundConnections@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribeInboundConnections@ operations, which returns results in the
-- next page.
describeInboundConnections_nextToken :: Lens.Lens' DescribeInboundConnections (Prelude.Maybe Prelude.Text)
describeInboundConnections_nextToken = Lens.lens (\DescribeInboundConnections' {nextToken} -> nextToken) (\s@DescribeInboundConnections' {} a -> s {nextToken = a} :: DescribeInboundConnections)

instance Core.AWSRequest DescribeInboundConnections where
  type
    AWSResponse DescribeInboundConnections =
      DescribeInboundConnectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInboundConnectionsResponse'
            Prelude.<$> (x Data..?> "Connections" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeInboundConnections where
  hashWithSalt _salt DescribeInboundConnections' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeInboundConnections where
  rnf DescribeInboundConnections' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders DescribeInboundConnections where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DescribeInboundConnections where
  toJSON DescribeInboundConnections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeInboundConnections where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/cc/inboundConnection/search"

instance Data.ToQuery DescribeInboundConnections where
  toQuery = Prelude.const Prelude.mempty

-- | Contains a list of connections matching the filter criteria.
--
-- /See:/ 'newDescribeInboundConnectionsResponse' smart constructor.
data DescribeInboundConnectionsResponse = DescribeInboundConnectionsResponse'
  { -- | List of inbound connections.
    connections :: Prelude.Maybe [InboundConnection],
    -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInboundConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connections', 'describeInboundConnectionsResponse_connections' - List of inbound connections.
--
-- 'nextToken', 'describeInboundConnectionsResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'httpStatus', 'describeInboundConnectionsResponse_httpStatus' - The response's http status code.
newDescribeInboundConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInboundConnectionsResponse
newDescribeInboundConnectionsResponse pHttpStatus_ =
  DescribeInboundConnectionsResponse'
    { connections =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of inbound connections.
describeInboundConnectionsResponse_connections :: Lens.Lens' DescribeInboundConnectionsResponse (Prelude.Maybe [InboundConnection])
describeInboundConnectionsResponse_connections = Lens.lens (\DescribeInboundConnectionsResponse' {connections} -> connections) (\s@DescribeInboundConnectionsResponse' {} a -> s {connections = a} :: DescribeInboundConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
describeInboundConnectionsResponse_nextToken :: Lens.Lens' DescribeInboundConnectionsResponse (Prelude.Maybe Prelude.Text)
describeInboundConnectionsResponse_nextToken = Lens.lens (\DescribeInboundConnectionsResponse' {nextToken} -> nextToken) (\s@DescribeInboundConnectionsResponse' {} a -> s {nextToken = a} :: DescribeInboundConnectionsResponse)

-- | The response's http status code.
describeInboundConnectionsResponse_httpStatus :: Lens.Lens' DescribeInboundConnectionsResponse Prelude.Int
describeInboundConnectionsResponse_httpStatus = Lens.lens (\DescribeInboundConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeInboundConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeInboundConnectionsResponse)

instance
  Prelude.NFData
    DescribeInboundConnectionsResponse
  where
  rnf DescribeInboundConnectionsResponse' {..} =
    Prelude.rnf connections `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
