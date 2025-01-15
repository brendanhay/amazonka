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
-- Module      : Amazonka.OpenSearch.DescribeOutboundConnections
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the outbound cross-cluster connections for a local (source)
-- Amazon OpenSearch Service domain. For more information, see
-- <https://docs.aws.amazon.com/opensearch-service/latest/developerguide/cross-cluster-search.html Cross-cluster search for Amazon OpenSearch Service>.
module Amazonka.OpenSearch.DescribeOutboundConnections
  ( -- * Creating a Request
    DescribeOutboundConnections (..),
    newDescribeOutboundConnections,

    -- * Request Lenses
    describeOutboundConnections_filters,
    describeOutboundConnections_maxResults,
    describeOutboundConnections_nextToken,

    -- * Destructuring the Response
    DescribeOutboundConnectionsResponse (..),
    newDescribeOutboundConnectionsResponse,

    -- * Response Lenses
    describeOutboundConnectionsResponse_connections,
    describeOutboundConnectionsResponse_nextToken,
    describeOutboundConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.OpenSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the @DescribeOutboundConnections@
-- operation.
--
-- /See:/ 'newDescribeOutboundConnections' smart constructor.
data DescribeOutboundConnections = DescribeOutboundConnections'
  { -- | List of filter names and values that you can use for requests.
    filters :: Prelude.Maybe [Filter],
    -- | An optional parameter that specifies the maximum number of results to
    -- return. You can use @nextToken@ to get the next page of results.
    maxResults :: Prelude.Maybe Prelude.Int,
    -- | If your initial @DescribeOutboundConnections@ operation returns a
    -- @nextToken@, you can include the returned @nextToken@ in subsequent
    -- @DescribeOutboundConnections@ operations, which returns results in the
    -- next page.
    nextToken :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOutboundConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeOutboundConnections_filters' - List of filter names and values that you can use for requests.
--
-- 'maxResults', 'describeOutboundConnections_maxResults' - An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
--
-- 'nextToken', 'describeOutboundConnections_nextToken' - If your initial @DescribeOutboundConnections@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribeOutboundConnections@ operations, which returns results in the
-- next page.
newDescribeOutboundConnections ::
  DescribeOutboundConnections
newDescribeOutboundConnections =
  DescribeOutboundConnections'
    { filters =
        Prelude.Nothing,
      maxResults = Prelude.Nothing,
      nextToken = Prelude.Nothing
    }

-- | List of filter names and values that you can use for requests.
describeOutboundConnections_filters :: Lens.Lens' DescribeOutboundConnections (Prelude.Maybe [Filter])
describeOutboundConnections_filters = Lens.lens (\DescribeOutboundConnections' {filters} -> filters) (\s@DescribeOutboundConnections' {} a -> s {filters = a} :: DescribeOutboundConnections) Prelude.. Lens.mapping Lens.coerced

-- | An optional parameter that specifies the maximum number of results to
-- return. You can use @nextToken@ to get the next page of results.
describeOutboundConnections_maxResults :: Lens.Lens' DescribeOutboundConnections (Prelude.Maybe Prelude.Int)
describeOutboundConnections_maxResults = Lens.lens (\DescribeOutboundConnections' {maxResults} -> maxResults) (\s@DescribeOutboundConnections' {} a -> s {maxResults = a} :: DescribeOutboundConnections)

-- | If your initial @DescribeOutboundConnections@ operation returns a
-- @nextToken@, you can include the returned @nextToken@ in subsequent
-- @DescribeOutboundConnections@ operations, which returns results in the
-- next page.
describeOutboundConnections_nextToken :: Lens.Lens' DescribeOutboundConnections (Prelude.Maybe Prelude.Text)
describeOutboundConnections_nextToken = Lens.lens (\DescribeOutboundConnections' {nextToken} -> nextToken) (\s@DescribeOutboundConnections' {} a -> s {nextToken = a} :: DescribeOutboundConnections)

instance Core.AWSRequest DescribeOutboundConnections where
  type
    AWSResponse DescribeOutboundConnections =
      DescribeOutboundConnectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOutboundConnectionsResponse'
            Prelude.<$> (x Data..?> "Connections" Core..!@ Prelude.mempty)
            Prelude.<*> (x Data..?> "NextToken")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable DescribeOutboundConnections where
  hashWithSalt _salt DescribeOutboundConnections' {..} =
    _salt
      `Prelude.hashWithSalt` filters
      `Prelude.hashWithSalt` maxResults
      `Prelude.hashWithSalt` nextToken

instance Prelude.NFData DescribeOutboundConnections where
  rnf DescribeOutboundConnections' {..} =
    Prelude.rnf filters `Prelude.seq`
      Prelude.rnf maxResults `Prelude.seq`
        Prelude.rnf nextToken

instance Data.ToHeaders DescribeOutboundConnections where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToJSON DescribeOutboundConnections where
  toJSON DescribeOutboundConnections' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Filters" Data..=) Prelude.<$> filters,
            ("MaxResults" Data..=) Prelude.<$> maxResults,
            ("NextToken" Data..=) Prelude.<$> nextToken
          ]
      )

instance Data.ToPath DescribeOutboundConnections where
  toPath =
    Prelude.const
      "/2021-01-01/opensearch/cc/outboundConnection/search"

instance Data.ToQuery DescribeOutboundConnections where
  toQuery = Prelude.const Prelude.mempty

-- | Contains a list of connections matching the filter criteria.
--
-- /See:/ 'newDescribeOutboundConnectionsResponse' smart constructor.
data DescribeOutboundConnectionsResponse = DescribeOutboundConnectionsResponse'
  { -- | List of outbound connections that match the filter criteria.
    connections :: Prelude.Maybe [OutboundConnection],
    -- | When @nextToken@ is returned, there are more results available. The
    -- value of @nextToken@ is a unique pagination token for each page. Make
    -- the call again using the returned token to retrieve the next page.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOutboundConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'connections', 'describeOutboundConnectionsResponse_connections' - List of outbound connections that match the filter criteria.
--
-- 'nextToken', 'describeOutboundConnectionsResponse_nextToken' - When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
--
-- 'httpStatus', 'describeOutboundConnectionsResponse_httpStatus' - The response's http status code.
newDescribeOutboundConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOutboundConnectionsResponse
newDescribeOutboundConnectionsResponse pHttpStatus_ =
  DescribeOutboundConnectionsResponse'
    { connections =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | List of outbound connections that match the filter criteria.
describeOutboundConnectionsResponse_connections :: Lens.Lens' DescribeOutboundConnectionsResponse (Prelude.Maybe [OutboundConnection])
describeOutboundConnectionsResponse_connections = Lens.lens (\DescribeOutboundConnectionsResponse' {connections} -> connections) (\s@DescribeOutboundConnectionsResponse' {} a -> s {connections = a} :: DescribeOutboundConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | When @nextToken@ is returned, there are more results available. The
-- value of @nextToken@ is a unique pagination token for each page. Make
-- the call again using the returned token to retrieve the next page.
describeOutboundConnectionsResponse_nextToken :: Lens.Lens' DescribeOutboundConnectionsResponse (Prelude.Maybe Prelude.Text)
describeOutboundConnectionsResponse_nextToken = Lens.lens (\DescribeOutboundConnectionsResponse' {nextToken} -> nextToken) (\s@DescribeOutboundConnectionsResponse' {} a -> s {nextToken = a} :: DescribeOutboundConnectionsResponse)

-- | The response's http status code.
describeOutboundConnectionsResponse_httpStatus :: Lens.Lens' DescribeOutboundConnectionsResponse Prelude.Int
describeOutboundConnectionsResponse_httpStatus = Lens.lens (\DescribeOutboundConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeOutboundConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeOutboundConnectionsResponse)

instance
  Prelude.NFData
    DescribeOutboundConnectionsResponse
  where
  rnf DescribeOutboundConnectionsResponse' {..} =
    Prelude.rnf connections `Prelude.seq`
      Prelude.rnf nextToken `Prelude.seq`
        Prelude.rnf httpStatus
