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
-- Module      : Amazonka.ElasticSearch.DescribeInboundCrossClusterSearchConnections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the inbound cross-cluster search connections for a destination
-- domain.
module Amazonka.ElasticSearch.DescribeInboundCrossClusterSearchConnections
  ( -- * Creating a Request
    DescribeInboundCrossClusterSearchConnections (..),
    newDescribeInboundCrossClusterSearchConnections,

    -- * Request Lenses
    describeInboundCrossClusterSearchConnections_nextToken,
    describeInboundCrossClusterSearchConnections_filters,
    describeInboundCrossClusterSearchConnections_maxResults,

    -- * Destructuring the Response
    DescribeInboundCrossClusterSearchConnectionsResponse (..),
    newDescribeInboundCrossClusterSearchConnectionsResponse,

    -- * Response Lenses
    describeInboundCrossClusterSearchConnectionsResponse_nextToken,
    describeInboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections,
    describeInboundCrossClusterSearchConnectionsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElasticSearch.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Container for the parameters to the
-- @DescribeInboundCrossClusterSearchConnections@ operation.
--
-- /See:/ 'newDescribeInboundCrossClusterSearchConnections' smart constructor.
data DescribeInboundCrossClusterSearchConnections = DescribeInboundCrossClusterSearchConnections'
  { -- | NextToken is sent in case the earlier API call results contain the
    -- NextToken. It is used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | A list of filters used to match properties for inbound cross-cluster
    -- search connection. Available @Filter@ names for this operation are:
    --
    -- -   cross-cluster-search-connection-id
    -- -   source-domain-info.domain-name
    -- -   source-domain-info.owner-id
    -- -   source-domain-info.region
    -- -   destination-domain-info.domain-name
    filters :: Prelude.Maybe [Filter],
    -- | Set this value to limit the number of results returned. If not
    -- specified, defaults to 100.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInboundCrossClusterSearchConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInboundCrossClusterSearchConnections_nextToken' - NextToken is sent in case the earlier API call results contain the
-- NextToken. It is used for pagination.
--
-- 'filters', 'describeInboundCrossClusterSearchConnections_filters' - A list of filters used to match properties for inbound cross-cluster
-- search connection. Available @Filter@ names for this operation are:
--
-- -   cross-cluster-search-connection-id
-- -   source-domain-info.domain-name
-- -   source-domain-info.owner-id
-- -   source-domain-info.region
-- -   destination-domain-info.domain-name
--
-- 'maxResults', 'describeInboundCrossClusterSearchConnections_maxResults' - Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
newDescribeInboundCrossClusterSearchConnections ::
  DescribeInboundCrossClusterSearchConnections
newDescribeInboundCrossClusterSearchConnections =
  DescribeInboundCrossClusterSearchConnections'
    { nextToken =
        Prelude.Nothing,
      filters = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | NextToken is sent in case the earlier API call results contain the
-- NextToken. It is used for pagination.
describeInboundCrossClusterSearchConnections_nextToken :: Lens.Lens' DescribeInboundCrossClusterSearchConnections (Prelude.Maybe Prelude.Text)
describeInboundCrossClusterSearchConnections_nextToken = Lens.lens (\DescribeInboundCrossClusterSearchConnections' {nextToken} -> nextToken) (\s@DescribeInboundCrossClusterSearchConnections' {} a -> s {nextToken = a} :: DescribeInboundCrossClusterSearchConnections)

-- | A list of filters used to match properties for inbound cross-cluster
-- search connection. Available @Filter@ names for this operation are:
--
-- -   cross-cluster-search-connection-id
-- -   source-domain-info.domain-name
-- -   source-domain-info.owner-id
-- -   source-domain-info.region
-- -   destination-domain-info.domain-name
describeInboundCrossClusterSearchConnections_filters :: Lens.Lens' DescribeInboundCrossClusterSearchConnections (Prelude.Maybe [Filter])
describeInboundCrossClusterSearchConnections_filters = Lens.lens (\DescribeInboundCrossClusterSearchConnections' {filters} -> filters) (\s@DescribeInboundCrossClusterSearchConnections' {} a -> s {filters = a} :: DescribeInboundCrossClusterSearchConnections) Prelude.. Lens.mapping Lens.coerced

-- | Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
describeInboundCrossClusterSearchConnections_maxResults :: Lens.Lens' DescribeInboundCrossClusterSearchConnections (Prelude.Maybe Prelude.Int)
describeInboundCrossClusterSearchConnections_maxResults = Lens.lens (\DescribeInboundCrossClusterSearchConnections' {maxResults} -> maxResults) (\s@DescribeInboundCrossClusterSearchConnections' {} a -> s {maxResults = a} :: DescribeInboundCrossClusterSearchConnections)

instance
  Core.AWSRequest
    DescribeInboundCrossClusterSearchConnections
  where
  type
    AWSResponse
      DescribeInboundCrossClusterSearchConnections =
      DescribeInboundCrossClusterSearchConnectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeInboundCrossClusterSearchConnectionsResponse'
            Prelude.<$> (x Data..?> "NextToken")
              Prelude.<*> ( x Data..?> "CrossClusterSearchConnections"
                              Core..!@ Prelude.mempty
                          )
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeInboundCrossClusterSearchConnections
  where
  hashWithSalt
    _salt
    DescribeInboundCrossClusterSearchConnections' {..} =
      _salt `Prelude.hashWithSalt` nextToken
        `Prelude.hashWithSalt` filters
        `Prelude.hashWithSalt` maxResults

instance
  Prelude.NFData
    DescribeInboundCrossClusterSearchConnections
  where
  rnf DescribeInboundCrossClusterSearchConnections' {..} =
    Prelude.rnf nextToken
      `Prelude.seq` Prelude.rnf filters
      `Prelude.seq` Prelude.rnf maxResults

instance
  Data.ToHeaders
    DescribeInboundCrossClusterSearchConnections
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Data.ToJSON
    DescribeInboundCrossClusterSearchConnections
  where
  toJSON
    DescribeInboundCrossClusterSearchConnections' {..} =
      Data.object
        ( Prelude.catMaybes
            [ ("NextToken" Data..=) Prelude.<$> nextToken,
              ("Filters" Data..=) Prelude.<$> filters,
              ("MaxResults" Data..=) Prelude.<$> maxResults
            ]
        )

instance
  Data.ToPath
    DescribeInboundCrossClusterSearchConnections
  where
  toPath =
    Prelude.const
      "/2015-01-01/es/ccs/inboundConnection/search"

instance
  Data.ToQuery
    DescribeInboundCrossClusterSearchConnections
  where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DescribeInboundCrossClusterSearchConnections@ request.
-- Contains the list of connections matching the filter criteria.
--
-- /See:/ 'newDescribeInboundCrossClusterSearchConnectionsResponse' smart constructor.
data DescribeInboundCrossClusterSearchConnectionsResponse = DescribeInboundCrossClusterSearchConnectionsResponse'
  { -- | If more results are available and NextToken is present, make the next
    -- request to the same API with the received NextToken to paginate the
    -- remaining results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Consists of list of @InboundCrossClusterSearchConnection@ matching the
    -- specified filter criteria.
    crossClusterSearchConnections :: Prelude.Maybe [InboundCrossClusterSearchConnection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeInboundCrossClusterSearchConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'nextToken', 'describeInboundCrossClusterSearchConnectionsResponse_nextToken' - If more results are available and NextToken is present, make the next
-- request to the same API with the received NextToken to paginate the
-- remaining results.
--
-- 'crossClusterSearchConnections', 'describeInboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections' - Consists of list of @InboundCrossClusterSearchConnection@ matching the
-- specified filter criteria.
--
-- 'httpStatus', 'describeInboundCrossClusterSearchConnectionsResponse_httpStatus' - The response's http status code.
newDescribeInboundCrossClusterSearchConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeInboundCrossClusterSearchConnectionsResponse
newDescribeInboundCrossClusterSearchConnectionsResponse
  pHttpStatus_ =
    DescribeInboundCrossClusterSearchConnectionsResponse'
      { nextToken =
          Prelude.Nothing,
        crossClusterSearchConnections =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | If more results are available and NextToken is present, make the next
-- request to the same API with the received NextToken to paginate the
-- remaining results.
describeInboundCrossClusterSearchConnectionsResponse_nextToken :: Lens.Lens' DescribeInboundCrossClusterSearchConnectionsResponse (Prelude.Maybe Prelude.Text)
describeInboundCrossClusterSearchConnectionsResponse_nextToken = Lens.lens (\DescribeInboundCrossClusterSearchConnectionsResponse' {nextToken} -> nextToken) (\s@DescribeInboundCrossClusterSearchConnectionsResponse' {} a -> s {nextToken = a} :: DescribeInboundCrossClusterSearchConnectionsResponse)

-- | Consists of list of @InboundCrossClusterSearchConnection@ matching the
-- specified filter criteria.
describeInboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections :: Lens.Lens' DescribeInboundCrossClusterSearchConnectionsResponse (Prelude.Maybe [InboundCrossClusterSearchConnection])
describeInboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections = Lens.lens (\DescribeInboundCrossClusterSearchConnectionsResponse' {crossClusterSearchConnections} -> crossClusterSearchConnections) (\s@DescribeInboundCrossClusterSearchConnectionsResponse' {} a -> s {crossClusterSearchConnections = a} :: DescribeInboundCrossClusterSearchConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeInboundCrossClusterSearchConnectionsResponse_httpStatus :: Lens.Lens' DescribeInboundCrossClusterSearchConnectionsResponse Prelude.Int
describeInboundCrossClusterSearchConnectionsResponse_httpStatus = Lens.lens (\DescribeInboundCrossClusterSearchConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeInboundCrossClusterSearchConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeInboundCrossClusterSearchConnectionsResponse)

instance
  Prelude.NFData
    DescribeInboundCrossClusterSearchConnectionsResponse
  where
  rnf
    DescribeInboundCrossClusterSearchConnectionsResponse' {..} =
      Prelude.rnf nextToken
        `Prelude.seq` Prelude.rnf crossClusterSearchConnections
        `Prelude.seq` Prelude.rnf httpStatus
