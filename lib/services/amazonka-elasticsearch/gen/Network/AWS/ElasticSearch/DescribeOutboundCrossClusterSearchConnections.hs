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
-- Module      : Network.AWS.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the outbound cross-cluster search connections for a source
-- domain.
module Network.AWS.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
  ( -- * Creating a Request
    DescribeOutboundCrossClusterSearchConnections (..),
    newDescribeOutboundCrossClusterSearchConnections,

    -- * Request Lenses
    describeOutboundCrossClusterSearchConnections_filters,
    describeOutboundCrossClusterSearchConnections_nextToken,
    describeOutboundCrossClusterSearchConnections_maxResults,

    -- * Destructuring the Response
    DescribeOutboundCrossClusterSearchConnectionsResponse (..),
    newDescribeOutboundCrossClusterSearchConnectionsResponse,

    -- * Response Lenses
    describeOutboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections,
    describeOutboundCrossClusterSearchConnectionsResponse_nextToken,
    describeOutboundCrossClusterSearchConnectionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Container for the parameters to the
-- @DescribeOutboundCrossClusterSearchConnections@ operation.
--
-- /See:/ 'newDescribeOutboundCrossClusterSearchConnections' smart constructor.
data DescribeOutboundCrossClusterSearchConnections = DescribeOutboundCrossClusterSearchConnections'
  { -- | A list of filters used to match properties for outbound cross-cluster
    -- search connection. Available @Filter@ names for this operation are:
    --
    -- -   cross-cluster-search-connection-id
    -- -   destination-domain-info.domain-name
    -- -   destination-domain-info.owner-id
    -- -   destination-domain-info.region
    -- -   source-domain-info.domain-name
    filters :: Prelude.Maybe [Filter],
    -- | NextToken is sent in case the earlier API call results contain the
    -- NextToken. It is used for pagination.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | Set this value to limit the number of results returned. If not
    -- specified, defaults to 100.
    maxResults :: Prelude.Maybe Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOutboundCrossClusterSearchConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'filters', 'describeOutboundCrossClusterSearchConnections_filters' - A list of filters used to match properties for outbound cross-cluster
-- search connection. Available @Filter@ names for this operation are:
--
-- -   cross-cluster-search-connection-id
-- -   destination-domain-info.domain-name
-- -   destination-domain-info.owner-id
-- -   destination-domain-info.region
-- -   source-domain-info.domain-name
--
-- 'nextToken', 'describeOutboundCrossClusterSearchConnections_nextToken' - NextToken is sent in case the earlier API call results contain the
-- NextToken. It is used for pagination.
--
-- 'maxResults', 'describeOutboundCrossClusterSearchConnections_maxResults' - Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
newDescribeOutboundCrossClusterSearchConnections ::
  DescribeOutboundCrossClusterSearchConnections
newDescribeOutboundCrossClusterSearchConnections =
  DescribeOutboundCrossClusterSearchConnections'
    { filters =
        Prelude.Nothing,
      nextToken = Prelude.Nothing,
      maxResults = Prelude.Nothing
    }

-- | A list of filters used to match properties for outbound cross-cluster
-- search connection. Available @Filter@ names for this operation are:
--
-- -   cross-cluster-search-connection-id
-- -   destination-domain-info.domain-name
-- -   destination-domain-info.owner-id
-- -   destination-domain-info.region
-- -   source-domain-info.domain-name
describeOutboundCrossClusterSearchConnections_filters :: Lens.Lens' DescribeOutboundCrossClusterSearchConnections (Prelude.Maybe [Filter])
describeOutboundCrossClusterSearchConnections_filters = Lens.lens (\DescribeOutboundCrossClusterSearchConnections' {filters} -> filters) (\s@DescribeOutboundCrossClusterSearchConnections' {} a -> s {filters = a} :: DescribeOutboundCrossClusterSearchConnections) Prelude.. Lens.mapping Lens.coerced

-- | NextToken is sent in case the earlier API call results contain the
-- NextToken. It is used for pagination.
describeOutboundCrossClusterSearchConnections_nextToken :: Lens.Lens' DescribeOutboundCrossClusterSearchConnections (Prelude.Maybe Prelude.Text)
describeOutboundCrossClusterSearchConnections_nextToken = Lens.lens (\DescribeOutboundCrossClusterSearchConnections' {nextToken} -> nextToken) (\s@DescribeOutboundCrossClusterSearchConnections' {} a -> s {nextToken = a} :: DescribeOutboundCrossClusterSearchConnections)

-- | Set this value to limit the number of results returned. If not
-- specified, defaults to 100.
describeOutboundCrossClusterSearchConnections_maxResults :: Lens.Lens' DescribeOutboundCrossClusterSearchConnections (Prelude.Maybe Prelude.Int)
describeOutboundCrossClusterSearchConnections_maxResults = Lens.lens (\DescribeOutboundCrossClusterSearchConnections' {maxResults} -> maxResults) (\s@DescribeOutboundCrossClusterSearchConnections' {} a -> s {maxResults = a} :: DescribeOutboundCrossClusterSearchConnections)

instance
  Core.AWSRequest
    DescribeOutboundCrossClusterSearchConnections
  where
  type
    AWSResponse
      DescribeOutboundCrossClusterSearchConnections =
      DescribeOutboundCrossClusterSearchConnectionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeOutboundCrossClusterSearchConnectionsResponse'
            Prelude.<$> ( x Core..?> "CrossClusterSearchConnections"
                            Core..!@ Prelude.mempty
                        )
              Prelude.<*> (x Core..?> "NextToken")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeOutboundCrossClusterSearchConnections

instance
  Prelude.NFData
    DescribeOutboundCrossClusterSearchConnections

instance
  Core.ToHeaders
    DescribeOutboundCrossClusterSearchConnections
  where
  toHeaders = Prelude.const Prelude.mempty

instance
  Core.ToJSON
    DescribeOutboundCrossClusterSearchConnections
  where
  toJSON
    DescribeOutboundCrossClusterSearchConnections' {..} =
      Core.object
        ( Prelude.catMaybes
            [ ("Filters" Core..=) Prelude.<$> filters,
              ("NextToken" Core..=) Prelude.<$> nextToken,
              ("MaxResults" Core..=) Prelude.<$> maxResults
            ]
        )

instance
  Core.ToPath
    DescribeOutboundCrossClusterSearchConnections
  where
  toPath =
    Prelude.const
      "/2015-01-01/es/ccs/outboundConnection/search"

instance
  Core.ToQuery
    DescribeOutboundCrossClusterSearchConnections
  where
  toQuery = Prelude.const Prelude.mempty

-- | The result of a @DescribeOutboundCrossClusterSearchConnections@ request.
-- Contains the list of connections matching the filter criteria.
--
-- /See:/ 'newDescribeOutboundCrossClusterSearchConnectionsResponse' smart constructor.
data DescribeOutboundCrossClusterSearchConnectionsResponse = DescribeOutboundCrossClusterSearchConnectionsResponse'
  { -- | Consists of list of @OutboundCrossClusterSearchConnection@ matching the
    -- specified filter criteria.
    crossClusterSearchConnections :: Prelude.Maybe [OutboundCrossClusterSearchConnection],
    -- | If more results are available and NextToken is present, make the next
    -- request to the same API with the received NextToken to paginate the
    -- remaining results.
    nextToken :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeOutboundCrossClusterSearchConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'crossClusterSearchConnections', 'describeOutboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections' - Consists of list of @OutboundCrossClusterSearchConnection@ matching the
-- specified filter criteria.
--
-- 'nextToken', 'describeOutboundCrossClusterSearchConnectionsResponse_nextToken' - If more results are available and NextToken is present, make the next
-- request to the same API with the received NextToken to paginate the
-- remaining results.
--
-- 'httpStatus', 'describeOutboundCrossClusterSearchConnectionsResponse_httpStatus' - The response's http status code.
newDescribeOutboundCrossClusterSearchConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DescribeOutboundCrossClusterSearchConnectionsResponse
newDescribeOutboundCrossClusterSearchConnectionsResponse
  pHttpStatus_ =
    DescribeOutboundCrossClusterSearchConnectionsResponse'
      { crossClusterSearchConnections =
          Prelude.Nothing,
        nextToken =
          Prelude.Nothing,
        httpStatus =
          pHttpStatus_
      }

-- | Consists of list of @OutboundCrossClusterSearchConnection@ matching the
-- specified filter criteria.
describeOutboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections :: Lens.Lens' DescribeOutboundCrossClusterSearchConnectionsResponse (Prelude.Maybe [OutboundCrossClusterSearchConnection])
describeOutboundCrossClusterSearchConnectionsResponse_crossClusterSearchConnections = Lens.lens (\DescribeOutboundCrossClusterSearchConnectionsResponse' {crossClusterSearchConnections} -> crossClusterSearchConnections) (\s@DescribeOutboundCrossClusterSearchConnectionsResponse' {} a -> s {crossClusterSearchConnections = a} :: DescribeOutboundCrossClusterSearchConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | If more results are available and NextToken is present, make the next
-- request to the same API with the received NextToken to paginate the
-- remaining results.
describeOutboundCrossClusterSearchConnectionsResponse_nextToken :: Lens.Lens' DescribeOutboundCrossClusterSearchConnectionsResponse (Prelude.Maybe Prelude.Text)
describeOutboundCrossClusterSearchConnectionsResponse_nextToken = Lens.lens (\DescribeOutboundCrossClusterSearchConnectionsResponse' {nextToken} -> nextToken) (\s@DescribeOutboundCrossClusterSearchConnectionsResponse' {} a -> s {nextToken = a} :: DescribeOutboundCrossClusterSearchConnectionsResponse)

-- | The response's http status code.
describeOutboundCrossClusterSearchConnectionsResponse_httpStatus :: Lens.Lens' DescribeOutboundCrossClusterSearchConnectionsResponse Prelude.Int
describeOutboundCrossClusterSearchConnectionsResponse_httpStatus = Lens.lens (\DescribeOutboundCrossClusterSearchConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeOutboundCrossClusterSearchConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeOutboundCrossClusterSearchConnectionsResponse)

instance
  Prelude.NFData
    DescribeOutboundCrossClusterSearchConnectionsResponse
