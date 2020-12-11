{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the outbound cross-cluster search connections for a source domain.
module Network.AWS.ElasticSearch.DescribeOutboundCrossClusterSearchConnections
  ( -- * Creating a request
    DescribeOutboundCrossClusterSearchConnections (..),
    mkDescribeOutboundCrossClusterSearchConnections,

    -- ** Request lenses
    doccscFilters,
    doccscNextToken,
    doccscMaxResults,

    -- * Destructuring the response
    DescribeOutboundCrossClusterSearchConnectionsResponse (..),
    mkDescribeOutboundCrossClusterSearchConnectionsResponse,

    -- ** Response lenses
    drsCrossClusterSearchConnections,
    drsNextToken,
    drsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeOutboundCrossClusterSearchConnections' @ operation.
--
-- /See:/ 'mkDescribeOutboundCrossClusterSearchConnections' smart constructor.
data DescribeOutboundCrossClusterSearchConnections = DescribeOutboundCrossClusterSearchConnections'
  { filters ::
      Lude.Maybe
        [Filter],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    maxResults ::
      Lude.Maybe
        Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeOutboundCrossClusterSearchConnections' with the minimum fields required to make a request.
--
-- * 'filters' - A list of filters used to match properties for outbound cross-cluster search connection. Available @'Filter' @ names for this operation are:
--
--     * cross-cluster-search-connection-id
--
--     * destination-domain-info.domain-name
--
--     * destination-domain-info.owner-id
--
--     * destination-domain-info.region
--
--     * source-domain-info.domain-name
--
--
-- * 'maxResults' - Set this value to limit the number of results returned. If not specified, defaults to 100.
-- * 'nextToken' - NextToken is sent in case the earlier API call results contain the NextToken. It is used for pagination.
mkDescribeOutboundCrossClusterSearchConnections ::
  DescribeOutboundCrossClusterSearchConnections
mkDescribeOutboundCrossClusterSearchConnections =
  DescribeOutboundCrossClusterSearchConnections'
    { filters =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A list of filters used to match properties for outbound cross-cluster search connection. Available @'Filter' @ names for this operation are:
--
--     * cross-cluster-search-connection-id
--
--     * destination-domain-info.domain-name
--
--     * destination-domain-info.owner-id
--
--     * destination-domain-info.region
--
--     * source-domain-info.domain-name
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doccscFilters :: Lens.Lens' DescribeOutboundCrossClusterSearchConnections (Lude.Maybe [Filter])
doccscFilters = Lens.lens (filters :: DescribeOutboundCrossClusterSearchConnections -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeOutboundCrossClusterSearchConnections)
{-# DEPRECATED doccscFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | NextToken is sent in case the earlier API call results contain the NextToken. It is used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doccscNextToken :: Lens.Lens' DescribeOutboundCrossClusterSearchConnections (Lude.Maybe Lude.Text)
doccscNextToken = Lens.lens (nextToken :: DescribeOutboundCrossClusterSearchConnections -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOutboundCrossClusterSearchConnections)
{-# DEPRECATED doccscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
doccscMaxResults :: Lens.Lens' DescribeOutboundCrossClusterSearchConnections (Lude.Maybe Lude.Int)
doccscMaxResults = Lens.lens (maxResults :: DescribeOutboundCrossClusterSearchConnections -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeOutboundCrossClusterSearchConnections)
{-# DEPRECATED doccscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance
  Lude.AWSRequest
    DescribeOutboundCrossClusterSearchConnections
  where
  type
    Rs DescribeOutboundCrossClusterSearchConnections =
      DescribeOutboundCrossClusterSearchConnectionsResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeOutboundCrossClusterSearchConnectionsResponse'
            Lude.<$> (x Lude..?> "CrossClusterSearchConnections" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DescribeOutboundCrossClusterSearchConnections
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DescribeOutboundCrossClusterSearchConnections where
  toJSON DescribeOutboundCrossClusterSearchConnections' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeOutboundCrossClusterSearchConnections where
  toPath = Lude.const "/2015-01-01/es/ccs/outboundConnection/search"

instance Lude.ToQuery DescribeOutboundCrossClusterSearchConnections where
  toQuery = Lude.const Lude.mempty

-- | The result of a @'DescribeOutboundCrossClusterSearchConnections' @ request. Contains the list of connections matching the filter criteria.
--
-- /See:/ 'mkDescribeOutboundCrossClusterSearchConnectionsResponse' smart constructor.
data DescribeOutboundCrossClusterSearchConnectionsResponse = DescribeOutboundCrossClusterSearchConnectionsResponse'
  { crossClusterSearchConnections ::
      Lude.Maybe
        [OutboundCrossClusterSearchConnection],
    nextToken ::
      Lude.Maybe
        Lude.Text,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass
    ( Lude.Hashable,
      Lude.NFData
    )

-- | Creates a value of 'DescribeOutboundCrossClusterSearchConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'crossClusterSearchConnections' - Consists of list of @'OutboundCrossClusterSearchConnection' @ matching the specified filter criteria.
-- * 'nextToken' - If more results are available and NextToken is present, make the next request to the same API with the received NextToken to paginate the remaining results.
-- * 'responseStatus' - The response status code.
mkDescribeOutboundCrossClusterSearchConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeOutboundCrossClusterSearchConnectionsResponse
mkDescribeOutboundCrossClusterSearchConnectionsResponse
  pResponseStatus_ =
    DescribeOutboundCrossClusterSearchConnectionsResponse'
      { crossClusterSearchConnections =
          Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Consists of list of @'OutboundCrossClusterSearchConnection' @ matching the specified filter criteria.
--
-- /Note:/ Consider using 'crossClusterSearchConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsCrossClusterSearchConnections :: Lens.Lens' DescribeOutboundCrossClusterSearchConnectionsResponse (Lude.Maybe [OutboundCrossClusterSearchConnection])
drsCrossClusterSearchConnections = Lens.lens (crossClusterSearchConnections :: DescribeOutboundCrossClusterSearchConnectionsResponse -> Lude.Maybe [OutboundCrossClusterSearchConnection]) (\s a -> s {crossClusterSearchConnections = a} :: DescribeOutboundCrossClusterSearchConnectionsResponse)
{-# DEPRECATED drsCrossClusterSearchConnections "Use generic-lens or generic-optics with 'crossClusterSearchConnections' instead." #-}

-- | If more results are available and NextToken is present, make the next request to the same API with the received NextToken to paginate the remaining results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsNextToken :: Lens.Lens' DescribeOutboundCrossClusterSearchConnectionsResponse (Lude.Maybe Lude.Text)
drsNextToken = Lens.lens (nextToken :: DescribeOutboundCrossClusterSearchConnectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeOutboundCrossClusterSearchConnectionsResponse)
{-# DEPRECATED drsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
drsResponseStatus :: Lens.Lens' DescribeOutboundCrossClusterSearchConnectionsResponse Lude.Int
drsResponseStatus = Lens.lens (responseStatus :: DescribeOutboundCrossClusterSearchConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeOutboundCrossClusterSearchConnectionsResponse)
{-# DEPRECATED drsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
