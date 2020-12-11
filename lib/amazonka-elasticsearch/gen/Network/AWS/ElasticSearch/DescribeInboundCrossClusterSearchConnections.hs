{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElasticSearch.DescribeInboundCrossClusterSearchConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all the inbound cross-cluster search connections for a destination domain.
module Network.AWS.ElasticSearch.DescribeInboundCrossClusterSearchConnections
  ( -- * Creating a request
    DescribeInboundCrossClusterSearchConnections (..),
    mkDescribeInboundCrossClusterSearchConnections,

    -- ** Request lenses
    diccscFilters,
    diccscNextToken,
    diccscMaxResults,

    -- * Destructuring the response
    DescribeInboundCrossClusterSearchConnectionsResponse (..),
    mkDescribeInboundCrossClusterSearchConnectionsResponse,

    -- ** Response lenses
    diccscrsCrossClusterSearchConnections,
    diccscrsNextToken,
    diccscrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Container for the parameters to the @'DescribeInboundCrossClusterSearchConnections' @ operation.
--
-- /See:/ 'mkDescribeInboundCrossClusterSearchConnections' smart constructor.
data DescribeInboundCrossClusterSearchConnections = DescribeInboundCrossClusterSearchConnections'
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
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeInboundCrossClusterSearchConnections' with the minimum fields required to make a request.
--
-- * 'filters' - A list of filters used to match properties for inbound cross-cluster search connection. Available @'Filter' @ names for this operation are:
--
--     * cross-cluster-search-connection-id
--
--     * source-domain-info.domain-name
--
--     * source-domain-info.owner-id
--
--     * source-domain-info.region
--
--     * destination-domain-info.domain-name
--
--
-- * 'maxResults' - Set this value to limit the number of results returned. If not specified, defaults to 100.
-- * 'nextToken' - NextToken is sent in case the earlier API call results contain the NextToken. It is used for pagination.
mkDescribeInboundCrossClusterSearchConnections ::
  DescribeInboundCrossClusterSearchConnections
mkDescribeInboundCrossClusterSearchConnections =
  DescribeInboundCrossClusterSearchConnections'
    { filters =
        Lude.Nothing,
      nextToken = Lude.Nothing,
      maxResults = Lude.Nothing
    }

-- | A list of filters used to match properties for inbound cross-cluster search connection. Available @'Filter' @ names for this operation are:
--
--     * cross-cluster-search-connection-id
--
--     * source-domain-info.domain-name
--
--     * source-domain-info.owner-id
--
--     * source-domain-info.region
--
--     * destination-domain-info.domain-name
--
--
--
-- /Note:/ Consider using 'filters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscFilters :: Lens.Lens' DescribeInboundCrossClusterSearchConnections (Lude.Maybe [Filter])
diccscFilters = Lens.lens (filters :: DescribeInboundCrossClusterSearchConnections -> Lude.Maybe [Filter]) (\s a -> s {filters = a} :: DescribeInboundCrossClusterSearchConnections)
{-# DEPRECATED diccscFilters "Use generic-lens or generic-optics with 'filters' instead." #-}

-- | NextToken is sent in case the earlier API call results contain the NextToken. It is used for pagination.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscNextToken :: Lens.Lens' DescribeInboundCrossClusterSearchConnections (Lude.Maybe Lude.Text)
diccscNextToken = Lens.lens (nextToken :: DescribeInboundCrossClusterSearchConnections -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInboundCrossClusterSearchConnections)
{-# DEPRECATED diccscNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
--
-- /Note:/ Consider using 'maxResults' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscMaxResults :: Lens.Lens' DescribeInboundCrossClusterSearchConnections (Lude.Maybe Lude.Int)
diccscMaxResults = Lens.lens (maxResults :: DescribeInboundCrossClusterSearchConnections -> Lude.Maybe Lude.Int) (\s a -> s {maxResults = a} :: DescribeInboundCrossClusterSearchConnections)
{-# DEPRECATED diccscMaxResults "Use generic-lens or generic-optics with 'maxResults' instead." #-}

instance
  Lude.AWSRequest
    DescribeInboundCrossClusterSearchConnections
  where
  type
    Rs DescribeInboundCrossClusterSearchConnections =
      DescribeInboundCrossClusterSearchConnectionsResponse
  request = Req.postJSON elasticSearchService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeInboundCrossClusterSearchConnectionsResponse'
            Lude.<$> (x Lude..?> "CrossClusterSearchConnections" Lude..!@ Lude.mempty)
            Lude.<*> (x Lude..?> "NextToken")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance
  Lude.ToHeaders
    DescribeInboundCrossClusterSearchConnections
  where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToJSON DescribeInboundCrossClusterSearchConnections where
  toJSON DescribeInboundCrossClusterSearchConnections' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("Filters" Lude..=) Lude.<$> filters,
            ("NextToken" Lude..=) Lude.<$> nextToken,
            ("MaxResults" Lude..=) Lude.<$> maxResults
          ]
      )

instance Lude.ToPath DescribeInboundCrossClusterSearchConnections where
  toPath = Lude.const "/2015-01-01/es/ccs/inboundConnection/search"

instance Lude.ToQuery DescribeInboundCrossClusterSearchConnections where
  toQuery = Lude.const Lude.mempty

-- | The result of a @'DescribeInboundCrossClusterSearchConnections' @ request. Contains the list of connections matching the filter criteria.
--
-- /See:/ 'mkDescribeInboundCrossClusterSearchConnectionsResponse' smart constructor.
data DescribeInboundCrossClusterSearchConnectionsResponse = DescribeInboundCrossClusterSearchConnectionsResponse'
  { crossClusterSearchConnections ::
      Lude.Maybe
        [InboundCrossClusterSearchConnection],
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

-- | Creates a value of 'DescribeInboundCrossClusterSearchConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'crossClusterSearchConnections' - Consists of list of @'InboundCrossClusterSearchConnection' @ matching the specified filter criteria.
-- * 'nextToken' - If more results are available and NextToken is present, make the next request to the same API with the received NextToken to paginate the remaining results.
-- * 'responseStatus' - The response status code.
mkDescribeInboundCrossClusterSearchConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeInboundCrossClusterSearchConnectionsResponse
mkDescribeInboundCrossClusterSearchConnectionsResponse
  pResponseStatus_ =
    DescribeInboundCrossClusterSearchConnectionsResponse'
      { crossClusterSearchConnections =
          Lude.Nothing,
        nextToken = Lude.Nothing,
        responseStatus = pResponseStatus_
      }

-- | Consists of list of @'InboundCrossClusterSearchConnection' @ matching the specified filter criteria.
--
-- /Note:/ Consider using 'crossClusterSearchConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscrsCrossClusterSearchConnections :: Lens.Lens' DescribeInboundCrossClusterSearchConnectionsResponse (Lude.Maybe [InboundCrossClusterSearchConnection])
diccscrsCrossClusterSearchConnections = Lens.lens (crossClusterSearchConnections :: DescribeInboundCrossClusterSearchConnectionsResponse -> Lude.Maybe [InboundCrossClusterSearchConnection]) (\s a -> s {crossClusterSearchConnections = a} :: DescribeInboundCrossClusterSearchConnectionsResponse)
{-# DEPRECATED diccscrsCrossClusterSearchConnections "Use generic-lens or generic-optics with 'crossClusterSearchConnections' instead." #-}

-- | If more results are available and NextToken is present, make the next request to the same API with the received NextToken to paginate the remaining results.
--
-- /Note:/ Consider using 'nextToken' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscrsNextToken :: Lens.Lens' DescribeInboundCrossClusterSearchConnectionsResponse (Lude.Maybe Lude.Text)
diccscrsNextToken = Lens.lens (nextToken :: DescribeInboundCrossClusterSearchConnectionsResponse -> Lude.Maybe Lude.Text) (\s a -> s {nextToken = a} :: DescribeInboundCrossClusterSearchConnectionsResponse)
{-# DEPRECATED diccscrsNextToken "Use generic-lens or generic-optics with 'nextToken' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
diccscrsResponseStatus :: Lens.Lens' DescribeInboundCrossClusterSearchConnectionsResponse Lude.Int
diccscrsResponseStatus = Lens.lens (responseStatus :: DescribeInboundCrossClusterSearchConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeInboundCrossClusterSearchConnectionsResponse)
{-# DEPRECATED diccscrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
