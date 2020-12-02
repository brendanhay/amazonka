{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
  ( -- * Creating a Request
    describeInboundCrossClusterSearchConnections,
    DescribeInboundCrossClusterSearchConnections,

    -- * Request Lenses
    diccscFilters,
    diccscNextToken,
    diccscMaxResults,

    -- * Destructuring the Response
    describeInboundCrossClusterSearchConnectionsResponse,
    DescribeInboundCrossClusterSearchConnectionsResponse,

    -- * Response Lenses
    diccscrsCrossClusterSearchConnections,
    diccscrsNextToken,
    diccscrsResponseStatus,
  )
where

import Network.AWS.ElasticSearch.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Container for the parameters to the @'DescribeInboundCrossClusterSearchConnections' @ operation.
--
--
--
-- /See:/ 'describeInboundCrossClusterSearchConnections' smart constructor.
data DescribeInboundCrossClusterSearchConnections = DescribeInboundCrossClusterSearchConnections'
  { _diccscFilters ::
      !( Maybe
           [Filter]
       ),
    _diccscNextToken ::
      !( Maybe
           Text
       ),
    _diccscMaxResults ::
      !( Maybe
           Int
       )
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeInboundCrossClusterSearchConnections' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diccscFilters' - A list of filters used to match properties for inbound cross-cluster search connection. Available @'Filter' @ names for this operation are:     * cross-cluster-search-connection-id    * source-domain-info.domain-name    * source-domain-info.owner-id    * source-domain-info.region    * destination-domain-info.domain-name
--
-- * 'diccscNextToken' - NextToken is sent in case the earlier API call results contain the NextToken. It is used for pagination.
--
-- * 'diccscMaxResults' - Set this value to limit the number of results returned. If not specified, defaults to 100.
describeInboundCrossClusterSearchConnections ::
  DescribeInboundCrossClusterSearchConnections
describeInboundCrossClusterSearchConnections =
  DescribeInboundCrossClusterSearchConnections'
    { _diccscFilters =
        Nothing,
      _diccscNextToken = Nothing,
      _diccscMaxResults = Nothing
    }

-- | A list of filters used to match properties for inbound cross-cluster search connection. Available @'Filter' @ names for this operation are:     * cross-cluster-search-connection-id    * source-domain-info.domain-name    * source-domain-info.owner-id    * source-domain-info.region    * destination-domain-info.domain-name
diccscFilters :: Lens' DescribeInboundCrossClusterSearchConnections [Filter]
diccscFilters = lens _diccscFilters (\s a -> s {_diccscFilters = a}) . _Default . _Coerce

-- | NextToken is sent in case the earlier API call results contain the NextToken. It is used for pagination.
diccscNextToken :: Lens' DescribeInboundCrossClusterSearchConnections (Maybe Text)
diccscNextToken = lens _diccscNextToken (\s a -> s {_diccscNextToken = a})

-- | Set this value to limit the number of results returned. If not specified, defaults to 100.
diccscMaxResults :: Lens' DescribeInboundCrossClusterSearchConnections (Maybe Int)
diccscMaxResults = lens _diccscMaxResults (\s a -> s {_diccscMaxResults = a})

instance AWSRequest DescribeInboundCrossClusterSearchConnections where
  type
    Rs DescribeInboundCrossClusterSearchConnections =
      DescribeInboundCrossClusterSearchConnectionsResponse
  request = postJSON elasticSearch
  response =
    receiveJSON
      ( \s h x ->
          DescribeInboundCrossClusterSearchConnectionsResponse'
            <$> (x .?> "CrossClusterSearchConnections" .!@ mempty)
            <*> (x .?> "NextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeInboundCrossClusterSearchConnections

instance NFData DescribeInboundCrossClusterSearchConnections

instance ToHeaders DescribeInboundCrossClusterSearchConnections where
  toHeaders = const mempty

instance ToJSON DescribeInboundCrossClusterSearchConnections where
  toJSON DescribeInboundCrossClusterSearchConnections' {..} =
    object
      ( catMaybes
          [ ("Filters" .=) <$> _diccscFilters,
            ("NextToken" .=) <$> _diccscNextToken,
            ("MaxResults" .=) <$> _diccscMaxResults
          ]
      )

instance ToPath DescribeInboundCrossClusterSearchConnections where
  toPath = const "/2015-01-01/es/ccs/inboundConnection/search"

instance ToQuery DescribeInboundCrossClusterSearchConnections where
  toQuery = const mempty

-- | The result of a @'DescribeInboundCrossClusterSearchConnections' @ request. Contains the list of connections matching the filter criteria.
--
--
--
-- /See:/ 'describeInboundCrossClusterSearchConnectionsResponse' smart constructor.
data DescribeInboundCrossClusterSearchConnectionsResponse = DescribeInboundCrossClusterSearchConnectionsResponse'
  { _diccscrsCrossClusterSearchConnections ::
      !( Maybe
           [InboundCrossClusterSearchConnection]
       ),
    _diccscrsNextToken ::
      !( Maybe
           Text
       ),
    _diccscrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DescribeInboundCrossClusterSearchConnectionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'diccscrsCrossClusterSearchConnections' - Consists of list of @'InboundCrossClusterSearchConnection' @ matching the specified filter criteria.
--
-- * 'diccscrsNextToken' - If more results are available and NextToken is present, make the next request to the same API with the received NextToken to paginate the remaining results.
--
-- * 'diccscrsResponseStatus' - -- | The response status code.
describeInboundCrossClusterSearchConnectionsResponse ::
  -- | 'diccscrsResponseStatus'
  Int ->
  DescribeInboundCrossClusterSearchConnectionsResponse
describeInboundCrossClusterSearchConnectionsResponse
  pResponseStatus_ =
    DescribeInboundCrossClusterSearchConnectionsResponse'
      { _diccscrsCrossClusterSearchConnections =
          Nothing,
        _diccscrsNextToken = Nothing,
        _diccscrsResponseStatus =
          pResponseStatus_
      }

-- | Consists of list of @'InboundCrossClusterSearchConnection' @ matching the specified filter criteria.
diccscrsCrossClusterSearchConnections :: Lens' DescribeInboundCrossClusterSearchConnectionsResponse [InboundCrossClusterSearchConnection]
diccscrsCrossClusterSearchConnections = lens _diccscrsCrossClusterSearchConnections (\s a -> s {_diccscrsCrossClusterSearchConnections = a}) . _Default . _Coerce

-- | If more results are available and NextToken is present, make the next request to the same API with the received NextToken to paginate the remaining results.
diccscrsNextToken :: Lens' DescribeInboundCrossClusterSearchConnectionsResponse (Maybe Text)
diccscrsNextToken = lens _diccscrsNextToken (\s a -> s {_diccscrsNextToken = a})

-- | -- | The response status code.
diccscrsResponseStatus :: Lens' DescribeInboundCrossClusterSearchConnectionsResponse Int
diccscrsResponseStatus = lens _diccscrsResponseStatus (\s a -> s {_diccscrsResponseStatus = a})

instance
  NFData
    DescribeInboundCrossClusterSearchConnectionsResponse
