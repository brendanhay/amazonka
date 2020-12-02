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
-- Module      : Network.AWS.EC2.SearchLocalGatewayRoutes
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Searches for routes in the specified local gateway route table.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.SearchLocalGatewayRoutes
  ( -- * Creating a Request
    searchLocalGatewayRoutes,
    SearchLocalGatewayRoutes,

    -- * Request Lenses
    slgrNextToken,
    slgrDryRun,
    slgrMaxResults,
    slgrLocalGatewayRouteTableId,
    slgrFilters,

    -- * Destructuring the Response
    searchLocalGatewayRoutesResponse,
    SearchLocalGatewayRoutesResponse,

    -- * Response Lenses
    slgrrsRoutes,
    slgrrsNextToken,
    slgrrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'searchLocalGatewayRoutes' smart constructor.
data SearchLocalGatewayRoutes = SearchLocalGatewayRoutes'
  { _slgrNextToken ::
      !(Maybe Text),
    _slgrDryRun :: !(Maybe Bool),
    _slgrMaxResults :: !(Maybe Int),
    _slgrLocalGatewayRouteTableId :: !Text,
    _slgrFilters :: ![Filter]
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchLocalGatewayRoutes' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slgrNextToken' - The token for the next page of results.
--
-- * 'slgrDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'slgrMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
--
-- * 'slgrLocalGatewayRouteTableId' - The ID of the local gateway route table.
--
-- * 'slgrFilters' - One or more filters.
searchLocalGatewayRoutes ::
  -- | 'slgrLocalGatewayRouteTableId'
  Text ->
  SearchLocalGatewayRoutes
searchLocalGatewayRoutes pLocalGatewayRouteTableId_ =
  SearchLocalGatewayRoutes'
    { _slgrNextToken = Nothing,
      _slgrDryRun = Nothing,
      _slgrMaxResults = Nothing,
      _slgrLocalGatewayRouteTableId = pLocalGatewayRouteTableId_,
      _slgrFilters = mempty
    }

-- | The token for the next page of results.
slgrNextToken :: Lens' SearchLocalGatewayRoutes (Maybe Text)
slgrNextToken = lens _slgrNextToken (\s a -> s {_slgrNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
slgrDryRun :: Lens' SearchLocalGatewayRoutes (Maybe Bool)
slgrDryRun = lens _slgrDryRun (\s a -> s {_slgrDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
slgrMaxResults :: Lens' SearchLocalGatewayRoutes (Maybe Int)
slgrMaxResults = lens _slgrMaxResults (\s a -> s {_slgrMaxResults = a})

-- | The ID of the local gateway route table.
slgrLocalGatewayRouteTableId :: Lens' SearchLocalGatewayRoutes Text
slgrLocalGatewayRouteTableId = lens _slgrLocalGatewayRouteTableId (\s a -> s {_slgrLocalGatewayRouteTableId = a})

-- | One or more filters.
slgrFilters :: Lens' SearchLocalGatewayRoutes [Filter]
slgrFilters = lens _slgrFilters (\s a -> s {_slgrFilters = a}) . _Coerce

instance AWSPager SearchLocalGatewayRoutes where
  page rq rs
    | stop (rs ^. slgrrsNextToken) = Nothing
    | stop (rs ^. slgrrsRoutes) = Nothing
    | otherwise = Just $ rq & slgrNextToken .~ rs ^. slgrrsNextToken

instance AWSRequest SearchLocalGatewayRoutes where
  type Rs SearchLocalGatewayRoutes = SearchLocalGatewayRoutesResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          SearchLocalGatewayRoutesResponse'
            <$> (x .@? "routeSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable SearchLocalGatewayRoutes

instance NFData SearchLocalGatewayRoutes

instance ToHeaders SearchLocalGatewayRoutes where
  toHeaders = const mempty

instance ToPath SearchLocalGatewayRoutes where
  toPath = const "/"

instance ToQuery SearchLocalGatewayRoutes where
  toQuery SearchLocalGatewayRoutes' {..} =
    mconcat
      [ "Action" =: ("SearchLocalGatewayRoutes" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        "NextToken" =: _slgrNextToken,
        "DryRun" =: _slgrDryRun,
        "MaxResults" =: _slgrMaxResults,
        "LocalGatewayRouteTableId" =: _slgrLocalGatewayRouteTableId,
        toQueryList "Filter" _slgrFilters
      ]

-- | /See:/ 'searchLocalGatewayRoutesResponse' smart constructor.
data SearchLocalGatewayRoutesResponse = SearchLocalGatewayRoutesResponse'
  { _slgrrsRoutes ::
      !( Maybe
           [LocalGatewayRoute]
       ),
    _slgrrsNextToken ::
      !(Maybe Text),
    _slgrrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'SearchLocalGatewayRoutesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'slgrrsRoutes' - Information about the routes.
--
-- * 'slgrrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'slgrrsResponseStatus' - -- | The response status code.
searchLocalGatewayRoutesResponse ::
  -- | 'slgrrsResponseStatus'
  Int ->
  SearchLocalGatewayRoutesResponse
searchLocalGatewayRoutesResponse pResponseStatus_ =
  SearchLocalGatewayRoutesResponse'
    { _slgrrsRoutes = Nothing,
      _slgrrsNextToken = Nothing,
      _slgrrsResponseStatus = pResponseStatus_
    }

-- | Information about the routes.
slgrrsRoutes :: Lens' SearchLocalGatewayRoutesResponse [LocalGatewayRoute]
slgrrsRoutes = lens _slgrrsRoutes (\s a -> s {_slgrrsRoutes = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
slgrrsNextToken :: Lens' SearchLocalGatewayRoutesResponse (Maybe Text)
slgrrsNextToken = lens _slgrrsNextToken (\s a -> s {_slgrrsNextToken = a})

-- | -- | The response status code.
slgrrsResponseStatus :: Lens' SearchLocalGatewayRoutesResponse Int
slgrrsResponseStatus = lens _slgrrsResponseStatus (\s a -> s {_slgrrsResponseStatus = a})

instance NFData SearchLocalGatewayRoutesResponse
