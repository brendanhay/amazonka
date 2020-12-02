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
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorFilters
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Traffic Mirror filters.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorFilters
  ( -- * Creating a Request
    describeTrafficMirrorFilters,
    DescribeTrafficMirrorFilters,

    -- * Request Lenses
    dtmfTrafficMirrorFilterIds,
    dtmfFilters,
    dtmfNextToken,
    dtmfDryRun,
    dtmfMaxResults,

    -- * Destructuring the Response
    describeTrafficMirrorFiltersResponse,
    DescribeTrafficMirrorFiltersResponse,

    -- * Response Lenses
    dtmfsrsTrafficMirrorFilters,
    dtmfsrsNextToken,
    dtmfsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTrafficMirrorFilters' smart constructor.
data DescribeTrafficMirrorFilters = DescribeTrafficMirrorFilters'
  { _dtmfTrafficMirrorFilterIds ::
      !(Maybe [Text]),
    _dtmfFilters :: !(Maybe [Filter]),
    _dtmfNextToken :: !(Maybe Text),
    _dtmfDryRun :: !(Maybe Bool),
    _dtmfMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrafficMirrorFilters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmfTrafficMirrorFilterIds' - The ID of the Traffic Mirror filter.
--
-- * 'dtmfFilters' - One or more filters. The possible values are:     * @description@ : The Traffic Mirror filter description.     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
--
-- * 'dtmfNextToken' - The token for the next page of results.
--
-- * 'dtmfDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtmfMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTrafficMirrorFilters ::
  DescribeTrafficMirrorFilters
describeTrafficMirrorFilters =
  DescribeTrafficMirrorFilters'
    { _dtmfTrafficMirrorFilterIds =
        Nothing,
      _dtmfFilters = Nothing,
      _dtmfNextToken = Nothing,
      _dtmfDryRun = Nothing,
      _dtmfMaxResults = Nothing
    }

-- | The ID of the Traffic Mirror filter.
dtmfTrafficMirrorFilterIds :: Lens' DescribeTrafficMirrorFilters [Text]
dtmfTrafficMirrorFilterIds = lens _dtmfTrafficMirrorFilterIds (\s a -> s {_dtmfTrafficMirrorFilterIds = a}) . _Default . _Coerce

-- | One or more filters. The possible values are:     * @description@ : The Traffic Mirror filter description.     * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.
dtmfFilters :: Lens' DescribeTrafficMirrorFilters [Filter]
dtmfFilters = lens _dtmfFilters (\s a -> s {_dtmfFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtmfNextToken :: Lens' DescribeTrafficMirrorFilters (Maybe Text)
dtmfNextToken = lens _dtmfNextToken (\s a -> s {_dtmfNextToken = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtmfDryRun :: Lens' DescribeTrafficMirrorFilters (Maybe Bool)
dtmfDryRun = lens _dtmfDryRun (\s a -> s {_dtmfDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtmfMaxResults :: Lens' DescribeTrafficMirrorFilters (Maybe Natural)
dtmfMaxResults = lens _dtmfMaxResults (\s a -> s {_dtmfMaxResults = a}) . mapping _Nat

instance AWSPager DescribeTrafficMirrorFilters where
  page rq rs
    | stop (rs ^. dtmfsrsNextToken) = Nothing
    | stop (rs ^. dtmfsrsTrafficMirrorFilters) = Nothing
    | otherwise = Just $ rq & dtmfNextToken .~ rs ^. dtmfsrsNextToken

instance AWSRequest DescribeTrafficMirrorFilters where
  type
    Rs DescribeTrafficMirrorFilters =
      DescribeTrafficMirrorFiltersResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeTrafficMirrorFiltersResponse'
            <$> ( x .@? "trafficMirrorFilterSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTrafficMirrorFilters

instance NFData DescribeTrafficMirrorFilters

instance ToHeaders DescribeTrafficMirrorFilters where
  toHeaders = const mempty

instance ToPath DescribeTrafficMirrorFilters where
  toPath = const "/"

instance ToQuery DescribeTrafficMirrorFilters where
  toQuery DescribeTrafficMirrorFilters' {..} =
    mconcat
      [ "Action" =: ("DescribeTrafficMirrorFilters" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery
          ( toQueryList "TrafficMirrorFilterId"
              <$> _dtmfTrafficMirrorFilterIds
          ),
        toQuery (toQueryList "Filter" <$> _dtmfFilters),
        "NextToken" =: _dtmfNextToken,
        "DryRun" =: _dtmfDryRun,
        "MaxResults" =: _dtmfMaxResults
      ]

-- | /See:/ 'describeTrafficMirrorFiltersResponse' smart constructor.
data DescribeTrafficMirrorFiltersResponse = DescribeTrafficMirrorFiltersResponse'
  { _dtmfsrsTrafficMirrorFilters ::
      !( Maybe
           [TrafficMirrorFilter]
       ),
    _dtmfsrsNextToken ::
      !(Maybe Text),
    _dtmfsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrafficMirrorFiltersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmfsrsTrafficMirrorFilters' - Information about one or more Traffic Mirror filters.
--
-- * 'dtmfsrsNextToken' - The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
--
-- * 'dtmfsrsResponseStatus' - -- | The response status code.
describeTrafficMirrorFiltersResponse ::
  -- | 'dtmfsrsResponseStatus'
  Int ->
  DescribeTrafficMirrorFiltersResponse
describeTrafficMirrorFiltersResponse pResponseStatus_ =
  DescribeTrafficMirrorFiltersResponse'
    { _dtmfsrsTrafficMirrorFilters =
        Nothing,
      _dtmfsrsNextToken = Nothing,
      _dtmfsrsResponseStatus = pResponseStatus_
    }

-- | Information about one or more Traffic Mirror filters.
dtmfsrsTrafficMirrorFilters :: Lens' DescribeTrafficMirrorFiltersResponse [TrafficMirrorFilter]
dtmfsrsTrafficMirrorFilters = lens _dtmfsrsTrafficMirrorFilters (\s a -> s {_dtmfsrsTrafficMirrorFilters = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
dtmfsrsNextToken :: Lens' DescribeTrafficMirrorFiltersResponse (Maybe Text)
dtmfsrsNextToken = lens _dtmfsrsNextToken (\s a -> s {_dtmfsrsNextToken = a})

-- | -- | The response status code.
dtmfsrsResponseStatus :: Lens' DescribeTrafficMirrorFiltersResponse Int
dtmfsrsResponseStatus = lens _dtmfsrsResponseStatus (\s a -> s {_dtmfsrsResponseStatus = a})

instance NFData DescribeTrafficMirrorFiltersResponse
