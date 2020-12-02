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
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorSessions
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more Traffic Mirror sessions. By default, all Traffic Mirror sessions are described. Alternatively, you can filter the results.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorSessions
  ( -- * Creating a Request
    describeTrafficMirrorSessions,
    DescribeTrafficMirrorSessions,

    -- * Request Lenses
    dtmsFilters,
    dtmsNextToken,
    dtmsTrafficMirrorSessionIds,
    dtmsDryRun,
    dtmsMaxResults,

    -- * Destructuring the Response
    describeTrafficMirrorSessionsResponse,
    DescribeTrafficMirrorSessionsResponse,

    -- * Response Lenses
    dtmsrsNextToken,
    dtmsrsTrafficMirrorSessions,
    dtmsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTrafficMirrorSessions' smart constructor.
data DescribeTrafficMirrorSessions = DescribeTrafficMirrorSessions'
  { _dtmsFilters ::
      !(Maybe [Filter]),
    _dtmsNextToken :: !(Maybe Text),
    _dtmsTrafficMirrorSessionIds ::
      !(Maybe [Text]),
    _dtmsDryRun :: !(Maybe Bool),
    _dtmsMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrafficMirrorSessions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmsFilters' - One or more filters. The possible values are:     * @description@ : The Traffic Mirror session description.     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.     * @packet-length@ : The assigned number of packets to mirror.      * @session-number@ : The assigned session number.      * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.     * @traffic-mirror-session-id@ : The ID of the Traffic Mirror session.     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.     * @virtual-network-id@ : The virtual network ID of the Traffic Mirror session.
--
-- * 'dtmsNextToken' - The token for the next page of results.
--
-- * 'dtmsTrafficMirrorSessionIds' - The ID of the Traffic Mirror session.
--
-- * 'dtmsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtmsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTrafficMirrorSessions ::
  DescribeTrafficMirrorSessions
describeTrafficMirrorSessions =
  DescribeTrafficMirrorSessions'
    { _dtmsFilters = Nothing,
      _dtmsNextToken = Nothing,
      _dtmsTrafficMirrorSessionIds = Nothing,
      _dtmsDryRun = Nothing,
      _dtmsMaxResults = Nothing
    }

-- | One or more filters. The possible values are:     * @description@ : The Traffic Mirror session description.     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.     * @packet-length@ : The assigned number of packets to mirror.      * @session-number@ : The assigned session number.      * @traffic-mirror-filter-id@ : The ID of the Traffic Mirror filter.     * @traffic-mirror-session-id@ : The ID of the Traffic Mirror session.     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.     * @virtual-network-id@ : The virtual network ID of the Traffic Mirror session.
dtmsFilters :: Lens' DescribeTrafficMirrorSessions [Filter]
dtmsFilters = lens _dtmsFilters (\s a -> s {_dtmsFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtmsNextToken :: Lens' DescribeTrafficMirrorSessions (Maybe Text)
dtmsNextToken = lens _dtmsNextToken (\s a -> s {_dtmsNextToken = a})

-- | The ID of the Traffic Mirror session.
dtmsTrafficMirrorSessionIds :: Lens' DescribeTrafficMirrorSessions [Text]
dtmsTrafficMirrorSessionIds = lens _dtmsTrafficMirrorSessionIds (\s a -> s {_dtmsTrafficMirrorSessionIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtmsDryRun :: Lens' DescribeTrafficMirrorSessions (Maybe Bool)
dtmsDryRun = lens _dtmsDryRun (\s a -> s {_dtmsDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtmsMaxResults :: Lens' DescribeTrafficMirrorSessions (Maybe Natural)
dtmsMaxResults = lens _dtmsMaxResults (\s a -> s {_dtmsMaxResults = a}) . mapping _Nat

instance AWSPager DescribeTrafficMirrorSessions where
  page rq rs
    | stop (rs ^. dtmsrsNextToken) = Nothing
    | stop (rs ^. dtmsrsTrafficMirrorSessions) = Nothing
    | otherwise = Just $ rq & dtmsNextToken .~ rs ^. dtmsrsNextToken

instance AWSRequest DescribeTrafficMirrorSessions where
  type
    Rs DescribeTrafficMirrorSessions =
      DescribeTrafficMirrorSessionsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeTrafficMirrorSessionsResponse'
            <$> (x .@? "nextToken")
            <*> ( x .@? "trafficMirrorSessionSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTrafficMirrorSessions

instance NFData DescribeTrafficMirrorSessions

instance ToHeaders DescribeTrafficMirrorSessions where
  toHeaders = const mempty

instance ToPath DescribeTrafficMirrorSessions where
  toPath = const "/"

instance ToQuery DescribeTrafficMirrorSessions where
  toQuery DescribeTrafficMirrorSessions' {..} =
    mconcat
      [ "Action" =: ("DescribeTrafficMirrorSessions" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dtmsFilters),
        "NextToken" =: _dtmsNextToken,
        toQuery
          ( toQueryList "TrafficMirrorSessionId"
              <$> _dtmsTrafficMirrorSessionIds
          ),
        "DryRun" =: _dtmsDryRun,
        "MaxResults" =: _dtmsMaxResults
      ]

-- | /See:/ 'describeTrafficMirrorSessionsResponse' smart constructor.
data DescribeTrafficMirrorSessionsResponse = DescribeTrafficMirrorSessionsResponse'
  { _dtmsrsNextToken ::
      !(Maybe Text),
    _dtmsrsTrafficMirrorSessions ::
      !( Maybe
           [TrafficMirrorSession]
       ),
    _dtmsrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrafficMirrorSessionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmsrsNextToken' - The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
--
-- * 'dtmsrsTrafficMirrorSessions' - Describes one or more Traffic Mirror sessions. By default, all Traffic Mirror sessions are described. Alternatively, you can filter the results.
--
-- * 'dtmsrsResponseStatus' - -- | The response status code.
describeTrafficMirrorSessionsResponse ::
  -- | 'dtmsrsResponseStatus'
  Int ->
  DescribeTrafficMirrorSessionsResponse
describeTrafficMirrorSessionsResponse pResponseStatus_ =
  DescribeTrafficMirrorSessionsResponse'
    { _dtmsrsNextToken =
        Nothing,
      _dtmsrsTrafficMirrorSessions = Nothing,
      _dtmsrsResponseStatus = pResponseStatus_
    }

-- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
dtmsrsNextToken :: Lens' DescribeTrafficMirrorSessionsResponse (Maybe Text)
dtmsrsNextToken = lens _dtmsrsNextToken (\s a -> s {_dtmsrsNextToken = a})

-- | Describes one or more Traffic Mirror sessions. By default, all Traffic Mirror sessions are described. Alternatively, you can filter the results.
dtmsrsTrafficMirrorSessions :: Lens' DescribeTrafficMirrorSessionsResponse [TrafficMirrorSession]
dtmsrsTrafficMirrorSessions = lens _dtmsrsTrafficMirrorSessions (\s a -> s {_dtmsrsTrafficMirrorSessions = a}) . _Default . _Coerce

-- | -- | The response status code.
dtmsrsResponseStatus :: Lens' DescribeTrafficMirrorSessionsResponse Int
dtmsrsResponseStatus = lens _dtmsrsResponseStatus (\s a -> s {_dtmsrsResponseStatus = a})

instance NFData DescribeTrafficMirrorSessionsResponse
