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
-- Module      : Network.AWS.EC2.DescribeTrafficMirrorTargets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Information about one or more Traffic Mirror targets.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeTrafficMirrorTargets
  ( -- * Creating a Request
    describeTrafficMirrorTargets,
    DescribeTrafficMirrorTargets,

    -- * Request Lenses
    dtmtFilters,
    dtmtNextToken,
    dtmtTrafficMirrorTargetIds,
    dtmtDryRun,
    dtmtMaxResults,

    -- * Destructuring the Response
    describeTrafficMirrorTargetsResponse,
    DescribeTrafficMirrorTargetsResponse,

    -- * Response Lenses
    dtmtrsTrafficMirrorTargets,
    dtmtrsNextToken,
    dtmtrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeTrafficMirrorTargets' smart constructor.
data DescribeTrafficMirrorTargets = DescribeTrafficMirrorTargets'
  { _dtmtFilters ::
      !(Maybe [Filter]),
    _dtmtNextToken :: !(Maybe Text),
    _dtmtTrafficMirrorTargetIds ::
      !(Maybe [Text]),
    _dtmtDryRun :: !(Maybe Bool),
    _dtmtMaxResults :: !(Maybe Nat)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrafficMirrorTargets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmtFilters' - One or more filters. The possible values are:     * @description@ : The Traffic Mirror target description.     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.     * @network-load-balancer-arn@ : The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the session.     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
--
-- * 'dtmtNextToken' - The token for the next page of results.
--
-- * 'dtmtTrafficMirrorTargetIds' - The ID of the Traffic Mirror targets.
--
-- * 'dtmtDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dtmtMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeTrafficMirrorTargets ::
  DescribeTrafficMirrorTargets
describeTrafficMirrorTargets =
  DescribeTrafficMirrorTargets'
    { _dtmtFilters = Nothing,
      _dtmtNextToken = Nothing,
      _dtmtTrafficMirrorTargetIds = Nothing,
      _dtmtDryRun = Nothing,
      _dtmtMaxResults = Nothing
    }

-- | One or more filters. The possible values are:     * @description@ : The Traffic Mirror target description.     * @network-interface-id@ : The ID of the Traffic Mirror session network interface.     * @network-load-balancer-arn@ : The Amazon Resource Name (ARN) of the Network Load Balancer that is associated with the session.     * @owner-id@ : The ID of the account that owns the Traffic Mirror session.     * @traffic-mirror-target-id@ : The ID of the Traffic Mirror target.
dtmtFilters :: Lens' DescribeTrafficMirrorTargets [Filter]
dtmtFilters = lens _dtmtFilters (\s a -> s {_dtmtFilters = a}) . _Default . _Coerce

-- | The token for the next page of results.
dtmtNextToken :: Lens' DescribeTrafficMirrorTargets (Maybe Text)
dtmtNextToken = lens _dtmtNextToken (\s a -> s {_dtmtNextToken = a})

-- | The ID of the Traffic Mirror targets.
dtmtTrafficMirrorTargetIds :: Lens' DescribeTrafficMirrorTargets [Text]
dtmtTrafficMirrorTargetIds = lens _dtmtTrafficMirrorTargetIds (\s a -> s {_dtmtTrafficMirrorTargetIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dtmtDryRun :: Lens' DescribeTrafficMirrorTargets (Maybe Bool)
dtmtDryRun = lens _dtmtDryRun (\s a -> s {_dtmtDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dtmtMaxResults :: Lens' DescribeTrafficMirrorTargets (Maybe Natural)
dtmtMaxResults = lens _dtmtMaxResults (\s a -> s {_dtmtMaxResults = a}) . mapping _Nat

instance AWSPager DescribeTrafficMirrorTargets where
  page rq rs
    | stop (rs ^. dtmtrsNextToken) = Nothing
    | stop (rs ^. dtmtrsTrafficMirrorTargets) = Nothing
    | otherwise = Just $ rq & dtmtNextToken .~ rs ^. dtmtrsNextToken

instance AWSRequest DescribeTrafficMirrorTargets where
  type
    Rs DescribeTrafficMirrorTargets =
      DescribeTrafficMirrorTargetsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeTrafficMirrorTargetsResponse'
            <$> ( x .@? "trafficMirrorTargetSet" .!@ mempty
                    >>= may (parseXMLList "item")
                )
            <*> (x .@? "nextToken")
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeTrafficMirrorTargets

instance NFData DescribeTrafficMirrorTargets

instance ToHeaders DescribeTrafficMirrorTargets where
  toHeaders = const mempty

instance ToPath DescribeTrafficMirrorTargets where
  toPath = const "/"

instance ToQuery DescribeTrafficMirrorTargets where
  toQuery DescribeTrafficMirrorTargets' {..} =
    mconcat
      [ "Action" =: ("DescribeTrafficMirrorTargets" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dtmtFilters),
        "NextToken" =: _dtmtNextToken,
        toQuery
          ( toQueryList "TrafficMirrorTargetId"
              <$> _dtmtTrafficMirrorTargetIds
          ),
        "DryRun" =: _dtmtDryRun,
        "MaxResults" =: _dtmtMaxResults
      ]

-- | /See:/ 'describeTrafficMirrorTargetsResponse' smart constructor.
data DescribeTrafficMirrorTargetsResponse = DescribeTrafficMirrorTargetsResponse'
  { _dtmtrsTrafficMirrorTargets ::
      !( Maybe
           [TrafficMirrorTarget]
       ),
    _dtmtrsNextToken ::
      !(Maybe Text),
    _dtmtrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeTrafficMirrorTargetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dtmtrsTrafficMirrorTargets' - Information about one or more Traffic Mirror targets.
--
-- * 'dtmtrsNextToken' - The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
--
-- * 'dtmtrsResponseStatus' - -- | The response status code.
describeTrafficMirrorTargetsResponse ::
  -- | 'dtmtrsResponseStatus'
  Int ->
  DescribeTrafficMirrorTargetsResponse
describeTrafficMirrorTargetsResponse pResponseStatus_ =
  DescribeTrafficMirrorTargetsResponse'
    { _dtmtrsTrafficMirrorTargets =
        Nothing,
      _dtmtrsNextToken = Nothing,
      _dtmtrsResponseStatus = pResponseStatus_
    }

-- | Information about one or more Traffic Mirror targets.
dtmtrsTrafficMirrorTargets :: Lens' DescribeTrafficMirrorTargetsResponse [TrafficMirrorTarget]
dtmtrsTrafficMirrorTargets = lens _dtmtrsTrafficMirrorTargets (\s a -> s {_dtmtrsTrafficMirrorTargets = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. The value is @null@ when there are no more results to return.
dtmtrsNextToken :: Lens' DescribeTrafficMirrorTargetsResponse (Maybe Text)
dtmtrsNextToken = lens _dtmtrsNextToken (\s a -> s {_dtmtrsNextToken = a})

-- | -- | The response status code.
dtmtrsResponseStatus :: Lens' DescribeTrafficMirrorTargetsResponse Int
dtmtrsResponseStatus = lens _dtmtrsResponseStatus (\s a -> s {_dtmtrsResponseStatus = a})

instance NFData DescribeTrafficMirrorTargetsResponse
