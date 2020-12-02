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
-- Module      : Network.AWS.EC2.DescribeFleets
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the specified EC2 Fleets or all of your EC2 Fleets.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFleets
  ( -- * Creating a Request
    describeFleets,
    DescribeFleets,

    -- * Request Lenses
    dfsFilters,
    dfsNextToken,
    dfsFleetIds,
    dfsDryRun,
    dfsMaxResults,

    -- * Destructuring the Response
    describeFleetsResponse,
    DescribeFleetsResponse,

    -- * Response Lenses
    dfsrsNextToken,
    dfsrsFleets,
    dfsrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFleets' smart constructor.
data DescribeFleets = DescribeFleets'
  { _dfsFilters ::
      !(Maybe [Filter]),
    _dfsNextToken :: !(Maybe Text),
    _dfsFleetIds :: !(Maybe [Text]),
    _dfsDryRun :: !(Maybe Bool),
    _dfsMaxResults :: !(Maybe Int)
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFleets' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsFilters' - The filters.     * @activity-status@ - The progress of the EC2 Fleet ( @error@ | @pending-fulfillment@ | @pending-termination@ | @fulfilled@ ).     * @excess-capacity-termination-policy@ - Indicates whether to terminate running instances if the target capacity is decreased below the current EC2 Fleet size (@true@ | @false@ ).     * @fleet-state@ - The state of the EC2 Fleet (@submitted@ | @active@ | @deleted@ | @failed@ | @deleted-running@ | @deleted-terminating@ | @modifying@ ).     * @replace-unhealthy-instances@ - Indicates whether EC2 Fleet should replace unhealthy instances (@true@ | @false@ ).     * @type@ - The type of request (@instant@ | @request@ | @maintain@ ).
--
-- * 'dfsNextToken' - The token for the next set of results.
--
-- * 'dfsFleetIds' - The ID of the EC2 Fleets.
--
-- * 'dfsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dfsMaxResults' - The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
describeFleets ::
  DescribeFleets
describeFleets =
  DescribeFleets'
    { _dfsFilters = Nothing,
      _dfsNextToken = Nothing,
      _dfsFleetIds = Nothing,
      _dfsDryRun = Nothing,
      _dfsMaxResults = Nothing
    }

-- | The filters.     * @activity-status@ - The progress of the EC2 Fleet ( @error@ | @pending-fulfillment@ | @pending-termination@ | @fulfilled@ ).     * @excess-capacity-termination-policy@ - Indicates whether to terminate running instances if the target capacity is decreased below the current EC2 Fleet size (@true@ | @false@ ).     * @fleet-state@ - The state of the EC2 Fleet (@submitted@ | @active@ | @deleted@ | @failed@ | @deleted-running@ | @deleted-terminating@ | @modifying@ ).     * @replace-unhealthy-instances@ - Indicates whether EC2 Fleet should replace unhealthy instances (@true@ | @false@ ).     * @type@ - The type of request (@instant@ | @request@ | @maintain@ ).
dfsFilters :: Lens' DescribeFleets [Filter]
dfsFilters = lens _dfsFilters (\s a -> s {_dfsFilters = a}) . _Default . _Coerce

-- | The token for the next set of results.
dfsNextToken :: Lens' DescribeFleets (Maybe Text)
dfsNextToken = lens _dfsNextToken (\s a -> s {_dfsNextToken = a})

-- | The ID of the EC2 Fleets.
dfsFleetIds :: Lens' DescribeFleets [Text]
dfsFleetIds = lens _dfsFleetIds (\s a -> s {_dfsFleetIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dfsDryRun :: Lens' DescribeFleets (Maybe Bool)
dfsDryRun = lens _dfsDryRun (\s a -> s {_dfsDryRun = a})

-- | The maximum number of results to return in a single call. Specify a value between 1 and 1000. The default value is 1000. To retrieve the remaining results, make another call with the returned @NextToken@ value.
dfsMaxResults :: Lens' DescribeFleets (Maybe Int)
dfsMaxResults = lens _dfsMaxResults (\s a -> s {_dfsMaxResults = a})

instance AWSPager DescribeFleets where
  page rq rs
    | stop (rs ^. dfsrsNextToken) = Nothing
    | stop (rs ^. dfsrsFleets) = Nothing
    | otherwise = Just $ rq & dfsNextToken .~ rs ^. dfsrsNextToken

instance AWSRequest DescribeFleets where
  type Rs DescribeFleets = DescribeFleetsResponse
  request = postQuery ec2
  response =
    receiveXML
      ( \s h x ->
          DescribeFleetsResponse'
            <$> (x .@? "nextToken")
            <*> (x .@? "fleetSet" .!@ mempty >>= may (parseXMLList "item"))
            <*> (pure (fromEnum s))
      )

instance Hashable DescribeFleets

instance NFData DescribeFleets

instance ToHeaders DescribeFleets where
  toHeaders = const mempty

instance ToPath DescribeFleets where
  toPath = const "/"

instance ToQuery DescribeFleets where
  toQuery DescribeFleets' {..} =
    mconcat
      [ "Action" =: ("DescribeFleets" :: ByteString),
        "Version" =: ("2016-11-15" :: ByteString),
        toQuery (toQueryList "Filter" <$> _dfsFilters),
        "NextToken" =: _dfsNextToken,
        toQuery (toQueryList "FleetId" <$> _dfsFleetIds),
        "DryRun" =: _dfsDryRun,
        "MaxResults" =: _dfsMaxResults
      ]

-- | /See:/ 'describeFleetsResponse' smart constructor.
data DescribeFleetsResponse = DescribeFleetsResponse'
  { _dfsrsNextToken ::
      !(Maybe Text),
    _dfsrsFleets :: !(Maybe [FleetData]),
    _dfsrsResponseStatus :: !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DescribeFleetsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dfsrsNextToken' - The token for the next set of results.
--
-- * 'dfsrsFleets' - Information about the EC2 Fleets.
--
-- * 'dfsrsResponseStatus' - -- | The response status code.
describeFleetsResponse ::
  -- | 'dfsrsResponseStatus'
  Int ->
  DescribeFleetsResponse
describeFleetsResponse pResponseStatus_ =
  DescribeFleetsResponse'
    { _dfsrsNextToken = Nothing,
      _dfsrsFleets = Nothing,
      _dfsrsResponseStatus = pResponseStatus_
    }

-- | The token for the next set of results.
dfsrsNextToken :: Lens' DescribeFleetsResponse (Maybe Text)
dfsrsNextToken = lens _dfsrsNextToken (\s a -> s {_dfsrsNextToken = a})

-- | Information about the EC2 Fleets.
dfsrsFleets :: Lens' DescribeFleetsResponse [FleetData]
dfsrsFleets = lens _dfsrsFleets (\s a -> s {_dfsrsFleets = a}) . _Default . _Coerce

-- | -- | The response status code.
dfsrsResponseStatus :: Lens' DescribeFleetsResponse Int
dfsrsResponseStatus = lens _dfsrsResponseStatus (\s a -> s {_dfsrsResponseStatus = a})

instance NFData DescribeFleetsResponse
