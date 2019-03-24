{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFlowLogs
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more flow logs. To view the information in your flow logs (the log streams for the network interfaces), you must use the CloudWatch Logs console or the CloudWatch Logs API.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeFlowLogs
    (
    -- * Creating a Request
      describeFlowLogs
    , DescribeFlowLogs
    -- * Request Lenses
    , dflsNextToken
    , dflsFlowLogIds
    , dflsFilter
    , dflsDryRun
    , dflsMaxResults

    -- * Destructuring the Response
    , describeFlowLogsResponse
    , DescribeFlowLogsResponse
    -- * Response Lenses
    , dflsrsNextToken
    , dflsrsFlowLogs
    , dflsrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'describeFlowLogs' smart constructor.
data DescribeFlowLogs = DescribeFlowLogs'
  { _dflsNextToken  :: !(Maybe Text)
  , _dflsFlowLogIds :: !(Maybe [Text])
  , _dflsFilter     :: !(Maybe [Filter])
  , _dflsDryRun     :: !(Maybe Bool)
  , _dflsMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFlowLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dflsNextToken' - The token for the next page of results.
--
-- * 'dflsFlowLogIds' - One or more flow log IDs.
--
-- * 'dflsFilter' - One or more filters.     * @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ | @FAILED@ ).     * @log-destination-type@ - The type of destination to which the flow log publishes data. Possible destination types include @cloud-watch-logs@ and @S3@ .     * @flow-log-id@ - The ID of the flow log.     * @log-group-name@ - The name of the log group.     * @resource-id@ - The ID of the VPC, subnet, or network interface.     * @traffic-type@ - The type of traffic (@ACCEPT@ | @REJECT@ | @ALL@ ).
--
-- * 'dflsDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'dflsMaxResults' - The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
describeFlowLogs
    :: DescribeFlowLogs
describeFlowLogs =
  DescribeFlowLogs'
    { _dflsNextToken = Nothing
    , _dflsFlowLogIds = Nothing
    , _dflsFilter = Nothing
    , _dflsDryRun = Nothing
    , _dflsMaxResults = Nothing
    }


-- | The token for the next page of results.
dflsNextToken :: Lens' DescribeFlowLogs (Maybe Text)
dflsNextToken = lens _dflsNextToken (\ s a -> s{_dflsNextToken = a})

-- | One or more flow log IDs.
dflsFlowLogIds :: Lens' DescribeFlowLogs [Text]
dflsFlowLogIds = lens _dflsFlowLogIds (\ s a -> s{_dflsFlowLogIds = a}) . _Default . _Coerce

-- | One or more filters.     * @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ | @FAILED@ ).     * @log-destination-type@ - The type of destination to which the flow log publishes data. Possible destination types include @cloud-watch-logs@ and @S3@ .     * @flow-log-id@ - The ID of the flow log.     * @log-group-name@ - The name of the log group.     * @resource-id@ - The ID of the VPC, subnet, or network interface.     * @traffic-type@ - The type of traffic (@ACCEPT@ | @REJECT@ | @ALL@ ).
dflsFilter :: Lens' DescribeFlowLogs [Filter]
dflsFilter = lens _dflsFilter (\ s a -> s{_dflsFilter = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
dflsDryRun :: Lens' DescribeFlowLogs (Maybe Bool)
dflsDryRun = lens _dflsDryRun (\ s a -> s{_dflsDryRun = a})

-- | The maximum number of results to return with a single call. To retrieve the remaining results, make another call with the returned @nextToken@ value.
dflsMaxResults :: Lens' DescribeFlowLogs (Maybe Int)
dflsMaxResults = lens _dflsMaxResults (\ s a -> s{_dflsMaxResults = a})

instance AWSPager DescribeFlowLogs where
        page rq rs
          | stop (rs ^. dflsrsNextToken) = Nothing
          | stop (rs ^. dflsrsFlowLogs) = Nothing
          | otherwise =
            Just $ rq & dflsNextToken .~ rs ^. dflsrsNextToken

instance AWSRequest DescribeFlowLogs where
        type Rs DescribeFlowLogs = DescribeFlowLogsResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeFlowLogsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "flowLogSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeFlowLogs where

instance NFData DescribeFlowLogs where

instance ToHeaders DescribeFlowLogs where
        toHeaders = const mempty

instance ToPath DescribeFlowLogs where
        toPath = const "/"

instance ToQuery DescribeFlowLogs where
        toQuery DescribeFlowLogs'{..}
          = mconcat
              ["Action" =: ("DescribeFlowLogs" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "NextToken" =: _dflsNextToken,
               toQuery
                 (toQueryList "FlowLogId" <$> _dflsFlowLogIds),
               toQuery (toQueryList "Filter" <$> _dflsFilter),
               "DryRun" =: _dflsDryRun,
               "MaxResults" =: _dflsMaxResults]

-- | /See:/ 'describeFlowLogsResponse' smart constructor.
data DescribeFlowLogsResponse = DescribeFlowLogsResponse'
  { _dflsrsNextToken      :: !(Maybe Text)
  , _dflsrsFlowLogs       :: !(Maybe [FlowLog])
  , _dflsrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFlowLogsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dflsrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'dflsrsFlowLogs' - Information about the flow logs.
--
-- * 'dflsrsResponseStatus' - -- | The response status code.
describeFlowLogsResponse
    :: Int -- ^ 'dflsrsResponseStatus'
    -> DescribeFlowLogsResponse
describeFlowLogsResponse pResponseStatus_ =
  DescribeFlowLogsResponse'
    { _dflsrsNextToken = Nothing
    , _dflsrsFlowLogs = Nothing
    , _dflsrsResponseStatus = pResponseStatus_
    }


-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
dflsrsNextToken :: Lens' DescribeFlowLogsResponse (Maybe Text)
dflsrsNextToken = lens _dflsrsNextToken (\ s a -> s{_dflsrsNextToken = a})

-- | Information about the flow logs.
dflsrsFlowLogs :: Lens' DescribeFlowLogsResponse [FlowLog]
dflsrsFlowLogs = lens _dflsrsFlowLogs (\ s a -> s{_dflsrsFlowLogs = a}) . _Default . _Coerce

-- | -- | The response status code.
dflsrsResponseStatus :: Lens' DescribeFlowLogsResponse Int
dflsrsResponseStatus = lens _dflsrsResponseStatus (\ s a -> s{_dflsrsResponseStatus = a})

instance NFData DescribeFlowLogsResponse where
