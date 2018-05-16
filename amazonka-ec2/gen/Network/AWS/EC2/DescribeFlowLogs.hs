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
module Network.AWS.EC2.DescribeFlowLogs
    (
    -- * Creating a Request
      describeFlowLogs
    , DescribeFlowLogs
    -- * Request Lenses
    , dNextToken
    , dFlowLogIds
    , dFilter
    , dMaxResults

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
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeFlowLogs.
--
--
--
-- /See:/ 'describeFlowLogs' smart constructor.
data DescribeFlowLogs = DescribeFlowLogs'
  { _dNextToken  :: !(Maybe Text)
  , _dFlowLogIds :: !(Maybe [Text])
  , _dFilter     :: !(Maybe [Filter])
  , _dMaxResults :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeFlowLogs' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dNextToken' - The token to retrieve the next page of results.
--
-- * 'dFlowLogIds' - One or more flow log IDs.
--
-- * 'dFilter' - One or more filters.     * @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ | @FAILED@ ).     * @flow-log-id@ - The ID of the flow log.     * @log-group-name@ - The name of the log group.     * @resource-id@ - The ID of the VPC, subnet, or network interface.     * @traffic-type@ - The type of traffic (@ACCEPT@ | @REJECT@ | @ALL@ )
--
-- * 'dMaxResults' - The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. You cannot specify this parameter and the flow log IDs parameter in the same request.
describeFlowLogs
    :: DescribeFlowLogs
describeFlowLogs =
  DescribeFlowLogs'
    { _dNextToken = Nothing
    , _dFlowLogIds = Nothing
    , _dFilter = Nothing
    , _dMaxResults = Nothing
    }


-- | The token to retrieve the next page of results.
dNextToken :: Lens' DescribeFlowLogs (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a})

-- | One or more flow log IDs.
dFlowLogIds :: Lens' DescribeFlowLogs [Text]
dFlowLogIds = lens _dFlowLogIds (\ s a -> s{_dFlowLogIds = a}) . _Default . _Coerce

-- | One or more filters.     * @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ | @FAILED@ ).     * @flow-log-id@ - The ID of the flow log.     * @log-group-name@ - The name of the log group.     * @resource-id@ - The ID of the VPC, subnet, or network interface.     * @traffic-type@ - The type of traffic (@ACCEPT@ | @REJECT@ | @ALL@ )
dFilter :: Lens' DescribeFlowLogs [Filter]
dFilter = lens _dFilter (\ s a -> s{_dFilter = a}) . _Default . _Coerce

-- | The maximum number of results to return for the request in a single page. The remaining results can be seen by sending another request with the returned @NextToken@ value. This value can be between 5 and 1000; if @MaxResults@ is given a value larger than 1000, only 1000 results are returned. You cannot specify this parameter and the flow log IDs parameter in the same request.
dMaxResults :: Lens' DescribeFlowLogs (Maybe Int)
dMaxResults = lens _dMaxResults (\ s a -> s{_dMaxResults = a})

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
               "NextToken" =: _dNextToken,
               toQuery (toQueryList "FlowLogId" <$> _dFlowLogIds),
               toQuery (toQueryList "Filter" <$> _dFilter),
               "MaxResults" =: _dMaxResults]

-- | Contains the output of DescribeFlowLogs.
--
--
--
-- /See:/ 'describeFlowLogsResponse' smart constructor.
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
