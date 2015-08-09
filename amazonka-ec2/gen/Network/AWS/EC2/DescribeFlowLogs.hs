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
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more flow logs. To view the information in your flow
-- logs (the log streams for the network interfaces), you must use the
-- CloudWatch Logs console or the CloudWatch Logs API.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeFlowLogs.html AWS API Reference> for DescribeFlowLogs.
module Network.AWS.EC2.DescribeFlowLogs
    (
    -- * Creating a Request
      DescribeFlowLogs
    , describeFlowLogs
    -- * Request Lenses
    , dNextToken
    , dFilter
    , dFlowLogIds
    , dMaxResults

    -- * Destructuring the Response
    , DescribeFlowLogsResponse
    , describeFlowLogsResponse
    -- * Response Lenses
    , dflsrsNextToken
    , dflsrsFlowLogs
    , dflsrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeFlowLogs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dNextToken'
--
-- * 'dFilter'
--
-- * 'dFlowLogIds'
--
-- * 'dMaxResults'
data DescribeFlowLogs = DescribeFlowLogs'
    { _dNextToken  :: !(Maybe Text)
    , _dFilter     :: !(Maybe [Filter])
    , _dFlowLogIds :: !(Maybe [Text])
    , _dMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeFlowLogs' smart constructor.
describeFlowLogs :: DescribeFlowLogs
describeFlowLogs =
    DescribeFlowLogs'
    { _dNextToken = Nothing
    , _dFilter = Nothing
    , _dFlowLogIds = Nothing
    , _dMaxResults = Nothing
    }

-- | The token to retrieve the next page of results.
dNextToken :: Lens' DescribeFlowLogs (Maybe Text)
dNextToken = lens _dNextToken (\ s a -> s{_dNextToken = a});

-- | One or more filters.
--
-- -   @deliver-log-status@ - The status of the logs delivery (@SUCCESS@ |
--     @FAILED@).
--
-- -   @flow-log-id@ - The ID of the flow log.
--
-- -   @log-group-name@ - The name of the log group.
--
-- -   @resource-id@ - The ID of the VPC, subnet, or network interface.
--
-- -   @traffic-type@ - The type of traffic (@ACCEPT@ | @REJECT@ | @ALL@)
--
dFilter :: Lens' DescribeFlowLogs [Filter]
dFilter = lens _dFilter (\ s a -> s{_dFilter = a}) . _Default . _Coerce;

-- | One or more flow log IDs.
dFlowLogIds :: Lens' DescribeFlowLogs [Text]
dFlowLogIds = lens _dFlowLogIds (\ s a -> s{_dFlowLogIds = a}) . _Default . _Coerce;

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @NextToken@ value. This value can be between 5 and 1000; if
-- @MaxResults@ is given a value larger than 1000, only 1000 results are
-- returned. You cannot specify this parameter and the flow log IDs
-- parameter in the same request.
dMaxResults :: Lens' DescribeFlowLogs (Maybe Int)
dMaxResults = lens _dMaxResults (\ s a -> s{_dMaxResults = a});

instance AWSRequest DescribeFlowLogs where
        type Sv DescribeFlowLogs = EC2
        type Rs DescribeFlowLogs = DescribeFlowLogsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeFlowLogsResponse' <$>
                   (x .@? "nextToken") <*>
                     (x .@? "flowLogSet" .!@ mempty >>=
                        may (parseXMLList "item"))
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeFlowLogs where
        toHeaders = const mempty

instance ToPath DescribeFlowLogs where
        toPath = const "/"

instance ToQuery DescribeFlowLogs where
        toQuery DescribeFlowLogs'{..}
          = mconcat
              ["Action" =: ("DescribeFlowLogs" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "NextToken" =: _dNextToken,
               toQuery (toQueryList "Filter" <$> _dFilter),
               toQuery (toQueryList "item" <$> _dFlowLogIds),
               "MaxResults" =: _dMaxResults]

-- | /See:/ 'describeFlowLogsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dflsrsNextToken'
--
-- * 'dflsrsFlowLogs'
--
-- * 'dflsrsStatus'
data DescribeFlowLogsResponse = DescribeFlowLogsResponse'
    { _dflsrsNextToken :: !(Maybe Text)
    , _dflsrsFlowLogs  :: !(Maybe [FlowLog])
    , _dflsrsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeFlowLogsResponse' smart constructor.
describeFlowLogsResponse :: Int -> DescribeFlowLogsResponse
describeFlowLogsResponse pStatus_ =
    DescribeFlowLogsResponse'
    { _dflsrsNextToken = Nothing
    , _dflsrsFlowLogs = Nothing
    , _dflsrsStatus = pStatus_
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
dflsrsNextToken :: Lens' DescribeFlowLogsResponse (Maybe Text)
dflsrsNextToken = lens _dflsrsNextToken (\ s a -> s{_dflsrsNextToken = a});

-- | Information about the flow logs.
dflsrsFlowLogs :: Lens' DescribeFlowLogsResponse [FlowLog]
dflsrsFlowLogs = lens _dflsrsFlowLogs (\ s a -> s{_dflsrsFlowLogs = a}) . _Default . _Coerce;

-- | Undocumented member.
dflsrsStatus :: Lens' DescribeFlowLogsResponse Int
dflsrsStatus = lens _dflsrsStatus (\ s a -> s{_dflsrsStatus = a});
