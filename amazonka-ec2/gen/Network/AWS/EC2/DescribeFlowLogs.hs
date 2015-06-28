{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeFlowLogs
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes one or more flow logs. To view the information in your flow
-- logs (the log streams for the network interfaces), you must use the
-- CloudWatch Logs console or the CloudWatch Logs API.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeFlowLogs.html>
module Network.AWS.EC2.DescribeFlowLogs
    (
    -- * Request
      DescribeFlowLogs
    -- ** Request constructor
    , describeFlowLogs
    -- ** Request lenses
    , desNextToken
    , desFilter
    , desFlowLogIds
    , desMaxResults

    -- * Response
    , DescribeFlowLogsResponse
    -- ** Response constructor
    , describeFlowLogsResponse
    -- ** Response lenses
    , dflr1NextToken
    , dflr1FlowLogs
    , dflr1Status
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeFlowLogs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'desNextToken'
--
-- * 'desFilter'
--
-- * 'desFlowLogIds'
--
-- * 'desMaxResults'
data DescribeFlowLogs = DescribeFlowLogs'
    { _desNextToken  :: !(Maybe Text)
    , _desFilter     :: !(Maybe [Filter])
    , _desFlowLogIds :: !(Maybe [Text])
    , _desMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'DescribeFlowLogs' smart constructor.
describeFlowLogs :: DescribeFlowLogs
describeFlowLogs =
    DescribeFlowLogs'
    { _desNextToken = Nothing
    , _desFilter = Nothing
    , _desFlowLogIds = Nothing
    , _desMaxResults = Nothing
    }

-- | The token to retrieve the next page of results.
desNextToken :: Lens' DescribeFlowLogs (Maybe Text)
desNextToken = lens _desNextToken (\ s a -> s{_desNextToken = a});

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
desFilter :: Lens' DescribeFlowLogs [Filter]
desFilter = lens _desFilter (\ s a -> s{_desFilter = a}) . _Default;

-- | One or more flow log IDs.
desFlowLogIds :: Lens' DescribeFlowLogs [Text]
desFlowLogIds = lens _desFlowLogIds (\ s a -> s{_desFlowLogIds = a}) . _Default;

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @NextToken@ value. This value can be between 5 and 1000; if
-- @MaxResults@ is given a value larger than 1000, only 1000 results are
-- returned. You cannot specify this parameter and the flow log IDs
-- parameter in the same request.
desMaxResults :: Lens' DescribeFlowLogs (Maybe Int)
desMaxResults = lens _desMaxResults (\ s a -> s{_desMaxResults = a});

instance AWSRequest DescribeFlowLogs where
        type Sv DescribeFlowLogs = EC2
        type Rs DescribeFlowLogs = DescribeFlowLogsResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeFlowLogsResponse' <$>
                   (x .@? "nextToken") <*> (may (parseXMLList "item") x)
                     <*> (pure s))

instance ToHeaders DescribeFlowLogs where
        toHeaders = const mempty

instance ToPath DescribeFlowLogs where
        toPath = const "/"

instance ToQuery DescribeFlowLogs where
        toQuery DescribeFlowLogs'{..}
          = mconcat
              ["Action" =: ("DescribeFlowLogs" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "NextToken" =: _desNextToken,
               toQuery (toQueryList "Filter" <$> _desFilter),
               toQuery (toQueryList "item" <$> _desFlowLogIds),
               "MaxResults" =: _desMaxResults]

-- | /See:/ 'describeFlowLogsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dflr1NextToken'
--
-- * 'dflr1FlowLogs'
--
-- * 'dflr1Status'
data DescribeFlowLogsResponse = DescribeFlowLogsResponse'
    { _dflr1NextToken :: !(Maybe Text)
    , _dflr1FlowLogs  :: !(Maybe [FlowLog])
    , _dflr1Status    :: !Status
    } deriving (Eq,Show)

-- | 'DescribeFlowLogsResponse' smart constructor.
describeFlowLogsResponse :: Status -> DescribeFlowLogsResponse
describeFlowLogsResponse pStatus =
    DescribeFlowLogsResponse'
    { _dflr1NextToken = Nothing
    , _dflr1FlowLogs = Nothing
    , _dflr1Status = pStatus
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
dflr1NextToken :: Lens' DescribeFlowLogsResponse (Maybe Text)
dflr1NextToken = lens _dflr1NextToken (\ s a -> s{_dflr1NextToken = a});

-- | Information about the flow logs.
dflr1FlowLogs :: Lens' DescribeFlowLogsResponse [FlowLog]
dflr1FlowLogs = lens _dflr1FlowLogs (\ s a -> s{_dflr1FlowLogs = a}) . _Default;

-- | FIXME: Undocumented member.
dflr1Status :: Lens' DescribeFlowLogsResponse Status
dflr1Status = lens _dflr1Status (\ s a -> s{_dflr1Status = a});
