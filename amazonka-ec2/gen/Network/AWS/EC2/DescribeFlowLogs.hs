{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DescribeFlowLogs
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Describes one or more flow logs. To view the information in your flow
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
    , descNextToken
    , descFlowLogs
    , descStatus
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
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

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
               "NextToken" =: _desNextToken,
               toQuery (toQueryList "Filter" <$> _desFilter),
               toQuery (toQueryList "item" <$> _desFlowLogIds),
               "MaxResults" =: _desMaxResults]

-- | /See:/ 'describeFlowLogsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'descNextToken'
--
-- * 'descFlowLogs'
--
-- * 'descStatus'
data DescribeFlowLogsResponse = DescribeFlowLogsResponse'
    { _descNextToken :: !(Maybe Text)
    , _descFlowLogs  :: !(Maybe [FlowLog])
    , _descStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeFlowLogsResponse' smart constructor.
describeFlowLogsResponse :: Int -> DescribeFlowLogsResponse
describeFlowLogsResponse pStatus =
    DescribeFlowLogsResponse'
    { _descNextToken = Nothing
    , _descFlowLogs = Nothing
    , _descStatus = pStatus
    }

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
descNextToken :: Lens' DescribeFlowLogsResponse (Maybe Text)
descNextToken = lens _descNextToken (\ s a -> s{_descNextToken = a});

-- | Information about the flow logs.
descFlowLogs :: Lens' DescribeFlowLogsResponse [FlowLog]
descFlowLogs = lens _descFlowLogs (\ s a -> s{_descFlowLogs = a}) . _Default;

-- | FIXME: Undocumented member.
descStatus :: Lens' DescribeFlowLogsResponse Int
descStatus = lens _descStatus (\ s a -> s{_descStatus = a});
