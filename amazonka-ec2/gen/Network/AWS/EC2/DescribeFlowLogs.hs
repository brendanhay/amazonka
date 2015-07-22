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
    , drqNextToken
    , drqFilter
    , drqFlowLogIds
    , drqMaxResults

    -- * Response
    , DescribeFlowLogsResponse
    -- ** Response constructor
    , describeFlowLogsResponse
    -- ** Response lenses
    , dflsrsNextToken
    , dflsrsFlowLogs
    , dflsrsStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeFlowLogs' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'drqNextToken'
--
-- * 'drqFilter'
--
-- * 'drqFlowLogIds'
--
-- * 'drqMaxResults'
data DescribeFlowLogs = DescribeFlowLogs'
    { _drqNextToken  :: !(Maybe Text)
    , _drqFilter     :: !(Maybe [Filter])
    , _drqFlowLogIds :: !(Maybe [Text])
    , _drqMaxResults :: !(Maybe Int)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeFlowLogs' smart constructor.
describeFlowLogs :: DescribeFlowLogs
describeFlowLogs =
    DescribeFlowLogs'
    { _drqNextToken = Nothing
    , _drqFilter = Nothing
    , _drqFlowLogIds = Nothing
    , _drqMaxResults = Nothing
    }

-- | The token to retrieve the next page of results.
drqNextToken :: Lens' DescribeFlowLogs (Maybe Text)
drqNextToken = lens _drqNextToken (\ s a -> s{_drqNextToken = a});

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
drqFilter :: Lens' DescribeFlowLogs [Filter]
drqFilter = lens _drqFilter (\ s a -> s{_drqFilter = a}) . _Default;

-- | One or more flow log IDs.
drqFlowLogIds :: Lens' DescribeFlowLogs [Text]
drqFlowLogIds = lens _drqFlowLogIds (\ s a -> s{_drqFlowLogIds = a}) . _Default;

-- | The maximum number of results to return for the request in a single
-- page. The remaining results can be seen by sending another request with
-- the returned @NextToken@ value. This value can be between 5 and 1000; if
-- @MaxResults@ is given a value larger than 1000, only 1000 results are
-- returned. You cannot specify this parameter and the flow log IDs
-- parameter in the same request.
drqMaxResults :: Lens' DescribeFlowLogs (Maybe Int)
drqMaxResults = lens _drqMaxResults (\ s a -> s{_drqMaxResults = a});

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
               "NextToken" =: _drqNextToken,
               toQuery (toQueryList "Filter" <$> _drqFilter),
               toQuery (toQueryList "item" <$> _drqFlowLogIds),
               "MaxResults" =: _drqMaxResults]

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
dflsrsFlowLogs = lens _dflsrsFlowLogs (\ s a -> s{_dflsrsFlowLogs = a}) . _Default;

-- | FIXME: Undocumented member.
dflsrsStatus :: Lens' DescribeFlowLogsResponse Int
dflsrsStatus = lens _dflsrsStatus (\ s a -> s{_dflsrsStatus = a});
