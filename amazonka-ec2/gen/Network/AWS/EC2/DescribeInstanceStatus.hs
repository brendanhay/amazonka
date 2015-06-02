{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.DescribeInstanceStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Describes the status of one or more instances.
--
-- Instance status includes the following components:
--
-- Status checks - Amazon EC2 performs status checks on running EC2 instances
-- to identify hardware and software issues. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-system-instance-status-check.html StatusChecks for Your Instances> and <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstances.html Troubleshooting Instances with Failed StatusChecks> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Scheduled events - Amazon EC2 can schedule events (such as reboot, stop,
-- or terminate) for your instances related to hardware issues, software
-- updates, or system maintenance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-instances-status-check_sched.html Scheduled Eventsfor Your Instances> in the /Amazon Elastic Compute Cloud User Guide/.
--
-- Instance state - You can manage your instances from the moment you launch
-- them through their termination. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance Lifecycle>
-- in the /Amazon Elastic Compute Cloud User Guide/.
--
--
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-DescribeInstanceStatus.html>
module Network.AWS.EC2.DescribeInstanceStatus
    (
    -- * Request
      DescribeInstanceStatus
    -- ** Request constructor
    , describeInstanceStatus
    -- ** Request lenses
    , disDryRun
    , disFilters
    , disIncludeAllInstances
    , disInstanceIds
    , disMaxResults
    , disNextToken

    -- * Response
    , DescribeInstanceStatusResponse
    -- ** Response constructor
    , describeInstanceStatusResponse
    -- ** Response lenses
    , disrInstanceStatuses
    , disrNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data DescribeInstanceStatus = DescribeInstanceStatus
    { _disDryRun              :: Maybe Bool
    , _disFilters             :: List "Filter" Filter
    , _disIncludeAllInstances :: Maybe Bool
    , _disInstanceIds         :: List "InstanceId" Text
    , _disMaxResults          :: Maybe Int
    , _disNextToken           :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeInstanceStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'disFilters' @::@ ['Filter']
--
-- * 'disIncludeAllInstances' @::@ 'Maybe' 'Bool'
--
-- * 'disInstanceIds' @::@ ['Text']
--
-- * 'disMaxResults' @::@ 'Maybe' 'Int'
--
-- * 'disNextToken' @::@ 'Maybe' 'Text'
--
describeInstanceStatus :: DescribeInstanceStatus
describeInstanceStatus = DescribeInstanceStatus
    { _disDryRun              = Nothing
    , _disInstanceIds         = mempty
    , _disFilters             = mempty
    , _disNextToken           = Nothing
    , _disMaxResults          = Nothing
    , _disIncludeAllInstances = Nothing
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have the
-- required permissions, the error response is 'DryRunOperation'. Otherwise, it is 'UnauthorizedOperation'.
disDryRun :: Lens' DescribeInstanceStatus (Maybe Bool)
disDryRun = lens _disDryRun (\s a -> s { _disDryRun = a })

-- | One or more filters.
--
-- 'availability-zone' - The Availability Zone of the instance.
--
-- 'event.code' - The code for the scheduled event ('instance-reboot' | 'system-reboot' | 'system-maintenance' | 'instance-retirement' | 'instance-stop').
--
-- 'event.description' - A description of the event.
--
-- 'event.not-after' - The latest end time for the scheduled event (for
-- example, '2014-09-15T17:15:20.000Z').
--
-- 'event.not-before' - The earliest start time for the scheduled event (for
-- example, '2014-09-15T17:15:20.000Z').
--
-- 'instance-state-code' - The code for the instance state, as a 16-bit
-- unsigned integer. The high byte is an opaque internal value and should be
-- ignored. The low byte is set based on the state represented. The valid values
-- are 0 (pending), 16 (running), 32 (shutting-down), 48 (terminated), 64
-- (stopping), and 80 (stopped).
--
-- 'instance-state-name' - The state of the instance ('pending' | 'running' | 'shutting-down' | 'terminated' | 'stopping' | 'stopped').
--
-- 'instance-status.reachability' - Filters on instance status where the name
-- is 'reachability' ('passed' | 'failed' | 'initializing' | 'insufficient-data').
--
-- 'instance-status.status' - The status of the instance ('ok' | 'impaired' | 'initializing' | 'insufficient-data' | 'not-applicable').
--
-- 'system-status.reachability' - Filters on system status where the name is 'reachability' ('passed' | 'failed' | 'initializing' | 'insufficient-data').
--
-- 'system-status.status' - The system status of the instance ('ok' | 'impaired' | 'initializing' | 'insufficient-data' | 'not-applicable').
--
--
disFilters :: Lens' DescribeInstanceStatus [Filter]
disFilters = lens _disFilters (\s a -> s { _disFilters = a }) . _List

-- | When 'true', includes the health status for all instances. When 'false', includes
-- the health status for running instances only.
--
-- Default: 'false'
disIncludeAllInstances :: Lens' DescribeInstanceStatus (Maybe Bool)
disIncludeAllInstances =
    lens _disIncludeAllInstances (\s a -> s { _disIncludeAllInstances = a })

-- | One or more instance IDs.
--
-- Default: Describes all your instances.
--
-- Constraints: Maximum 100 explicitly specified instance IDs.
disInstanceIds :: Lens' DescribeInstanceStatus [Text]
disInstanceIds = lens _disInstanceIds (\s a -> s { _disInstanceIds = a }) . _List

-- | The maximum number of results to return for the request in a single page. The
-- remaining results of the initial request can be seen by sending another
-- request with the returned 'NextToken' value. This value can be between 5 and
-- 1000; if 'MaxResults' is given a value larger than 1000, only 1000 results are
-- returned. You cannot specify this parameter and the instance IDs parameter in
-- the same request.
disMaxResults :: Lens' DescribeInstanceStatus (Maybe Int)
disMaxResults = lens _disMaxResults (\s a -> s { _disMaxResults = a })

-- | The token to retrieve the next page of results.
disNextToken :: Lens' DescribeInstanceStatus (Maybe Text)
disNextToken = lens _disNextToken (\s a -> s { _disNextToken = a })

data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse
    { _disrInstanceStatuses :: List "item" InstanceStatus
    , _disrNextToken        :: Maybe Text
    } deriving (Eq, Read, Show)

-- | 'DescribeInstanceStatusResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disrInstanceStatuses' @::@ ['InstanceStatus']
--
-- * 'disrNextToken' @::@ 'Maybe' 'Text'
--
describeInstanceStatusResponse :: DescribeInstanceStatusResponse
describeInstanceStatusResponse = DescribeInstanceStatusResponse
    { _disrInstanceStatuses = mempty
    , _disrNextToken        = Nothing
    }

-- | One or more instance status descriptions.
disrInstanceStatuses :: Lens' DescribeInstanceStatusResponse [InstanceStatus]
disrInstanceStatuses =
    lens _disrInstanceStatuses (\s a -> s { _disrInstanceStatuses = a })
        . _List

-- | The token to use to retrieve the next page of results. This value is 'null'
-- when there are no more results to return.
disrNextToken :: Lens' DescribeInstanceStatusResponse (Maybe Text)
disrNextToken = lens _disrNextToken (\s a -> s { _disrNextToken = a })

instance ToPath DescribeInstanceStatus where
    toPath = const "/"

instance ToQuery DescribeInstanceStatus where
    toQuery DescribeInstanceStatus{..} = mconcat
        [ "DryRun"              =? _disDryRun
        , "Filter"              `toQueryList` _disFilters
        , "IncludeAllInstances" =? _disIncludeAllInstances
        , "InstanceId"          `toQueryList` _disInstanceIds
        , "MaxResults"          =? _disMaxResults
        , "NextToken"           =? _disNextToken
        ]

instance ToHeaders DescribeInstanceStatus

instance AWSRequest DescribeInstanceStatus where
    type Sv DescribeInstanceStatus = EC2
    type Rs DescribeInstanceStatus = DescribeInstanceStatusResponse

    request  = post "DescribeInstanceStatus"
    response = xmlResponse

instance FromXML DescribeInstanceStatusResponse where
    parseXML x = DescribeInstanceStatusResponse
        <$> x .@? "instanceStatusSet" .!@ mempty
        <*> x .@? "nextToken"

instance AWSPager DescribeInstanceStatus where
    page rq rs
        | stop (rs ^. disrNextToken) = Nothing
        | otherwise = (\x -> rq & disNextToken ?~ x)
            <$> (rs ^. disrNextToken)
