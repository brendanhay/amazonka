{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

-- Module      : Network.AWS.EC2.DescribeInstanceStatus
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

-- | Describes the status of one or more instances.
--
-- Instance status includes the following components:
--
-- -   __Status checks__ - Amazon EC2 performs status checks on running EC2
--     instances to identify hardware and software issues. For more
--     information, see
--     <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-system-instance-status-check.html Status Checks for Your Instances>
--     and
--     <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstances.html Troubleshooting Instances with Failed Status Checks>
--     in the /Amazon Elastic Compute Cloud User Guide/.
--
-- -   __Scheduled events__ - Amazon EC2 can schedule events (such as
--     reboot, stop, or terminate) for your instances related to hardware
--     issues, software updates, or system maintenance. For more
--     information, see
--     <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-instances-status-check_sched.html Scheduled Events for Your Instances>
--     in the /Amazon Elastic Compute Cloud User Guide/.
--
-- -   __Instance state__ - You can manage your instances from the moment
--     you launch them through their termination. For more information, see
--     <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance Lifecycle>
--     in the /Amazon Elastic Compute Cloud User Guide/.
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
    , dis1IncludeAllInstances
    , dis1Filters
    , dis1NextToken
    , dis1InstanceIds
    , dis1DryRun
    , dis1MaxResults

    -- * Response
    , DescribeInstanceStatusResponse
    -- ** Response constructor
    , describeInstanceStatusResponse
    -- ** Response lenses
    , disrInstanceStatuses
    , disrNextToken
    ) where

import Network.AWS.Request
import Network.AWS.Response
import Network.AWS.Prelude
import Network.AWS.EC2.Types

-- | /See:/ 'describeInstanceStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dis1IncludeAllInstances'
--
-- * 'dis1Filters'
--
-- * 'dis1NextToken'
--
-- * 'dis1InstanceIds'
--
-- * 'dis1DryRun'
--
-- * 'dis1MaxResults'
data DescribeInstanceStatus = DescribeInstanceStatus'{_dis1IncludeAllInstances :: Maybe Bool, _dis1Filters :: [Filter], _dis1NextToken :: Maybe Text, _dis1InstanceIds :: [Text], _dis1DryRun :: Maybe Bool, _dis1MaxResults :: Maybe Int} deriving (Eq, Read, Show)

-- | 'DescribeInstanceStatus' smart constructor.
describeInstanceStatus :: DescribeInstanceStatus
describeInstanceStatus = DescribeInstanceStatus'{_dis1IncludeAllInstances = Nothing, _dis1Filters = mempty, _dis1NextToken = Nothing, _dis1InstanceIds = mempty, _dis1DryRun = Nothing, _dis1MaxResults = Nothing};

-- | When @true@, includes the health status for all instances. When @false@,
-- includes the health status for running instances only.
--
-- Default: @false@
dis1IncludeAllInstances :: Lens' DescribeInstanceStatus (Maybe Bool)
dis1IncludeAllInstances = lens _dis1IncludeAllInstances (\ s a -> s{_dis1IncludeAllInstances = a});

-- | One or more filters.
--
-- -   @availability-zone@ - The Availability Zone of the instance.
--
-- -   @event.code@ - The code for the scheduled event (@instance-reboot@ |
--     @system-reboot@ | @system-maintenance@ | @instance-retirement@ |
--     @instance-stop@).
--
-- -   @event.description@ - A description of the event.
--
-- -   @event.not-after@ - The latest end time for the scheduled event (for
--     example, @2014-09-15T17:15:20.000Z@).
--
-- -   @event.not-before@ - The earliest start time for the scheduled event
--     (for example, @2014-09-15T17:15:20.000Z@).
--
-- -   @instance-state-code@ - The code for the instance state, as a 16-bit
--     unsigned integer. The high byte is an opaque internal value and
--     should be ignored. The low byte is set based on the state
--     represented. The valid values are 0 (pending), 16 (running), 32
--     (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).
--
-- -   @instance-state-name@ - The state of the instance (@pending@ |
--     @running@ | @shutting-down@ | @terminated@ | @stopping@ |
--     @stopped@).
--
-- -   @instance-status.reachability@ - Filters on instance status where
--     the name is @reachability@ (@passed@ | @failed@ | @initializing@ |
--     @insufficient-data@).
--
-- -   @instance-status.status@ - The status of the instance (@ok@ |
--     @impaired@ | @initializing@ | @insufficient-data@ |
--     @not-applicable@).
--
-- -   @system-status.reachability@ - Filters on system status where the
--     name is @reachability@ (@passed@ | @failed@ | @initializing@ |
--     @insufficient-data@).
--
-- -   @system-status.status@ - The system status of the instance (@ok@ |
--     @impaired@ | @initializing@ | @insufficient-data@ |
--     @not-applicable@).
--
dis1Filters :: Lens' DescribeInstanceStatus [Filter]
dis1Filters = lens _dis1Filters (\ s a -> s{_dis1Filters = a});

-- | The token to retrieve the next page of results.
dis1NextToken :: Lens' DescribeInstanceStatus (Maybe Text)
dis1NextToken = lens _dis1NextToken (\ s a -> s{_dis1NextToken = a});

-- | One or more instance IDs.
--
-- Default: Describes all your instances.
--
-- Constraints: Maximum 100 explicitly specified instance IDs.
dis1InstanceIds :: Lens' DescribeInstanceStatus [Text]
dis1InstanceIds = lens _dis1InstanceIds (\ s a -> s{_dis1InstanceIds = a});

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
dis1DryRun :: Lens' DescribeInstanceStatus (Maybe Bool)
dis1DryRun = lens _dis1DryRun (\ s a -> s{_dis1DryRun = a});

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value larger than
-- 1000, only 1000 results are returned. You cannot specify this parameter
-- and the instance IDs parameter in the same request.
dis1MaxResults :: Lens' DescribeInstanceStatus (Maybe Int)
dis1MaxResults = lens _dis1MaxResults (\ s a -> s{_dis1MaxResults = a});

instance AWSRequest DescribeInstanceStatus where
        type Sv DescribeInstanceStatus = EC2
        type Rs DescribeInstanceStatus =
             DescribeInstanceStatusResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeInstanceStatusResponse' <$>
                   parseXMLList "item" x <*> x .@? "nextToken")

instance ToHeaders DescribeInstanceStatus where
        toHeaders = const mempty

instance ToPath DescribeInstanceStatus where
        toPath = const "/"

instance ToQuery DescribeInstanceStatus where
        toQuery DescribeInstanceStatus'{..}
          = mconcat
              ["Action" =:
                 ("DescribeInstanceStatus" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "IncludeAllInstances" =: _dis1IncludeAllInstances,
               "Filter" =: _dis1Filters,
               "NextToken" =: _dis1NextToken,
               "InstanceId" =: _dis1InstanceIds,
               "DryRun" =: _dis1DryRun,
               "MaxResults" =: _dis1MaxResults]

-- | /See:/ 'describeInstanceStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disrInstanceStatuses'
--
-- * 'disrNextToken'
data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse'{_disrInstanceStatuses :: [InstanceStatus], _disrNextToken :: Maybe Text} deriving (Eq, Read, Show)

-- | 'DescribeInstanceStatusResponse' smart constructor.
describeInstanceStatusResponse :: DescribeInstanceStatusResponse
describeInstanceStatusResponse = DescribeInstanceStatusResponse'{_disrInstanceStatuses = mempty, _disrNextToken = Nothing};

-- | One or more instance status descriptions.
disrInstanceStatuses :: Lens' DescribeInstanceStatusResponse [InstanceStatus]
disrInstanceStatuses = lens _disrInstanceStatuses (\ s a -> s{_disrInstanceStatuses = a});

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
disrNextToken :: Lens' DescribeInstanceStatusResponse (Maybe Text)
disrNextToken = lens _disrNextToken (\ s a -> s{_disrNextToken = a});
