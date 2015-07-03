{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}

-- Module      : Network.AWS.EC2.DescribeInstanceStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
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
    , descIncludeAllInstances
    , descFilters
    , descNextToken
    , descInstanceIds
    , descDryRun
    , descMaxResults

    -- * Response
    , DescribeInstanceStatusResponse
    -- ** Response constructor
    , describeInstanceStatusResponse
    -- ** Response lenses
    , disrInstanceStatuses
    , disrNextToken
    , disrStatus
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'describeInstanceStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'descIncludeAllInstances'
--
-- * 'descFilters'
--
-- * 'descNextToken'
--
-- * 'descInstanceIds'
--
-- * 'descDryRun'
--
-- * 'descMaxResults'
data DescribeInstanceStatus = DescribeInstanceStatus'
    { _descIncludeAllInstances :: !(Maybe Bool)
    , _descFilters             :: !(Maybe [Filter])
    , _descNextToken           :: !(Maybe Text)
    , _descInstanceIds         :: !(Maybe [Text])
    , _descDryRun              :: !(Maybe Bool)
    , _descMaxResults          :: !(Maybe Int)
    } deriving (Eq,Read,Show)

-- | 'DescribeInstanceStatus' smart constructor.
describeInstanceStatus :: DescribeInstanceStatus
describeInstanceStatus =
    DescribeInstanceStatus'
    { _descIncludeAllInstances = Nothing
    , _descFilters = Nothing
    , _descNextToken = Nothing
    , _descInstanceIds = Nothing
    , _descDryRun = Nothing
    , _descMaxResults = Nothing
    }

-- | When @true@, includes the health status for all instances. When @false@,
-- includes the health status for running instances only.
--
-- Default: @false@
descIncludeAllInstances :: Lens' DescribeInstanceStatus (Maybe Bool)
descIncludeAllInstances = lens _descIncludeAllInstances (\ s a -> s{_descIncludeAllInstances = a});

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
descFilters :: Lens' DescribeInstanceStatus [Filter]
descFilters = lens _descFilters (\ s a -> s{_descFilters = a}) . _Default;

-- | The token to retrieve the next page of results.
descNextToken :: Lens' DescribeInstanceStatus (Maybe Text)
descNextToken = lens _descNextToken (\ s a -> s{_descNextToken = a});

-- | One or more instance IDs.
--
-- Default: Describes all your instances.
--
-- Constraints: Maximum 100 explicitly specified instance IDs.
descInstanceIds :: Lens' DescribeInstanceStatus [Text]
descInstanceIds = lens _descInstanceIds (\ s a -> s{_descInstanceIds = a}) . _Default;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
descDryRun :: Lens' DescribeInstanceStatus (Maybe Bool)
descDryRun = lens _descDryRun (\ s a -> s{_descDryRun = a});

-- | The maximum number of results to return for the request in a single
-- page. The remaining results of the initial request can be seen by
-- sending another request with the returned @NextToken@ value. This value
-- can be between 5 and 1000; if @MaxResults@ is given a value larger than
-- 1000, only 1000 results are returned. You cannot specify this parameter
-- and the instance IDs parameter in the same request.
descMaxResults :: Lens' DescribeInstanceStatus (Maybe Int)
descMaxResults = lens _descMaxResults (\ s a -> s{_descMaxResults = a});

instance AWSPager DescribeInstanceStatus where
        page rq rs
          | stop (rs ^. disrNextToken) = Nothing
          | stop (rs ^. disrInstanceStatuses) = Nothing
          | otherwise =
            Just $ rq & descNextToken .~ rs ^. disrNextToken

instance AWSRequest DescribeInstanceStatus where
        type Sv DescribeInstanceStatus = EC2
        type Rs DescribeInstanceStatus =
             DescribeInstanceStatusResponse
        request = post
        response
          = receiveXML
              (\ s h x ->
                 DescribeInstanceStatusResponse' <$>
                   (x .@? "instanceStatusSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

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
               "IncludeAllInstances" =: _descIncludeAllInstances,
               toQuery (toQueryList "Filter" <$> _descFilters),
               "NextToken" =: _descNextToken,
               toQuery
                 (toQueryList "InstanceId" <$> _descInstanceIds),
               "DryRun" =: _descDryRun,
               "MaxResults" =: _descMaxResults]

-- | /See:/ 'describeInstanceStatusResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'disrInstanceStatuses'
--
-- * 'disrNextToken'
--
-- * 'disrStatus'
data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse'
    { _disrInstanceStatuses :: !(Maybe [InstanceStatus])
    , _disrNextToken        :: !(Maybe Text)
    , _disrStatus           :: !Int
    } deriving (Eq,Read,Show)

-- | 'DescribeInstanceStatusResponse' smart constructor.
describeInstanceStatusResponse :: Int -> DescribeInstanceStatusResponse
describeInstanceStatusResponse pStatus =
    DescribeInstanceStatusResponse'
    { _disrInstanceStatuses = Nothing
    , _disrNextToken = Nothing
    , _disrStatus = pStatus
    }

-- | One or more instance status descriptions.
disrInstanceStatuses :: Lens' DescribeInstanceStatusResponse [InstanceStatus]
disrInstanceStatuses = lens _disrInstanceStatuses (\ s a -> s{_disrInstanceStatuses = a}) . _Default;

-- | The token to use to retrieve the next page of results. This value is
-- @null@ when there are no more results to return.
disrNextToken :: Lens' DescribeInstanceStatusResponse (Maybe Text)
disrNextToken = lens _disrNextToken (\ s a -> s{_disrNextToken = a});

-- | FIXME: Undocumented member.
disrStatus :: Lens' DescribeInstanceStatusResponse Int
disrStatus = lens _disrStatus (\ s a -> s{_disrStatus = a});
