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
-- Module      : Network.AWS.EC2.DescribeInstanceStatus
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Describes the status of one or more instances. By default, only running instances are described, unless you specifically indicate to return the status of all instances.
--
--
-- Instance status includes the following components:
--
--     * __Status checks__ - Amazon EC2 performs status checks on running EC2 instances to identify hardware and software issues. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-system-instance-status-check.html Status Checks for Your Instances> and <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/TroubleshootingInstances.html Troubleshooting Instances with Failed Status Checks> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--     * __Scheduled events__ - Amazon EC2 can schedule events (such as reboot, stop, or terminate) for your instances related to hardware issues, software updates, or system maintenance. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/monitoring-instances-status-check_sched.html Scheduled Events for Your Instances> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--     * __Instance state__ - You can manage your instances from the moment you launch them through their termination. For more information, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/ec2-instance-lifecycle.html Instance Lifecycle> in the /Amazon Elastic Compute Cloud User Guide/ .
--
--
--
--
-- This operation returns paginated results.
module Network.AWS.EC2.DescribeInstanceStatus
    (
    -- * Creating a Request
      describeInstanceStatus
    , DescribeInstanceStatus
    -- * Request Lenses
    , disIncludeAllInstances
    , disFilters
    , disNextToken
    , disInstanceIds
    , disDryRun
    , disMaxResults

    -- * Destructuring the Response
    , describeInstanceStatusResponse
    , DescribeInstanceStatusResponse
    -- * Response Lenses
    , disrsInstanceStatuses
    , disrsNextToken
    , disrsResponseStatus
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for DescribeInstanceStatus.
--
--
--
-- /See:/ 'describeInstanceStatus' smart constructor.
data DescribeInstanceStatus = DescribeInstanceStatus'
  { _disIncludeAllInstances :: !(Maybe Bool)
  , _disFilters             :: !(Maybe [Filter])
  , _disNextToken           :: !(Maybe Text)
  , _disInstanceIds         :: !(Maybe [Text])
  , _disDryRun              :: !(Maybe Bool)
  , _disMaxResults          :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstanceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disIncludeAllInstances' - When @true@ , includes the health status for all instances. When @false@ , includes the health status for running instances only. Default: @false@
--
-- * 'disFilters' - One or more filters.     * @availability-zone@ - The Availability Zone of the instance.     * @event.code@ - The code for the scheduled event (@instance-reboot@ | @system-reboot@ | @system-maintenance@ | @instance-retirement@ | @instance-stop@ ).     * @event.description@ - A description of the event.     * @event.not-after@ - The latest end time for the scheduled event (for example, @2014-09-15T17:15:20.000Z@ ).     * @event.not-before@ - The earliest start time for the scheduled event (for example, @2014-09-15T17:15:20.000Z@ ).     * @instance-state-code@ - The code for the instance state, as a 16-bit unsigned integer. The high byte is an opaque internal value and should be ignored. The low byte is set based on the state represented. The valid values are 0 (pending), 16 (running), 32 (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).     * @instance-state-name@ - The state of the instance (@pending@ | @running@ | @shutting-down@ | @terminated@ | @stopping@ | @stopped@ ).     * @instance-status.reachability@ - Filters on instance status where the name is @reachability@ (@passed@ | @failed@ | @initializing@ | @insufficient-data@ ).     * @instance-status.status@ - The status of the instance (@ok@ | @impaired@ | @initializing@ | @insufficient-data@ | @not-applicable@ ).     * @system-status.reachability@ - Filters on system status where the name is @reachability@ (@passed@ | @failed@ | @initializing@ | @insufficient-data@ ).     * @system-status.status@ - The system status of the instance (@ok@ | @impaired@ | @initializing@ | @insufficient-data@ | @not-applicable@ ).
--
-- * 'disNextToken' - The token to retrieve the next page of results.
--
-- * 'disInstanceIds' - One or more instance IDs. Default: Describes all your instances. Constraints: Maximum 100 explicitly specified instance IDs.
--
-- * 'disDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'disMaxResults' - The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
describeInstanceStatus
    :: DescribeInstanceStatus
describeInstanceStatus =
  DescribeInstanceStatus'
    { _disIncludeAllInstances = Nothing
    , _disFilters = Nothing
    , _disNextToken = Nothing
    , _disInstanceIds = Nothing
    , _disDryRun = Nothing
    , _disMaxResults = Nothing
    }


-- | When @true@ , includes the health status for all instances. When @false@ , includes the health status for running instances only. Default: @false@
disIncludeAllInstances :: Lens' DescribeInstanceStatus (Maybe Bool)
disIncludeAllInstances = lens _disIncludeAllInstances (\ s a -> s{_disIncludeAllInstances = a})

-- | One or more filters.     * @availability-zone@ - The Availability Zone of the instance.     * @event.code@ - The code for the scheduled event (@instance-reboot@ | @system-reboot@ | @system-maintenance@ | @instance-retirement@ | @instance-stop@ ).     * @event.description@ - A description of the event.     * @event.not-after@ - The latest end time for the scheduled event (for example, @2014-09-15T17:15:20.000Z@ ).     * @event.not-before@ - The earliest start time for the scheduled event (for example, @2014-09-15T17:15:20.000Z@ ).     * @instance-state-code@ - The code for the instance state, as a 16-bit unsigned integer. The high byte is an opaque internal value and should be ignored. The low byte is set based on the state represented. The valid values are 0 (pending), 16 (running), 32 (shutting-down), 48 (terminated), 64 (stopping), and 80 (stopped).     * @instance-state-name@ - The state of the instance (@pending@ | @running@ | @shutting-down@ | @terminated@ | @stopping@ | @stopped@ ).     * @instance-status.reachability@ - Filters on instance status where the name is @reachability@ (@passed@ | @failed@ | @initializing@ | @insufficient-data@ ).     * @instance-status.status@ - The status of the instance (@ok@ | @impaired@ | @initializing@ | @insufficient-data@ | @not-applicable@ ).     * @system-status.reachability@ - Filters on system status where the name is @reachability@ (@passed@ | @failed@ | @initializing@ | @insufficient-data@ ).     * @system-status.status@ - The system status of the instance (@ok@ | @impaired@ | @initializing@ | @insufficient-data@ | @not-applicable@ ).
disFilters :: Lens' DescribeInstanceStatus [Filter]
disFilters = lens _disFilters (\ s a -> s{_disFilters = a}) . _Default . _Coerce

-- | The token to retrieve the next page of results.
disNextToken :: Lens' DescribeInstanceStatus (Maybe Text)
disNextToken = lens _disNextToken (\ s a -> s{_disNextToken = a})

-- | One or more instance IDs. Default: Describes all your instances. Constraints: Maximum 100 explicitly specified instance IDs.
disInstanceIds :: Lens' DescribeInstanceStatus [Text]
disInstanceIds = lens _disInstanceIds (\ s a -> s{_disInstanceIds = a}) . _Default . _Coerce

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
disDryRun :: Lens' DescribeInstanceStatus (Maybe Bool)
disDryRun = lens _disDryRun (\ s a -> s{_disDryRun = a})

-- | The maximum number of results to return in a single call. To retrieve the remaining results, make another call with the returned @NextToken@ value. This value can be between 5 and 1000. You cannot specify this parameter and the instance IDs parameter in the same call.
disMaxResults :: Lens' DescribeInstanceStatus (Maybe Int)
disMaxResults = lens _disMaxResults (\ s a -> s{_disMaxResults = a})

instance AWSPager DescribeInstanceStatus where
        page rq rs
          | stop (rs ^. disrsNextToken) = Nothing
          | stop (rs ^. disrsInstanceStatuses) = Nothing
          | otherwise =
            Just $ rq & disNextToken .~ rs ^. disrsNextToken

instance AWSRequest DescribeInstanceStatus where
        type Rs DescribeInstanceStatus =
             DescribeInstanceStatusResponse
        request = postQuery ec2
        response
          = receiveXML
              (\ s h x ->
                 DescribeInstanceStatusResponse' <$>
                   (x .@? "instanceStatusSet" .!@ mempty >>=
                      may (parseXMLList "item"))
                     <*> (x .@? "nextToken")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeInstanceStatus where

instance NFData DescribeInstanceStatus where

instance ToHeaders DescribeInstanceStatus where
        toHeaders = const mempty

instance ToPath DescribeInstanceStatus where
        toPath = const "/"

instance ToQuery DescribeInstanceStatus where
        toQuery DescribeInstanceStatus'{..}
          = mconcat
              ["Action" =:
                 ("DescribeInstanceStatus" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "IncludeAllInstances" =: _disIncludeAllInstances,
               toQuery (toQueryList "Filter" <$> _disFilters),
               "NextToken" =: _disNextToken,
               toQuery
                 (toQueryList "InstanceId" <$> _disInstanceIds),
               "DryRun" =: _disDryRun,
               "MaxResults" =: _disMaxResults]

-- | Contains the output of DescribeInstanceStatus.
--
--
--
-- /See:/ 'describeInstanceStatusResponse' smart constructor.
data DescribeInstanceStatusResponse = DescribeInstanceStatusResponse'
  { _disrsInstanceStatuses :: !(Maybe [InstanceStatus])
  , _disrsNextToken        :: !(Maybe Text)
  , _disrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeInstanceStatusResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'disrsInstanceStatuses' - One or more instance status descriptions.
--
-- * 'disrsNextToken' - The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
--
-- * 'disrsResponseStatus' - -- | The response status code.
describeInstanceStatusResponse
    :: Int -- ^ 'disrsResponseStatus'
    -> DescribeInstanceStatusResponse
describeInstanceStatusResponse pResponseStatus_ =
  DescribeInstanceStatusResponse'
    { _disrsInstanceStatuses = Nothing
    , _disrsNextToken = Nothing
    , _disrsResponseStatus = pResponseStatus_
    }


-- | One or more instance status descriptions.
disrsInstanceStatuses :: Lens' DescribeInstanceStatusResponse [InstanceStatus]
disrsInstanceStatuses = lens _disrsInstanceStatuses (\ s a -> s{_disrsInstanceStatuses = a}) . _Default . _Coerce

-- | The token to use to retrieve the next page of results. This value is @null@ when there are no more results to return.
disrsNextToken :: Lens' DescribeInstanceStatusResponse (Maybe Text)
disrsNextToken = lens _disrsNextToken (\ s a -> s{_disrsNextToken = a})

-- | -- | The response status code.
disrsResponseStatus :: Lens' DescribeInstanceStatusResponse Int
disrsResponseStatus = lens _disrsResponseStatus (\ s a -> s{_disrsResponseStatus = a})

instance NFData DescribeInstanceStatusResponse where
