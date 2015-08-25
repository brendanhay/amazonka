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
-- Module      : Network.AWS.EC2.ReportInstanceStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits feedback about the status of an instance. The instance must be
-- in the 'running' state. If your experience with the instance differs
-- from the instance status returned by DescribeInstanceStatus, use
-- ReportInstanceStatus to report your experience with the instance. Amazon
-- EC2 collects this information to improve the accuracy of status checks.
--
-- Use of this action does not change the value returned by
-- DescribeInstanceStatus.
--
-- /See:/ <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReportInstanceStatus.html AWS API Reference> for ReportInstanceStatus.
module Network.AWS.EC2.ReportInstanceStatus
    (
    -- * Creating a Request
      reportInstanceStatus
    , ReportInstanceStatus
    -- * Request Lenses
    , risStartTime
    , risEndTime
    , risDryRun
    , risDescription
    , risInstances
    , risStatus
    , risReasonCodes

    -- * Destructuring the Response
    , reportInstanceStatusResponse
    , ReportInstanceStatusResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.EC2.Types.Product
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'reportInstanceStatus' smart constructor.
data ReportInstanceStatus = ReportInstanceStatus'
    { _risStartTime   :: !(Maybe ISO8601)
    , _risEndTime     :: !(Maybe ISO8601)
    , _risDryRun      :: !(Maybe Bool)
    , _risDescription :: !(Maybe Text)
    , _risInstances   :: ![Text]
    , _risStatus      :: !ReportStatusType
    , _risReasonCodes :: ![ReportInstanceReasonCodes]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReportInstanceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'risStartTime'
--
-- * 'risEndTime'
--
-- * 'risDryRun'
--
-- * 'risDescription'
--
-- * 'risInstances'
--
-- * 'risStatus'
--
-- * 'risReasonCodes'
reportInstanceStatus
    :: ReportStatusType -- ^ 'risStatus'
    -> ReportInstanceStatus
reportInstanceStatus pStatus_ =
    ReportInstanceStatus'
    { _risStartTime = Nothing
    , _risEndTime = Nothing
    , _risDryRun = Nothing
    , _risDescription = Nothing
    , _risInstances = mempty
    , _risStatus = pStatus_
    , _risReasonCodes = mempty
    }

-- | The time at which the reported instance health state began.
risStartTime :: Lens' ReportInstanceStatus (Maybe UTCTime)
risStartTime = lens _risStartTime (\ s a -> s{_risStartTime = a}) . mapping _Time;

-- | The time at which the reported instance health state ended.
risEndTime :: Lens' ReportInstanceStatus (Maybe UTCTime)
risEndTime = lens _risEndTime (\ s a -> s{_risEndTime = a}) . mapping _Time;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is 'DryRunOperation'.
-- Otherwise, it is 'UnauthorizedOperation'.
risDryRun :: Lens' ReportInstanceStatus (Maybe Bool)
risDryRun = lens _risDryRun (\ s a -> s{_risDryRun = a});

-- | Descriptive text about the health state of your instance.
risDescription :: Lens' ReportInstanceStatus (Maybe Text)
risDescription = lens _risDescription (\ s a -> s{_risDescription = a});

-- | One or more instances.
risInstances :: Lens' ReportInstanceStatus [Text]
risInstances = lens _risInstances (\ s a -> s{_risInstances = a}) . _Coerce;

-- | The status of all instances listed.
risStatus :: Lens' ReportInstanceStatus ReportStatusType
risStatus = lens _risStatus (\ s a -> s{_risStatus = a});

-- | One or more reason codes that describes the health state of your
-- instance.
--
-- -   'instance-stuck-in-state': My instance is stuck in a state.
--
-- -   'unresponsive': My instance is unresponsive.
--
-- -   'not-accepting-credentials': My instance is not accepting my
--     credentials.
--
-- -   'password-not-available': A password is not available for my
--     instance.
--
-- -   'performance-network': My instance is experiencing performance
--     problems which I believe are network related.
--
-- -   'performance-instance-store': My instance is experiencing
--     performance problems which I believe are related to the instance
--     stores.
--
-- -   'performance-ebs-volume': My instance is experiencing performance
--     problems which I believe are related to an EBS volume.
--
-- -   'performance-other': My instance is experiencing performance
--     problems.
--
-- -   'other': [explain using the description parameter]
--
risReasonCodes :: Lens' ReportInstanceStatus [ReportInstanceReasonCodes]
risReasonCodes = lens _risReasonCodes (\ s a -> s{_risReasonCodes = a}) . _Coerce;

instance AWSRequest ReportInstanceStatus where
        type Rs ReportInstanceStatus =
             ReportInstanceStatusResponse
        request = postQuery eC2
        response = receiveNull ReportInstanceStatusResponse'

instance ToHeaders ReportInstanceStatus where
        toHeaders = const mempty

instance ToPath ReportInstanceStatus where
        toPath = const "/"

instance ToQuery ReportInstanceStatus where
        toQuery ReportInstanceStatus'{..}
          = mconcat
              ["Action" =: ("ReportInstanceStatus" :: ByteString),
               "Version" =: ("2015-04-15" :: ByteString),
               "StartTime" =: _risStartTime,
               "EndTime" =: _risEndTime, "DryRun" =: _risDryRun,
               "Description" =: _risDescription,
               toQueryList "InstanceId" _risInstances,
               "Status" =: _risStatus,
               toQueryList "ReasonCode" _risReasonCodes]

-- | /See:/ 'reportInstanceStatusResponse' smart constructor.
data ReportInstanceStatusResponse =
    ReportInstanceStatusResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ReportInstanceStatusResponse' with the minimum fields required to make a request.
--
reportInstanceStatusResponse
    :: ReportInstanceStatusResponse
reportInstanceStatusResponse = ReportInstanceStatusResponse'
