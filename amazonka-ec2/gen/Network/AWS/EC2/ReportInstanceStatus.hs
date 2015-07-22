{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.ReportInstanceStatus
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Submits feedback about the status of an instance. The instance must be
-- in the @running@ state. If your experience with the instance differs
-- from the instance status returned by DescribeInstanceStatus, use
-- ReportInstanceStatus to report your experience with the instance. Amazon
-- EC2 collects this information to improve the accuracy of status checks.
--
-- Use of this action does not change the value returned by
-- DescribeInstanceStatus.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-ReportInstanceStatus.html>
module Network.AWS.EC2.ReportInstanceStatus
    (
    -- * Request
      ReportInstanceStatus
    -- ** Request constructor
    , reportInstanceStatus
    -- ** Request lenses
    , risrqStartTime
    , risrqEndTime
    , risrqDryRun
    , risrqDescription
    , risrqInstances
    , risrqStatus
    , risrqReasonCodes

    -- * Response
    , ReportInstanceStatusResponse
    -- ** Response constructor
    , reportInstanceStatusResponse
    ) where

import           Network.AWS.EC2.Types
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | /See:/ 'reportInstanceStatus' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'risrqStartTime'
--
-- * 'risrqEndTime'
--
-- * 'risrqDryRun'
--
-- * 'risrqDescription'
--
-- * 'risrqInstances'
--
-- * 'risrqStatus'
--
-- * 'risrqReasonCodes'
data ReportInstanceStatus = ReportInstanceStatus'
    { _risrqStartTime   :: !(Maybe ISO8601)
    , _risrqEndTime     :: !(Maybe ISO8601)
    , _risrqDryRun      :: !(Maybe Bool)
    , _risrqDescription :: !(Maybe Text)
    , _risrqInstances   :: ![Text]
    , _risrqStatus      :: !ReportStatusType
    , _risrqReasonCodes :: ![ReportInstanceReasonCodes]
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReportInstanceStatus' smart constructor.
reportInstanceStatus :: ReportStatusType -> ReportInstanceStatus
reportInstanceStatus pStatus_ =
    ReportInstanceStatus'
    { _risrqStartTime = Nothing
    , _risrqEndTime = Nothing
    , _risrqDryRun = Nothing
    , _risrqDescription = Nothing
    , _risrqInstances = mempty
    , _risrqStatus = pStatus_
    , _risrqReasonCodes = mempty
    }

-- | The time at which the reported instance health state began.
risrqStartTime :: Lens' ReportInstanceStatus (Maybe UTCTime)
risrqStartTime = lens _risrqStartTime (\ s a -> s{_risrqStartTime = a}) . mapping _Time;

-- | The time at which the reported instance health state ended.
risrqEndTime :: Lens' ReportInstanceStatus (Maybe UTCTime)
risrqEndTime = lens _risrqEndTime (\ s a -> s{_risrqEndTime = a}) . mapping _Time;

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
risrqDryRun :: Lens' ReportInstanceStatus (Maybe Bool)
risrqDryRun = lens _risrqDryRun (\ s a -> s{_risrqDryRun = a});

-- | Descriptive text about the health state of your instance.
risrqDescription :: Lens' ReportInstanceStatus (Maybe Text)
risrqDescription = lens _risrqDescription (\ s a -> s{_risrqDescription = a});

-- | One or more instances.
risrqInstances :: Lens' ReportInstanceStatus [Text]
risrqInstances = lens _risrqInstances (\ s a -> s{_risrqInstances = a});

-- | The status of all instances listed.
risrqStatus :: Lens' ReportInstanceStatus ReportStatusType
risrqStatus = lens _risrqStatus (\ s a -> s{_risrqStatus = a});

-- | One or more reason codes that describes the health state of your
-- instance.
--
-- -   @instance-stuck-in-state@: My instance is stuck in a state.
--
-- -   @unresponsive@: My instance is unresponsive.
--
-- -   @not-accepting-credentials@: My instance is not accepting my
--     credentials.
--
-- -   @password-not-available@: A password is not available for my
--     instance.
--
-- -   @performance-network@: My instance is experiencing performance
--     problems which I believe are network related.
--
-- -   @performance-instance-store@: My instance is experiencing
--     performance problems which I believe are related to the instance
--     stores.
--
-- -   @performance-ebs-volume@: My instance is experiencing performance
--     problems which I believe are related to an EBS volume.
--
-- -   @performance-other@: My instance is experiencing performance
--     problems.
--
-- -   @other@: [explain using the description parameter]
--
risrqReasonCodes :: Lens' ReportInstanceStatus [ReportInstanceReasonCodes]
risrqReasonCodes = lens _risrqReasonCodes (\ s a -> s{_risrqReasonCodes = a});

instance AWSRequest ReportInstanceStatus where
        type Sv ReportInstanceStatus = EC2
        type Rs ReportInstanceStatus =
             ReportInstanceStatusResponse
        request = post
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
               "StartTime" =: _risrqStartTime,
               "EndTime" =: _risrqEndTime, "DryRun" =: _risrqDryRun,
               "Description" =: _risrqDescription,
               toQueryList "InstanceId" _risrqInstances,
               "Status" =: _risrqStatus,
               toQueryList "item" _risrqReasonCodes]

-- | /See:/ 'reportInstanceStatusResponse' smart constructor.
data ReportInstanceStatusResponse =
    ReportInstanceStatusResponse'
    deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ReportInstanceStatusResponse' smart constructor.
reportInstanceStatusResponse :: ReportInstanceStatusResponse
reportInstanceStatusResponse = ReportInstanceStatusResponse'
