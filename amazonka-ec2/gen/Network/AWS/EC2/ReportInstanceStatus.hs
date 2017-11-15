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
-- Copyright   : (c) 2013-2017 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Submits feedback about the status of an instance. The instance must be in the @running@ state. If your experience with the instance differs from the instance status returned by 'DescribeInstanceStatus' , use 'ReportInstanceStatus' to report your experience with the instance. Amazon EC2 collects this information to improve the accuracy of status checks.
--
--
-- Use of this action does not change the value returned by 'DescribeInstanceStatus' .
--
module Network.AWS.EC2.ReportInstanceStatus
    (
    -- * Creating a Request
      reportInstanceStatus
    , ReportInstanceStatus
    -- * Request Lenses
    , risStartTime
    , risEndTime
    , risDescription
    , risDryRun
    , risInstances
    , risReasonCodes
    , risStatus

    -- * Destructuring the Response
    , reportInstanceStatusResponse
    , ReportInstanceStatusResponse
    ) where

import Network.AWS.EC2.Types
import Network.AWS.EC2.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Contains the parameters for ReportInstanceStatus.
--
--
--
-- /See:/ 'reportInstanceStatus' smart constructor.
data ReportInstanceStatus = ReportInstanceStatus'
  { _risStartTime   :: !(Maybe ISO8601)
  , _risEndTime     :: !(Maybe ISO8601)
  , _risDescription :: !(Maybe Text)
  , _risDryRun      :: !(Maybe Bool)
  , _risInstances   :: ![Text]
  , _risReasonCodes :: ![ReportInstanceReasonCodes]
  , _risStatus      :: !ReportStatusType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReportInstanceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'risStartTime' - The time at which the reported instance health state began.
--
-- * 'risEndTime' - The time at which the reported instance health state ended.
--
-- * 'risDescription' - Descriptive text about the health state of your instance.
--
-- * 'risDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'risInstances' - One or more instances.
--
-- * 'risReasonCodes' - One or more reason codes that describe the health state of your instance.     * @instance-stuck-in-state@ : My instance is stuck in a state.     * @unresponsive@ : My instance is unresponsive.     * @not-accepting-credentials@ : My instance is not accepting my credentials.     * @password-not-available@ : A password is not available for my instance.     * @performance-network@ : My instance is experiencing performance problems that I believe are network related.     * @performance-instance-store@ : My instance is experiencing performance problems that I believe are related to the instance stores.     * @performance-ebs-volume@ : My instance is experiencing performance problems that I believe are related to an EBS volume.     * @performance-other@ : My instance is experiencing performance problems.     * @other@ : [explain using the description parameter]
--
-- * 'risStatus' - The status of all instances listed.
reportInstanceStatus
    :: ReportStatusType -- ^ 'risStatus'
    -> ReportInstanceStatus
reportInstanceStatus pStatus_ =
  ReportInstanceStatus'
  { _risStartTime = Nothing
  , _risEndTime = Nothing
  , _risDescription = Nothing
  , _risDryRun = Nothing
  , _risInstances = mempty
  , _risReasonCodes = mempty
  , _risStatus = pStatus_
  }


-- | The time at which the reported instance health state began.
risStartTime :: Lens' ReportInstanceStatus (Maybe UTCTime)
risStartTime = lens _risStartTime (\ s a -> s{_risStartTime = a}) . mapping _Time;

-- | The time at which the reported instance health state ended.
risEndTime :: Lens' ReportInstanceStatus (Maybe UTCTime)
risEndTime = lens _risEndTime (\ s a -> s{_risEndTime = a}) . mapping _Time;

-- | Descriptive text about the health state of your instance.
risDescription :: Lens' ReportInstanceStatus (Maybe Text)
risDescription = lens _risDescription (\ s a -> s{_risDescription = a});

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
risDryRun :: Lens' ReportInstanceStatus (Maybe Bool)
risDryRun = lens _risDryRun (\ s a -> s{_risDryRun = a});

-- | One or more instances.
risInstances :: Lens' ReportInstanceStatus [Text]
risInstances = lens _risInstances (\ s a -> s{_risInstances = a}) . _Coerce;

-- | One or more reason codes that describe the health state of your instance.     * @instance-stuck-in-state@ : My instance is stuck in a state.     * @unresponsive@ : My instance is unresponsive.     * @not-accepting-credentials@ : My instance is not accepting my credentials.     * @password-not-available@ : A password is not available for my instance.     * @performance-network@ : My instance is experiencing performance problems that I believe are network related.     * @performance-instance-store@ : My instance is experiencing performance problems that I believe are related to the instance stores.     * @performance-ebs-volume@ : My instance is experiencing performance problems that I believe are related to an EBS volume.     * @performance-other@ : My instance is experiencing performance problems.     * @other@ : [explain using the description parameter]
risReasonCodes :: Lens' ReportInstanceStatus [ReportInstanceReasonCodes]
risReasonCodes = lens _risReasonCodes (\ s a -> s{_risReasonCodes = a}) . _Coerce;

-- | The status of all instances listed.
risStatus :: Lens' ReportInstanceStatus ReportStatusType
risStatus = lens _risStatus (\ s a -> s{_risStatus = a});

instance AWSRequest ReportInstanceStatus where
        type Rs ReportInstanceStatus =
             ReportInstanceStatusResponse
        request = postQuery ec2
        response = receiveNull ReportInstanceStatusResponse'

instance Hashable ReportInstanceStatus where

instance NFData ReportInstanceStatus where

instance ToHeaders ReportInstanceStatus where
        toHeaders = const mempty

instance ToPath ReportInstanceStatus where
        toPath = const "/"

instance ToQuery ReportInstanceStatus where
        toQuery ReportInstanceStatus'{..}
          = mconcat
              ["Action" =: ("ReportInstanceStatus" :: ByteString),
               "Version" =: ("2016-11-15" :: ByteString),
               "StartTime" =: _risStartTime,
               "EndTime" =: _risEndTime,
               "Description" =: _risDescription,
               "DryRun" =: _risDryRun,
               toQueryList "InstanceId" _risInstances,
               toQueryList "ReasonCode" _risReasonCodes,
               "Status" =: _risStatus]

-- | /See:/ 'reportInstanceStatusResponse' smart constructor.
data ReportInstanceStatusResponse =
  ReportInstanceStatusResponse'
  deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReportInstanceStatusResponse' with the minimum fields required to make a request.
--
reportInstanceStatusResponse
    :: ReportInstanceStatusResponse
reportInstanceStatusResponse = ReportInstanceStatusResponse'


instance NFData ReportInstanceStatusResponse where
