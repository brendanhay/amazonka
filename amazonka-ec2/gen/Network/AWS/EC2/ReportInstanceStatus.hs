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
-- Copyright   : (c) 2013-2018 Brendan Hay
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
    , rissStartTime
    , rissEndTime
    , rissDescription
    , rissDryRun
    , rissInstances
    , rissReasonCodes
    , rissStatus

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

-- | /See:/ 'reportInstanceStatus' smart constructor.
data ReportInstanceStatus = ReportInstanceStatus'
  { _rissStartTime   :: !(Maybe ISO8601)
  , _rissEndTime     :: !(Maybe ISO8601)
  , _rissDescription :: !(Maybe Text)
  , _rissDryRun      :: !(Maybe Bool)
  , _rissInstances   :: ![Text]
  , _rissReasonCodes :: ![ReportInstanceReasonCodes]
  , _rissStatus      :: !ReportStatusType
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ReportInstanceStatus' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rissStartTime' - The time at which the reported instance health state began.
--
-- * 'rissEndTime' - The time at which the reported instance health state ended.
--
-- * 'rissDescription' - Descriptive text about the health state of your instance.
--
-- * 'rissDryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- * 'rissInstances' - One or more instances.
--
-- * 'rissReasonCodes' - One or more reason codes that describe the health state of your instance.     * @instance-stuck-in-state@ : My instance is stuck in a state.     * @unresponsive@ : My instance is unresponsive.     * @not-accepting-credentials@ : My instance is not accepting my credentials.     * @password-not-available@ : A password is not available for my instance.     * @performance-network@ : My instance is experiencing performance problems that I believe are network related.     * @performance-instance-store@ : My instance is experiencing performance problems that I believe are related to the instance stores.     * @performance-ebs-volume@ : My instance is experiencing performance problems that I believe are related to an EBS volume.     * @performance-other@ : My instance is experiencing performance problems.     * @other@ : [explain using the description parameter]
--
-- * 'rissStatus' - The status of all instances listed.
reportInstanceStatus
    :: ReportStatusType -- ^ 'rissStatus'
    -> ReportInstanceStatus
reportInstanceStatus pStatus_ =
  ReportInstanceStatus'
    { _rissStartTime = Nothing
    , _rissEndTime = Nothing
    , _rissDescription = Nothing
    , _rissDryRun = Nothing
    , _rissInstances = mempty
    , _rissReasonCodes = mempty
    , _rissStatus = pStatus_
    }


-- | The time at which the reported instance health state began.
rissStartTime :: Lens' ReportInstanceStatus (Maybe UTCTime)
rissStartTime = lens _rissStartTime (\ s a -> s{_rissStartTime = a}) . mapping _Time

-- | The time at which the reported instance health state ended.
rissEndTime :: Lens' ReportInstanceStatus (Maybe UTCTime)
rissEndTime = lens _rissEndTime (\ s a -> s{_rissEndTime = a}) . mapping _Time

-- | Descriptive text about the health state of your instance.
rissDescription :: Lens' ReportInstanceStatus (Maybe Text)
rissDescription = lens _rissDescription (\ s a -> s{_rissDescription = a})

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
rissDryRun :: Lens' ReportInstanceStatus (Maybe Bool)
rissDryRun = lens _rissDryRun (\ s a -> s{_rissDryRun = a})

-- | One or more instances.
rissInstances :: Lens' ReportInstanceStatus [Text]
rissInstances = lens _rissInstances (\ s a -> s{_rissInstances = a}) . _Coerce

-- | One or more reason codes that describe the health state of your instance.     * @instance-stuck-in-state@ : My instance is stuck in a state.     * @unresponsive@ : My instance is unresponsive.     * @not-accepting-credentials@ : My instance is not accepting my credentials.     * @password-not-available@ : A password is not available for my instance.     * @performance-network@ : My instance is experiencing performance problems that I believe are network related.     * @performance-instance-store@ : My instance is experiencing performance problems that I believe are related to the instance stores.     * @performance-ebs-volume@ : My instance is experiencing performance problems that I believe are related to an EBS volume.     * @performance-other@ : My instance is experiencing performance problems.     * @other@ : [explain using the description parameter]
rissReasonCodes :: Lens' ReportInstanceStatus [ReportInstanceReasonCodes]
rissReasonCodes = lens _rissReasonCodes (\ s a -> s{_rissReasonCodes = a}) . _Coerce

-- | The status of all instances listed.
rissStatus :: Lens' ReportInstanceStatus ReportStatusType
rissStatus = lens _rissStatus (\ s a -> s{_rissStatus = a})

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
               "StartTime" =: _rissStartTime,
               "EndTime" =: _rissEndTime,
               "Description" =: _rissDescription,
               "DryRun" =: _rissDryRun,
               toQueryList "InstanceId" _rissInstances,
               toQueryList "ReasonCode" _rissReasonCodes,
               "Status" =: _rissStatus]

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
