{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EC2.V2014_06_15.ReportInstanceStatus
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Submits feedback about the status of an instance. The instance must be in
-- the running state. If your experience with the instance differs from the
-- instance status returned by DescribeInstanceStatus, use
-- ReportInstanceStatus to report your experience with the instance. Amazon
-- EC2 collects this information to improve the accuracy of status checks.
-- Example 1 This example reports instance health state for two instances.
-- https://ec2.amazonaws.com/?Action=ReportInstanceStatus &amp;Status=impaired
-- &amp;InstanceId.0=i-9440effb &amp;InstanceId.1=i-0cf27c63 &amp;AUTHPARAMS
-- Example 2 This example reports instance health state for two instances with
-- reason codes. https://ec2.amazonaws.com/?Action=ReportInstanceStatus
-- &amp;Description=Description+of+my+issue. &amp;Status=impaired
-- &amp;InstanceId.0=i-9440effb &amp;InstanceId.1=i-0cf27c63
-- &amp;ReasonCode.0=instance-performance-network
-- &amp;ReasonCode.1=instance-performance-disk &amp;AUTHPARAMS.
module Network.AWS.EC2.V2014_06_15.ReportInstanceStatus
    (
    -- * Request
      ReportInstanceStatus
    -- ** Request constructor
    , mkReportInstanceStatus
    -- ** Request lenses
    , risInstances
    , risStatus
    , risStartTime
    , risEndTime
    , risReasonCodes
    , risDescription

    -- * Response
    , ReportInstanceStatusResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.EC2.V2014_06_15.Types
import Network.AWS.Prelude

-- | 
data ReportInstanceStatus = ReportInstanceStatus
    { _risInstances :: [Text]
    , _risStatus :: ReportStatusType
    , _risStartTime :: Maybe ISO8601
    , _risEndTime :: Maybe ISO8601
    , _risReasonCodes :: [ReportInstanceReasonCodes]
    , _risDescription :: Maybe Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ReportInstanceStatus' request.
mkReportInstanceStatus :: [Text] -- ^ 'risInstances'
                       -> ReportStatusType -- ^ 'risStatus'
                       -> [ReportInstanceReasonCodes] -- ^ 'risReasonCodes'
                       -> ReportInstanceStatus
mkReportInstanceStatus p1 p2 p5 = ReportInstanceStatus
    { _risInstances = p1
    , _risStatus = p2
    , _risStartTime = Nothing
    , _risEndTime = Nothing
    , _risReasonCodes = p5
    , _risDescription = Nothing
    }
{-# INLINE mkReportInstanceStatus #-}

-- | One or more instances.
risInstances :: Lens' ReportInstanceStatus [Text]
risInstances = lens _risInstances (\s a -> s { _risInstances = a })
{-# INLINE risInstances #-}

-- | The status of all instances listed.
risStatus :: Lens' ReportInstanceStatus ReportStatusType
risStatus = lens _risStatus (\s a -> s { _risStatus = a })
{-# INLINE risStatus #-}

-- | The time at which the reported instance health state began.
risStartTime :: Lens' ReportInstanceStatus (Maybe ISO8601)
risStartTime = lens _risStartTime (\s a -> s { _risStartTime = a })
{-# INLINE risStartTime #-}

-- | The time at which the reported instance health state ended.
risEndTime :: Lens' ReportInstanceStatus (Maybe ISO8601)
risEndTime = lens _risEndTime (\s a -> s { _risEndTime = a })
{-# INLINE risEndTime #-}

-- | One or more reason codes that describes the health state of your instance.
-- instance-stuck-in-state: My instance is stuck in a state. unresponsive: My
-- instance is unresponsive. not-accepting-credentials: My instance is not
-- accepting my credentials. password-not-available: A password is not
-- available for my instance. performance-network: My instance is experiencing
-- performance problems which I believe are network related.
-- performance-instance-store: My instance is experiencing performance
-- problems which I believe are related to the instance stores.
-- performance-ebs-volume: My instance is experiencing performance problems
-- which I believe are related to an EBS volume. performance-other: My
-- instance is experiencing performance problems. other: [explain using the
-- description parameter].
risReasonCodes :: Lens' ReportInstanceStatus [ReportInstanceReasonCodes]
risReasonCodes = lens _risReasonCodes (\s a -> s { _risReasonCodes = a })
{-# INLINE risReasonCodes #-}

-- | Descriptive text about the health state of your instance.
risDescription :: Lens' ReportInstanceStatus (Maybe Text)
risDescription = lens _risDescription (\s a -> s { _risDescription = a })
{-# INLINE risDescription #-}

instance ToQuery ReportInstanceStatus where
    toQuery = genericQuery def

data ReportInstanceStatusResponse = ReportInstanceStatusResponse
    deriving (Eq, Show, Generic)

instance AWSRequest ReportInstanceStatus where
    type Sv ReportInstanceStatus = EC2
    type Rs ReportInstanceStatus = ReportInstanceStatusResponse

    request = post "ReportInstanceStatus"
    response _ = nullaryResponse ReportInstanceStatusResponse
