{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

-- Module      : Network.AWS.EC2.ReportInstanceStatus
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
-- EC2 collects this information to improve the accuracy of status checks. Use
-- of this action does not change the value returned by
-- DescribeInstanceStatus.
module Network.AWS.EC2.ReportInstanceStatus
    (
    -- * Request
      ReportInstanceStatus
    -- ** Request constructor
    , reportInstanceStatus
    -- ** Request lenses
    , risDescription
    , risDryRun
    , risEndTime
    , risInstances
    , risReasonCodes
    , risStartTime
    , risStatus

    -- * Response
    , ReportInstanceStatusResponse
    -- ** Response constructor
    , reportInstanceStatusResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data ReportInstanceStatus = ReportInstanceStatus
    { _risDescription :: Maybe Text
    , _risDryRun      :: Maybe Bool
    , _risEndTime     :: Maybe RFC822
    , _risInstances   :: [Text]
    , _risReasonCodes :: [Text]
    , _risStartTime   :: Maybe RFC822
    , _risStatus      :: Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ReportInstanceStatus' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'risDescription' @::@ 'Maybe' 'Text'
--
-- * 'risDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'risEndTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'risInstances' @::@ ['Text']
--
-- * 'risReasonCodes' @::@ ['Text']
--
-- * 'risStartTime' @::@ 'Maybe' 'UTCTime'
--
-- * 'risStatus' @::@ 'Text'
--
reportInstanceStatus :: Text -- ^ 'risStatus'
                     -> ReportInstanceStatus
reportInstanceStatus p1 = ReportInstanceStatus
    { _risStatus      = p1
    , _risDryRun      = Nothing
    , _risInstances   = mempty
    , _risStartTime   = Nothing
    , _risEndTime     = Nothing
    , _risReasonCodes = mempty
    , _risDescription = Nothing
    }

-- | Descriptive text about the health state of your instance.
risDescription :: Lens' ReportInstanceStatus (Maybe Text)
risDescription = lens _risDescription (\s a -> s { _risDescription = a })

risDryRun :: Lens' ReportInstanceStatus (Maybe Bool)
risDryRun = lens _risDryRun (\s a -> s { _risDryRun = a })

-- | The time at which the reported instance health state ended.
risEndTime :: Lens' ReportInstanceStatus (Maybe UTCTime)
risEndTime = lens _risEndTime (\s a -> s { _risEndTime = a })
    . mapping _Time

-- | One or more instances.
risInstances :: Lens' ReportInstanceStatus [Text]
risInstances = lens _risInstances (\s a -> s { _risInstances = a })

-- | One or more reason codes that describes the health state of your
-- instance. instance-stuck-in-state: My instance is stuck in a state.
-- unresponsive: My instance is unresponsive. not-accepting-credentials: My
-- instance is not accepting my credentials. password-not-available: A
-- password is not available for my instance. performance-network: My
-- instance is experiencing performance problems which I believe are network
-- related. performance-instance-store: My instance is experiencing
-- performance problems which I believe are related to the instance stores.
-- performance-ebs-volume: My instance is experiencing performance problems
-- which I believe are related to an EBS volume. performance-other: My
-- instance is experiencing performance problems. other: [explain using the
-- description parameter].
risReasonCodes :: Lens' ReportInstanceStatus [Text]
risReasonCodes = lens _risReasonCodes (\s a -> s { _risReasonCodes = a })

-- | The time at which the reported instance health state began.
risStartTime :: Lens' ReportInstanceStatus (Maybe UTCTime)
risStartTime = lens _risStartTime (\s a -> s { _risStartTime = a })
    . mapping _Time

-- | The status of all instances listed.
risStatus :: Lens' ReportInstanceStatus Text
risStatus = lens _risStatus (\s a -> s { _risStatus = a })

instance ToPath ReportInstanceStatus where
    toPath = const "/"

instance ToQuery ReportInstanceStatus

data ReportInstanceStatusResponse = ReportInstanceStatusResponse

-- | 'ReportInstanceStatusResponse' constructor.
reportInstanceStatusResponse :: ReportInstanceStatusResponse
reportInstanceStatusResponse = ReportInstanceStatusResponse

instance AWSRequest ReportInstanceStatus where
    type Sv ReportInstanceStatus = EC2
    type Rs ReportInstanceStatus = ReportInstanceStatusResponse

    request  = post "ReportInstanceStatus"
    response = const (nullaryResponse ReportInstanceStatusResponse)
