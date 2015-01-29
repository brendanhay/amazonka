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

-- Module      : Network.AWS.EC2.MonitorInstances
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

-- | Enables monitoring for a running instance. For more information about
-- monitoring instances, see <http://docs.aws.amazon.com/AWSEC2/latest/UserGuide/using-cloudwatch.html Monitoring Your Instances and Volumes> in the /AmazonElastic Compute Cloud User Guide for Linux/.
--
-- <http://docs.aws.amazon.com/AWSEC2/latest/APIReference/ApiReference-query-MonitorInstances.html>
module Network.AWS.EC2.MonitorInstances
    (
    -- * Request
      MonitorInstances
    -- ** Request constructor
    , monitorInstances
    -- ** Request lenses
    , miDryRun
    , miInstanceIds

    -- * Response
    , MonitorInstancesResponse
    -- ** Response constructor
    , monitorInstancesResponse
    -- ** Response lenses
    , mirInstanceMonitorings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types
import qualified GHC.Exts

data MonitorInstances = MonitorInstances
    { _miDryRun      :: Maybe Bool
    , _miInstanceIds :: List "InstanceId" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'MonitorInstances' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'miDryRun' @::@ 'Maybe' 'Bool'
--
-- * 'miInstanceIds' @::@ ['Text']
--
monitorInstances :: MonitorInstances
monitorInstances = MonitorInstances
    { _miDryRun      = Nothing
    , _miInstanceIds = mempty
    }

miDryRun :: Lens' MonitorInstances (Maybe Bool)
miDryRun = lens _miDryRun (\s a -> s { _miDryRun = a })

-- | One or more instance IDs.
miInstanceIds :: Lens' MonitorInstances [Text]
miInstanceIds = lens _miInstanceIds (\s a -> s { _miInstanceIds = a }) . _List

newtype MonitorInstancesResponse = MonitorInstancesResponse
    { _mirInstanceMonitorings :: List "item" InstanceMonitoring
    } deriving (Eq, Read, Show, Monoid, Semigroup)

-- | 'MonitorInstancesResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mirInstanceMonitorings' @::@ ['InstanceMonitoring']
--
monitorInstancesResponse :: MonitorInstancesResponse
monitorInstancesResponse = MonitorInstancesResponse
    { _mirInstanceMonitorings = mempty
    }

-- | Monitoring information for one or more instances.
mirInstanceMonitorings :: Lens' MonitorInstancesResponse [InstanceMonitoring]
mirInstanceMonitorings =
    lens _mirInstanceMonitorings (\s a -> s { _mirInstanceMonitorings = a })
        . _List

instance ToPath MonitorInstances where
    toPath = const "/"

instance ToQuery MonitorInstances where
    toQuery MonitorInstances{..} = mconcat
        [ "DryRun"     =? _miDryRun
        , "InstanceId" `toQueryList` _miInstanceIds
        ]

instance ToHeaders MonitorInstances

instance AWSRequest MonitorInstances where
    type Sv MonitorInstances = EC2
    type Rs MonitorInstances = MonitorInstancesResponse

    request  = post "MonitorInstances"
    response = xmlResponse

instance FromXML MonitorInstancesResponse where
    parseXML x = MonitorInstancesResponse
        <$> x .@? "instancesSet" .!@ mempty
