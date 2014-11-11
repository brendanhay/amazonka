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

-- Module      : Network.AWS.EC2.MonitorInstances
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables monitoring for a running instance. For more information about
-- monitoring instances, see Monitoring Your Instances and Volumes in the
-- Amazon Elastic Compute Cloud User Guide.
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
    , MonitorInstancesResult
    -- ** Response constructor
    , monitorInstancesResult
    -- ** Response lenses
    , mirInstanceMonitorings
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.EC2.Types

data MonitorInstances = MonitorInstances
    { _miDryRun      :: Maybe Bool
    , _miInstanceIds :: [Text]
    } deriving (Eq, Ord, Show, Generic)

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
miInstanceIds = lens _miInstanceIds (\s a -> s { _miInstanceIds = a })
instance ToQuery MonitorInstances

instance ToPath MonitorInstances where
    toPath = const "/"

newtype MonitorInstancesResult = MonitorInstancesResult
    { _mirInstanceMonitorings :: [InstanceMonitoring]
    } deriving (Eq, Show, Generic, Monoid)

-- | 'MonitorInstancesResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'mirInstanceMonitorings' @::@ ['InstanceMonitoring']
--
monitorInstancesResult :: MonitorInstancesResult
monitorInstancesResult = MonitorInstancesResult
    { _mirInstanceMonitorings = mempty
    }

-- | Monitoring information for one or more instances.
mirInstanceMonitorings :: Lens' MonitorInstancesResult [InstanceMonitoring]
mirInstanceMonitorings =
    lens _mirInstanceMonitorings (\s a -> s { _mirInstanceMonitorings = a })
instance FromXML MonitorInstancesResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "MonitorInstancesResult"

instance AWSRequest MonitorInstances where
    type Sv MonitorInstances = EC2
    type Rs MonitorInstances = MonitorInstancesResult

    request  = post "MonitorInstances"
    response = xmlResponse $ \h x -> MonitorInstancesResult
        <$> x %| "instancesSet"
