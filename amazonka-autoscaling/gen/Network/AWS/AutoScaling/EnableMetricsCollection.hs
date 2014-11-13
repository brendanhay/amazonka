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

-- Module      : Network.AWS.AutoScaling.EnableMetricsCollection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables monitoring of group metrics for the Auto Scaling group specified in
-- AutoScalingGroupName. You can specify the list of enabled metrics with the
-- Metrics parameter. Auto Scaling metrics collection can be turned on only if
-- the InstanceMonitoring flag, in the Auto Scaling group's launch
-- configuration, is set to True.
module Network.AWS.AutoScaling.EnableMetricsCollection
    (
    -- * Request
      EnableMetricsCollection
    -- ** Request constructor
    , enableMetricsCollection
    -- ** Request lenses
    , emcAutoScalingGroupName
    , emcGranularity
    , emcMetrics

    -- * Response
    , EnableMetricsCollectionResponse
    -- ** Response constructor
    , enableMetricsCollectionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data EnableMetricsCollection = EnableMetricsCollection
    { _emcAutoScalingGroupName :: Text
    , _emcGranularity          :: Text
    , _emcMetrics              :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnableMetricsCollection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'emcAutoScalingGroupName' @::@ 'Text'
--
-- * 'emcGranularity' @::@ 'Text'
--
-- * 'emcMetrics' @::@ ['Text']
--
enableMetricsCollection :: Text -- ^ 'emcAutoScalingGroupName'
                        -> Text -- ^ 'emcGranularity'
                        -> EnableMetricsCollection
enableMetricsCollection p1 p2 = EnableMetricsCollection
    { _emcAutoScalingGroupName = p1
    , _emcGranularity          = p2
    , _emcMetrics              = mempty
    }

-- | The name or ARN of the Auto Scaling group.
emcAutoScalingGroupName :: Lens' EnableMetricsCollection Text
emcAutoScalingGroupName =
    lens _emcAutoScalingGroupName (\s a -> s { _emcAutoScalingGroupName = a })

-- | The granularity to associate with the metrics to collect. Currently, the
-- only legal granularity is "1Minute".
emcGranularity :: Lens' EnableMetricsCollection Text
emcGranularity = lens _emcGranularity (\s a -> s { _emcGranularity = a })

-- | The list of metrics to collect. If no metrics are specified, all metrics
-- are enabled. The following metrics are supported: GroupMinSize
-- GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
-- GroupPendingInstances GroupStandbyInstances GroupTerminatingInstances
-- GroupTotalInstances.
emcMetrics :: Lens' EnableMetricsCollection [Text]
emcMetrics = lens _emcMetrics (\s a -> s { _emcMetrics = a })

instance ToQuery EnableMetricsCollection

instance ToPath EnableMetricsCollection where
    toPath = const "/"

data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'EnableMetricsCollectionResponse' constructor.
enableMetricsCollectionResponse :: EnableMetricsCollectionResponse
enableMetricsCollectionResponse = EnableMetricsCollectionResponse

instance FromXML EnableMetricsCollectionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnableMetricsCollectionResponse"

instance AWSRequest EnableMetricsCollection where
    type Sv EnableMetricsCollection = AutoScaling
    type Rs EnableMetricsCollection = EnableMetricsCollectionResponse

    request  = post "EnableMetricsCollection"
    response = nullaryResponse EnableMetricsCollectionResponse
