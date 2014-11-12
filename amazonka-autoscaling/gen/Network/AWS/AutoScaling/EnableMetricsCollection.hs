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
      EnableMetricsCollectionQuery
    -- ** Request constructor
    , enableMetricsCollectionQuery
    -- ** Request lenses
    , emcqAutoScalingGroupName
    , emcqGranularity
    , emcqMetrics

    -- * Response
    , EnableMetricsCollectionResponse
    -- ** Response constructor
    , enableMetricsCollectionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data EnableMetricsCollectionQuery = EnableMetricsCollectionQuery
    { _emcqAutoScalingGroupName :: Text
    , _emcqGranularity          :: Text
    , _emcqMetrics              :: [Text]
    } deriving (Eq, Ord, Show, Generic)

-- | 'EnableMetricsCollectionQuery' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'emcqAutoScalingGroupName' @::@ 'Text'
--
-- * 'emcqGranularity' @::@ 'Text'
--
-- * 'emcqMetrics' @::@ ['Text']
--
enableMetricsCollectionQuery :: Text -- ^ 'emcqAutoScalingGroupName'
                             -> Text -- ^ 'emcqGranularity'
                             -> EnableMetricsCollectionQuery
enableMetricsCollectionQuery p1 p2 = EnableMetricsCollectionQuery
    { _emcqAutoScalingGroupName = p1
    , _emcqGranularity          = p2
    , _emcqMetrics              = mempty
    }

-- | The name or ARN of the Auto Scaling group.
emcqAutoScalingGroupName :: Lens' EnableMetricsCollectionQuery Text
emcqAutoScalingGroupName =
    lens _emcqAutoScalingGroupName
        (\s a -> s { _emcqAutoScalingGroupName = a })

-- | The granularity to associate with the metrics to collect. Currently, the
-- only legal granularity is "1Minute".
emcqGranularity :: Lens' EnableMetricsCollectionQuery Text
emcqGranularity = lens _emcqGranularity (\s a -> s { _emcqGranularity = a })

-- | The list of metrics to collect. If no metrics are specified, all metrics
-- are enabled. The following metrics are supported: GroupMinSize
-- GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
-- GroupPendingInstances GroupStandbyInstances GroupTerminatingInstances
-- GroupTotalInstances.
emcqMetrics :: Lens' EnableMetricsCollectionQuery [Text]
emcqMetrics = lens _emcqMetrics (\s a -> s { _emcqMetrics = a })

instance ToQuery EnableMetricsCollectionQuery

instance ToPath EnableMetricsCollectionQuery where
    toPath = const "/"

data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'EnableMetricsCollectionResponse' constructor.
enableMetricsCollectionResponse :: EnableMetricsCollectionResponse
enableMetricsCollectionResponse = EnableMetricsCollectionResponse

instance FromXML EnableMetricsCollectionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "EnableMetricsCollectionResponse"

instance AWSRequest EnableMetricsCollectionQuery where
    type Sv EnableMetricsCollectionQuery = AutoScaling
    type Rs EnableMetricsCollectionQuery = EnableMetricsCollectionResponse

    request  = post "EnableMetricsCollection"
    response = nullaryResponse EnableMetricsCollectionResponse
