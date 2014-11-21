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

-- Module      : Network.AWS.AutoScaling.EnableMetricsCollection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Enables monitoring of the specified metrics for the specified Auto Scaling
-- group. You can only enable metrics collection if InstanceMonitoring in the
-- launch configuration for the group is set to True.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_EnableMetricsCollection.html>
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
import qualified GHC.Exts

data EnableMetricsCollection = EnableMetricsCollection
    { _emcAutoScalingGroupName :: Text
    , _emcGranularity          :: Text
    , _emcMetrics              :: List "Metrics" Text
    } deriving (Eq, Ord, Show)

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
-- only valid value is "1Minute".
emcGranularity :: Lens' EnableMetricsCollection Text
emcGranularity = lens _emcGranularity (\s a -> s { _emcGranularity = a })

-- | One or more of the following metrics: GroupMinSize GroupMaxSize
-- GroupDesiredCapacity GroupInServiceInstances GroupPendingInstances
-- GroupStandbyInstances GroupTerminatingInstances GroupTotalInstances If
-- you omit this parameter, all metrics are enabled. The
-- GroupStandbyInstances metric is not returned by default. You must
-- explicitly request it when calling EnableMetricsCollection.
emcMetrics :: Lens' EnableMetricsCollection [Text]
emcMetrics = lens _emcMetrics (\s a -> s { _emcMetrics = a }) . _List

data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'EnableMetricsCollectionResponse' constructor.
enableMetricsCollectionResponse :: EnableMetricsCollectionResponse
enableMetricsCollectionResponse = EnableMetricsCollectionResponse

instance ToPath EnableMetricsCollection where
    toPath = const "/"

instance ToQuery EnableMetricsCollection where
    toQuery EnableMetricsCollection{..} = mconcat
        [ "AutoScalingGroupName" =? _emcAutoScalingGroupName
        , "Granularity"          =? _emcGranularity
        , "Metrics"              =? _emcMetrics
        ]

instance ToHeaders EnableMetricsCollection

instance AWSRequest EnableMetricsCollection where
    type Sv EnableMetricsCollection = AutoScaling
    type Rs EnableMetricsCollection = EnableMetricsCollectionResponse

    request  = post "EnableMetricsCollection"
    response = nullResponse EnableMetricsCollectionResponse
