{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , emcMetrics
    , emcGranularity

    -- * Response
    , EnableMetricsCollectionResponse
    -- ** Response constructor
    , enableMetricsCollectionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data EnableMetricsCollection = EnableMetricsCollection
    { _emcAutoScalingGroupName :: Text
    , _emcMetrics :: [Text]
    , _emcGranularity :: Text
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnableMetricsCollection' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @Metrics ::@ @[Text]@
--
-- * @Granularity ::@ @Text@
--
enableMetricsCollection :: Text -- ^ 'emcAutoScalingGroupName'
                        -> Text -- ^ 'emcGranularity'
                        -> EnableMetricsCollection
enableMetricsCollection p1 p3 = EnableMetricsCollection
    { _emcAutoScalingGroupName = p1
    , _emcMetrics = mempty
    , _emcGranularity = p3
    }

-- | The name or ARN of the Auto Scaling group.
emcAutoScalingGroupName :: Lens' EnableMetricsCollection Text
emcAutoScalingGroupName =
    lens _emcAutoScalingGroupName
         (\s a -> s { _emcAutoScalingGroupName = a })

-- | The list of metrics to collect. If no metrics are specified, all metrics
-- are enabled. The following metrics are supported: GroupMinSize GroupMaxSize
-- GroupDesiredCapacity GroupInServiceInstances GroupPendingInstances
-- GroupStandbyInstances GroupTerminatingInstances GroupTotalInstances The
-- GroupStandbyInstances metric is not returned by default. You must
-- explicitly request it when calling EnableMetricsCollection.
emcMetrics :: Lens' EnableMetricsCollection [Text]
emcMetrics = lens _emcMetrics (\s a -> s { _emcMetrics = a })

-- | The granularity to associate with the metrics to collect. Currently, the
-- only legal granularity is "1Minute".
emcGranularity :: Lens' EnableMetricsCollection Text
emcGranularity = lens _emcGranularity (\s a -> s { _emcGranularity = a })

instance ToQuery EnableMetricsCollection where
    toQuery = genericQuery def

data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'EnableMetricsCollectionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
enableMetricsCollectionResponse :: EnableMetricsCollectionResponse
enableMetricsCollectionResponse = EnableMetricsCollectionResponse

instance AWSRequest EnableMetricsCollection where
    type Sv EnableMetricsCollection = AutoScaling
    type Rs EnableMetricsCollection = EnableMetricsCollectionResponse

    request = post "EnableMetricsCollection"
    response _ = nullaryResponse EnableMetricsCollectionResponse
