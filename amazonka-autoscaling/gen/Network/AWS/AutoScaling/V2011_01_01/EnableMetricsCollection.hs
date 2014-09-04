{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.EnableMetricsCollection
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
module Network.AWS.AutoScaling.V2011_01_01.EnableMetricsCollection
    (
    -- * Request
      EnableMetricsCollection
    -- ** Request constructor
    , enableMetricsCollection
    -- ** Request lenses
    , emcqAutoScalingGroupName
    , emcqGranularity
    , emcqMetrics

    -- * Response
    , EnableMetricsCollectionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'EnableMetricsCollection' request.
enableMetricsCollection :: Text -- ^ 'emcqAutoScalingGroupName'
                        -> Text -- ^ 'emcqGranularity'
                        -> EnableMetricsCollection
enableMetricsCollection p1 p2 = EnableMetricsCollection
    { _emcqAutoScalingGroupName = p1
    , _emcqGranularity = p2
    , _emcqMetrics = mempty
    }
{-# INLINE enableMetricsCollection #-}

data EnableMetricsCollection = EnableMetricsCollection
    { _emcqAutoScalingGroupName :: Text
      -- ^ The name or ARN of the Auto Scaling group.
    , _emcqGranularity :: Text
      -- ^ The granularity to associate with the metrics to collect.
      -- Currently, the only legal granularity is "1Minute".
    , _emcqMetrics :: [Text]
      -- ^ The list of metrics to collect. If no metrics are specified, all
      -- metrics are enabled. The following metrics are supported:
      -- GroupMinSize GroupMaxSize GroupDesiredCapacity
      -- GroupInServiceInstances GroupPendingInstances
      -- GroupStandbyInstances GroupTerminatingInstances
      -- GroupTotalInstances The GroupStandbyInstances metric is not
      -- returned by default. You must explicitly request it when calling
      -- EnableMetricsCollection.
    } deriving (Show, Generic)

-- | The name or ARN of the Auto Scaling group.
emcqAutoScalingGroupName :: Lens' EnableMetricsCollection (Text)
emcqAutoScalingGroupName f x =
    f (_emcqAutoScalingGroupName x)
        <&> \y -> x { _emcqAutoScalingGroupName = y }
{-# INLINE emcqAutoScalingGroupName #-}

-- | The granularity to associate with the metrics to collect. Currently, the
-- only legal granularity is "1Minute".
emcqGranularity :: Lens' EnableMetricsCollection (Text)
emcqGranularity f x =
    f (_emcqGranularity x)
        <&> \y -> x { _emcqGranularity = y }
{-# INLINE emcqGranularity #-}

-- | The list of metrics to collect. If no metrics are specified, all metrics
-- are enabled. The following metrics are supported: GroupMinSize GroupMaxSize
-- GroupDesiredCapacity GroupInServiceInstances GroupPendingInstances
-- GroupStandbyInstances GroupTerminatingInstances GroupTotalInstances The
-- GroupStandbyInstances metric is not returned by default. You must
-- explicitly request it when calling EnableMetricsCollection.
emcqMetrics :: Lens' EnableMetricsCollection ([Text])
emcqMetrics f x =
    f (_emcqMetrics x)
        <&> \y -> x { _emcqMetrics = y }
{-# INLINE emcqMetrics #-}

instance ToQuery EnableMetricsCollection where
    toQuery = genericQuery def

data EnableMetricsCollectionResponse = EnableMetricsCollectionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest EnableMetricsCollection where
    type Sv EnableMetricsCollection = AutoScaling
    type Rs EnableMetricsCollection = EnableMetricsCollectionResponse

    request = post "EnableMetricsCollection"
    response _ = nullaryResponse EnableMetricsCollectionResponse
