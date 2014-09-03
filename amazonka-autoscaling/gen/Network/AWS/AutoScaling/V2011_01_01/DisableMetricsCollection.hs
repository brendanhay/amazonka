{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DisableMetricsCollection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Disables monitoring of group metrics for the Auto Scaling group specified
-- in AutoScalingGroupName. You can specify the list of affected metrics with
-- the Metrics parameter.
module Network.AWS.AutoScaling.V2011_01_01.DisableMetricsCollection
    (
    -- * Request
      DisableMetricsCollection
    -- ** Request constructor
    , disableMetricsCollection
    -- ** Request lenses
    , dmcqAutoScalingGroupName
    , dmcqMetrics

    -- * Response
    , DisableMetricsCollectionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DisableMetricsCollection' request.
disableMetricsCollection :: Text -- ^ 'dmcqAutoScalingGroupName'
                         -> DisableMetricsCollection
disableMetricsCollection p1 = DisableMetricsCollection
    { _dmcqAutoScalingGroupName = p1
    , _dmcqMetrics = mempty
    }

data DisableMetricsCollection = DisableMetricsCollection
    { _dmcqAutoScalingGroupName :: Text
      -- ^ The name or ARN of the Auto Scaling Group.
    , _dmcqMetrics :: [Text]
      -- ^ The list of metrics to disable. If no metrics are specified, all
      -- metrics are disabled. The following metrics are supported:
      -- GroupMinSize GroupMaxSize GroupDesiredCapacity
      -- GroupInServiceInstances GroupPendingInstances
      -- GroupStandbyInstances GroupTerminatingInstances
      -- GroupTotalInstances.
    } deriving (Show, Generic)

-- | The name or ARN of the Auto Scaling Group.
dmcqAutoScalingGroupName
    :: Functor f
    => (Text
    -> f (Text))
    -> DisableMetricsCollection
    -> f DisableMetricsCollection
dmcqAutoScalingGroupName f x =
    (\y -> x { _dmcqAutoScalingGroupName = y })
       <$> f (_dmcqAutoScalingGroupName x)
{-# INLINE dmcqAutoScalingGroupName #-}

-- | The list of metrics to disable. If no metrics are specified, all metrics
-- are disabled. The following metrics are supported: GroupMinSize
-- GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
-- GroupPendingInstances GroupStandbyInstances GroupTerminatingInstances
-- GroupTotalInstances.
dmcqMetrics
    :: Functor f
    => ([Text]
    -> f ([Text]))
    -> DisableMetricsCollection
    -> f DisableMetricsCollection
dmcqMetrics f x =
    (\y -> x { _dmcqMetrics = y })
       <$> f (_dmcqMetrics x)
{-# INLINE dmcqMetrics #-}

instance ToQuery DisableMetricsCollection where
    toQuery = genericQuery def

data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse
    deriving (Eq, Show, Generic)

instance AWSRequest DisableMetricsCollection where
    type Sv DisableMetricsCollection = AutoScaling
    type Rs DisableMetricsCollection = DisableMetricsCollectionResponse

    request = post "DisableMetricsCollection"
    response _ = nullaryResponse DisableMetricsCollectionResponse
