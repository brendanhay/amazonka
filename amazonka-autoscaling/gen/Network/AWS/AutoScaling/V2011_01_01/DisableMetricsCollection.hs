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
    , mkDisableMetricsCollectionQuery
    -- ** Request lenses
    , dmcqAutoScalingGroupName
    , dmcqMetrics

    -- * Response
    , DisableMetricsCollectionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableMetricsCollection' request.
mkDisableMetricsCollectionQuery :: Text -- ^ 'dmcqAutoScalingGroupName'
                                -> DisableMetricsCollection
mkDisableMetricsCollectionQuery p1 = DisableMetricsCollection
    { _dmcqAutoScalingGroupName = p1
    , _dmcqMetrics = mempty
    }
{-# INLINE mkDisableMetricsCollectionQuery #-}

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
dmcqAutoScalingGroupName :: Lens' DisableMetricsCollection (Text)
dmcqAutoScalingGroupName = lens _dmcqAutoScalingGroupName (\s a -> s { _dmcqAutoScalingGroupName = a })
{-# INLINE dmcqAutoScalingGroupName #-}

-- | The list of metrics to disable. If no metrics are specified, all metrics
-- are disabled. The following metrics are supported: GroupMinSize
-- GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
-- GroupPendingInstances GroupStandbyInstances GroupTerminatingInstances
-- GroupTotalInstances.
dmcqMetrics :: Lens' DisableMetricsCollection ([Text])
dmcqMetrics = lens _dmcqMetrics (\s a -> s { _dmcqMetrics = a })
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
