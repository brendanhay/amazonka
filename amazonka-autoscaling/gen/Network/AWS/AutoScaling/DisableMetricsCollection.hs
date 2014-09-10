{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.DisableMetricsCollection
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
module Network.AWS.AutoScaling.DisableMetricsCollection
    (
    -- * Request
      DisableMetricsCollection
    -- ** Request constructor
    , mkDisableMetricsCollection
    -- ** Request lenses
    , dmcAutoScalingGroupName
    , dmcMetrics

    -- * Response
    , DisableMetricsCollectionResponse
    -- ** Response constructor
    , mkDisableMetricsCollectionResponse
    ) where

import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import Network.AWS.Prelude

data DisableMetricsCollection = DisableMetricsCollection
    { _dmcAutoScalingGroupName :: !Text
    , _dmcMetrics :: [Text]
    } deriving (Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableMetricsCollection' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @AutoScalingGroupName ::@ @Text@
--
-- * @Metrics ::@ @[Text]@
--
mkDisableMetricsCollection :: Text -- ^ 'dmcAutoScalingGroupName'
                           -> DisableMetricsCollection
mkDisableMetricsCollection p1 = DisableMetricsCollection
    { _dmcAutoScalingGroupName = p1
    , _dmcMetrics = mempty
    }

-- | The name or ARN of the Auto Scaling Group.
dmcAutoScalingGroupName :: Lens' DisableMetricsCollection Text
dmcAutoScalingGroupName =
    lens _dmcAutoScalingGroupName
         (\s a -> s { _dmcAutoScalingGroupName = a })

-- | The list of metrics to disable. If no metrics are specified, all metrics
-- are disabled. The following metrics are supported: GroupMinSize
-- GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
-- GroupPendingInstances GroupStandbyInstances GroupTerminatingInstances
-- GroupTotalInstances.
dmcMetrics :: Lens' DisableMetricsCollection [Text]
dmcMetrics = lens _dmcMetrics (\s a -> s { _dmcMetrics = a })

instance ToQuery DisableMetricsCollection where
    toQuery = genericQuery def

data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse
    deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'DisableMetricsCollectionResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
mkDisableMetricsCollectionResponse :: DisableMetricsCollectionResponse
mkDisableMetricsCollectionResponse = DisableMetricsCollectionResponse

instance AWSRequest DisableMetricsCollection where
    type Sv DisableMetricsCollection = AutoScaling
    type Rs DisableMetricsCollection = DisableMetricsCollectionResponse

    request = post "DisableMetricsCollection"
    response _ = nullaryResponse DisableMetricsCollectionResponse
