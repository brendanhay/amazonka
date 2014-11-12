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
      DisableMetricsCollectionQuery
    -- ** Request constructor
    , disableMetricsCollectionQuery
    -- ** Request lenses
    , dmcqAutoScalingGroupName
    , dmcqMetrics

    -- * Response
    , DisableMetricsCollectionResponse
    -- ** Response constructor
    , disableMetricsCollectionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types

data DisableMetricsCollectionQuery = DisableMetricsCollectionQuery
    { _dmcqAutoScalingGroupName :: Text
    , _dmcqMetrics              :: [Text]
    } (Eq, Ord, Show, Generic)

-- | 'DisableMetricsCollectionQuery' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmcqAutoScalingGroupName' @::@ 'Text'
--
-- * 'dmcqMetrics' @::@ ['Text']
--
disableMetricsCollectionQuery :: Text -- ^ 'dmcqAutoScalingGroupName'
                              -> DisableMetricsCollectionQuery
disableMetricsCollectionQuery p1 = DisableMetricsCollectionQuery
    { _dmcqAutoScalingGroupName = p1
    , _dmcqMetrics              = mempty
    }

-- | The name or ARN of the Auto Scaling Group.
dmcqAutoScalingGroupName :: Lens' DisableMetricsCollectionQuery Text
dmcqAutoScalingGroupName =
    lens _dmcqAutoScalingGroupName
        (\s a -> s { _dmcqAutoScalingGroupName = a })

-- | The list of metrics to disable. If no metrics are specified, all metrics
-- are disabled. The following metrics are supported: GroupMinSize
-- GroupMaxSize GroupDesiredCapacity GroupInServiceInstances
-- GroupPendingInstances GroupStandbyInstances GroupTerminatingInstances
-- GroupTotalInstances.
dmcqMetrics :: Lens' DisableMetricsCollectionQuery [Text]
dmcqMetrics = lens _dmcqMetrics (\s a -> s { _dmcqMetrics = a })
instance ToQuery DisableMetricsCollectionQuery

instance ToPath DisableMetricsCollectionQuery where
    toPath = const "/"

data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'DisableMetricsCollectionResponse' constructor.
disableMetricsCollectionResponse :: DisableMetricsCollectionResponse
disableMetricsCollectionResponse = DisableMetricsCollectionResponse

instance FromXML DisableMetricsCollectionResponse where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DisableMetricsCollectionResponse"

instance AWSRequest DisableMetricsCollectionQuery where
    type Sv DisableMetricsCollectionQuery = AutoScaling
    type Rs DisableMetricsCollectionQuery = DisableMetricsCollectionResponse

    request  = post "DisableMetricsCollection"
    response = nullaryResponse DisableMetricsCollectionResponse
