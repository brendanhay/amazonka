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

-- Module      : Network.AWS.AutoScaling.DisableMetricsCollection
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | Disables monitoring of the specified metrics for the specified Auto Scaling
-- group.
--
-- <http://docs.aws.amazon.com/AutoScaling/latest/APIReference/API_DisableMetricsCollection.html>
module Network.AWS.AutoScaling.DisableMetricsCollection
    (
    -- * Request
      DisableMetricsCollection
    -- ** Request constructor
    , disableMetricsCollection
    -- ** Request lenses
    , dmcAutoScalingGroupName
    , dmcMetrics

    -- * Response
    , DisableMetricsCollectionResponse
    -- ** Response constructor
    , disableMetricsCollectionResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.Types
import qualified GHC.Exts

data DisableMetricsCollection = DisableMetricsCollection
    { _dmcAutoScalingGroupName :: Text
    , _dmcMetrics              :: List "member" Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DisableMetricsCollection' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dmcAutoScalingGroupName' @::@ 'Text'
--
-- * 'dmcMetrics' @::@ ['Text']
--
disableMetricsCollection :: Text -- ^ 'dmcAutoScalingGroupName'
                         -> DisableMetricsCollection
disableMetricsCollection p1 = DisableMetricsCollection
    { _dmcAutoScalingGroupName = p1
    , _dmcMetrics              = mempty
    }

-- | The name or Amazon Resource Name (ARN) of the group.
dmcAutoScalingGroupName :: Lens' DisableMetricsCollection Text
dmcAutoScalingGroupName =
    lens _dmcAutoScalingGroupName (\s a -> s { _dmcAutoScalingGroupName = a })

-- | One or more of the following metrics:
--
-- GroupMinSize
--
-- GroupMaxSize
--
-- GroupDesiredCapacity
--
-- GroupInServiceInstances
--
-- GroupPendingInstances
--
-- GroupStandbyInstances
--
-- GroupTerminatingInstances
--
-- GroupTotalInstances
--
-- If you omit this parameter, all metrics are disabled.
dmcMetrics :: Lens' DisableMetricsCollection [Text]
dmcMetrics = lens _dmcMetrics (\s a -> s { _dmcMetrics = a }) . _List

data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse
    deriving (Eq, Ord, Read, Show, Generic)

-- | 'DisableMetricsCollectionResponse' constructor.
disableMetricsCollectionResponse :: DisableMetricsCollectionResponse
disableMetricsCollectionResponse = DisableMetricsCollectionResponse

instance ToPath DisableMetricsCollection where
    toPath = const "/"

instance ToQuery DisableMetricsCollection where
    toQuery DisableMetricsCollection{..} = mconcat
        [ "AutoScalingGroupName" =? _dmcAutoScalingGroupName
        , "Metrics"              =? _dmcMetrics
        ]

instance ToHeaders DisableMetricsCollection

instance AWSRequest DisableMetricsCollection where
    type Sv DisableMetricsCollection = AutoScaling
    type Rs DisableMetricsCollection = DisableMetricsCollectionResponse

    request  = post "DisableMetricsCollection"
    response = nullResponse DisableMetricsCollectionResponse
