{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TemplateHaskell   #-}
{-# LANGUAGE TypeFamilies      #-}

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
module Network.AWS.AutoScaling.V2011_01_01.DisableMetricsCollection where

import Control.Lens.TH (makeLenses)
import Network.AWS.Request.Query
import Network.AWS.AutoScaling.V2011_01_01.Types
import Network.AWS.Prelude

-- | Minimum specification for a 'DisableMetricsCollection' request.
disableMetricsCollection :: Text -- ^ '_dmcqAutoScalingGroupName'
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
      -- GroupTerminatingInstances GroupTotalInstances.
    } deriving (Show, Generic)

makeLenses ''DisableMetricsCollection

instance ToQuery DisableMetricsCollection where
    toQuery = genericToQuery def

data DisableMetricsCollectionResponse = DisableMetricsCollectionResponse
    deriving (Eq, Show, Generic)

makeLenses ''DisableMetricsCollectionResponse

instance AWSRequest DisableMetricsCollection where
    type Sv DisableMetricsCollection = AutoScaling
    type Rs DisableMetricsCollection = DisableMetricsCollectionResponse

    request = post "DisableMetricsCollection"
    response _ _ = return (Right DisableMetricsCollectionResponse)
