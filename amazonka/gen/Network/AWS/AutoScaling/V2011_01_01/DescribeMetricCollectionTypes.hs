{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.AutoScaling.V2011_01_01.DescribeMetricCollectionTypes
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of metrics and a corresponding list of granularities for
-- each metric.
-- https://autoscaling.amazonaws.com/?Version=2011-01-01&Action=DescribeMetricCollectionTypes
-- &AUTHPARAMS oc/2011-01-01/"> GroupMinSize GroupMaxSize GroupDesiredCapacity
-- GroupInServiceInstances GroupPendingInstances GroupTerminatingInstances
-- GroupTotalInstances 1Minute 07f3fea2-bf3c-11e2-9b6f-f3cdbb80c073.
module Network.AWS.AutoScaling.V2011_01_01.DescribeMetricCollectionTypes where

import           Control.Applicative
import           Data.ByteString      (ByteString)
import           Data.Default
import           Data.HashMap.Strict  (HashMap)
import           Data.Monoid
import           Data.Text            (Text)
import qualified Data.Text            as Text
import           GHC.Generics
import           Network.AWS.Data
import           Network.AWS.Response
import           Network.AWS.Types    hiding (Error, Endpoint, Region)
import           Network.AWS.Request.Query
import           Network.AWS.AutoScaling.V2011_01_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

data DescribeMetricCollectionTypes = DescribeMetricCollectionTypes
    deriving (Eq, Show, Generic)

instance ToQuery DescribeMetricCollectionTypes where
    toQuery = genericToQuery def

instance AWSRequest DescribeMetricCollectionTypes where
    type Sv DescribeMetricCollectionTypes = AutoScaling
    type Rs DescribeMetricCollectionTypes = DescribeMetricCollectionTypesResponse

    request = post "DescribeMetricCollectionTypes"
    response _ = xmlResponse

data DescribeMetricCollectionTypesResponse = DescribeMetricCollectionTypesResponse
    { _dmctaMetrics :: [MetricCollectionType]
      -- ^ The list of Metrics collected. The following metrics are
      -- supported: GroupMinSize GroupMaxSize GroupDesiredCapacity
      -- GroupInServiceInstances GroupPendingInstances
      -- GroupTerminatingInstances GroupTotalInstances.
    , _dmctaGranularities :: [MetricGranularityType]
      -- ^ A list of granularities for the listed Metrics.
    } deriving (Generic)

instance FromXML DescribeMetricCollectionTypesResponse where
    fromXMLOptions = xmlOptions
