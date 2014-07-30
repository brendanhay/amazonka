{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.CloudWatch.V2010_08_01.ListMetrics
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Returns a list of valid metrics stored for the AWS account owner. Returned
-- metrics can be used with GetMetricStatistics to obtain statistical data for
-- a given metric. Up to 500 results are returned for any one call. To
-- retrieve further results, use returned NextToken values with subsequent
-- ListMetrics operations. If you create a metric with the PutMetricData
-- action, allow up to fifteen minutes for the metric to appear in calls to
-- the ListMetrics action.
module Network.AWS.CloudWatch.V2010_08_01.ListMetrics where

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
import           Network.AWS.CloudWatch.V2010_08_01.Types
import           Network.HTTP.Client  (RequestBody, Response)
import           Prelude              hiding (head)

-- | Minimum specification for a 'ListMetrics' request.
listMetrics :: ListMetrics
listMetrics = ListMetrics
    { _lmiDimensions = mempty
    , _lmiMetricName = Nothing
    , _lmiNamespace = Nothing
    , _lmiNextToken = Nothing
    }

data ListMetrics = ListMetrics
    { _lmiDimensions :: [DimensionFilter]
      -- ^ A list of dimensions to filter against.
    , _lmiMetricName :: Maybe Text
      -- ^ The name of the metric to filter against.
    , _lmiNamespace :: Maybe Text
      -- ^ The namespace to filter against.
    , _lmiNextToken :: Maybe Text
      -- ^ The token returned by a previous call to indicate that there is
      -- more data available.
    } deriving (Generic)

instance ToQuery ListMetrics where
    toQuery = genericToQuery def

instance AWSRequest ListMetrics where
    type Sv ListMetrics = CloudWatch
    type Rs ListMetrics = ListMetricsResponse

    request = post "ListMetrics"
    response _ = xmlResponse

instance AWSPager ListMetrics where
    next rq rs = (\x -> rq { _lmiNextToken = Just x })
        <$> _lmoNextToken rs

data ListMetricsResponse = ListMetricsResponse
    { _lmoMetrics :: [Metric]
      -- ^ A list of metrics used to generate statistics for an AWS account.
    , _lmoNextToken :: Maybe Text
      -- ^ A string that marks the start of the next batch of returned
      -- results.
    } deriving (Generic)

instance FromXML ListMetricsResponse where
    fromXMLOptions = xmlOptions
