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

-- Module      : Network.AWS.CloudWatch.ListMetrics
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
-- a given metric.
module Network.AWS.CloudWatch.ListMetrics
    (
    -- * Request
      ListMetricsInput
    -- ** Request constructor
    , listMetrics
    -- ** Request lenses
    , lmiDimensions
    , lmiMetricName
    , lmiNamespace
    , lmiNextToken

    -- * Response
    , ListMetricsOutput
    -- ** Response constructor
    , listMetricsResponse
    -- ** Response lenses
    , lmoMetrics
    , lmoNextToken
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.CloudWatch.Types

data ListMetricsInput = ListMetricsInput
    { _lmiDimensions :: [DimensionFilter]
    , _lmiMetricName :: Maybe Text
    , _lmiNamespace  :: Maybe Text
    , _lmiNextToken  :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListMetricsInput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmiDimensions' @::@ ['DimensionFilter']
--
-- * 'lmiMetricName' @::@ 'Maybe' 'Text'
--
-- * 'lmiNamespace' @::@ 'Maybe' 'Text'
--
-- * 'lmiNextToken' @::@ 'Maybe' 'Text'
--
listMetrics :: ListMetricsInput
listMetrics = ListMetricsInput
    { _lmiNamespace  = Nothing
    , _lmiMetricName = Nothing
    , _lmiDimensions = mempty
    , _lmiNextToken  = Nothing
    }

-- | A list of dimensions to filter against.
lmiDimensions :: Lens' ListMetricsInput [DimensionFilter]
lmiDimensions = lens _lmiDimensions (\s a -> s { _lmiDimensions = a })

-- | The name of the metric to filter against.
lmiMetricName :: Lens' ListMetricsInput (Maybe Text)
lmiMetricName = lens _lmiMetricName (\s a -> s { _lmiMetricName = a })

-- | The namespace to filter against.
lmiNamespace :: Lens' ListMetricsInput (Maybe Text)
lmiNamespace = lens _lmiNamespace (\s a -> s { _lmiNamespace = a })

-- | The token returned by a previous call to indicate that there is more data
-- available.
lmiNextToken :: Lens' ListMetricsInput (Maybe Text)
lmiNextToken = lens _lmiNextToken (\s a -> s { _lmiNextToken = a })

instance ToQuery ListMetricsInput

instance ToPath ListMetricsInput where
    toPath = const "/"

data ListMetricsOutput = ListMetricsOutput
    { _lmoMetrics   :: [Metric]
    , _lmoNextToken :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | 'ListMetricsOutput' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lmoMetrics' @::@ ['Metric']
--
-- * 'lmoNextToken' @::@ 'Maybe' 'Text'
--
listMetricsResponse :: ListMetricsOutput
listMetricsResponse = ListMetricsOutput
    { _lmoMetrics   = mempty
    , _lmoNextToken = Nothing
    }

-- | A list of metrics used to generate statistics for an AWS account.
lmoMetrics :: Lens' ListMetricsOutput [Metric]
lmoMetrics = lens _lmoMetrics (\s a -> s { _lmoMetrics = a })

-- | A string that marks the start of the next batch of returned results.
lmoNextToken :: Lens' ListMetricsOutput (Maybe Text)
lmoNextToken = lens _lmoNextToken (\s a -> s { _lmoNextToken = a })

instance FromXML ListMetricsOutput where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "ListMetricsOutput"

instance AWSRequest ListMetricsInput where
    type Sv ListMetricsInput = CloudWatch
    type Rs ListMetricsInput = ListMetricsOutput

    request  = post "ListMetrics"
    response = xmlResponse $ \h x -> ListMetricsOutput
        <$> x %| "Metrics"
        <*> x %| "NextToken"
