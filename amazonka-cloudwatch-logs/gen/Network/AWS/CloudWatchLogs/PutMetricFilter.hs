{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

{-# OPTIONS_GHC -w                      #-}

-- Module      : Network.AWS.CloudWatchLogs.PutMetricFilter
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Creates or updates a metric filter and associates it with the specified log
-- group. Metric filters allow you to configure rules to extract metric data
-- from log events ingested through PutLogEvents requests.
module Network.AWS.CloudWatchLogs.PutMetricFilter
    (
    -- * Request
      PutMetricFilter
    -- ** Request constructor
    , putMetricFilter
    -- ** Request lenses
    , pmfFilterName
    , pmfFilterPattern
    , pmfLogGroupName
    , pmfMetricTransformations

    -- * Response
    , PutMetricFilterResponse
    -- ** Response constructor
    , putMetricFilterResponse
    ) where

import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.CloudWatchLogs.Types

data PutMetricFilter = PutMetricFilter
    { _pmfFilterName            :: Text
    , _pmfFilterPattern         :: Text
    , _pmfLogGroupName          :: Text
    , _pmfMetricTransformations :: List1 MetricTransformation
    } deriving (Eq, Show, Generic)

-- | 'PutMetricFilter' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'pmfFilterName' @::@ 'Text'
--
-- * 'pmfFilterPattern' @::@ 'Text'
--
-- * 'pmfLogGroupName' @::@ 'Text'
--
-- * 'pmfMetricTransformations' @::@ 'NonEmpty' 'MetricTransformation'
--
putMetricFilter :: Text -- ^ 'pmfLogGroupName'
                -> Text -- ^ 'pmfFilterName'
                -> Text -- ^ 'pmfFilterPattern'
                -> NonEmpty MetricTransformation -- ^ 'pmfMetricTransformations'
                -> PutMetricFilter
putMetricFilter p1 p2 p3 p4 = PutMetricFilter
    { _pmfLogGroupName          = p1
    , _pmfFilterName            = p2
    , _pmfFilterPattern         = p3
    , _pmfMetricTransformations = withIso _List1 (const id) p4
    }

pmfFilterName :: Lens' PutMetricFilter Text
pmfFilterName = lens _pmfFilterName (\s a -> s { _pmfFilterName = a })

pmfFilterPattern :: Lens' PutMetricFilter Text
pmfFilterPattern = lens _pmfFilterPattern (\s a -> s { _pmfFilterPattern = a })

pmfLogGroupName :: Lens' PutMetricFilter Text
pmfLogGroupName = lens _pmfLogGroupName (\s a -> s { _pmfLogGroupName = a })

pmfMetricTransformations :: Lens' PutMetricFilter (NonEmpty MetricTransformation)
pmfMetricTransformations =
    lens _pmfMetricTransformations
        (\s a -> s { _pmfMetricTransformations = a })
            . _List1

instance ToPath PutMetricFilter where
    toPath = const "/"

instance ToQuery PutMetricFilter where
    toQuery = const mempty

instance ToHeaders PutMetricFilter

instance ToBody PutMetricFilter where
    toBody = toBody . encode . _pmfLogGroupName

data PutMetricFilterResponse = PutMetricFilterResponse
    deriving (Eq, Ord, Show, Generic)

-- | 'PutMetricFilterResponse' constructor.
putMetricFilterResponse :: PutMetricFilterResponse
putMetricFilterResponse = PutMetricFilterResponse

instance AWSRequest PutMetricFilter where
    type Sv PutMetricFilter = CloudWatchLogs
    type Rs PutMetricFilter = PutMetricFilterResponse

    request  = post
    response = nullaryResponse PutMetricFilterResponse
