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

-- Module      : Network.AWS.ElastiCache.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeEngineDefaultParameters operation returns the default engine
-- and system parameter information for the specified cache engine.
module Network.AWS.ElastiCache.DescribeEngineDefaultParameters
    (
    -- * Request
      DescribeEngineDefaultParametersMessage
    -- ** Request constructor
    , describeEngineDefaultParametersMessage
    -- ** Request lenses
    , dedpmCacheParameterGroupFamily
    , dedpmMarker
    , dedpmMaxRecords

    -- * Response
    , DescribeEngineDefaultParametersResult
    -- ** Response constructor
    , describeEngineDefaultParametersResult
    -- ** Response lenses
    , dedprEngineDefaults
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeEngineDefaultParametersMessage = DescribeEngineDefaultParametersMessage
    { _dedpmCacheParameterGroupFamily :: Text
    , _dedpmMarker                    :: Maybe Text
    , _dedpmMaxRecords                :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEngineDefaultParametersMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedpmCacheParameterGroupFamily' @::@ 'Text'
--
-- * 'dedpmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dedpmMaxRecords' @::@ 'Maybe' 'Int'
--
describeEngineDefaultParametersMessage :: Text -- ^ 'dedpmCacheParameterGroupFamily'
                                       -> DescribeEngineDefaultParametersMessage
describeEngineDefaultParametersMessage p1 = DescribeEngineDefaultParametersMessage
    { _dedpmCacheParameterGroupFamily = p1
    , _dedpmMaxRecords                = Nothing
    , _dedpmMarker                    = Nothing
    }

-- | The name of the cache parameter group family. Valid values are:
-- memcached1.4 | redis2.6 | redis2.8.
dedpmCacheParameterGroupFamily :: Lens' DescribeEngineDefaultParametersMessage Text
dedpmCacheParameterGroupFamily =
    lens _dedpmCacheParameterGroupFamily
        (\s a -> s { _dedpmCacheParameterGroupFamily = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dedpmMarker :: Lens' DescribeEngineDefaultParametersMessage (Maybe Text)
dedpmMarker = lens _dedpmMarker (\s a -> s { _dedpmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dedpmMaxRecords :: Lens' DescribeEngineDefaultParametersMessage (Maybe Int)
dedpmMaxRecords = lens _dedpmMaxRecords (\s a -> s { _dedpmMaxRecords = a })

instance ToQuery DescribeEngineDefaultParametersMessage

instance ToPath DescribeEngineDefaultParametersMessage where
    toPath = const "/"

newtype DescribeEngineDefaultParametersResult = DescribeEngineDefaultParametersResult
    { _dedprEngineDefaults :: Maybe EngineDefaults
    } deriving (Eq, Show, Generic)

-- | 'DescribeEngineDefaultParametersResult' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedprEngineDefaults' @::@ 'Maybe' 'EngineDefaults'
--
describeEngineDefaultParametersResult :: DescribeEngineDefaultParametersResult
describeEngineDefaultParametersResult = DescribeEngineDefaultParametersResult
    { _dedprEngineDefaults = Nothing
    }

dedprEngineDefaults :: Lens' DescribeEngineDefaultParametersResult (Maybe EngineDefaults)
dedprEngineDefaults =
    lens _dedprEngineDefaults (\s a -> s { _dedprEngineDefaults = a })

instance FromXML DescribeEngineDefaultParametersResult where
    fromXMLOptions = xmlOptions
    fromXMLRoot    = fromRoot "DescribeEngineDefaultParametersResult"

instance AWSRequest DescribeEngineDefaultParametersMessage where
    type Sv DescribeEngineDefaultParametersMessage = ElastiCache
    type Rs DescribeEngineDefaultParametersMessage = DescribeEngineDefaultParametersResult

    request  = post "DescribeEngineDefaultParameters"
    response = xmlResponse $ \h x -> DescribeEngineDefaultParametersResult
        <$> x %| "EngineDefaults"
