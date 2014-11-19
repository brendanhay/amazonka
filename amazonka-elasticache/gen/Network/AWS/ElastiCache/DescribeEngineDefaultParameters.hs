{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

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
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeEngineDefaultParameters.html>
module Network.AWS.ElastiCache.DescribeEngineDefaultParameters
    (
    -- * Request
      DescribeEngineDefaultParameters
    -- ** Request constructor
    , describeEngineDefaultParameters
    -- ** Request lenses
    , dedpCacheParameterGroupFamily
    , dedpMarker
    , dedpMaxRecords

    -- * Response
    , DescribeEngineDefaultParametersResponse
    -- ** Response constructor
    , describeEngineDefaultParametersResponse
    -- ** Response lenses
    , dedprEngineDefaults
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters
    { _dedpCacheParameterGroupFamily :: Text
    , _dedpMarker                    :: Maybe Text
    , _dedpMaxRecords                :: Maybe Int
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeEngineDefaultParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedpCacheParameterGroupFamily' @::@ 'Text'
--
-- * 'dedpMarker' @::@ 'Maybe' 'Text'
--
-- * 'dedpMaxRecords' @::@ 'Maybe' 'Int'
--
describeEngineDefaultParameters :: Text -- ^ 'dedpCacheParameterGroupFamily'
                                -> DescribeEngineDefaultParameters
describeEngineDefaultParameters p1 = DescribeEngineDefaultParameters
    { _dedpCacheParameterGroupFamily = p1
    , _dedpMaxRecords                = Nothing
    , _dedpMarker                    = Nothing
    }

-- | The name of the cache parameter group family. Valid values are:
-- memcached1.4 | redis2.6 | redis2.8.
dedpCacheParameterGroupFamily :: Lens' DescribeEngineDefaultParameters Text
dedpCacheParameterGroupFamily =
    lens _dedpCacheParameterGroupFamily
        (\s a -> s { _dedpCacheParameterGroupFamily = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dedpMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedpMarker = lens _dedpMarker (\s a -> s { _dedpMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dedpMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Int)
dedpMaxRecords = lens _dedpMaxRecords (\s a -> s { _dedpMaxRecords = a })

newtype DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse
    { _dedprEngineDefaults :: EngineDefaults
    } deriving (Eq, Show, Generic)

-- | 'DescribeEngineDefaultParametersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedprEngineDefaults' @::@ 'EngineDefaults'
--
describeEngineDefaultParametersResponse :: EngineDefaults -- ^ 'dedprEngineDefaults'
                                        -> DescribeEngineDefaultParametersResponse
describeEngineDefaultParametersResponse p1 = DescribeEngineDefaultParametersResponse
    { _dedprEngineDefaults = p1
    }

dedprEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse EngineDefaults
dedprEngineDefaults =
    lens _dedprEngineDefaults (\s a -> s { _dedprEngineDefaults = a })

instance ToPath DescribeEngineDefaultParameters where
    toPath = const "/"

instance ToQuery DescribeEngineDefaultParameters

instance ToHeaders DescribeEngineDefaultParameters

instance AWSRequest DescribeEngineDefaultParameters where
    type Sv DescribeEngineDefaultParameters = ElastiCache
    type Rs DescribeEngineDefaultParameters = DescribeEngineDefaultParametersResponse

    request  = post "DescribeEngineDefaultParameters"
    response = xmlResponse

instance FromXML DescribeEngineDefaultParametersResponse where
    parseXML = withElement "DescribeEngineDefaultParametersResult" $ \x ->
            <$> x .@ "EngineDefaults"

instance AWSPager DescribeEngineDefaultParameters where
    next rq rs = (\x -> rq & dedpMarker ?~ x)
        <$> (rs ^. dedprEngineDefaults . edMarker)
