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

-- Module      : Network.AWS.ElastiCache.DescribeCacheParameters
-- Copyright   : (c) 2013-2015 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- | The /DescribeCacheParameters/ operation returns the detailed parameter list for
-- a particular cache parameter group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheParameters.html>
module Network.AWS.ElastiCache.DescribeCacheParameters
    (
    -- * Request
      DescribeCacheParameters
    -- ** Request constructor
    , describeCacheParameters
    -- ** Request lenses
    , dcpCacheParameterGroupName
    , dcpMarker
    , dcpMaxRecords
    , dcpSource

    -- * Response
    , DescribeCacheParametersResponse
    -- ** Response constructor
    , describeCacheParametersResponse
    -- ** Response lenses
    , dcprCacheNodeTypeSpecificParameters
    , dcprMarker
    , dcprParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types
import qualified GHC.Exts

data DescribeCacheParameters = DescribeCacheParameters
    { _dcpCacheParameterGroupName :: Text
    , _dcpMarker                  :: Maybe Text
    , _dcpMaxRecords              :: Maybe Int
    , _dcpSource                  :: Maybe Text
    } deriving (Eq, Ord, Read, Show)

-- | 'DescribeCacheParameters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpCacheParameterGroupName' @::@ 'Text'
--
-- * 'dcpMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcpMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dcpSource' @::@ 'Maybe' 'Text'
--
describeCacheParameters :: Text -- ^ 'dcpCacheParameterGroupName'
                        -> DescribeCacheParameters
describeCacheParameters p1 = DescribeCacheParameters
    { _dcpCacheParameterGroupName = p1
    , _dcpSource                  = Nothing
    , _dcpMaxRecords              = Nothing
    , _dcpMarker                  = Nothing
    }

-- | The name of a specific cache parameter group to return details for.
dcpCacheParameterGroupName :: Lens' DescribeCacheParameters Text
dcpCacheParameterGroupName =
    lens _dcpCacheParameterGroupName
        (\s a -> s { _dcpCacheParameterGroupName = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcpMarker :: Lens' DescribeCacheParameters (Maybe Text)
dcpMarker = lens _dcpMarker (\s a -> s { _dcpMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified 'MaxRecords' value, a marker is included in the
-- response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcpMaxRecords :: Lens' DescribeCacheParameters (Maybe Int)
dcpMaxRecords = lens _dcpMaxRecords (\s a -> s { _dcpMaxRecords = a })

-- | The parameter types to return.
--
-- Valid values: 'user' | 'system' | 'engine-default'
dcpSource :: Lens' DescribeCacheParameters (Maybe Text)
dcpSource = lens _dcpSource (\s a -> s { _dcpSource = a })

data DescribeCacheParametersResponse = DescribeCacheParametersResponse
    { _dcprCacheNodeTypeSpecificParameters :: List "member" CacheNodeTypeSpecificParameter
    , _dcprMarker                          :: Maybe Text
    , _dcprParameters                      :: List "member" Parameter
    } deriving (Eq, Read, Show)

-- | 'DescribeCacheParametersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcprCacheNodeTypeSpecificParameters' @::@ ['CacheNodeTypeSpecificParameter']
--
-- * 'dcprMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcprParameters' @::@ ['Parameter']
--
describeCacheParametersResponse :: DescribeCacheParametersResponse
describeCacheParametersResponse = DescribeCacheParametersResponse
    { _dcprMarker                          = Nothing
    , _dcprParameters                      = mempty
    , _dcprCacheNodeTypeSpecificParameters = mempty
    }

-- | A list of parameters specific to a particular cache node type. Each element
-- in the list contains detailed information about one parameter.
dcprCacheNodeTypeSpecificParameters :: Lens' DescribeCacheParametersResponse [CacheNodeTypeSpecificParameter]
dcprCacheNodeTypeSpecificParameters =
    lens _dcprCacheNodeTypeSpecificParameters
        (\s a -> s { _dcprCacheNodeTypeSpecificParameters = a })
            . _List

-- | Provides an identifier to allow retrieval of paginated results.
dcprMarker :: Lens' DescribeCacheParametersResponse (Maybe Text)
dcprMarker = lens _dcprMarker (\s a -> s { _dcprMarker = a })

-- | A list of 'Parameter' instances.
dcprParameters :: Lens' DescribeCacheParametersResponse [Parameter]
dcprParameters = lens _dcprParameters (\s a -> s { _dcprParameters = a }) . _List

instance ToPath DescribeCacheParameters where
    toPath = const "/"

instance ToQuery DescribeCacheParameters where
    toQuery DescribeCacheParameters{..} = mconcat
        [ "CacheParameterGroupName" =? _dcpCacheParameterGroupName
        , "Marker"                  =? _dcpMarker
        , "MaxRecords"              =? _dcpMaxRecords
        , "Source"                  =? _dcpSource
        ]

instance ToHeaders DescribeCacheParameters

instance AWSRequest DescribeCacheParameters where
    type Sv DescribeCacheParameters = ElastiCache
    type Rs DescribeCacheParameters = DescribeCacheParametersResponse

    request  = post "DescribeCacheParameters"
    response = xmlResponse

instance FromXML DescribeCacheParametersResponse where
    parseXML = withElement "DescribeCacheParametersResult" $ \x -> DescribeCacheParametersResponse
        <$> x .@? "CacheNodeTypeSpecificParameters" .!@ mempty
        <*> x .@? "Marker"
        <*> x .@? "Parameters" .!@ mempty

instance AWSPager DescribeCacheParameters where
    page rq rs
        | stop (rs ^. dcprMarker) = Nothing
        | otherwise = (\x -> rq & dcpMarker ?~ x)
            <$> (rs ^. dcprMarker)
