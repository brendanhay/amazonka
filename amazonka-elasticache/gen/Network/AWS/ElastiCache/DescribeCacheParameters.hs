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

-- Module      : Network.AWS.ElastiCache.DescribeCacheParameters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | The DescribeCacheParameters operation returns the detailed parameter list
-- for a particular cache parameter group.
module Network.AWS.ElastiCache.DescribeCacheParameters
    (
    -- * Request
      DescribeCacheParametersMessage
    -- ** Request constructor
    , describeCacheParametersMessage
    -- ** Request lenses
    , dcpmCacheParameterGroupName
    , dcpmMarker
    , dcpmMaxRecords
    , dcpmSource

    -- * Response
    , CacheParameterGroupDetails
    -- ** Response constructor
    , cacheParameterGroupDetails
    -- ** Response lenses
    , cpgdCacheNodeTypeSpecificParameters
    , cpgdMarker
    , cpgdParameters
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.Query
import Network.AWS.ElastiCache.Types

data DescribeCacheParametersMessage = DescribeCacheParametersMessage
    { _dcpmCacheParameterGroupName :: Text
    , _dcpmMarker                  :: Maybe Text
    , _dcpmMaxRecords              :: Maybe Int
    , _dcpmSource                  :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'DescribeCacheParametersMessage' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpmCacheParameterGroupName' @::@ 'Text'
--
-- * 'dcpmMarker' @::@ 'Maybe' 'Text'
--
-- * 'dcpmMaxRecords' @::@ 'Maybe' 'Int'
--
-- * 'dcpmSource' @::@ 'Maybe' 'Text'
--
describeCacheParametersMessage :: Text -- ^ 'dcpmCacheParameterGroupName'
                               -> DescribeCacheParametersMessage
describeCacheParametersMessage p1 = DescribeCacheParametersMessage
    { _dcpmCacheParameterGroupName = p1
    , _dcpmSource                  = Nothing
    , _dcpmMaxRecords              = Nothing
    , _dcpmMarker                  = Nothing
    }

-- | The name of a specific cache parameter group to return details for.
dcpmCacheParameterGroupName :: Lens' DescribeCacheParametersMessage Text
dcpmCacheParameterGroupName =
    lens _dcpmCacheParameterGroupName
        (\s a -> s { _dcpmCacheParameterGroupName = a })

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this operation. If this parameter is
-- specified, the response includes only records beyond the marker, up to
-- the value specified by MaxRecords.
dcpmMarker :: Lens' DescribeCacheParametersMessage (Maybe Text)
dcpmMarker = lens _dcpmMarker (\s a -> s { _dcpmMarker = a })

-- | The maximum number of records to include in the response. If more records
-- exist than the specified MaxRecords value, a marker is included in the
-- response so that the remaining results can be retrieved. Default: 100
-- Constraints: minimum 20; maximum 100.
dcpmMaxRecords :: Lens' DescribeCacheParametersMessage (Maybe Int)
dcpmMaxRecords = lens _dcpmMaxRecords (\s a -> s { _dcpmMaxRecords = a })

-- | The parameter types to return. Valid values: user | system |
-- engine-default.
dcpmSource :: Lens' DescribeCacheParametersMessage (Maybe Text)
dcpmSource = lens _dcpmSource (\s a -> s { _dcpmSource = a })

instance ToPath DescribeCacheParametersMessage where
    toPath = const "/"

instance ToQuery DescribeCacheParametersMessage

data CacheParameterGroupDetails = CacheParameterGroupDetails
    { _cpgdCacheNodeTypeSpecificParameters :: [CacheNodeTypeSpecificParameter]
    , _cpgdMarker                          :: Maybe Text
    , _cpgdParameters                      :: [Parameter]
    } deriving (Eq, Show, Generic)

-- | 'CacheParameterGroupDetails' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'cpgdCacheNodeTypeSpecificParameters' @::@ ['CacheNodeTypeSpecificParameter']
--
-- * 'cpgdMarker' @::@ 'Maybe' 'Text'
--
-- * 'cpgdParameters' @::@ ['Parameter']
--
cacheParameterGroupDetails :: CacheParameterGroupDetails
cacheParameterGroupDetails = CacheParameterGroupDetails
    { _cpgdMarker                          = Nothing
    , _cpgdParameters                      = mempty
    , _cpgdCacheNodeTypeSpecificParameters = mempty
    }

-- | A list of parameters specific to a particular cache node type. Each
-- element in the list contains detailed information about one parameter.
cpgdCacheNodeTypeSpecificParameters :: Lens' CacheParameterGroupDetails [CacheNodeTypeSpecificParameter]
cpgdCacheNodeTypeSpecificParameters =
    lens _cpgdCacheNodeTypeSpecificParameters
        (\s a -> s { _cpgdCacheNodeTypeSpecificParameters = a })

-- | Provides an identifier to allow retrieval of paginated results.
cpgdMarker :: Lens' CacheParameterGroupDetails (Maybe Text)
cpgdMarker = lens _cpgdMarker (\s a -> s { _cpgdMarker = a })

-- | A list of Parameter instances.
cpgdParameters :: Lens' CacheParameterGroupDetails [Parameter]
cpgdParameters = lens _cpgdParameters (\s a -> s { _cpgdParameters = a })

instance AWSRequest DescribeCacheParametersMessage where
    type Sv DescribeCacheParametersMessage = ElastiCache
    type Rs DescribeCacheParametersMessage = CacheParameterGroupDetails

    request  = post "DescribeCacheParameters"
    response = const . xmlResponse $ \h x -> CacheParameterGroupDetails
record
