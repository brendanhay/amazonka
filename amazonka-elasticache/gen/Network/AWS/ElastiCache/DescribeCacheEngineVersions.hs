{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheEngineVersions
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeCacheEngineVersions/ action returns a list of the available
-- cache engines and their versions.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheEngineVersions.html>
module Network.AWS.ElastiCache.DescribeCacheEngineVersions
    (
    -- * Request
      DescribeCacheEngineVersions
    -- ** Request constructor
    , describeCacheEngineVersions
    -- ** Request lenses
    , dcevCacheParameterGroupFamily
    , dcevEngineVersion
    , dcevDefaultOnly
    , dcevEngine
    , dcevMaxRecords
    , dcevMarker

    -- * Response
    , DescribeCacheEngineVersionsResponse
    -- ** Response constructor
    , describeCacheEngineVersionsResponse
    -- ** Response lenses
    , dcevrsCacheEngineVersions
    , dcevrsMarker
    , dcevrsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeCacheEngineVersions/ action.
--
-- /See:/ 'describeCacheEngineVersions' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcevCacheParameterGroupFamily'
--
-- * 'dcevEngineVersion'
--
-- * 'dcevDefaultOnly'
--
-- * 'dcevEngine'
--
-- * 'dcevMaxRecords'
--
-- * 'dcevMarker'
data DescribeCacheEngineVersions = DescribeCacheEngineVersions'
    { _dcevCacheParameterGroupFamily :: !(Maybe Text)
    , _dcevEngineVersion             :: !(Maybe Text)
    , _dcevDefaultOnly               :: !(Maybe Bool)
    , _dcevEngine                    :: !(Maybe Text)
    , _dcevMaxRecords                :: !(Maybe Int)
    , _dcevMarker                    :: !(Maybe Text)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheEngineVersions' smart constructor.
describeCacheEngineVersions :: DescribeCacheEngineVersions
describeCacheEngineVersions =
    DescribeCacheEngineVersions'
    { _dcevCacheParameterGroupFamily = Nothing
    , _dcevEngineVersion = Nothing
    , _dcevDefaultOnly = Nothing
    , _dcevEngine = Nothing
    , _dcevMaxRecords = Nothing
    , _dcevMarker = Nothing
    }

-- | The name of a specific cache parameter group family to return details
-- for.
--
-- Constraints:
--
-- -   Must be 1 to 255 alphanumeric characters
-- -   First character must be a letter
-- -   Cannot end with a hyphen or contain two consecutive hyphens
dcevCacheParameterGroupFamily :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevCacheParameterGroupFamily = lens _dcevCacheParameterGroupFamily (\ s a -> s{_dcevCacheParameterGroupFamily = a});

-- | The cache engine version to return.
--
-- Example: @1.4.14@
dcevEngineVersion :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevEngineVersion = lens _dcevEngineVersion (\ s a -> s{_dcevEngineVersion = a});

-- | If /true/, specifies that only the default version of the specified
-- engine or engine and major version combination is to be returned.
dcevDefaultOnly :: Lens' DescribeCacheEngineVersions (Maybe Bool)
dcevDefaultOnly = lens _dcevDefaultOnly (\ s a -> s{_dcevDefaultOnly = a});

-- | The cache engine to return. Valid values: @memcached@ | @redis@
dcevEngine :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevEngine = lens _dcevEngine (\ s a -> s{_dcevEngine = a});

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcevMaxRecords :: Lens' DescribeCacheEngineVersions (Maybe Int)
dcevMaxRecords = lens _dcevMaxRecords (\ s a -> s{_dcevMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcevMarker :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevMarker = lens _dcevMarker (\ s a -> s{_dcevMarker = a});

instance AWSPager DescribeCacheEngineVersions where
        page rq rs
          | stop (rs ^. dcevrsMarker) = Nothing
          | stop (rs ^. dcevrsCacheEngineVersions) = Nothing
          | otherwise =
            Just $ rq & dcevMarker .~ rs ^. dcevrsMarker

instance AWSRequest DescribeCacheEngineVersions where
        type Sv DescribeCacheEngineVersions = ElastiCache
        type Rs DescribeCacheEngineVersions =
             DescribeCacheEngineVersionsResponse
        request = postQuery
        response
          = receiveXMLWrapper
              "DescribeCacheEngineVersionsResult"
              (\ s h x ->
                 DescribeCacheEngineVersionsResponse' <$>
                   (x .@? "CacheEngineVersions" .!@ mempty >>=
                      may (parseXMLList "CacheEngineVersion"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders DescribeCacheEngineVersions where
        toHeaders = const mempty

instance ToPath DescribeCacheEngineVersions where
        toPath = const mempty

instance ToQuery DescribeCacheEngineVersions where
        toQuery DescribeCacheEngineVersions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCacheEngineVersions" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheParameterGroupFamily" =:
                 _dcevCacheParameterGroupFamily,
               "EngineVersion" =: _dcevEngineVersion,
               "DefaultOnly" =: _dcevDefaultOnly,
               "Engine" =: _dcevEngine,
               "MaxRecords" =: _dcevMaxRecords,
               "Marker" =: _dcevMarker]

-- | Represents the output of a DescribeCacheEngineVersions action.
--
-- /See:/ 'describeCacheEngineVersionsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcevrsCacheEngineVersions'
--
-- * 'dcevrsMarker'
--
-- * 'dcevrsStatus'
data DescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse'
    { _dcevrsCacheEngineVersions :: !(Maybe [CacheEngineVersion])
    , _dcevrsMarker              :: !(Maybe Text)
    , _dcevrsStatus              :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheEngineVersionsResponse' smart constructor.
describeCacheEngineVersionsResponse :: Int -> DescribeCacheEngineVersionsResponse
describeCacheEngineVersionsResponse pStatus_ =
    DescribeCacheEngineVersionsResponse'
    { _dcevrsCacheEngineVersions = Nothing
    , _dcevrsMarker = Nothing
    , _dcevrsStatus = pStatus_
    }

-- | A list of cache engine version details. Each element in the list
-- contains detailed information about one cache engine version.
dcevrsCacheEngineVersions :: Lens' DescribeCacheEngineVersionsResponse [CacheEngineVersion]
dcevrsCacheEngineVersions = lens _dcevrsCacheEngineVersions (\ s a -> s{_dcevrsCacheEngineVersions = a}) . _Default . _Coerce;

-- | Provides an identifier to allow retrieval of paginated results.
dcevrsMarker :: Lens' DescribeCacheEngineVersionsResponse (Maybe Text)
dcevrsMarker = lens _dcevrsMarker (\ s a -> s{_dcevrsMarker = a});

-- | FIXME: Undocumented member.
dcevrsStatus :: Lens' DescribeCacheEngineVersionsResponse Int
dcevrsStatus = lens _dcevrsStatus (\ s a -> s{_dcevrsStatus = a});
