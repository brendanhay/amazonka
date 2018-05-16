{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheEngineVersions
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of the available cache engines and their versions.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheEngineVersions
    (
    -- * Creating a Request
      describeCacheEngineVersions
    , DescribeCacheEngineVersions
    -- * Request Lenses
    , dcevEngineVersion
    , dcevCacheParameterGroupFamily
    , dcevDefaultOnly
    , dcevEngine
    , dcevMarker
    , dcevMaxRecords

    -- * Destructuring the Response
    , describeCacheEngineVersionsResponse
    , DescribeCacheEngineVersionsResponse
    -- * Response Lenses
    , dcevrsCacheEngineVersions
    , dcevrsMarker
    , dcevrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeCacheEngineVersions@ operation.
--
--
--
-- /See:/ 'describeCacheEngineVersions' smart constructor.
data DescribeCacheEngineVersions = DescribeCacheEngineVersions'
  { _dcevEngineVersion             :: !(Maybe Text)
  , _dcevCacheParameterGroupFamily :: !(Maybe Text)
  , _dcevDefaultOnly               :: !(Maybe Bool)
  , _dcevEngine                    :: !(Maybe Text)
  , _dcevMarker                    :: !(Maybe Text)
  , _dcevMaxRecords                :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCacheEngineVersions' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcevEngineVersion' - The cache engine version to return. Example: @1.4.14@
--
-- * 'dcevCacheParameterGroupFamily' - The name of a specific cache parameter group family to return details for. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@  Constraints:     * Must be 1 to 255 alphanumeric characters     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
--
-- * 'dcevDefaultOnly' - If @true@ , specifies that only the default version of the specified engine or engine and major version combination is to be returned.
--
-- * 'dcevEngine' - The cache engine to return. Valid values: @memcached@ | @redis@
--
-- * 'dcevMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dcevMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
describeCacheEngineVersions
    :: DescribeCacheEngineVersions
describeCacheEngineVersions =
  DescribeCacheEngineVersions'
    { _dcevEngineVersion = Nothing
    , _dcevCacheParameterGroupFamily = Nothing
    , _dcevDefaultOnly = Nothing
    , _dcevEngine = Nothing
    , _dcevMarker = Nothing
    , _dcevMaxRecords = Nothing
    }


-- | The cache engine version to return. Example: @1.4.14@
dcevEngineVersion :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevEngineVersion = lens _dcevEngineVersion (\ s a -> s{_dcevEngineVersion = a})

-- | The name of a specific cache parameter group family to return details for. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@  Constraints:     * Must be 1 to 255 alphanumeric characters     * First character must be a letter     * Cannot end with a hyphen or contain two consecutive hyphens
dcevCacheParameterGroupFamily :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevCacheParameterGroupFamily = lens _dcevCacheParameterGroupFamily (\ s a -> s{_dcevCacheParameterGroupFamily = a})

-- | If @true@ , specifies that only the default version of the specified engine or engine and major version combination is to be returned.
dcevDefaultOnly :: Lens' DescribeCacheEngineVersions (Maybe Bool)
dcevDefaultOnly = lens _dcevDefaultOnly (\ s a -> s{_dcevDefaultOnly = a})

-- | The cache engine to return. Valid values: @memcached@ | @redis@
dcevEngine :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevEngine = lens _dcevEngine (\ s a -> s{_dcevEngine = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dcevMarker :: Lens' DescribeCacheEngineVersions (Maybe Text)
dcevMarker = lens _dcevMarker (\ s a -> s{_dcevMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
dcevMaxRecords :: Lens' DescribeCacheEngineVersions (Maybe Int)
dcevMaxRecords = lens _dcevMaxRecords (\ s a -> s{_dcevMaxRecords = a})

instance AWSPager DescribeCacheEngineVersions where
        page rq rs
          | stop (rs ^. dcevrsMarker) = Nothing
          | stop (rs ^. dcevrsCacheEngineVersions) = Nothing
          | otherwise =
            Just $ rq & dcevMarker .~ rs ^. dcevrsMarker

instance AWSRequest DescribeCacheEngineVersions where
        type Rs DescribeCacheEngineVersions =
             DescribeCacheEngineVersionsResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper
              "DescribeCacheEngineVersionsResult"
              (\ s h x ->
                 DescribeCacheEngineVersionsResponse' <$>
                   (x .@? "CacheEngineVersions" .!@ mempty >>=
                      may (parseXMLList "CacheEngineVersion"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCacheEngineVersions where

instance NFData DescribeCacheEngineVersions where

instance ToHeaders DescribeCacheEngineVersions where
        toHeaders = const mempty

instance ToPath DescribeCacheEngineVersions where
        toPath = const "/"

instance ToQuery DescribeCacheEngineVersions where
        toQuery DescribeCacheEngineVersions'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCacheEngineVersions" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "EngineVersion" =: _dcevEngineVersion,
               "CacheParameterGroupFamily" =:
                 _dcevCacheParameterGroupFamily,
               "DefaultOnly" =: _dcevDefaultOnly,
               "Engine" =: _dcevEngine, "Marker" =: _dcevMarker,
               "MaxRecords" =: _dcevMaxRecords]

-- | Represents the output of a 'DescribeCacheEngineVersions' operation.
--
--
--
-- /See:/ 'describeCacheEngineVersionsResponse' smart constructor.
data DescribeCacheEngineVersionsResponse = DescribeCacheEngineVersionsResponse'
  { _dcevrsCacheEngineVersions :: !(Maybe [CacheEngineVersion])
  , _dcevrsMarker              :: !(Maybe Text)
  , _dcevrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCacheEngineVersionsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcevrsCacheEngineVersions' - A list of cache engine version details. Each element in the list contains detailed information about one cache engine version.
--
-- * 'dcevrsMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'dcevrsResponseStatus' - -- | The response status code.
describeCacheEngineVersionsResponse
    :: Int -- ^ 'dcevrsResponseStatus'
    -> DescribeCacheEngineVersionsResponse
describeCacheEngineVersionsResponse pResponseStatus_ =
  DescribeCacheEngineVersionsResponse'
    { _dcevrsCacheEngineVersions = Nothing
    , _dcevrsMarker = Nothing
    , _dcevrsResponseStatus = pResponseStatus_
    }


-- | A list of cache engine version details. Each element in the list contains detailed information about one cache engine version.
dcevrsCacheEngineVersions :: Lens' DescribeCacheEngineVersionsResponse [CacheEngineVersion]
dcevrsCacheEngineVersions = lens _dcevrsCacheEngineVersions (\ s a -> s{_dcevrsCacheEngineVersions = a}) . _Default . _Coerce

-- | Provides an identifier to allow retrieval of paginated results.
dcevrsMarker :: Lens' DescribeCacheEngineVersionsResponse (Maybe Text)
dcevrsMarker = lens _dcevrsMarker (\ s a -> s{_dcevrsMarker = a})

-- | -- | The response status code.
dcevrsResponseStatus :: Lens' DescribeCacheEngineVersionsResponse Int
dcevrsResponseStatus = lens _dcevrsResponseStatus (\ s a -> s{_dcevrsResponseStatus = a})

instance NFData DescribeCacheEngineVersionsResponse
         where
