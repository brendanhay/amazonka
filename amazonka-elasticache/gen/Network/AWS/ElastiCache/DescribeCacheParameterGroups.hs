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
-- Module      : Network.AWS.ElastiCache.DescribeCacheParameterGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache parameter group descriptions. If a cache parameter group name is specified, the list contains only the descriptions for that group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheParameterGroups
    (
    -- * Creating a Request
      describeCacheParameterGroups
    , DescribeCacheParameterGroups
    -- * Request Lenses
    , dcpgCacheParameterGroupName
    , dcpgMarker
    , dcpgMaxRecords

    -- * Destructuring the Response
    , describeCacheParameterGroupsResponse
    , DescribeCacheParameterGroupsResponse
    -- * Response Lenses
    , dcpgrsCacheParameterGroups
    , dcpgrsMarker
    , dcpgrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeCacheParameterGroups@ operation.
--
--
--
-- /See:/ 'describeCacheParameterGroups' smart constructor.
data DescribeCacheParameterGroups = DescribeCacheParameterGroups'
  { _dcpgCacheParameterGroupName :: !(Maybe Text)
  , _dcpgMarker                  :: !(Maybe Text)
  , _dcpgMaxRecords              :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCacheParameterGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpgCacheParameterGroupName' - The name of a specific cache parameter group to return details for.
--
-- * 'dcpgMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dcpgMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
describeCacheParameterGroups
    :: DescribeCacheParameterGroups
describeCacheParameterGroups =
  DescribeCacheParameterGroups'
    { _dcpgCacheParameterGroupName = Nothing
    , _dcpgMarker = Nothing
    , _dcpgMaxRecords = Nothing
    }


-- | The name of a specific cache parameter group to return details for.
dcpgCacheParameterGroupName :: Lens' DescribeCacheParameterGroups (Maybe Text)
dcpgCacheParameterGroupName = lens _dcpgCacheParameterGroupName (\ s a -> s{_dcpgCacheParameterGroupName = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dcpgMarker :: Lens' DescribeCacheParameterGroups (Maybe Text)
dcpgMarker = lens _dcpgMarker (\ s a -> s{_dcpgMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
dcpgMaxRecords :: Lens' DescribeCacheParameterGroups (Maybe Int)
dcpgMaxRecords = lens _dcpgMaxRecords (\ s a -> s{_dcpgMaxRecords = a})

instance AWSPager DescribeCacheParameterGroups where
        page rq rs
          | stop (rs ^. dcpgrsMarker) = Nothing
          | stop (rs ^. dcpgrsCacheParameterGroups) = Nothing
          | otherwise =
            Just $ rq & dcpgMarker .~ rs ^. dcpgrsMarker

instance AWSRequest DescribeCacheParameterGroups
         where
        type Rs DescribeCacheParameterGroups =
             DescribeCacheParameterGroupsResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper
              "DescribeCacheParameterGroupsResult"
              (\ s h x ->
                 DescribeCacheParameterGroupsResponse' <$>
                   (x .@? "CacheParameterGroups" .!@ mempty >>=
                      may (parseXMLList "CacheParameterGroup"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCacheParameterGroups where

instance NFData DescribeCacheParameterGroups where

instance ToHeaders DescribeCacheParameterGroups where
        toHeaders = const mempty

instance ToPath DescribeCacheParameterGroups where
        toPath = const "/"

instance ToQuery DescribeCacheParameterGroups where
        toQuery DescribeCacheParameterGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCacheParameterGroups" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheParameterGroupName" =:
                 _dcpgCacheParameterGroupName,
               "Marker" =: _dcpgMarker,
               "MaxRecords" =: _dcpgMaxRecords]

-- | Represents the output of a @DescribeCacheParameterGroups@ operation.
--
--
--
-- /See:/ 'describeCacheParameterGroupsResponse' smart constructor.
data DescribeCacheParameterGroupsResponse = DescribeCacheParameterGroupsResponse'
  { _dcpgrsCacheParameterGroups :: !(Maybe [CacheParameterGroup])
  , _dcpgrsMarker               :: !(Maybe Text)
  , _dcpgrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCacheParameterGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpgrsCacheParameterGroups' - A list of cache parameter groups. Each element in the list contains detailed information about one cache parameter group.
--
-- * 'dcpgrsMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'dcpgrsResponseStatus' - -- | The response status code.
describeCacheParameterGroupsResponse
    :: Int -- ^ 'dcpgrsResponseStatus'
    -> DescribeCacheParameterGroupsResponse
describeCacheParameterGroupsResponse pResponseStatus_ =
  DescribeCacheParameterGroupsResponse'
    { _dcpgrsCacheParameterGroups = Nothing
    , _dcpgrsMarker = Nothing
    , _dcpgrsResponseStatus = pResponseStatus_
    }


-- | A list of cache parameter groups. Each element in the list contains detailed information about one cache parameter group.
dcpgrsCacheParameterGroups :: Lens' DescribeCacheParameterGroupsResponse [CacheParameterGroup]
dcpgrsCacheParameterGroups = lens _dcpgrsCacheParameterGroups (\ s a -> s{_dcpgrsCacheParameterGroups = a}) . _Default . _Coerce

-- | Provides an identifier to allow retrieval of paginated results.
dcpgrsMarker :: Lens' DescribeCacheParameterGroupsResponse (Maybe Text)
dcpgrsMarker = lens _dcpgrsMarker (\ s a -> s{_dcpgrsMarker = a})

-- | -- | The response status code.
dcpgrsResponseStatus :: Lens' DescribeCacheParameterGroupsResponse Int
dcpgrsResponseStatus = lens _dcpgrsResponseStatus (\ s a -> s{_dcpgrsResponseStatus = a})

instance NFData DescribeCacheParameterGroupsResponse
         where
