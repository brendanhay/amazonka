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
-- Module      : Network.AWS.ElastiCache.DescribeCacheSecurityGroups
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns a list of cache security group descriptions. If a cache security group name is specified, the list contains only the description of that group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheSecurityGroups
    (
    -- * Creating a Request
      describeCacheSecurityGroups
    , DescribeCacheSecurityGroups
    -- * Request Lenses
    , dcsgsCacheSecurityGroupName
    , dcsgsMarker
    , dcsgsMaxRecords

    -- * Destructuring the Response
    , describeCacheSecurityGroupsResponse
    , DescribeCacheSecurityGroupsResponse
    -- * Response Lenses
    , dcsgsrsCacheSecurityGroups
    , dcsgsrsMarker
    , dcsgsrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeCacheSecurityGroups@ operation.
--
--
--
-- /See:/ 'describeCacheSecurityGroups' smart constructor.
data DescribeCacheSecurityGroups = DescribeCacheSecurityGroups'
  { _dcsgsCacheSecurityGroupName :: !(Maybe Text)
  , _dcsgsMarker                 :: !(Maybe Text)
  , _dcsgsMaxRecords             :: !(Maybe Int)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCacheSecurityGroups' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsgsCacheSecurityGroupName' - The name of the cache security group to return details for.
--
-- * 'dcsgsMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dcsgsMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
describeCacheSecurityGroups
    :: DescribeCacheSecurityGroups
describeCacheSecurityGroups =
  DescribeCacheSecurityGroups'
    { _dcsgsCacheSecurityGroupName = Nothing
    , _dcsgsMarker = Nothing
    , _dcsgsMaxRecords = Nothing
    }


-- | The name of the cache security group to return details for.
dcsgsCacheSecurityGroupName :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsgsCacheSecurityGroupName = lens _dcsgsCacheSecurityGroupName (\ s a -> s{_dcsgsCacheSecurityGroupName = a})

-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dcsgsMarker :: Lens' DescribeCacheSecurityGroups (Maybe Text)
dcsgsMarker = lens _dcsgsMarker (\ s a -> s{_dcsgsMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
dcsgsMaxRecords :: Lens' DescribeCacheSecurityGroups (Maybe Int)
dcsgsMaxRecords = lens _dcsgsMaxRecords (\ s a -> s{_dcsgsMaxRecords = a})

instance AWSPager DescribeCacheSecurityGroups where
        page rq rs
          | stop (rs ^. dcsgsrsMarker) = Nothing
          | stop (rs ^. dcsgsrsCacheSecurityGroups) = Nothing
          | otherwise =
            Just $ rq & dcsgsMarker .~ rs ^. dcsgsrsMarker

instance AWSRequest DescribeCacheSecurityGroups where
        type Rs DescribeCacheSecurityGroups =
             DescribeCacheSecurityGroupsResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper
              "DescribeCacheSecurityGroupsResult"
              (\ s h x ->
                 DescribeCacheSecurityGroupsResponse' <$>
                   (x .@? "CacheSecurityGroups" .!@ mempty >>=
                      may (parseXMLList "CacheSecurityGroup"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCacheSecurityGroups where

instance NFData DescribeCacheSecurityGroups where

instance ToHeaders DescribeCacheSecurityGroups where
        toHeaders = const mempty

instance ToPath DescribeCacheSecurityGroups where
        toPath = const "/"

instance ToQuery DescribeCacheSecurityGroups where
        toQuery DescribeCacheSecurityGroups'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCacheSecurityGroups" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheSecurityGroupName" =:
                 _dcsgsCacheSecurityGroupName,
               "Marker" =: _dcsgsMarker,
               "MaxRecords" =: _dcsgsMaxRecords]

-- | Represents the output of a @DescribeCacheSecurityGroups@ operation.
--
--
--
-- /See:/ 'describeCacheSecurityGroupsResponse' smart constructor.
data DescribeCacheSecurityGroupsResponse = DescribeCacheSecurityGroupsResponse'
  { _dcsgsrsCacheSecurityGroups :: !(Maybe [CacheSecurityGroup])
  , _dcsgsrsMarker              :: !(Maybe Text)
  , _dcsgsrsResponseStatus      :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCacheSecurityGroupsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcsgsrsCacheSecurityGroups' - A list of cache security groups. Each element in the list contains detailed information about one group.
--
-- * 'dcsgsrsMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'dcsgsrsResponseStatus' - -- | The response status code.
describeCacheSecurityGroupsResponse
    :: Int -- ^ 'dcsgsrsResponseStatus'
    -> DescribeCacheSecurityGroupsResponse
describeCacheSecurityGroupsResponse pResponseStatus_ =
  DescribeCacheSecurityGroupsResponse'
    { _dcsgsrsCacheSecurityGroups = Nothing
    , _dcsgsrsMarker = Nothing
    , _dcsgsrsResponseStatus = pResponseStatus_
    }


-- | A list of cache security groups. Each element in the list contains detailed information about one group.
dcsgsrsCacheSecurityGroups :: Lens' DescribeCacheSecurityGroupsResponse [CacheSecurityGroup]
dcsgsrsCacheSecurityGroups = lens _dcsgsrsCacheSecurityGroups (\ s a -> s{_dcsgsrsCacheSecurityGroups = a}) . _Default . _Coerce

-- | Provides an identifier to allow retrieval of paginated results.
dcsgsrsMarker :: Lens' DescribeCacheSecurityGroupsResponse (Maybe Text)
dcsgsrsMarker = lens _dcsgsrsMarker (\ s a -> s{_dcsgsrsMarker = a})

-- | -- | The response status code.
dcsgsrsResponseStatus :: Lens' DescribeCacheSecurityGroupsResponse Int
dcsgsrsResponseStatus = lens _dcsgsrsResponseStatus (\ s a -> s{_dcsgsrsResponseStatus = a})

instance NFData DescribeCacheSecurityGroupsResponse
         where
