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
-- Module      : Network.AWS.ElastiCache.DescribeCacheParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the detailed parameter list for a particular cache parameter group.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeCacheParameters
    (
    -- * Creating a Request
      describeCacheParameters
    , DescribeCacheParameters
    -- * Request Lenses
    , dcpMarker
    , dcpMaxRecords
    , dcpSource
    , dcpCacheParameterGroupName

    -- * Destructuring the Response
    , describeCacheParametersResponse
    , DescribeCacheParametersResponse
    -- * Response Lenses
    , dcprsCacheNodeTypeSpecificParameters
    , dcprsMarker
    , dcprsParameters
    , dcprsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeCacheParameters@ operation.
--
--
--
-- /See:/ 'describeCacheParameters' smart constructor.
data DescribeCacheParameters = DescribeCacheParameters'
  { _dcpMarker                  :: !(Maybe Text)
  , _dcpMaxRecords              :: !(Maybe Int)
  , _dcpSource                  :: !(Maybe Text)
  , _dcpCacheParameterGroupName :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCacheParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcpMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dcpMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
--
-- * 'dcpSource' - The parameter types to return. Valid values: @user@ | @system@ | @engine-default@
--
-- * 'dcpCacheParameterGroupName' - The name of a specific cache parameter group to return details for.
describeCacheParameters
    :: Text -- ^ 'dcpCacheParameterGroupName'
    -> DescribeCacheParameters
describeCacheParameters pCacheParameterGroupName_ =
  DescribeCacheParameters'
    { _dcpMarker = Nothing
    , _dcpMaxRecords = Nothing
    , _dcpSource = Nothing
    , _dcpCacheParameterGroupName = pCacheParameterGroupName_
    }


-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dcpMarker :: Lens' DescribeCacheParameters (Maybe Text)
dcpMarker = lens _dcpMarker (\ s a -> s{_dcpMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
dcpMaxRecords :: Lens' DescribeCacheParameters (Maybe Int)
dcpMaxRecords = lens _dcpMaxRecords (\ s a -> s{_dcpMaxRecords = a})

-- | The parameter types to return. Valid values: @user@ | @system@ | @engine-default@
dcpSource :: Lens' DescribeCacheParameters (Maybe Text)
dcpSource = lens _dcpSource (\ s a -> s{_dcpSource = a})

-- | The name of a specific cache parameter group to return details for.
dcpCacheParameterGroupName :: Lens' DescribeCacheParameters Text
dcpCacheParameterGroupName = lens _dcpCacheParameterGroupName (\ s a -> s{_dcpCacheParameterGroupName = a})

instance AWSPager DescribeCacheParameters where
        page rq rs
          | stop (rs ^. dcprsMarker) = Nothing
          | stop (rs ^. dcprsParameters) = Nothing
          | otherwise =
            Just $ rq & dcpMarker .~ rs ^. dcprsMarker

instance AWSRequest DescribeCacheParameters where
        type Rs DescribeCacheParameters =
             DescribeCacheParametersResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "DescribeCacheParametersResult"
              (\ s h x ->
                 DescribeCacheParametersResponse' <$>
                   (x .@? "CacheNodeTypeSpecificParameters" .!@ mempty
                      >>=
                      may (parseXMLList "CacheNodeTypeSpecificParameter"))
                     <*> (x .@? "Marker")
                     <*>
                     (x .@? "Parameters" .!@ mempty >>=
                        may (parseXMLList "Parameter"))
                     <*> (pure (fromEnum s)))

instance Hashable DescribeCacheParameters where

instance NFData DescribeCacheParameters where

instance ToHeaders DescribeCacheParameters where
        toHeaders = const mempty

instance ToPath DescribeCacheParameters where
        toPath = const "/"

instance ToQuery DescribeCacheParameters where
        toQuery DescribeCacheParameters'{..}
          = mconcat
              ["Action" =:
                 ("DescribeCacheParameters" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "Marker" =: _dcpMarker,
               "MaxRecords" =: _dcpMaxRecords,
               "Source" =: _dcpSource,
               "CacheParameterGroupName" =:
                 _dcpCacheParameterGroupName]

-- | Represents the output of a @DescribeCacheParameters@ operation.
--
--
--
-- /See:/ 'describeCacheParametersResponse' smart constructor.
data DescribeCacheParametersResponse = DescribeCacheParametersResponse'
  { _dcprsCacheNodeTypeSpecificParameters :: !(Maybe [CacheNodeTypeSpecificParameter])
  , _dcprsMarker :: !(Maybe Text)
  , _dcprsParameters :: !(Maybe [Parameter])
  , _dcprsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeCacheParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dcprsCacheNodeTypeSpecificParameters' - A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
--
-- * 'dcprsMarker' - Provides an identifier to allow retrieval of paginated results.
--
-- * 'dcprsParameters' - A list of 'Parameter' instances.
--
-- * 'dcprsResponseStatus' - -- | The response status code.
describeCacheParametersResponse
    :: Int -- ^ 'dcprsResponseStatus'
    -> DescribeCacheParametersResponse
describeCacheParametersResponse pResponseStatus_ =
  DescribeCacheParametersResponse'
    { _dcprsCacheNodeTypeSpecificParameters = Nothing
    , _dcprsMarker = Nothing
    , _dcprsParameters = Nothing
    , _dcprsResponseStatus = pResponseStatus_
    }


-- | A list of parameters specific to a particular cache node type. Each element in the list contains detailed information about one parameter.
dcprsCacheNodeTypeSpecificParameters :: Lens' DescribeCacheParametersResponse [CacheNodeTypeSpecificParameter]
dcprsCacheNodeTypeSpecificParameters = lens _dcprsCacheNodeTypeSpecificParameters (\ s a -> s{_dcprsCacheNodeTypeSpecificParameters = a}) . _Default . _Coerce

-- | Provides an identifier to allow retrieval of paginated results.
dcprsMarker :: Lens' DescribeCacheParametersResponse (Maybe Text)
dcprsMarker = lens _dcprsMarker (\ s a -> s{_dcprsMarker = a})

-- | A list of 'Parameter' instances.
dcprsParameters :: Lens' DescribeCacheParametersResponse [Parameter]
dcprsParameters = lens _dcprsParameters (\ s a -> s{_dcprsParameters = a}) . _Default . _Coerce

-- | -- | The response status code.
dcprsResponseStatus :: Lens' DescribeCacheParametersResponse Int
dcprsResponseStatus = lens _dcprsResponseStatus (\ s a -> s{_dcprsResponseStatus = a})

instance NFData DescribeCacheParametersResponse where
