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
-- Module      : Network.AWS.ElastiCache.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the default engine and system parameter information for the specified cache engine.
--
--
--
-- This operation returns paginated results.
module Network.AWS.ElastiCache.DescribeEngineDefaultParameters
    (
    -- * Creating a Request
      describeEngineDefaultParameters
    , DescribeEngineDefaultParameters
    -- * Request Lenses
    , dedpMarker
    , dedpMaxRecords
    , dedpCacheParameterGroupFamily

    -- * Destructuring the Response
    , describeEngineDefaultParametersResponse
    , DescribeEngineDefaultParametersResponse
    -- * Response Lenses
    , dedprsResponseStatus
    , dedprsEngineDefaults
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input of a @DescribeEngineDefaultParameters@ operation.
--
--
--
-- /See:/ 'describeEngineDefaultParameters' smart constructor.
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
  { _dedpMarker                    :: !(Maybe Text)
  , _dedpMaxRecords                :: !(Maybe Int)
  , _dedpCacheParameterGroupFamily :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEngineDefaultParameters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedpMarker' - An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
--
-- * 'dedpMaxRecords' - The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
--
-- * 'dedpCacheParameterGroupFamily' - The name of the cache parameter group family. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@
describeEngineDefaultParameters
    :: Text -- ^ 'dedpCacheParameterGroupFamily'
    -> DescribeEngineDefaultParameters
describeEngineDefaultParameters pCacheParameterGroupFamily_ =
  DescribeEngineDefaultParameters'
    { _dedpMarker = Nothing
    , _dedpMaxRecords = Nothing
    , _dedpCacheParameterGroupFamily = pCacheParameterGroupFamily_
    }


-- | An optional marker returned from a prior request. Use this marker for pagination of results from this operation. If this parameter is specified, the response includes only records beyond the marker, up to the value specified by @MaxRecords@ .
dedpMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedpMarker = lens _dedpMarker (\ s a -> s{_dedpMarker = a})

-- | The maximum number of records to include in the response. If more records exist than the specified @MaxRecords@ value, a marker is included in the response so that the remaining results can be retrieved. Default: 100 Constraints: minimum 20; maximum 100.
dedpMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Int)
dedpMaxRecords = lens _dedpMaxRecords (\ s a -> s{_dedpMaxRecords = a})

-- | The name of the cache parameter group family. Valid values are: @memcached1.4@ | @redis2.6@ | @redis2.8@ | @redis3.2@
dedpCacheParameterGroupFamily :: Lens' DescribeEngineDefaultParameters Text
dedpCacheParameterGroupFamily = lens _dedpCacheParameterGroupFamily (\ s a -> s{_dedpCacheParameterGroupFamily = a})

instance AWSPager DescribeEngineDefaultParameters
         where
        page rq rs
          | stop
              (rs ^? dedprsEngineDefaults . edMarker . _Just)
            = Nothing
          | stop (rs ^. dedprsEngineDefaults . edParameters) =
            Nothing
          | otherwise =
            Just $ rq &
              dedpMarker .~
                rs ^? dedprsEngineDefaults . edMarker . _Just

instance AWSRequest DescribeEngineDefaultParameters
         where
        type Rs DescribeEngineDefaultParameters =
             DescribeEngineDefaultParametersResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper
              "DescribeEngineDefaultParametersResult"
              (\ s h x ->
                 DescribeEngineDefaultParametersResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "EngineDefaults"))

instance Hashable DescribeEngineDefaultParameters
         where

instance NFData DescribeEngineDefaultParameters where

instance ToHeaders DescribeEngineDefaultParameters
         where
        toHeaders = const mempty

instance ToPath DescribeEngineDefaultParameters where
        toPath = const "/"

instance ToQuery DescribeEngineDefaultParameters
         where
        toQuery DescribeEngineDefaultParameters'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEngineDefaultParameters" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "Marker" =: _dedpMarker,
               "MaxRecords" =: _dedpMaxRecords,
               "CacheParameterGroupFamily" =:
                 _dedpCacheParameterGroupFamily]

-- | /See:/ 'describeEngineDefaultParametersResponse' smart constructor.
data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse'
  { _dedprsResponseStatus :: !Int
  , _dedprsEngineDefaults :: !EngineDefaults
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DescribeEngineDefaultParametersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dedprsResponseStatus' - -- | The response status code.
--
-- * 'dedprsEngineDefaults' - Undocumented member.
describeEngineDefaultParametersResponse
    :: Int -- ^ 'dedprsResponseStatus'
    -> EngineDefaults -- ^ 'dedprsEngineDefaults'
    -> DescribeEngineDefaultParametersResponse
describeEngineDefaultParametersResponse pResponseStatus_ pEngineDefaults_ =
  DescribeEngineDefaultParametersResponse'
    { _dedprsResponseStatus = pResponseStatus_
    , _dedprsEngineDefaults = pEngineDefaults_
    }


-- | -- | The response status code.
dedprsResponseStatus :: Lens' DescribeEngineDefaultParametersResponse Int
dedprsResponseStatus = lens _dedprsResponseStatus (\ s a -> s{_dedprsResponseStatus = a})

-- | Undocumented member.
dedprsEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse EngineDefaults
dedprsEngineDefaults = lens _dedprsEngineDefaults (\ s a -> s{_dedprsEngineDefaults = a})

instance NFData
           DescribeEngineDefaultParametersResponse
         where
