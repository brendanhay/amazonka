{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeCacheParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeCacheParameters/ action returns the detailed parameter list
-- for a particular cache parameter group.
--
-- /See:/ <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheParameters.html AWS API Reference> for DescribeCacheParameters.
module Network.AWS.ElastiCache.DescribeCacheParameters
    (
    -- * Creating a Request
      DescribeCacheParameters
    , describeCacheParameters
    -- * Request Lenses
    , dcpMaxRecords
    , dcpMarker
    , dcpSource
    , dcpCacheParameterGroupName

    -- * Destructuring the Response
    , DescribeCacheParametersResponse
    , describeCacheParametersResponse
    -- * Response Lenses
    , dcprsCacheNodeTypeSpecificParameters
    , dcprsParameters
    , dcprsMarker
    , dcprsStatus
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeCacheParameters/ action.
--
-- /See:/ 'describeCacheParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcpMaxRecords'
--
-- * 'dcpMarker'
--
-- * 'dcpSource'
--
-- * 'dcpCacheParameterGroupName'
data DescribeCacheParameters = DescribeCacheParameters'
    { _dcpMaxRecords              :: !(Maybe Int)
    , _dcpMarker                  :: !(Maybe Text)
    , _dcpSource                  :: !(Maybe Text)
    , _dcpCacheParameterGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheParameters' smart constructor.
describeCacheParameters :: Text -> DescribeCacheParameters
describeCacheParameters pCacheParameterGroupName_ =
    DescribeCacheParameters'
    { _dcpMaxRecords = Nothing
    , _dcpMarker = Nothing
    , _dcpSource = Nothing
    , _dcpCacheParameterGroupName = pCacheParameterGroupName_
    }

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcpMaxRecords :: Lens' DescribeCacheParameters (Maybe Int)
dcpMaxRecords = lens _dcpMaxRecords (\ s a -> s{_dcpMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcpMarker :: Lens' DescribeCacheParameters (Maybe Text)
dcpMarker = lens _dcpMarker (\ s a -> s{_dcpMarker = a});

-- | The parameter types to return.
--
-- Valid values: @user@ | @system@ | @engine-default@
dcpSource :: Lens' DescribeCacheParameters (Maybe Text)
dcpSource = lens _dcpSource (\ s a -> s{_dcpSource = a});

-- | The name of a specific cache parameter group to return details for.
dcpCacheParameterGroupName :: Lens' DescribeCacheParameters Text
dcpCacheParameterGroupName = lens _dcpCacheParameterGroupName (\ s a -> s{_dcpCacheParameterGroupName = a});

instance AWSPager DescribeCacheParameters where
        page rq rs
          | stop (rs ^. dcprsMarker) = Nothing
          | stop (rs ^. dcprsParameters) = Nothing
          | otherwise =
            Just $ rq & dcpMarker .~ rs ^. dcprsMarker

instance AWSRequest DescribeCacheParameters where
        type Sv DescribeCacheParameters = ElastiCache
        type Rs DescribeCacheParameters =
             DescribeCacheParametersResponse
        request = postQuery
        response
          = receiveXMLWrapper "DescribeCacheParametersResult"
              (\ s h x ->
                 DescribeCacheParametersResponse' <$>
                   (x .@? "CacheNodeTypeSpecificParameters" .!@ mempty
                      >>=
                      may (parseXMLList "CacheNodeTypeSpecificParameter"))
                     <*>
                     (x .@? "Parameters" .!@ mempty >>=
                        may (parseXMLList "Parameter"))
                     <*> (x .@? "Marker")
                     <*> (pure (fromEnum s)))

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
               "MaxRecords" =: _dcpMaxRecords,
               "Marker" =: _dcpMarker, "Source" =: _dcpSource,
               "CacheParameterGroupName" =:
                 _dcpCacheParameterGroupName]

-- | Represents the output of a /DescribeCacheParameters/ action.
--
-- /See:/ 'describeCacheParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dcprsCacheNodeTypeSpecificParameters'
--
-- * 'dcprsParameters'
--
-- * 'dcprsMarker'
--
-- * 'dcprsStatus'
data DescribeCacheParametersResponse = DescribeCacheParametersResponse'
    { _dcprsCacheNodeTypeSpecificParameters :: !(Maybe [CacheNodeTypeSpecificParameter])
    , _dcprsParameters                      :: !(Maybe [Parameter])
    , _dcprsMarker                          :: !(Maybe Text)
    , _dcprsStatus                          :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheParametersResponse' smart constructor.
describeCacheParametersResponse :: Int -> DescribeCacheParametersResponse
describeCacheParametersResponse pStatus_ =
    DescribeCacheParametersResponse'
    { _dcprsCacheNodeTypeSpecificParameters = Nothing
    , _dcprsParameters = Nothing
    , _dcprsMarker = Nothing
    , _dcprsStatus = pStatus_
    }

-- | A list of parameters specific to a particular cache node type. Each
-- element in the list contains detailed information about one parameter.
dcprsCacheNodeTypeSpecificParameters :: Lens' DescribeCacheParametersResponse [CacheNodeTypeSpecificParameter]
dcprsCacheNodeTypeSpecificParameters = lens _dcprsCacheNodeTypeSpecificParameters (\ s a -> s{_dcprsCacheNodeTypeSpecificParameters = a}) . _Default . _Coerce;

-- | A list of Parameter instances.
dcprsParameters :: Lens' DescribeCacheParametersResponse [Parameter]
dcprsParameters = lens _dcprsParameters (\ s a -> s{_dcprsParameters = a}) . _Default . _Coerce;

-- | Provides an identifier to allow retrieval of paginated results.
dcprsMarker :: Lens' DescribeCacheParametersResponse (Maybe Text)
dcprsMarker = lens _dcprsMarker (\ s a -> s{_dcprsMarker = a});

-- | Undocumented member.
dcprsStatus :: Lens' DescribeCacheParametersResponse Int
dcprsStatus = lens _dcprsStatus (\ s a -> s{_dcprsStatus = a});
