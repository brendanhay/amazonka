{-# LANGUAGE RecordWildCards   #-}
{-# LANGUAGE TypeFamilies      #-}
{-# LANGUAGE OverloadedStrings #-}

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

-- | The /DescribeCacheParameters/ action returns the detailed parameter list
-- for a particular cache parameter group.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheParameters.html>
module Network.AWS.ElastiCache.DescribeCacheParameters
    (
    -- * Request
      DescribeCacheParameters
    -- ** Request constructor
    , describeCacheParameters
    -- ** Request lenses
    , dcpMaxRecords
    , dcpMarker
    , dcpSource
    , dcpCacheParameterGroupName

    -- * Response
    , DescribeCacheParametersResponse
    -- ** Response constructor
    , describeCacheParametersResponse
    -- ** Response lenses
    , dcprCacheNodeTypeSpecificParameters
    , dcprParameters
    , dcprMarker
    , dcprStatusCode
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

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
data DescribeCacheParameters = DescribeCacheParameters'{_dcpMaxRecords :: Maybe Int, _dcpMarker :: Maybe Text, _dcpSource :: Maybe Text, _dcpCacheParameterGroupName :: Text} deriving (Eq, Read, Show)

-- | 'DescribeCacheParameters' smart constructor.
describeCacheParameters :: Text -> DescribeCacheParameters
describeCacheParameters pCacheParameterGroupName = DescribeCacheParameters'{_dcpMaxRecords = Nothing, _dcpMarker = Nothing, _dcpSource = Nothing, _dcpCacheParameterGroupName = pCacheParameterGroupName};

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
          | stop (rs ^. dcprMarker) = Nothing
          | stop (rs ^. dcprParameters) = Nothing
          | otherwise =
            Just $ rq & dcpMarker .~ rs ^. dcprMarker

instance AWSRequest DescribeCacheParameters where
        type Sv DescribeCacheParameters = ElastiCache
        type Rs DescribeCacheParameters =
             DescribeCacheParametersResponse
        request = post
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
-- * 'dcprCacheNodeTypeSpecificParameters'
--
-- * 'dcprParameters'
--
-- * 'dcprMarker'
--
-- * 'dcprStatusCode'
data DescribeCacheParametersResponse = DescribeCacheParametersResponse'{_dcprCacheNodeTypeSpecificParameters :: Maybe [CacheNodeTypeSpecificParameter], _dcprParameters :: Maybe [Parameter], _dcprMarker :: Maybe Text, _dcprStatusCode :: Int} deriving (Eq, Read, Show)

-- | 'DescribeCacheParametersResponse' smart constructor.
describeCacheParametersResponse :: Int -> DescribeCacheParametersResponse
describeCacheParametersResponse pStatusCode = DescribeCacheParametersResponse'{_dcprCacheNodeTypeSpecificParameters = Nothing, _dcprParameters = Nothing, _dcprMarker = Nothing, _dcprStatusCode = pStatusCode};

-- | A list of parameters specific to a particular cache node type. Each
-- element in the list contains detailed information about one parameter.
dcprCacheNodeTypeSpecificParameters :: Lens' DescribeCacheParametersResponse [CacheNodeTypeSpecificParameter]
dcprCacheNodeTypeSpecificParameters = lens _dcprCacheNodeTypeSpecificParameters (\ s a -> s{_dcprCacheNodeTypeSpecificParameters = a}) . _Default;

-- | A list of Parameter instances.
dcprParameters :: Lens' DescribeCacheParametersResponse [Parameter]
dcprParameters = lens _dcprParameters (\ s a -> s{_dcprParameters = a}) . _Default;

-- | Provides an identifier to allow retrieval of paginated results.
dcprMarker :: Lens' DescribeCacheParametersResponse (Maybe Text)
dcprMarker = lens _dcprMarker (\ s a -> s{_dcprMarker = a});

-- | FIXME: Undocumented member.
dcprStatusCode :: Lens' DescribeCacheParametersResponse Int
dcprStatusCode = lens _dcprStatusCode (\ s a -> s{_dcprStatusCode = a});
