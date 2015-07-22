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
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeCacheParameters.html>
module Network.AWS.ElastiCache.DescribeCacheParameters
    (
    -- * Request
      DescribeCacheParameters
    -- ** Request constructor
    , describeCacheParameters
    -- ** Request lenses
    , dcprqMaxRecords
    , dcprqMarker
    , dcprqSource
    , dcprqCacheParameterGroupName

    -- * Response
    , DescribeCacheParametersResponse
    -- ** Response constructor
    , describeCacheParametersResponse
    -- ** Response lenses
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
-- * 'dcprqMaxRecords'
--
-- * 'dcprqMarker'
--
-- * 'dcprqSource'
--
-- * 'dcprqCacheParameterGroupName'
data DescribeCacheParameters = DescribeCacheParameters'
    { _dcprqMaxRecords              :: !(Maybe Int)
    , _dcprqMarker                  :: !(Maybe Text)
    , _dcprqSource                  :: !(Maybe Text)
    , _dcprqCacheParameterGroupName :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeCacheParameters' smart constructor.
describeCacheParameters :: Text -> DescribeCacheParameters
describeCacheParameters pCacheParameterGroupName_ =
    DescribeCacheParameters'
    { _dcprqMaxRecords = Nothing
    , _dcprqMarker = Nothing
    , _dcprqSource = Nothing
    , _dcprqCacheParameterGroupName = pCacheParameterGroupName_
    }

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dcprqMaxRecords :: Lens' DescribeCacheParameters (Maybe Int)
dcprqMaxRecords = lens _dcprqMaxRecords (\ s a -> s{_dcprqMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dcprqMarker :: Lens' DescribeCacheParameters (Maybe Text)
dcprqMarker = lens _dcprqMarker (\ s a -> s{_dcprqMarker = a});

-- | The parameter types to return.
--
-- Valid values: @user@ | @system@ | @engine-default@
dcprqSource :: Lens' DescribeCacheParameters (Maybe Text)
dcprqSource = lens _dcprqSource (\ s a -> s{_dcprqSource = a});

-- | The name of a specific cache parameter group to return details for.
dcprqCacheParameterGroupName :: Lens' DescribeCacheParameters Text
dcprqCacheParameterGroupName = lens _dcprqCacheParameterGroupName (\ s a -> s{_dcprqCacheParameterGroupName = a});

instance AWSPager DescribeCacheParameters where
        page rq rs
          | stop (rs ^. dcprsMarker) = Nothing
          | stop (rs ^. dcprsParameters) = Nothing
          | otherwise =
            Just $ rq & dcprqMarker .~ rs ^. dcprsMarker

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
               "MaxRecords" =: _dcprqMaxRecords,
               "Marker" =: _dcprqMarker, "Source" =: _dcprqSource,
               "CacheParameterGroupName" =:
                 _dcprqCacheParameterGroupName]

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
dcprsCacheNodeTypeSpecificParameters = lens _dcprsCacheNodeTypeSpecificParameters (\ s a -> s{_dcprsCacheNodeTypeSpecificParameters = a}) . _Default;

-- | A list of Parameter instances.
dcprsParameters :: Lens' DescribeCacheParametersResponse [Parameter]
dcprsParameters = lens _dcprsParameters (\ s a -> s{_dcprsParameters = a}) . _Default;

-- | Provides an identifier to allow retrieval of paginated results.
dcprsMarker :: Lens' DescribeCacheParametersResponse (Maybe Text)
dcprsMarker = lens _dcprsMarker (\ s a -> s{_dcprsMarker = a});

-- | FIXME: Undocumented member.
dcprsStatus :: Lens' DescribeCacheParametersResponse Int
dcprsStatus = lens _dcprsStatus (\ s a -> s{_dcprsStatus = a});
