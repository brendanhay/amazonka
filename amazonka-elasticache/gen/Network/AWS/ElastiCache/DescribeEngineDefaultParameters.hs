{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DescribeEngineDefaultParameters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- The /DescribeEngineDefaultParameters/ action returns the default engine
-- and system parameter information for the specified cache engine.
--
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/APIReference/API_DescribeEngineDefaultParameters.html>
module Network.AWS.ElastiCache.DescribeEngineDefaultParameters
    (
    -- * Request
      DescribeEngineDefaultParameters
    -- ** Request constructor
    , describeEngineDefaultParameters
    -- ** Request lenses
    , dedprqMaxRecords
    , dedprqMarker
    , dedprqCacheParameterGroupFamily

    -- * Response
    , DescribeEngineDefaultParametersResponse
    -- ** Response constructor
    , describeEngineDefaultParametersResponse
    -- ** Response lenses
    , dedprsStatus
    , dedprsEngineDefaults
    ) where

import           Network.AWS.ElastiCache.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | Represents the input of a /DescribeEngineDefaultParameters/ action.
--
-- /See:/ 'describeEngineDefaultParameters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedprqMaxRecords'
--
-- * 'dedprqMarker'
--
-- * 'dedprqCacheParameterGroupFamily'
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
    { _dedprqMaxRecords                :: !(Maybe Int)
    , _dedprqMarker                    :: !(Maybe Text)
    , _dedprqCacheParameterGroupFamily :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEngineDefaultParameters' smart constructor.
describeEngineDefaultParameters :: Text -> DescribeEngineDefaultParameters
describeEngineDefaultParameters pCacheParameterGroupFamily =
    DescribeEngineDefaultParameters'
    { _dedprqMaxRecords = Nothing
    , _dedprqMarker = Nothing
    , _dedprqCacheParameterGroupFamily = pCacheParameterGroupFamily
    }

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dedprqMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Int)
dedprqMaxRecords = lens _dedprqMaxRecords (\ s a -> s{_dedprqMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dedprqMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedprqMarker = lens _dedprqMarker (\ s a -> s{_dedprqMarker = a});

-- | The name of the cache parameter group family. Valid values are:
-- @memcached1.4@ | @redis2.6@ | @redis2.8@
dedprqCacheParameterGroupFamily :: Lens' DescribeEngineDefaultParameters Text
dedprqCacheParameterGroupFamily = lens _dedprqCacheParameterGroupFamily (\ s a -> s{_dedprqCacheParameterGroupFamily = a});

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
              dedprqMarker .~
                rs ^? dedprsEngineDefaults . edMarker . _Just

instance AWSRequest DescribeEngineDefaultParameters
         where
        type Sv DescribeEngineDefaultParameters = ElastiCache
        type Rs DescribeEngineDefaultParameters =
             DescribeEngineDefaultParametersResponse
        request = post
        response
          = receiveXMLWrapper
              "DescribeEngineDefaultParametersResult"
              (\ s h x ->
                 DescribeEngineDefaultParametersResponse' <$>
                   (pure (fromEnum s)) <*> (x .@ "EngineDefaults"))

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
               "MaxRecords" =: _dedprqMaxRecords,
               "Marker" =: _dedprqMarker,
               "CacheParameterGroupFamily" =:
                 _dedprqCacheParameterGroupFamily]

-- | /See:/ 'describeEngineDefaultParametersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'dedprsStatus'
--
-- * 'dedprsEngineDefaults'
data DescribeEngineDefaultParametersResponse = DescribeEngineDefaultParametersResponse'
    { _dedprsStatus         :: !Int
    , _dedprsEngineDefaults :: !EngineDefaults
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEngineDefaultParametersResponse' smart constructor.
describeEngineDefaultParametersResponse :: Int -> EngineDefaults -> DescribeEngineDefaultParametersResponse
describeEngineDefaultParametersResponse pStatus pEngineDefaults =
    DescribeEngineDefaultParametersResponse'
    { _dedprsStatus = pStatus
    , _dedprsEngineDefaults = pEngineDefaults
    }

-- | FIXME: Undocumented member.
dedprsStatus :: Lens' DescribeEngineDefaultParametersResponse Int
dedprsStatus = lens _dedprsStatus (\ s a -> s{_dedprsStatus = a});

-- | FIXME: Undocumented member.
dedprsEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse EngineDefaults
dedprsEngineDefaults = lens _dedprsEngineDefaults (\ s a -> s{_dedprsEngineDefaults = a});
