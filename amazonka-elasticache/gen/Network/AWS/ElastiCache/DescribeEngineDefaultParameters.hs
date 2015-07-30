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
    , dedpMaxRecords
    , dedpMarker
    , dedpCacheParameterGroupFamily

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
-- * 'dedpMaxRecords'
--
-- * 'dedpMarker'
--
-- * 'dedpCacheParameterGroupFamily'
data DescribeEngineDefaultParameters = DescribeEngineDefaultParameters'
    { _dedpMaxRecords                :: !(Maybe Int)
    , _dedpMarker                    :: !(Maybe Text)
    , _dedpCacheParameterGroupFamily :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'DescribeEngineDefaultParameters' smart constructor.
describeEngineDefaultParameters :: Text -> DescribeEngineDefaultParameters
describeEngineDefaultParameters pCacheParameterGroupFamily_ =
    DescribeEngineDefaultParameters'
    { _dedpMaxRecords = Nothing
    , _dedpMarker = Nothing
    , _dedpCacheParameterGroupFamily = pCacheParameterGroupFamily_
    }

-- | The maximum number of records to include in the response. If more
-- records exist than the specified @MaxRecords@ value, a marker is
-- included in the response so that the remaining results can be retrieved.
--
-- Default: 100
--
-- Constraints: minimum 20; maximum 100.
dedpMaxRecords :: Lens' DescribeEngineDefaultParameters (Maybe Int)
dedpMaxRecords = lens _dedpMaxRecords (\ s a -> s{_dedpMaxRecords = a});

-- | An optional marker returned from a prior request. Use this marker for
-- pagination of results from this action. If this parameter is specified,
-- the response includes only records beyond the marker, up to the value
-- specified by /MaxRecords/.
dedpMarker :: Lens' DescribeEngineDefaultParameters (Maybe Text)
dedpMarker = lens _dedpMarker (\ s a -> s{_dedpMarker = a});

-- | The name of the cache parameter group family. Valid values are:
-- @memcached1.4@ | @redis2.6@ | @redis2.8@
dedpCacheParameterGroupFamily :: Lens' DescribeEngineDefaultParameters Text
dedpCacheParameterGroupFamily = lens _dedpCacheParameterGroupFamily (\ s a -> s{_dedpCacheParameterGroupFamily = a});

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
        type Sv DescribeEngineDefaultParameters = ElastiCache
        type Rs DescribeEngineDefaultParameters =
             DescribeEngineDefaultParametersResponse
        request = postQuery
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
        toPath = const mempty

instance ToQuery DescribeEngineDefaultParameters
         where
        toQuery DescribeEngineDefaultParameters'{..}
          = mconcat
              ["Action" =:
                 ("DescribeEngineDefaultParameters" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "MaxRecords" =: _dedpMaxRecords,
               "Marker" =: _dedpMarker,
               "CacheParameterGroupFamily" =:
                 _dedpCacheParameterGroupFamily]

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
describeEngineDefaultParametersResponse pStatus_ pEngineDefaults_ =
    DescribeEngineDefaultParametersResponse'
    { _dedprsStatus = pStatus_
    , _dedprsEngineDefaults = pEngineDefaults_
    }

-- | FIXME: Undocumented member.
dedprsStatus :: Lens' DescribeEngineDefaultParametersResponse Int
dedprsStatus = lens _dedprsStatus (\ s a -> s{_dedprsStatus = a});

-- | FIXME: Undocumented member.
dedprsEngineDefaults :: Lens' DescribeEngineDefaultParametersResponse EngineDefaults
dedprsEngineDefaults = lens _dedprsEngineDefaults (\ s a -> s{_dedprsEngineDefaults = a});
