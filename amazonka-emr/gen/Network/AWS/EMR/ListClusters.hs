{-# LANGUAGE DataKinds                   #-}
{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE GeneralizedNewtypeDeriving  #-}
{-# LANGUAGE LambdaCase                  #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.ListClusters
-- Copyright   : (c) 2013-2014 Brendan Hay <brendan.g.hay@gmail.com>
-- License     : This Source Code Form is subject to the terms of
--               the Mozilla Public License, v. 2.0.
--               A copy of the MPL can be found in the LICENSE file or
--               you can obtain it at http://mozilla.org/MPL/2.0/.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)

-- | Provides the status of all clusters visible to this AWS account. Allows you
-- to filter the list of clusters based on certain criteria; for example,
-- filtering by cluster creation date and time or by status. This call returns a
-- maximum of 50 clusters per call, but returns a marker to track the paging of
-- the cluster list across multiple ListClusters calls.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ListClusters.html>
module Network.AWS.EMR.ListClusters
    (
    -- * Request
      ListClusters
    -- ** Request constructor
    , listClusters
    -- ** Request lenses
    , lcClusterStates
    , lcCreatedAfter
    , lcCreatedBefore
    , lcMarker

    -- * Response
    , ListClustersResponse
    -- ** Response constructor
    , listClustersResponse
    -- ** Response lenses
    , lcrClusters
    , lcrMarker
    ) where

import Network.AWS.Prelude
import Network.AWS.Request.JSON
import Network.AWS.EMR.Types
import qualified GHC.Exts

data ListClusters = ListClusters
    { _lcClusterStates :: List "ClusterStates" ClusterState
    , _lcCreatedAfter  :: Maybe RFC822
    , _lcCreatedBefore :: Maybe RFC822
    , _lcMarker        :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListClusters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcClusterStates' @::@ ['ClusterState']
--
-- * 'lcCreatedAfter' @::@ 'Maybe' 'UTCTime'
--
-- * 'lcCreatedBefore' @::@ 'Maybe' 'UTCTime'
--
-- * 'lcMarker' @::@ 'Maybe' 'Text'
--
listClusters :: ListClusters
listClusters = ListClusters
    { _lcCreatedAfter  = Nothing
    , _lcCreatedBefore = Nothing
    , _lcClusterStates = mempty
    , _lcMarker        = Nothing
    }

-- | The cluster state filters to apply when listing clusters.
--
lcClusterStates :: Lens' ListClusters [ClusterState]
lcClusterStates = lens _lcClusterStates (\s a -> s { _lcClusterStates = a }) . _List

-- | The creation date and time beginning value filter for listing clusters .
--
lcCreatedAfter :: Lens' ListClusters (Maybe UTCTime)
lcCreatedAfter = lens _lcCreatedAfter (\s a -> s { _lcCreatedAfter = a }) . mapping _Time

-- | The creation date and time end value filter for listing clusters .
--
lcCreatedBefore :: Lens' ListClusters (Maybe UTCTime)
lcCreatedBefore = lens _lcCreatedBefore (\s a -> s { _lcCreatedBefore = a }) . mapping _Time

-- | The pagination token that indicates the next set of results to retrieve.
--
lcMarker :: Lens' ListClusters (Maybe Text)
lcMarker = lens _lcMarker (\s a -> s { _lcMarker = a })

data ListClustersResponse = ListClustersResponse
    { _lcrClusters :: List "Clusters" ClusterSummary
    , _lcrMarker   :: Maybe Text
    } deriving (Eq, Show)

-- | 'ListClustersResponse' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcrClusters' @::@ ['ClusterSummary']
--
-- * 'lcrMarker' @::@ 'Maybe' 'Text'
--
listClustersResponse :: ListClustersResponse
listClustersResponse = ListClustersResponse
    { _lcrClusters = mempty
    , _lcrMarker   = Nothing
    }

-- | The list of clusters for the account based on the given filters.
--
lcrClusters :: Lens' ListClustersResponse [ClusterSummary]
lcrClusters = lens _lcrClusters (\s a -> s { _lcrClusters = a }) . _List

-- | The pagination token that indicates the next set of results to retrieve.
--
lcrMarker :: Lens' ListClustersResponse (Maybe Text)
lcrMarker = lens _lcrMarker (\s a -> s { _lcrMarker = a })

instance ToPath ListClusters where
    toPath = const "/"

instance ToQuery ListClusters where
    toQuery = const mempty

instance ToHeaders ListClusters

instance ToJSON ListClusters where
    toJSON ListClusters{..} = object
        [ "CreatedAfter"  .= _lcCreatedAfter
        , "CreatedBefore" .= _lcCreatedBefore
        , "ClusterStates" .= _lcClusterStates
        , "Marker"        .= _lcMarker
        ]

instance AWSRequest ListClusters where
    type Sv ListClusters = EMR
    type Rs ListClusters = ListClustersResponse

    request  = post "ListClusters"
    response = jsonResponse

instance FromJSON ListClustersResponse where
    parseJSON = withObject "ListClustersResponse" $ \o -> ListClustersResponse
        <$> o .:  "Clusters"
        <*> o .:? "Marker"

instance AWSPager ListClusters where
    page rq rs
        | stop (rq ^. lcMarker) = Nothing
        | otherwise = (\x -> rq & lcMarker ?~ x)
            <$> (rs ^. lcrMarker)
