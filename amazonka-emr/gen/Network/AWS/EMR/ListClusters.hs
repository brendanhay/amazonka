{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE NoImplicitPrelude          #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE TypeFamilies               #-}

-- {-# OPTIONS_GHC -fno-warn-unused-imports #-}
-- {-# OPTIONS_GHC -fno-warn-unused-binds  #-} doesnt work if wall is used
{-# OPTIONS_GHC -w #-}

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
-- filtering by cluster creation date and time or by status. This call returns
-- a maximum of 50 clusters per call, but returns a marker to track the paging
-- of the cluster list across multiple ListClusters calls.
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

import Data.Aeson
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.EMR.Types

data ListClusters = ListClusters
    { _lcClusterStates :: [Text]
    , _lcCreatedAfter  :: Maybe RFC822
    , _lcCreatedBefore :: Maybe RFC822
    , _lcMarker        :: Maybe Text
    } deriving (Eq, Ord, Show, Generic)

-- | 'ListClusters' constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcClusterStates' @::@ ['Text']
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
lcClusterStates :: Lens' ListClusters [Text]
lcClusterStates = lens _lcClusterStates (\s a -> s { _lcClusterStates = a })

-- | The creation date and time beginning value filter for listing clusters .
lcCreatedAfter :: Lens' ListClusters (Maybe UTCTime)
lcCreatedAfter = lens _lcCreatedAfter (\s a -> s { _lcCreatedAfter = a })
    . mapping _Time

-- | The creation date and time end value filter for listing clusters .
lcCreatedBefore :: Lens' ListClusters (Maybe UTCTime)
lcCreatedBefore = lens _lcCreatedBefore (\s a -> s { _lcCreatedBefore = a })
    . mapping _Time

-- | The pagination token that indicates the next set of results to retrieve.
lcMarker :: Lens' ListClusters (Maybe Text)
lcMarker = lens _lcMarker (\s a -> s { _lcMarker = a })

instance ToPath ListClusters where
    toPath = const "/"

instance ToQuery ListClusters where
    toQuery = const mempty

instance ToHeaders ListClusters

instance ToBody ListClusters where
    toBody = toBody . encode . _lcCreatedAfter

data ListClustersResponse = ListClustersResponse
    { _lcrClusters :: [ClusterSummary]
    , _lcrMarker   :: Maybe Text
    } deriving (Eq, Show, Generic)

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
lcrClusters :: Lens' ListClustersResponse [ClusterSummary]
lcrClusters = lens _lcrClusters (\s a -> s { _lcrClusters = a })

-- | The pagination token that indicates the next set of results to retrieve.
lcrMarker :: Lens' ListClustersResponse (Maybe Text)
lcrMarker = lens _lcrMarker (\s a -> s { _lcrMarker = a })

-- FromJSON

instance AWSRequest ListClusters where
    type Sv ListClusters = EMR
    type Rs ListClusters = ListClustersResponse

    request  = post'
    response = jsonResponse $ \h o -> ListClustersResponse
        <$> o .: "Clusters"
        <*> o .: "Marker"
