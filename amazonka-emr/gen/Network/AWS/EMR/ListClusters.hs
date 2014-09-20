{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
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
    , lcCreatedAfter
    , lcCreatedBefore
    , lcClusterStates
    , lcMarker

    -- * Response
    , ListClustersResponse
    -- ** Response constructor
    , listClustersResponse
    -- ** Response lenses
    , lcrClusters
    , lcrMarker
    ) where

import Network.AWS.EMR.Types
import Network.AWS.Prelude
import Network.AWS.Request.JSON

-- | This input determines how the ListClusters action filters the list of
-- clusters that it returns.
data ListClusters = ListClusters
    { _lcCreatedAfter :: Maybe POSIX
    , _lcCreatedBefore :: Maybe POSIX
    , _lcClusterStates :: [ClusterState]
    , _lcMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListClusters' request.
--
-- The fields accessible through corresponding lenses are:
--
-- * @CreatedAfter ::@ @Maybe POSIX@
--
-- * @CreatedBefore ::@ @Maybe POSIX@
--
-- * @ClusterStates ::@ @[ClusterState]@
--
-- * @Marker ::@ @Maybe Text@
--
listClusters :: ListClusters
listClusters = ListClusters
    { _lcCreatedAfter = Nothing
    , _lcCreatedBefore = Nothing
    , _lcClusterStates = mempty
    , _lcMarker = Nothing
    }

-- | The creation date and time beginning value filter for listing clusters .
lcCreatedAfter :: Lens' ListClusters (Maybe POSIX)
lcCreatedAfter = lens _lcCreatedAfter (\s a -> s { _lcCreatedAfter = a })

-- | The creation date and time end value filter for listing clusters .
lcCreatedBefore :: Lens' ListClusters (Maybe POSIX)
lcCreatedBefore = lens _lcCreatedBefore (\s a -> s { _lcCreatedBefore = a })

-- | The cluster state filters to apply when listing clusters.
lcClusterStates :: Lens' ListClusters [ClusterState]
lcClusterStates = lens _lcClusterStates (\s a -> s { _lcClusterStates = a })

-- | The pagination token that indicates the next set of results to retrieve.
lcMarker :: Lens' ListClusters (Maybe Text)
lcMarker = lens _lcMarker (\s a -> s { _lcMarker = a })

instance ToPath ListClusters

instance ToQuery ListClusters

instance ToHeaders ListClusters

instance ToJSON ListClusters

-- | This contains a ClusterSummaryList with the cluster details; for example,
-- the cluster IDs, names, and status.
data ListClustersResponse = ListClustersResponse
    { _lcrClusters :: [ClusterSummary]
    , _lcrMarker :: Maybe Text
    } deriving (Eq, Show, Generic)

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListClustersResponse' response.
--
-- This constructor is provided for convenience and testing purposes.
--
-- The fields accessible through corresponding lenses are:
--
-- * @Clusters ::@ @[ClusterSummary]@
--
-- * @Marker ::@ @Maybe Text@
--
listClustersResponse :: ListClustersResponse
listClustersResponse = ListClustersResponse
    { _lcrClusters = mempty
    , _lcrMarker = Nothing
    }

-- | The list of clusters for the account based on the given filters.
lcrClusters :: Lens' ListClustersResponse [ClusterSummary]
lcrClusters = lens _lcrClusters (\s a -> s { _lcrClusters = a })

-- | The pagination token that indicates the next set of results to retrieve.
lcrMarker :: Lens' ListClustersResponse (Maybe Text)
lcrMarker = lens _lcrMarker (\s a -> s { _lcrMarker = a })

instance FromJSON ListClustersResponse

instance AWSRequest ListClusters where
    type Sv ListClusters = EMR
    type Rs ListClusters = ListClustersResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListClusters where
    next rq rs = (\x -> rq & lcMarker ?~ x)
        <$> (rs ^. lcrMarker)
