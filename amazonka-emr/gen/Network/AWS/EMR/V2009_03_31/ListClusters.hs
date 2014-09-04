{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TypeFamilies                #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Module      : Network.AWS.EMR.V2009_03_31.ListClusters
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
module Network.AWS.EMR.V2009_03_31.ListClusters
    (
    -- * Request
      ListClusters
    -- ** Request constructor
    , mkListClustersInput
    -- ** Request lenses
    , lciCreatedAfter
    , lciCreatedBefore
    , lciClusterStates
    , lciMarker

    -- * Response
    , ListClustersResponse
    -- ** Response lenses
    , lcoClusters
    , lcoMarker
    ) where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Smart constructor for the minimum required parameters to construct
-- a valid 'ListClusters' request.
mkListClustersInput :: ListClusters
mkListClustersInput = ListClusters
    { _lciCreatedAfter = Nothing
    , _lciCreatedBefore = Nothing
    , _lciClusterStates = mempty
    , _lciMarker = Nothing
    }
{-# INLINE mkListClustersInput #-}

data ListClusters = ListClusters
    { _lciCreatedAfter :: Maybe POSIX
      -- ^ The creation date and time beginning value filter for listing
      -- clusters .
    , _lciCreatedBefore :: Maybe POSIX
      -- ^ The creation date and time end value filter for listing clusters
      -- .
    , _lciClusterStates :: [ClusterState]
      -- ^ The cluster state filters to apply when listing clusters.
    , _lciMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    } deriving (Show, Generic)

-- | The creation date and time beginning value filter for listing clusters .
lciCreatedAfter :: Lens' ListClusters (Maybe POSIX)
lciCreatedAfter = lens _lciCreatedAfter (\s a -> s { _lciCreatedAfter = a })
{-# INLINE lciCreatedAfter #-}

-- | The creation date and time end value filter for listing clusters .
lciCreatedBefore :: Lens' ListClusters (Maybe POSIX)
lciCreatedBefore = lens _lciCreatedBefore (\s a -> s { _lciCreatedBefore = a })
{-# INLINE lciCreatedBefore #-}

-- | The cluster state filters to apply when listing clusters.
lciClusterStates :: Lens' ListClusters ([ClusterState])
lciClusterStates = lens _lciClusterStates (\s a -> s { _lciClusterStates = a })
{-# INLINE lciClusterStates #-}

-- | The pagination token that indicates the next set of results to retrieve.
lciMarker :: Lens' ListClusters (Maybe Text)
lciMarker = lens _lciMarker (\s a -> s { _lciMarker = a })
{-# INLINE lciMarker #-}

instance ToPath ListClusters

instance ToQuery ListClusters

instance ToHeaders ListClusters

instance ToJSON ListClusters

data ListClustersResponse = ListClustersResponse
    { _lcoClusters :: [ClusterSummary]
      -- ^ The list of clusters for the account based on the given filters.
    , _lcoMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    } deriving (Show, Generic)

-- | The list of clusters for the account based on the given filters.
lcoClusters :: Lens' ListClustersResponse ([ClusterSummary])
lcoClusters = lens _lcoClusters (\s a -> s { _lcoClusters = a })
{-# INLINE lcoClusters #-}

-- | The pagination token that indicates the next set of results to retrieve.
lcoMarker :: Lens' ListClustersResponse (Maybe Text)
lcoMarker = lens _lcoMarker (\s a -> s { _lcoMarker = a })
{-# INLINE lcoMarker #-}

instance FromJSON ListClustersResponse

instance AWSRequest ListClusters where
    type Sv ListClusters = EMR
    type Rs ListClusters = ListClustersResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListClusters where
    next rq rs = (\x -> rq { _lciMarker = Just x })
        <$> (_lcoMarker rs)
