{-# LANGUAGE DeriveGeneric               #-}
{-# LANGUAGE FlexibleInstances           #-}
{-# LANGUAGE NoImplicitPrelude           #-}
{-# LANGUAGE OverloadedStrings           #-}
{-# LANGUAGE RecordWildCards             #-}
{-# LANGUAGE TemplateHaskell             #-}
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
module Network.AWS.EMR.V2009_03_31.ListClusters where

import           Network.AWS.EMR.V2009_03_31.Types
import           Network.AWS.Prelude
import           Network.AWS.Request.JSON
import qualified Network.AWS.Types.Map    as Map

-- | Minimum specification for a 'ListClusters' request.
listClusters :: ListClusters
listClusters = ListClusters
    { _lciClusterStates = mempty
    , _lciCreatedAfter = Nothing
    , _lciCreatedBefore = Nothing
    , _lciMarker = Nothing
    }

data ListClusters = ListClusters
    { _lciClusterStates :: [ClusterState]
      -- ^ The cluster state filters to apply when listing clusters.
    , _lciCreatedAfter :: Maybe POSIX
      -- ^ The creation date and time beginning value filter for listing
      -- clusters .
    , _lciCreatedBefore :: Maybe POSIX
      -- ^ The creation date and time end value filter for listing clusters
      -- .
    , _lciMarker :: Maybe Text
      -- ^ The pagination token that indicates the next set of results to
      -- retrieve.
    } deriving (Show, Generic)

makeLenses ''ListClusters

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

makeLenses ''ListClustersResponse

instance FromJSON ListClustersResponse

instance AWSRequest ListClusters where
    type Sv ListClusters = EMR
    type Rs ListClusters = ListClustersResponse

    request = get
    response _ = jsonResponse

instance AWSPager ListClusters where
    next rq rs = (\x -> rq { _lciMarker = Just x })
        <$> (_lcoMarker rs)
