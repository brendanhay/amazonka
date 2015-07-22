{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListClusters
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provides the status of all clusters visible to this AWS account. Allows
-- you to filter the list of clusters based on certain criteria; for
-- example, filtering by cluster creation date and time or by status. This
-- call returns a maximum of 50 clusters per call, but returns a marker to
-- track the paging of the cluster list across multiple ListClusters calls.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ListClusters.html>
module Network.AWS.EMR.ListClusters
    (
    -- * Request
      ListClusters
    -- ** Request constructor
    , listClusters
    -- ** Request lenses
    , lcrqCreatedAfter
    , lcrqMarker
    , lcrqClusterStates
    , lcrqCreatedBefore

    -- * Response
    , ListClustersResponse
    -- ** Response constructor
    , listClustersResponse
    -- ** Response lenses
    , lcrsMarker
    , lcrsClusters
    , lcrsStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input determines how the ListClusters action filters the list of
-- clusters that it returns.
--
-- /See:/ 'listClusters' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcrqCreatedAfter'
--
-- * 'lcrqMarker'
--
-- * 'lcrqClusterStates'
--
-- * 'lcrqCreatedBefore'
data ListClusters = ListClusters'
    { _lcrqCreatedAfter  :: !(Maybe POSIX)
    , _lcrqMarker        :: !(Maybe Text)
    , _lcrqClusterStates :: !(Maybe [ClusterState])
    , _lcrqCreatedBefore :: !(Maybe POSIX)
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListClusters' smart constructor.
listClusters :: ListClusters
listClusters =
    ListClusters'
    { _lcrqCreatedAfter = Nothing
    , _lcrqMarker = Nothing
    , _lcrqClusterStates = Nothing
    , _lcrqCreatedBefore = Nothing
    }

-- | The creation date and time beginning value filter for listing clusters .
lcrqCreatedAfter :: Lens' ListClusters (Maybe UTCTime)
lcrqCreatedAfter = lens _lcrqCreatedAfter (\ s a -> s{_lcrqCreatedAfter = a}) . mapping _Time;

-- | The pagination token that indicates the next set of results to retrieve.
lcrqMarker :: Lens' ListClusters (Maybe Text)
lcrqMarker = lens _lcrqMarker (\ s a -> s{_lcrqMarker = a});

-- | The cluster state filters to apply when listing clusters.
lcrqClusterStates :: Lens' ListClusters [ClusterState]
lcrqClusterStates = lens _lcrqClusterStates (\ s a -> s{_lcrqClusterStates = a}) . _Default;

-- | The creation date and time end value filter for listing clusters .
lcrqCreatedBefore :: Lens' ListClusters (Maybe UTCTime)
lcrqCreatedBefore = lens _lcrqCreatedBefore (\ s a -> s{_lcrqCreatedBefore = a}) . mapping _Time;

instance AWSPager ListClusters where
        page rq rs
          | stop (rs ^. lcrsMarker) = Nothing
          | stop (rs ^. lcrsClusters) = Nothing
          | otherwise =
            Just $ rq & lcrqMarker .~ rs ^. lcrsMarker

instance AWSRequest ListClusters where
        type Sv ListClusters = EMR
        type Rs ListClusters = ListClustersResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListClustersResponse' <$>
                   (x .?> "Marker") <*> (x .?> "Clusters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListClusters where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.ListClusters" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListClusters where
        toJSON ListClusters'{..}
          = object
              ["CreatedAfter" .= _lcrqCreatedAfter,
               "Marker" .= _lcrqMarker,
               "ClusterStates" .= _lcrqClusterStates,
               "CreatedBefore" .= _lcrqCreatedBefore]

instance ToPath ListClusters where
        toPath = const "/"

instance ToQuery ListClusters where
        toQuery = const mempty

-- | This contains a ClusterSummaryList with the cluster details; for
-- example, the cluster IDs, names, and status.
--
-- /See:/ 'listClustersResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lcrsMarker'
--
-- * 'lcrsClusters'
--
-- * 'lcrsStatus'
data ListClustersResponse = ListClustersResponse'
    { _lcrsMarker   :: !(Maybe Text)
    , _lcrsClusters :: !(Maybe [ClusterSummary])
    , _lcrsStatus   :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListClustersResponse' smart constructor.
listClustersResponse :: Int -> ListClustersResponse
listClustersResponse pStatus =
    ListClustersResponse'
    { _lcrsMarker = Nothing
    , _lcrsClusters = Nothing
    , _lcrsStatus = pStatus
    }

-- | The pagination token that indicates the next set of results to retrieve.
lcrsMarker :: Lens' ListClustersResponse (Maybe Text)
lcrsMarker = lens _lcrsMarker (\ s a -> s{_lcrsMarker = a});

-- | The list of clusters for the account based on the given filters.
lcrsClusters :: Lens' ListClustersResponse [ClusterSummary]
lcrsClusters = lens _lcrsClusters (\ s a -> s{_lcrsClusters = a}) . _Default;

-- | FIXME: Undocumented member.
lcrsStatus :: Lens' ListClustersResponse Int
lcrsStatus = lens _lcrsStatus (\ s a -> s{_lcrsStatus = a});
