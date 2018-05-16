{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListClusters
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides the status of all clusters visible to this AWS account. Allows you to filter the list of clusters based on certain criteria; for example, filtering by cluster creation date and time or by status. This call returns a maximum of 50 clusters per call, but returns a marker to track the paging of the cluster list across multiple ListClusters calls.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListClusters
    (
    -- * Creating a Request
      listClusters
    , ListClusters
    -- * Request Lenses
    , lcCreatedAfter
    , lcMarker
    , lcClusterStates
    , lcCreatedBefore

    -- * Destructuring the Response
    , listClustersResponse
    , ListClustersResponse
    -- * Response Lenses
    , lcrsMarker
    , lcrsClusters
    , lcrsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | This input determines how the ListClusters action filters the list of clusters that it returns.
--
--
--
-- /See:/ 'listClusters' smart constructor.
data ListClusters = ListClusters'
  { _lcCreatedAfter  :: !(Maybe POSIX)
  , _lcMarker        :: !(Maybe Text)
  , _lcClusterStates :: !(Maybe [ClusterState])
  , _lcCreatedBefore :: !(Maybe POSIX)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListClusters' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcCreatedAfter' - The creation date and time beginning value filter for listing clusters.
--
-- * 'lcMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'lcClusterStates' - The cluster state filters to apply when listing clusters.
--
-- * 'lcCreatedBefore' - The creation date and time end value filter for listing clusters.
listClusters
    :: ListClusters
listClusters =
  ListClusters'
    { _lcCreatedAfter = Nothing
    , _lcMarker = Nothing
    , _lcClusterStates = Nothing
    , _lcCreatedBefore = Nothing
    }


-- | The creation date and time beginning value filter for listing clusters.
lcCreatedAfter :: Lens' ListClusters (Maybe UTCTime)
lcCreatedAfter = lens _lcCreatedAfter (\ s a -> s{_lcCreatedAfter = a}) . mapping _Time

-- | The pagination token that indicates the next set of results to retrieve.
lcMarker :: Lens' ListClusters (Maybe Text)
lcMarker = lens _lcMarker (\ s a -> s{_lcMarker = a})

-- | The cluster state filters to apply when listing clusters.
lcClusterStates :: Lens' ListClusters [ClusterState]
lcClusterStates = lens _lcClusterStates (\ s a -> s{_lcClusterStates = a}) . _Default . _Coerce

-- | The creation date and time end value filter for listing clusters.
lcCreatedBefore :: Lens' ListClusters (Maybe UTCTime)
lcCreatedBefore = lens _lcCreatedBefore (\ s a -> s{_lcCreatedBefore = a}) . mapping _Time

instance AWSPager ListClusters where
        page rq rs
          | stop (rs ^. lcrsMarker) = Nothing
          | stop (rs ^. lcrsClusters) = Nothing
          | otherwise =
            Just $ rq & lcMarker .~ rs ^. lcrsMarker

instance AWSRequest ListClusters where
        type Rs ListClusters = ListClustersResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 ListClustersResponse' <$>
                   (x .?> "Marker") <*> (x .?> "Clusters" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListClusters where

instance NFData ListClusters where

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
              (catMaybes
                 [("CreatedAfter" .=) <$> _lcCreatedAfter,
                  ("Marker" .=) <$> _lcMarker,
                  ("ClusterStates" .=) <$> _lcClusterStates,
                  ("CreatedBefore" .=) <$> _lcCreatedBefore])

instance ToPath ListClusters where
        toPath = const "/"

instance ToQuery ListClusters where
        toQuery = const mempty

-- | This contains a ClusterSummaryList with the cluster details; for example, the cluster IDs, names, and status.
--
--
--
-- /See:/ 'listClustersResponse' smart constructor.
data ListClustersResponse = ListClustersResponse'
  { _lcrsMarker         :: !(Maybe Text)
  , _lcrsClusters       :: !(Maybe [ClusterSummary])
  , _lcrsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListClustersResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lcrsMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'lcrsClusters' - The list of clusters for the account based on the given filters.
--
-- * 'lcrsResponseStatus' - -- | The response status code.
listClustersResponse
    :: Int -- ^ 'lcrsResponseStatus'
    -> ListClustersResponse
listClustersResponse pResponseStatus_ =
  ListClustersResponse'
    { _lcrsMarker = Nothing
    , _lcrsClusters = Nothing
    , _lcrsResponseStatus = pResponseStatus_
    }


-- | The pagination token that indicates the next set of results to retrieve.
lcrsMarker :: Lens' ListClustersResponse (Maybe Text)
lcrsMarker = lens _lcrsMarker (\ s a -> s{_lcrsMarker = a})

-- | The list of clusters for the account based on the given filters.
lcrsClusters :: Lens' ListClustersResponse [ClusterSummary]
lcrsClusters = lens _lcrsClusters (\ s a -> s{_lcrsClusters = a}) . _Default . _Coerce

-- | -- | The response status code.
lcrsResponseStatus :: Lens' ListClustersResponse Int
lcrsResponseStatus = lens _lcrsResponseStatus (\ s a -> s{_lcrsResponseStatus = a})

instance NFData ListClustersResponse where
