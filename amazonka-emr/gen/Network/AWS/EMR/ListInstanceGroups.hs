{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListInstanceGroups
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provides all available details about the instance groups in a cluster.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ListInstanceGroups.html AWS API Reference> for ListInstanceGroups.
module Network.AWS.EMR.ListInstanceGroups
    (
    -- * Creating a Request
      ListInstanceGroups
    , listInstanceGroups
    -- * Request Lenses
    , ligMarker
    , ligClusterId

    -- * Destructuring the Response
    , ListInstanceGroupsResponse
    , listInstanceGroupsResponse
    -- * Response Lenses
    , ligrsMarker
    , ligrsInstanceGroups
    , ligrsStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'listInstanceGroups' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ligMarker'
--
-- * 'ligClusterId'
data ListInstanceGroups = ListInstanceGroups'
    { _ligMarker    :: !(Maybe Text)
    , _ligClusterId :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListInstanceGroups' smart constructor.
listInstanceGroups :: Text -> ListInstanceGroups
listInstanceGroups pClusterId_ =
    ListInstanceGroups'
    { _ligMarker = Nothing
    , _ligClusterId = pClusterId_
    }

-- | The pagination token that indicates the next set of results to retrieve.
ligMarker :: Lens' ListInstanceGroups (Maybe Text)
ligMarker = lens _ligMarker (\ s a -> s{_ligMarker = a});

-- | The identifier of the cluster for which to list the instance groups.
ligClusterId :: Lens' ListInstanceGroups Text
ligClusterId = lens _ligClusterId (\ s a -> s{_ligClusterId = a});

instance AWSPager ListInstanceGroups where
        page rq rs
          | stop (rs ^. ligrsMarker) = Nothing
          | stop (rs ^. ligrsInstanceGroups) = Nothing
          | otherwise =
            Just $ rq & ligMarker .~ rs ^. ligrsMarker

instance AWSRequest ListInstanceGroups where
        type Sv ListInstanceGroups = EMR
        type Rs ListInstanceGroups =
             ListInstanceGroupsResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListInstanceGroupsResponse' <$>
                   (x .?> "Marker") <*>
                     (x .?> "InstanceGroups" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance ToHeaders ListInstanceGroups where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.ListInstanceGroups" ::
                       ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListInstanceGroups where
        toJSON ListInstanceGroups'{..}
          = object
              ["Marker" .= _ligMarker,
               "ClusterId" .= _ligClusterId]

instance ToPath ListInstanceGroups where
        toPath = const "/"

instance ToQuery ListInstanceGroups where
        toQuery = const mempty

-- | This input determines which instance groups to retrieve.
--
-- /See:/ 'listInstanceGroupsResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'ligrsMarker'
--
-- * 'ligrsInstanceGroups'
--
-- * 'ligrsStatus'
data ListInstanceGroupsResponse = ListInstanceGroupsResponse'
    { _ligrsMarker         :: !(Maybe Text)
    , _ligrsInstanceGroups :: !(Maybe [InstanceGroup])
    , _ligrsStatus         :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListInstanceGroupsResponse' smart constructor.
listInstanceGroupsResponse :: Int -> ListInstanceGroupsResponse
listInstanceGroupsResponse pStatus_ =
    ListInstanceGroupsResponse'
    { _ligrsMarker = Nothing
    , _ligrsInstanceGroups = Nothing
    , _ligrsStatus = pStatus_
    }

-- | The pagination token that indicates the next set of results to retrieve.
ligrsMarker :: Lens' ListInstanceGroupsResponse (Maybe Text)
ligrsMarker = lens _ligrsMarker (\ s a -> s{_ligrsMarker = a});

-- | The list of instance groups for the cluster and given filters.
ligrsInstanceGroups :: Lens' ListInstanceGroupsResponse [InstanceGroup]
ligrsInstanceGroups = lens _ligrsInstanceGroups (\ s a -> s{_ligrsInstanceGroups = a}) . _Default . _Coerce;

-- | Undocumented member.
ligrsStatus :: Lens' ListInstanceGroupsResponse Int
ligrsStatus = lens _ligrsStatus (\ s a -> s{_ligrsStatus = a});
