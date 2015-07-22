{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric      #-}
{-# LANGUAGE OverloadedStrings  #-}
{-# LANGUAGE RecordWildCards    #-}
{-# LANGUAGE TypeFamilies       #-}

{-# OPTIONS_GHC -fno-warn-unused-binds   #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EMR.ListInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : experimental
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the cluster instances that Amazon EMR
-- provisions on behalf of a user when it creates the cluster. For example,
-- this operation indicates when the EC2 instances reach the Ready state,
-- when instances become available to Amazon EMR to use for jobs, and the
-- IP addresses for cluster instances, etc.
--
-- <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ListInstances.html>
module Network.AWS.EMR.ListInstances
    (
    -- * Request
      ListInstances
    -- ** Request constructor
    , listInstances
    -- ** Request lenses
    , lirqInstanceGroupTypes
    , lirqMarker
    , lirqInstanceGroupId
    , lirqClusterId

    -- * Response
    , ListInstancesResponse
    -- ** Response constructor
    , listInstancesResponse
    -- ** Response lenses
    , lirsInstances
    , lirsMarker
    , lirsStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input determines which instances to list.
--
-- /See:/ 'listInstances' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirqInstanceGroupTypes'
--
-- * 'lirqMarker'
--
-- * 'lirqInstanceGroupId'
--
-- * 'lirqClusterId'
data ListInstances = ListInstances'
    { _lirqInstanceGroupTypes :: !(Maybe [InstanceGroupType])
    , _lirqMarker             :: !(Maybe Text)
    , _lirqInstanceGroupId    :: !(Maybe Text)
    , _lirqClusterId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListInstances' smart constructor.
listInstances :: Text -> ListInstances
listInstances pClusterId_ =
    ListInstances'
    { _lirqInstanceGroupTypes = Nothing
    , _lirqMarker = Nothing
    , _lirqInstanceGroupId = Nothing
    , _lirqClusterId = pClusterId_
    }

-- | The type of instance group for which to list the instances.
lirqInstanceGroupTypes :: Lens' ListInstances [InstanceGroupType]
lirqInstanceGroupTypes = lens _lirqInstanceGroupTypes (\ s a -> s{_lirqInstanceGroupTypes = a}) . _Default;

-- | The pagination token that indicates the next set of results to retrieve.
lirqMarker :: Lens' ListInstances (Maybe Text)
lirqMarker = lens _lirqMarker (\ s a -> s{_lirqMarker = a});

-- | The identifier of the instance group for which to list the instances.
lirqInstanceGroupId :: Lens' ListInstances (Maybe Text)
lirqInstanceGroupId = lens _lirqInstanceGroupId (\ s a -> s{_lirqInstanceGroupId = a});

-- | The identifier of the cluster for which to list the instances.
lirqClusterId :: Lens' ListInstances Text
lirqClusterId = lens _lirqClusterId (\ s a -> s{_lirqClusterId = a});

instance AWSPager ListInstances where
        page rq rs
          | stop (rs ^. lirsMarker) = Nothing
          | stop (rs ^. lirsInstances) = Nothing
          | otherwise =
            Just $ rq & lirqMarker .~ rs ^. lirsMarker

instance AWSRequest ListInstances where
        type Sv ListInstances = EMR
        type Rs ListInstances = ListInstancesResponse
        request = postJSON
        response
          = receiveJSON
              (\ s h x ->
                 ListInstancesResponse' <$>
                   (x .?> "Instances" .!@ mempty) <*> (x .?> "Marker")
                     <*> (pure (fromEnum s)))

instance ToHeaders ListInstances where
        toHeaders
          = const
              (mconcat
                 ["X-Amz-Target" =#
                    ("ElasticMapReduce.ListInstances" :: ByteString),
                  "Content-Type" =#
                    ("application/x-amz-json-1.1" :: ByteString)])

instance ToJSON ListInstances where
        toJSON ListInstances'{..}
          = object
              ["InstanceGroupTypes" .= _lirqInstanceGroupTypes,
               "Marker" .= _lirqMarker,
               "InstanceGroupId" .= _lirqInstanceGroupId,
               "ClusterId" .= _lirqClusterId]

instance ToPath ListInstances where
        toPath = const "/"

instance ToQuery ListInstances where
        toQuery = const mempty

-- | This output contains the list of instances.
--
-- /See:/ 'listInstancesResponse' smart constructor.
--
-- The fields accessible through corresponding lenses are:
--
-- * 'lirsInstances'
--
-- * 'lirsMarker'
--
-- * 'lirsStatus'
data ListInstancesResponse = ListInstancesResponse'
    { _lirsInstances :: !(Maybe [Instance])
    , _lirsMarker    :: !(Maybe Text)
    , _lirsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | 'ListInstancesResponse' smart constructor.
listInstancesResponse :: Int -> ListInstancesResponse
listInstancesResponse pStatus_ =
    ListInstancesResponse'
    { _lirsInstances = Nothing
    , _lirsMarker = Nothing
    , _lirsStatus = pStatus_
    }

-- | The list of instances for the cluster and given filters.
lirsInstances :: Lens' ListInstancesResponse [Instance]
lirsInstances = lens _lirsInstances (\ s a -> s{_lirsInstances = a}) . _Default;

-- | The pagination token that indicates the next set of results to retrieve.
lirsMarker :: Lens' ListInstancesResponse (Maybe Text)
lirsMarker = lens _lirsMarker (\ s a -> s{_lirsMarker = a});

-- | FIXME: Undocumented member.
lirsStatus :: Lens' ListInstancesResponse Int
lirsStatus = lens _lirsStatus (\ s a -> s{_lirsStatus = a});
