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
-- Module      : Network.AWS.EMR.ListInstances
-- Copyright   : (c) 2013-2015 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information about the cluster instances that Amazon EMR
-- provisions on behalf of a user when it creates the cluster. For example,
-- this operation indicates when the EC2 instances reach the Ready state,
-- when instances become available to Amazon EMR to use for jobs, and the
-- IP addresses for cluster instances, etc.
--
-- /See:/ <http://docs.aws.amazon.com/ElasticMapReduce/latest/API/API_ListInstances.html AWS API Reference> for ListInstances.
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstances
    (
    -- * Creating a Request
      listInstances
    , ListInstances
    -- * Request Lenses
    , liInstanceGroupTypes
    , liMarker
    , liInstanceGroupId
    , liClusterId

    -- * Destructuring the Response
    , listInstancesResponse
    , ListInstancesResponse
    -- * Response Lenses
    , lirsInstances
    , lirsMarker
    , lirsStatus
    ) where

import           Network.AWS.EMR.Types
import           Network.AWS.EMR.Types.Product
import           Network.AWS.Pager
import           Network.AWS.Prelude
import           Network.AWS.Request
import           Network.AWS.Response

-- | This input determines which instances to list.
--
-- /See:/ 'listInstances' smart constructor.
data ListInstances = ListInstances'
    { _liInstanceGroupTypes :: !(Maybe [InstanceGroupType])
    , _liMarker             :: !(Maybe Text)
    , _liInstanceGroupId    :: !(Maybe Text)
    , _liClusterId          :: !Text
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liInstanceGroupTypes'
--
-- * 'liMarker'
--
-- * 'liInstanceGroupId'
--
-- * 'liClusterId'
listInstances
    :: Text -- ^ 'liClusterId'
    -> ListInstances
listInstances pClusterId_ =
    ListInstances'
    { _liInstanceGroupTypes = Nothing
    , _liMarker = Nothing
    , _liInstanceGroupId = Nothing
    , _liClusterId = pClusterId_
    }

-- | The type of instance group for which to list the instances.
liInstanceGroupTypes :: Lens' ListInstances [InstanceGroupType]
liInstanceGroupTypes = lens _liInstanceGroupTypes (\ s a -> s{_liInstanceGroupTypes = a}) . _Default . _Coerce;

-- | The pagination token that indicates the next set of results to retrieve.
liMarker :: Lens' ListInstances (Maybe Text)
liMarker = lens _liMarker (\ s a -> s{_liMarker = a});

-- | The identifier of the instance group for which to list the instances.
liInstanceGroupId :: Lens' ListInstances (Maybe Text)
liInstanceGroupId = lens _liInstanceGroupId (\ s a -> s{_liInstanceGroupId = a});

-- | The identifier of the cluster for which to list the instances.
liClusterId :: Lens' ListInstances Text
liClusterId = lens _liClusterId (\ s a -> s{_liClusterId = a});

instance AWSPager ListInstances where
        page rq rs
          | stop (rs ^. lirsMarker) = Nothing
          | stop (rs ^. lirsInstances) = Nothing
          | otherwise =
            Just $ rq & liMarker .~ rs ^. lirsMarker

instance AWSRequest ListInstances where
        type Rs ListInstances = ListInstancesResponse
        request = postJSON eMR
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
              (catMaybes
                 [("InstanceGroupTypes" .=) <$> _liInstanceGroupTypes,
                  ("Marker" .=) <$> _liMarker,
                  ("InstanceGroupId" .=) <$> _liInstanceGroupId,
                  Just ("ClusterId" .= _liClusterId)])

instance ToPath ListInstances where
        toPath = const "/"

instance ToQuery ListInstances where
        toQuery = const mempty

-- | This output contains the list of instances.
--
-- /See:/ 'listInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
    { _lirsInstances :: !(Maybe [Instance])
    , _lirsMarker    :: !(Maybe Text)
    , _lirsStatus    :: !Int
    } deriving (Eq,Read,Show,Data,Typeable,Generic)

-- | Creates a value of 'ListInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsInstances'
--
-- * 'lirsMarker'
--
-- * 'lirsStatus'
listInstancesResponse
    :: Int -- ^ 'lirsStatus'
    -> ListInstancesResponse
listInstancesResponse pStatus_ =
    ListInstancesResponse'
    { _lirsInstances = Nothing
    , _lirsMarker = Nothing
    , _lirsStatus = pStatus_
    }

-- | The list of instances for the cluster and given filters.
lirsInstances :: Lens' ListInstancesResponse [Instance]
lirsInstances = lens _lirsInstances (\ s a -> s{_lirsInstances = a}) . _Default . _Coerce;

-- | The pagination token that indicates the next set of results to retrieve.
lirsMarker :: Lens' ListInstancesResponse (Maybe Text)
lirsMarker = lens _lirsMarker (\ s a -> s{_lirsMarker = a});

-- | The response status code.
lirsStatus :: Lens' ListInstancesResponse Int
lirsStatus = lens _lirsStatus (\ s a -> s{_lirsStatus = a});
