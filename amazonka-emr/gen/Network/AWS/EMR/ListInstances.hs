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
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Provides information for all active EC2 instances and EC2 instances terminated in the last 30 days, up to a maximum of 2,000. EC2 instances in any of the following states are considered active: AWAITING_FULFILLMENT, PROVISIONING, BOOTSTRAPPING, RUNNING.
--
--
--
-- This operation returns paginated results.
module Network.AWS.EMR.ListInstances
    (
    -- * Creating a Request
      listInstances
    , ListInstances
    -- * Request Lenses
    , liInstanceGroupTypes
    , liInstanceFleetType
    , liMarker
    , liInstanceFleetId
    , liInstanceStates
    , liInstanceGroupId
    , liClusterId

    -- * Destructuring the Response
    , listInstancesResponse
    , ListInstancesResponse
    -- * Response Lenses
    , lirsMarker
    , lirsInstances
    , lirsResponseStatus
    ) where

import Network.AWS.EMR.Types
import Network.AWS.EMR.Types.Product
import Network.AWS.Lens
import Network.AWS.Pager
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | This input determines which instances to list.
--
--
--
-- /See:/ 'listInstances' smart constructor.
data ListInstances = ListInstances'
  { _liInstanceGroupTypes :: !(Maybe [InstanceGroupType])
  , _liInstanceFleetType  :: !(Maybe InstanceFleetType)
  , _liMarker             :: !(Maybe Text)
  , _liInstanceFleetId    :: !(Maybe Text)
  , _liInstanceStates     :: !(Maybe [InstanceState])
  , _liInstanceGroupId    :: !(Maybe Text)
  , _liClusterId          :: !Text
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListInstances' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'liInstanceGroupTypes' - The type of instance group for which to list the instances.
--
-- * 'liInstanceFleetType' - The node type of the instance fleet. For example MASTER, CORE, or TASK.
--
-- * 'liMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'liInstanceFleetId' - The unique identifier of the instance fleet.
--
-- * 'liInstanceStates' - A list of instance states that will filter the instances returned with this request.
--
-- * 'liInstanceGroupId' - The identifier of the instance group for which to list the instances.
--
-- * 'liClusterId' - The identifier of the cluster for which to list the instances.
listInstances
    :: Text -- ^ 'liClusterId'
    -> ListInstances
listInstances pClusterId_ =
  ListInstances'
    { _liInstanceGroupTypes = Nothing
    , _liInstanceFleetType = Nothing
    , _liMarker = Nothing
    , _liInstanceFleetId = Nothing
    , _liInstanceStates = Nothing
    , _liInstanceGroupId = Nothing
    , _liClusterId = pClusterId_
    }


-- | The type of instance group for which to list the instances.
liInstanceGroupTypes :: Lens' ListInstances [InstanceGroupType]
liInstanceGroupTypes = lens _liInstanceGroupTypes (\ s a -> s{_liInstanceGroupTypes = a}) . _Default . _Coerce

-- | The node type of the instance fleet. For example MASTER, CORE, or TASK.
liInstanceFleetType :: Lens' ListInstances (Maybe InstanceFleetType)
liInstanceFleetType = lens _liInstanceFleetType (\ s a -> s{_liInstanceFleetType = a})

-- | The pagination token that indicates the next set of results to retrieve.
liMarker :: Lens' ListInstances (Maybe Text)
liMarker = lens _liMarker (\ s a -> s{_liMarker = a})

-- | The unique identifier of the instance fleet.
liInstanceFleetId :: Lens' ListInstances (Maybe Text)
liInstanceFleetId = lens _liInstanceFleetId (\ s a -> s{_liInstanceFleetId = a})

-- | A list of instance states that will filter the instances returned with this request.
liInstanceStates :: Lens' ListInstances [InstanceState]
liInstanceStates = lens _liInstanceStates (\ s a -> s{_liInstanceStates = a}) . _Default . _Coerce

-- | The identifier of the instance group for which to list the instances.
liInstanceGroupId :: Lens' ListInstances (Maybe Text)
liInstanceGroupId = lens _liInstanceGroupId (\ s a -> s{_liInstanceGroupId = a})

-- | The identifier of the cluster for which to list the instances.
liClusterId :: Lens' ListInstances Text
liClusterId = lens _liClusterId (\ s a -> s{_liClusterId = a})

instance AWSPager ListInstances where
        page rq rs
          | stop (rs ^. lirsMarker) = Nothing
          | stop (rs ^. lirsInstances) = Nothing
          | otherwise =
            Just $ rq & liMarker .~ rs ^. lirsMarker

instance AWSRequest ListInstances where
        type Rs ListInstances = ListInstancesResponse
        request = postJSON emr
        response
          = receiveJSON
              (\ s h x ->
                 ListInstancesResponse' <$>
                   (x .?> "Marker") <*> (x .?> "Instances" .!@ mempty)
                     <*> (pure (fromEnum s)))

instance Hashable ListInstances where

instance NFData ListInstances where

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
                  ("InstanceFleetType" .=) <$> _liInstanceFleetType,
                  ("Marker" .=) <$> _liMarker,
                  ("InstanceFleetId" .=) <$> _liInstanceFleetId,
                  ("InstanceStates" .=) <$> _liInstanceStates,
                  ("InstanceGroupId" .=) <$> _liInstanceGroupId,
                  Just ("ClusterId" .= _liClusterId)])

instance ToPath ListInstances where
        toPath = const "/"

instance ToQuery ListInstances where
        toQuery = const mempty

-- | This output contains the list of instances.
--
--
--
-- /See:/ 'listInstancesResponse' smart constructor.
data ListInstancesResponse = ListInstancesResponse'
  { _lirsMarker         :: !(Maybe Text)
  , _lirsInstances      :: !(Maybe [Instance])
  , _lirsResponseStatus :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListInstancesResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lirsMarker' - The pagination token that indicates the next set of results to retrieve.
--
-- * 'lirsInstances' - The list of instances for the cluster and given filters.
--
-- * 'lirsResponseStatus' - -- | The response status code.
listInstancesResponse
    :: Int -- ^ 'lirsResponseStatus'
    -> ListInstancesResponse
listInstancesResponse pResponseStatus_ =
  ListInstancesResponse'
    { _lirsMarker = Nothing
    , _lirsInstances = Nothing
    , _lirsResponseStatus = pResponseStatus_
    }


-- | The pagination token that indicates the next set of results to retrieve.
lirsMarker :: Lens' ListInstancesResponse (Maybe Text)
lirsMarker = lens _lirsMarker (\ s a -> s{_lirsMarker = a})

-- | The list of instances for the cluster and given filters.
lirsInstances :: Lens' ListInstancesResponse [Instance]
lirsInstances = lens _lirsInstances (\ s a -> s{_lirsInstances = a}) . _Default . _Coerce

-- | -- | The response status code.
lirsResponseStatus :: Lens' ListInstancesResponse Int
lirsResponseStatus = lens _lirsResponseStatus (\ s a -> s{_lirsResponseStatus = a})

instance NFData ListInstancesResponse where
