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
-- Module      : Network.AWS.ElastiCache.ListAllowedNodeTypeModifications
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Lists all available node types that you can scale your Redis cluster's or replication group's current node type up to.
--
--
-- When you use the @ModifyCacheCluster@ or @ModifyReplicationGroup@ operations to scale up your cluster or replication group, the value of the @CacheNodeType@ parameter must be one of the node types returned by this operation.
--
module Network.AWS.ElastiCache.ListAllowedNodeTypeModifications
    (
    -- * Creating a Request
      listAllowedNodeTypeModifications
    , ListAllowedNodeTypeModifications
    -- * Request Lenses
    , lantmCacheClusterId
    , lantmReplicationGroupId

    -- * Destructuring the Response
    , listAllowedNodeTypeModificationsResponse
    , ListAllowedNodeTypeModificationsResponse
    -- * Response Lenses
    , lantmrsScaleUpModifications
    , lantmrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | The input parameters for the @ListAllowedNodeTypeModifications@ operation.
--
--
--
-- /See:/ 'listAllowedNodeTypeModifications' smart constructor.
data ListAllowedNodeTypeModifications = ListAllowedNodeTypeModifications'
  { _lantmCacheClusterId     :: !(Maybe Text)
  , _lantmReplicationGroupId :: !(Maybe Text)
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAllowedNodeTypeModifications' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lantmCacheClusterId' - The name of the cluster you want to scale up to a larger node instanced type. ElastiCache uses the cluster id to identify the current node type of this cluster and from that to create a list of node types you can scale up to. /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
--
-- * 'lantmReplicationGroupId' - The name of the replication group want to scale up to a larger node type. ElastiCache uses the replication group id to identify the current node type being used by this replication group, and from that to create a list of node types you can scale up to. /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
listAllowedNodeTypeModifications
    :: ListAllowedNodeTypeModifications
listAllowedNodeTypeModifications =
  ListAllowedNodeTypeModifications'
    {_lantmCacheClusterId = Nothing, _lantmReplicationGroupId = Nothing}


-- | The name of the cluster you want to scale up to a larger node instanced type. ElastiCache uses the cluster id to identify the current node type of this cluster and from that to create a list of node types you can scale up to. /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
lantmCacheClusterId :: Lens' ListAllowedNodeTypeModifications (Maybe Text)
lantmCacheClusterId = lens _lantmCacheClusterId (\ s a -> s{_lantmCacheClusterId = a})

-- | The name of the replication group want to scale up to a larger node type. ElastiCache uses the replication group id to identify the current node type being used by this replication group, and from that to create a list of node types you can scale up to. /Important:/ You must provide a value for either the @CacheClusterId@ or the @ReplicationGroupId@ .
lantmReplicationGroupId :: Lens' ListAllowedNodeTypeModifications (Maybe Text)
lantmReplicationGroupId = lens _lantmReplicationGroupId (\ s a -> s{_lantmReplicationGroupId = a})

instance AWSRequest ListAllowedNodeTypeModifications
         where
        type Rs ListAllowedNodeTypeModifications =
             ListAllowedNodeTypeModificationsResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper
              "ListAllowedNodeTypeModificationsResult"
              (\ s h x ->
                 ListAllowedNodeTypeModificationsResponse' <$>
                   (x .@? "ScaleUpModifications" .!@ mempty >>=
                      may (parseXMLList "member"))
                     <*> (pure (fromEnum s)))

instance Hashable ListAllowedNodeTypeModifications
         where

instance NFData ListAllowedNodeTypeModifications
         where

instance ToHeaders ListAllowedNodeTypeModifications
         where
        toHeaders = const mempty

instance ToPath ListAllowedNodeTypeModifications
         where
        toPath = const "/"

instance ToQuery ListAllowedNodeTypeModifications
         where
        toQuery ListAllowedNodeTypeModifications'{..}
          = mconcat
              ["Action" =:
                 ("ListAllowedNodeTypeModifications" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "CacheClusterId" =: _lantmCacheClusterId,
               "ReplicationGroupId" =: _lantmReplicationGroupId]

-- | Represents the allowed node types you can use to modify your cluster or replication group.
--
--
--
-- /See:/ 'listAllowedNodeTypeModificationsResponse' smart constructor.
data ListAllowedNodeTypeModificationsResponse = ListAllowedNodeTypeModificationsResponse'
  { _lantmrsScaleUpModifications :: !(Maybe [Text])
  , _lantmrsResponseStatus       :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ListAllowedNodeTypeModificationsResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'lantmrsScaleUpModifications' - A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group. When scaling up a Redis cluster or replication group using @ModifyCacheCluster@ or @ModifyReplicationGroup@ , use a value from this list for the @CacheNodeType@ parameter.
--
-- * 'lantmrsResponseStatus' - -- | The response status code.
listAllowedNodeTypeModificationsResponse
    :: Int -- ^ 'lantmrsResponseStatus'
    -> ListAllowedNodeTypeModificationsResponse
listAllowedNodeTypeModificationsResponse pResponseStatus_ =
  ListAllowedNodeTypeModificationsResponse'
    { _lantmrsScaleUpModifications = Nothing
    , _lantmrsResponseStatus = pResponseStatus_
    }


-- | A string list, each element of which specifies a cache node type which you can use to scale your cluster or replication group. When scaling up a Redis cluster or replication group using @ModifyCacheCluster@ or @ModifyReplicationGroup@ , use a value from this list for the @CacheNodeType@ parameter.
lantmrsScaleUpModifications :: Lens' ListAllowedNodeTypeModificationsResponse [Text]
lantmrsScaleUpModifications = lens _lantmrsScaleUpModifications (\ s a -> s{_lantmrsScaleUpModifications = a}) . _Default . _Coerce

-- | -- | The response status code.
lantmrsResponseStatus :: Lens' ListAllowedNodeTypeModificationsResponse Int
lantmrsResponseStatus = lens _lantmrsResponseStatus (\ s a -> s{_lantmrsResponseStatus = a})

instance NFData
           ListAllowedNodeTypeModificationsResponse
         where
