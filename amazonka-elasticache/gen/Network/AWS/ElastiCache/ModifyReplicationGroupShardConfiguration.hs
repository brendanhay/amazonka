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
-- Module      : Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Performs horizontal scaling on a Redis (cluster mode enabled) cluster with no downtime. Requires Redis engine version 3.2.10 or newer. For information on upgrading your engine to a newer version, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/VersionManagement.html Upgrading Engine Versions> in the Amazon ElastiCache User Guide.
--
--
-- For more information on ElastiCache for Redis online horizontal scaling, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/UserGuide/redis-cluster-resharding-online.html ElastiCache for Redis Horizontal Scaling>
--
module Network.AWS.ElastiCache.ModifyReplicationGroupShardConfiguration
    (
    -- * Creating a Request
      modifyReplicationGroupShardConfiguration
    , ModifyReplicationGroupShardConfiguration
    -- * Request Lenses
    , mrgscReshardingConfiguration
    , mrgscNodeGroupsToRemove
    , mrgscReplicationGroupId
    , mrgscNodeGroupCount
    , mrgscApplyImmediately

    -- * Destructuring the Response
    , modifyReplicationGroupShardConfigurationResponse
    , ModifyReplicationGroupShardConfigurationResponse
    -- * Response Lenses
    , mrgscrsReplicationGroup
    , mrgscrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | Represents the input for a @ModifyReplicationGroupShardConfiguration@ operation.
--
--
--
-- /See:/ 'modifyReplicationGroupShardConfiguration' smart constructor.
data ModifyReplicationGroupShardConfiguration = ModifyReplicationGroupShardConfiguration'
  { _mrgscReshardingConfiguration :: !(Maybe [ReshardingConfiguration])
  , _mrgscNodeGroupsToRemove      :: !(Maybe [Text])
  , _mrgscReplicationGroupId      :: !Text
  , _mrgscNodeGroupCount          :: !Int
  , _mrgscApplyImmediately        :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationGroupShardConfiguration' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrgscReshardingConfiguration' - Specifies the preferred availability zones for each node group in the cluster. If the value of @NodeGroupCount@ is greater than the current number of node groups (shards), you can use this parameter to specify the preferred availability zones of the cluster's shards. If you omit this parameter ElastiCache selects availability zones for you. You can specify this parameter only if the value of @NodeGroupCount@ is greater than the current number of node groups (shards).
--
-- * 'mrgscNodeGroupsToRemove' - If the value of @NodeGroupCount@ is less than the current number of node groups (shards), @NodeGroupsToRemove@ is a required list of node group ids to remove from the cluster.
--
-- * 'mrgscReplicationGroupId' - The name of the Redis (cluster mode enabled) cluster (replication group) on which the shards are to be configured.
--
-- * 'mrgscNodeGroupCount' - The number of node groups (shards) that results from the modification of the shard configuration.
--
-- * 'mrgscApplyImmediately' - Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is @true@ . Value: true
modifyReplicationGroupShardConfiguration
    :: Text -- ^ 'mrgscReplicationGroupId'
    -> Int -- ^ 'mrgscNodeGroupCount'
    -> Bool -- ^ 'mrgscApplyImmediately'
    -> ModifyReplicationGroupShardConfiguration
modifyReplicationGroupShardConfiguration pReplicationGroupId_ pNodeGroupCount_ pApplyImmediately_ =
  ModifyReplicationGroupShardConfiguration'
    { _mrgscReshardingConfiguration = Nothing
    , _mrgscNodeGroupsToRemove = Nothing
    , _mrgscReplicationGroupId = pReplicationGroupId_
    , _mrgscNodeGroupCount = pNodeGroupCount_
    , _mrgscApplyImmediately = pApplyImmediately_
    }


-- | Specifies the preferred availability zones for each node group in the cluster. If the value of @NodeGroupCount@ is greater than the current number of node groups (shards), you can use this parameter to specify the preferred availability zones of the cluster's shards. If you omit this parameter ElastiCache selects availability zones for you. You can specify this parameter only if the value of @NodeGroupCount@ is greater than the current number of node groups (shards).
mrgscReshardingConfiguration :: Lens' ModifyReplicationGroupShardConfiguration [ReshardingConfiguration]
mrgscReshardingConfiguration = lens _mrgscReshardingConfiguration (\ s a -> s{_mrgscReshardingConfiguration = a}) . _Default . _Coerce

-- | If the value of @NodeGroupCount@ is less than the current number of node groups (shards), @NodeGroupsToRemove@ is a required list of node group ids to remove from the cluster.
mrgscNodeGroupsToRemove :: Lens' ModifyReplicationGroupShardConfiguration [Text]
mrgscNodeGroupsToRemove = lens _mrgscNodeGroupsToRemove (\ s a -> s{_mrgscNodeGroupsToRemove = a}) . _Default . _Coerce

-- | The name of the Redis (cluster mode enabled) cluster (replication group) on which the shards are to be configured.
mrgscReplicationGroupId :: Lens' ModifyReplicationGroupShardConfiguration Text
mrgscReplicationGroupId = lens _mrgscReplicationGroupId (\ s a -> s{_mrgscReplicationGroupId = a})

-- | The number of node groups (shards) that results from the modification of the shard configuration.
mrgscNodeGroupCount :: Lens' ModifyReplicationGroupShardConfiguration Int
mrgscNodeGroupCount = lens _mrgscNodeGroupCount (\ s a -> s{_mrgscNodeGroupCount = a})

-- | Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is @true@ . Value: true
mrgscApplyImmediately :: Lens' ModifyReplicationGroupShardConfiguration Bool
mrgscApplyImmediately = lens _mrgscApplyImmediately (\ s a -> s{_mrgscApplyImmediately = a})

instance AWSRequest
           ModifyReplicationGroupShardConfiguration
         where
        type Rs ModifyReplicationGroupShardConfiguration =
             ModifyReplicationGroupShardConfigurationResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper
              "ModifyReplicationGroupShardConfigurationResult"
              (\ s h x ->
                 ModifyReplicationGroupShardConfigurationResponse' <$>
                   (x .@? "ReplicationGroup") <*> (pure (fromEnum s)))

instance Hashable
           ModifyReplicationGroupShardConfiguration
         where

instance NFData
           ModifyReplicationGroupShardConfiguration
         where

instance ToHeaders
           ModifyReplicationGroupShardConfiguration
         where
        toHeaders = const mempty

instance ToPath
           ModifyReplicationGroupShardConfiguration
         where
        toPath = const "/"

instance ToQuery
           ModifyReplicationGroupShardConfiguration
         where
        toQuery ModifyReplicationGroupShardConfiguration'{..}
          = mconcat
              ["Action" =:
                 ("ModifyReplicationGroupShardConfiguration" ::
                    ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "ReshardingConfiguration" =:
                 toQuery
                   (toQueryList "ReshardingConfiguration" <$>
                      _mrgscReshardingConfiguration),
               "NodeGroupsToRemove" =:
                 toQuery
                   (toQueryList "NodeGroupToRemove" <$>
                      _mrgscNodeGroupsToRemove),
               "ReplicationGroupId" =: _mrgscReplicationGroupId,
               "NodeGroupCount" =: _mrgscNodeGroupCount,
               "ApplyImmediately" =: _mrgscApplyImmediately]

-- | /See:/ 'modifyReplicationGroupShardConfigurationResponse' smart constructor.
data ModifyReplicationGroupShardConfigurationResponse = ModifyReplicationGroupShardConfigurationResponse'
  { _mrgscrsReplicationGroup :: !(Maybe ReplicationGroup)
  , _mrgscrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'ModifyReplicationGroupShardConfigurationResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'mrgscrsReplicationGroup' - Undocumented member.
--
-- * 'mrgscrsResponseStatus' - -- | The response status code.
modifyReplicationGroupShardConfigurationResponse
    :: Int -- ^ 'mrgscrsResponseStatus'
    -> ModifyReplicationGroupShardConfigurationResponse
modifyReplicationGroupShardConfigurationResponse pResponseStatus_ =
  ModifyReplicationGroupShardConfigurationResponse'
    { _mrgscrsReplicationGroup = Nothing
    , _mrgscrsResponseStatus = pResponseStatus_
    }


-- | Undocumented member.
mrgscrsReplicationGroup :: Lens' ModifyReplicationGroupShardConfigurationResponse (Maybe ReplicationGroup)
mrgscrsReplicationGroup = lens _mrgscrsReplicationGroup (\ s a -> s{_mrgscrsReplicationGroup = a})

-- | -- | The response status code.
mrgscrsResponseStatus :: Lens' ModifyReplicationGroupShardConfigurationResponse Int
mrgscrsResponseStatus = lens _mrgscrsResponseStatus (\ s a -> s{_mrgscrsResponseStatus = a})

instance NFData
           ModifyReplicationGroupShardConfigurationResponse
         where
