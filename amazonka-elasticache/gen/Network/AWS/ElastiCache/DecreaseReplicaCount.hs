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
-- Module      : Network.AWS.ElastiCache.DecreaseReplicaCount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dynamically decreases the number of replics in a Redis (cluster mode disabled) replication group or the number of replica nodes in one or more node groups (shards) of a Redis (cluster mode enabled) replication group. This operation is performed with no cluster down time.
--
--
module Network.AWS.ElastiCache.DecreaseReplicaCount
    (
    -- * Creating a Request
      decreaseReplicaCount
    , DecreaseReplicaCount
    -- * Request Lenses
    , drcNewReplicaCount
    , drcReplicaConfiguration
    , drcReplicasToRemove
    , drcReplicationGroupId
    , drcApplyImmediately

    -- * Destructuring the Response
    , decreaseReplicaCountResponse
    , DecreaseReplicaCountResponse
    -- * Response Lenses
    , drcrsReplicationGroup
    , drcrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'decreaseReplicaCount' smart constructor.
data DecreaseReplicaCount = DecreaseReplicaCount'
  { _drcNewReplicaCount      :: !(Maybe Int)
  , _drcReplicaConfiguration :: !(Maybe [ConfigureShard])
  , _drcReplicasToRemove     :: !(Maybe [Text])
  , _drcReplicationGroupId   :: !Text
  , _drcApplyImmediately     :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DecreaseReplicaCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcNewReplicaCount' - The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups. The minimum number of replicas in a shard or replication group is:     * Redis (cluster mode disabled)     * If Multi-AZ with Automatic Failover is enabled: 1     * If Multi-AZ with Automatic Failover is not enabled: 0     * Redis (cluster mode enabled): 0 (though you will not be able to failover to a replica if your primary node fails)
--
-- * 'drcReplicaConfiguration' - A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
--
-- * 'drcReplicasToRemove' - A list of the node ids to remove from the replication group or node group (shard).
--
-- * 'drcReplicationGroupId' - The id of the replication group from which you want to remove replica nodes.
--
-- * 'drcApplyImmediately' - If @True@ , the number of replica nodes is decreased immediately. If @False@ , the number of replica nodes is decreased during the next maintenance window.
decreaseReplicaCount
    :: Text -- ^ 'drcReplicationGroupId'
    -> Bool -- ^ 'drcApplyImmediately'
    -> DecreaseReplicaCount
decreaseReplicaCount pReplicationGroupId_ pApplyImmediately_ =
  DecreaseReplicaCount'
    { _drcNewReplicaCount = Nothing
    , _drcReplicaConfiguration = Nothing
    , _drcReplicasToRemove = Nothing
    , _drcReplicationGroupId = pReplicationGroupId_
    , _drcApplyImmediately = pApplyImmediately_
    }


-- | The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups. The minimum number of replicas in a shard or replication group is:     * Redis (cluster mode disabled)     * If Multi-AZ with Automatic Failover is enabled: 1     * If Multi-AZ with Automatic Failover is not enabled: 0     * Redis (cluster mode enabled): 0 (though you will not be able to failover to a replica if your primary node fails)
drcNewReplicaCount :: Lens' DecreaseReplicaCount (Maybe Int)
drcNewReplicaCount = lens _drcNewReplicaCount (\ s a -> s{_drcNewReplicaCount = a})

-- | A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
drcReplicaConfiguration :: Lens' DecreaseReplicaCount [ConfigureShard]
drcReplicaConfiguration = lens _drcReplicaConfiguration (\ s a -> s{_drcReplicaConfiguration = a}) . _Default . _Coerce

-- | A list of the node ids to remove from the replication group or node group (shard).
drcReplicasToRemove :: Lens' DecreaseReplicaCount [Text]
drcReplicasToRemove = lens _drcReplicasToRemove (\ s a -> s{_drcReplicasToRemove = a}) . _Default . _Coerce

-- | The id of the replication group from which you want to remove replica nodes.
drcReplicationGroupId :: Lens' DecreaseReplicaCount Text
drcReplicationGroupId = lens _drcReplicationGroupId (\ s a -> s{_drcReplicationGroupId = a})

-- | If @True@ , the number of replica nodes is decreased immediately. If @False@ , the number of replica nodes is decreased during the next maintenance window.
drcApplyImmediately :: Lens' DecreaseReplicaCount Bool
drcApplyImmediately = lens _drcApplyImmediately (\ s a -> s{_drcApplyImmediately = a})

instance AWSRequest DecreaseReplicaCount where
        type Rs DecreaseReplicaCount =
             DecreaseReplicaCountResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "DecreaseReplicaCountResult"
              (\ s h x ->
                 DecreaseReplicaCountResponse' <$>
                   (x .@? "ReplicationGroup") <*> (pure (fromEnum s)))

instance Hashable DecreaseReplicaCount where

instance NFData DecreaseReplicaCount where

instance ToHeaders DecreaseReplicaCount where
        toHeaders = const mempty

instance ToPath DecreaseReplicaCount where
        toPath = const "/"

instance ToQuery DecreaseReplicaCount where
        toQuery DecreaseReplicaCount'{..}
          = mconcat
              ["Action" =: ("DecreaseReplicaCount" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "NewReplicaCount" =: _drcNewReplicaCount,
               "ReplicaConfiguration" =:
                 toQuery
                   (toQueryList "ConfigureShard" <$>
                      _drcReplicaConfiguration),
               "ReplicasToRemove" =:
                 toQuery
                   (toQueryList "member" <$> _drcReplicasToRemove),
               "ReplicationGroupId" =: _drcReplicationGroupId,
               "ApplyImmediately" =: _drcApplyImmediately]

-- | /See:/ 'decreaseReplicaCountResponse' smart constructor.
data DecreaseReplicaCountResponse = DecreaseReplicaCountResponse'
  { _drcrsReplicationGroup :: !(Maybe ReplicationGroup)
  , _drcrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'DecreaseReplicaCountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'drcrsReplicationGroup' - Undocumented member.
--
-- * 'drcrsResponseStatus' - -- | The response status code.
decreaseReplicaCountResponse
    :: Int -- ^ 'drcrsResponseStatus'
    -> DecreaseReplicaCountResponse
decreaseReplicaCountResponse pResponseStatus_ =
  DecreaseReplicaCountResponse'
    {_drcrsReplicationGroup = Nothing, _drcrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
drcrsReplicationGroup :: Lens' DecreaseReplicaCountResponse (Maybe ReplicationGroup)
drcrsReplicationGroup = lens _drcrsReplicationGroup (\ s a -> s{_drcrsReplicationGroup = a})

-- | -- | The response status code.
drcrsResponseStatus :: Lens' DecreaseReplicaCountResponse Int
drcrsResponseStatus = lens _drcrsResponseStatus (\ s a -> s{_drcrsResponseStatus = a})

instance NFData DecreaseReplicaCountResponse where
