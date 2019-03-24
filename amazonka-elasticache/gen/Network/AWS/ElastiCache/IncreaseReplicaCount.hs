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
-- Module      : Network.AWS.ElastiCache.IncreaseReplicaCount
-- Copyright   : (c) 2013-2018 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Dynamically increases the number of replics in a Redis (cluster mode disabled) replication group or the number of replica nodes in one or more node groups (shards) of a Redis (cluster mode enabled) replication group. This operation is performed with no cluster down time.
--
--
module Network.AWS.ElastiCache.IncreaseReplicaCount
    (
    -- * Creating a Request
      increaseReplicaCount
    , IncreaseReplicaCount
    -- * Request Lenses
    , ircNewReplicaCount
    , ircReplicaConfiguration
    , ircReplicationGroupId
    , ircApplyImmediately

    -- * Destructuring the Response
    , increaseReplicaCountResponse
    , IncreaseReplicaCountResponse
    -- * Response Lenses
    , ircrsReplicationGroup
    , ircrsResponseStatus
    ) where

import Network.AWS.ElastiCache.Types
import Network.AWS.ElastiCache.Types.Product
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'increaseReplicaCount' smart constructor.
data IncreaseReplicaCount = IncreaseReplicaCount'
  { _ircNewReplicaCount      :: !(Maybe Int)
  , _ircReplicaConfiguration :: !(Maybe [ConfigureShard])
  , _ircReplicationGroupId   :: !Text
  , _ircApplyImmediately     :: !Bool
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IncreaseReplicaCount' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ircNewReplicaCount' - The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups.
--
-- * 'ircReplicaConfiguration' - A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
--
-- * 'ircReplicationGroupId' - The id of the replication group to which you want to add replica nodes.
--
-- * 'ircApplyImmediately' - If @True@ , the number of replica nodes is increased immediately. If @False@ , the number of replica nodes is increased during the next maintenance window.
increaseReplicaCount
    :: Text -- ^ 'ircReplicationGroupId'
    -> Bool -- ^ 'ircApplyImmediately'
    -> IncreaseReplicaCount
increaseReplicaCount pReplicationGroupId_ pApplyImmediately_ =
  IncreaseReplicaCount'
    { _ircNewReplicaCount = Nothing
    , _ircReplicaConfiguration = Nothing
    , _ircReplicationGroupId = pReplicationGroupId_
    , _ircApplyImmediately = pApplyImmediately_
    }


-- | The number of read replica nodes you want at the completion of this operation. For Redis (cluster mode disabled) replication groups, this is the number of replica nodes in the replication group. For Redis (cluster mode enabled) replication groups, this is the number of replica nodes in each of the replication group's node groups.
ircNewReplicaCount :: Lens' IncreaseReplicaCount (Maybe Int)
ircNewReplicaCount = lens _ircNewReplicaCount (\ s a -> s{_ircNewReplicaCount = a})

-- | A list of @ConfigureShard@ objects that can be used to configure each shard in a Redis (cluster mode enabled) replication group. The @ConfigureShard@ has three members: @NewReplicaCount@ , @NodeGroupId@ , and @PreferredAvailabilityZones@ .
ircReplicaConfiguration :: Lens' IncreaseReplicaCount [ConfigureShard]
ircReplicaConfiguration = lens _ircReplicaConfiguration (\ s a -> s{_ircReplicaConfiguration = a}) . _Default . _Coerce

-- | The id of the replication group to which you want to add replica nodes.
ircReplicationGroupId :: Lens' IncreaseReplicaCount Text
ircReplicationGroupId = lens _ircReplicationGroupId (\ s a -> s{_ircReplicationGroupId = a})

-- | If @True@ , the number of replica nodes is increased immediately. If @False@ , the number of replica nodes is increased during the next maintenance window.
ircApplyImmediately :: Lens' IncreaseReplicaCount Bool
ircApplyImmediately = lens _ircApplyImmediately (\ s a -> s{_ircApplyImmediately = a})

instance AWSRequest IncreaseReplicaCount where
        type Rs IncreaseReplicaCount =
             IncreaseReplicaCountResponse
        request = postQuery elastiCache
        response
          = receiveXMLWrapper "IncreaseReplicaCountResult"
              (\ s h x ->
                 IncreaseReplicaCountResponse' <$>
                   (x .@? "ReplicationGroup") <*> (pure (fromEnum s)))

instance Hashable IncreaseReplicaCount where

instance NFData IncreaseReplicaCount where

instance ToHeaders IncreaseReplicaCount where
        toHeaders = const mempty

instance ToPath IncreaseReplicaCount where
        toPath = const "/"

instance ToQuery IncreaseReplicaCount where
        toQuery IncreaseReplicaCount'{..}
          = mconcat
              ["Action" =: ("IncreaseReplicaCount" :: ByteString),
               "Version" =: ("2015-02-02" :: ByteString),
               "NewReplicaCount" =: _ircNewReplicaCount,
               "ReplicaConfiguration" =:
                 toQuery
                   (toQueryList "ConfigureShard" <$>
                      _ircReplicaConfiguration),
               "ReplicationGroupId" =: _ircReplicationGroupId,
               "ApplyImmediately" =: _ircApplyImmediately]

-- | /See:/ 'increaseReplicaCountResponse' smart constructor.
data IncreaseReplicaCountResponse = IncreaseReplicaCountResponse'
  { _ircrsReplicationGroup :: !(Maybe ReplicationGroup)
  , _ircrsResponseStatus   :: !Int
  } deriving (Eq, Read, Show, Data, Typeable, Generic)


-- | Creates a value of 'IncreaseReplicaCountResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ircrsReplicationGroup' - Undocumented member.
--
-- * 'ircrsResponseStatus' - -- | The response status code.
increaseReplicaCountResponse
    :: Int -- ^ 'ircrsResponseStatus'
    -> IncreaseReplicaCountResponse
increaseReplicaCountResponse pResponseStatus_ =
  IncreaseReplicaCountResponse'
    {_ircrsReplicationGroup = Nothing, _ircrsResponseStatus = pResponseStatus_}


-- | Undocumented member.
ircrsReplicationGroup :: Lens' IncreaseReplicaCountResponse (Maybe ReplicationGroup)
ircrsReplicationGroup = lens _ircrsReplicationGroup (\ s a -> s{_ircrsReplicationGroup = a})

-- | -- | The response status code.
ircrsResponseStatus :: Lens' IncreaseReplicaCountResponse Int
ircrsResponseStatus = lens _ircrsResponseStatus (\ s a -> s{_ircrsResponseStatus = a})

instance NFData IncreaseReplicaCountResponse where
