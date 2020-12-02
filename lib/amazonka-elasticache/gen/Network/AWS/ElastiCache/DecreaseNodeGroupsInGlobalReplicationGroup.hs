{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TypeFamilies #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Decreases the number of node groups in a Global Datastore
module Network.AWS.ElastiCache.DecreaseNodeGroupsInGlobalReplicationGroup
  ( -- * Creating a Request
    decreaseNodeGroupsInGlobalReplicationGroup,
    DecreaseNodeGroupsInGlobalReplicationGroup,

    -- * Request Lenses
    dngigrgGlobalNodeGroupsToRemove,
    dngigrgGlobalNodeGroupsToRetain,
    dngigrgGlobalReplicationGroupId,
    dngigrgNodeGroupCount,
    dngigrgApplyImmediately,

    -- * Destructuring the Response
    decreaseNodeGroupsInGlobalReplicationGroupResponse,
    DecreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- * Response Lenses
    dngigrgrsGlobalReplicationGroup,
    dngigrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'decreaseNodeGroupsInGlobalReplicationGroup' smart constructor.
data DecreaseNodeGroupsInGlobalReplicationGroup = DecreaseNodeGroupsInGlobalReplicationGroup'
  { _dngigrgGlobalNodeGroupsToRemove ::
      !( Maybe
           [Text]
       ),
    _dngigrgGlobalNodeGroupsToRetain ::
      !( Maybe
           [Text]
       ),
    _dngigrgGlobalReplicationGroupId ::
      !Text,
    _dngigrgNodeGroupCount ::
      !Int,
    _dngigrgApplyImmediately ::
      !Bool
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DecreaseNodeGroupsInGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dngigrgGlobalNodeGroupsToRemove' - If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster.
--
-- * 'dngigrgGlobalNodeGroupsToRetain' - If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster.
--
-- * 'dngigrgGlobalReplicationGroupId' - The name of the Global Datastore
--
-- * 'dngigrgNodeGroupCount' - The number of node groups (shards) that results from the modification of the shard configuration
--
-- * 'dngigrgApplyImmediately' - Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is true.
decreaseNodeGroupsInGlobalReplicationGroup ::
  -- | 'dngigrgGlobalReplicationGroupId'
  Text ->
  -- | 'dngigrgNodeGroupCount'
  Int ->
  -- | 'dngigrgApplyImmediately'
  Bool ->
  DecreaseNodeGroupsInGlobalReplicationGroup
decreaseNodeGroupsInGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pNodeGroupCount_
  pApplyImmediately_ =
    DecreaseNodeGroupsInGlobalReplicationGroup'
      { _dngigrgGlobalNodeGroupsToRemove =
          Nothing,
        _dngigrgGlobalNodeGroupsToRetain = Nothing,
        _dngigrgGlobalReplicationGroupId =
          pGlobalReplicationGroupId_,
        _dngigrgNodeGroupCount = pNodeGroupCount_,
        _dngigrgApplyImmediately = pApplyImmediately_
      }

-- | If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster.
dngigrgGlobalNodeGroupsToRemove :: Lens' DecreaseNodeGroupsInGlobalReplicationGroup [Text]
dngigrgGlobalNodeGroupsToRemove = lens _dngigrgGlobalNodeGroupsToRemove (\s a -> s {_dngigrgGlobalNodeGroupsToRemove = a}) . _Default . _Coerce

-- | If the value of NodeGroupCount is less than the current number of node groups (shards), then either NodeGroupsToRemove or NodeGroupsToRetain is required. NodeGroupsToRemove is a list of NodeGroupIds to remove from the cluster. ElastiCache for Redis will attempt to remove all node groups listed by NodeGroupsToRemove from the cluster.
dngigrgGlobalNodeGroupsToRetain :: Lens' DecreaseNodeGroupsInGlobalReplicationGroup [Text]
dngigrgGlobalNodeGroupsToRetain = lens _dngigrgGlobalNodeGroupsToRetain (\s a -> s {_dngigrgGlobalNodeGroupsToRetain = a}) . _Default . _Coerce

-- | The name of the Global Datastore
dngigrgGlobalReplicationGroupId :: Lens' DecreaseNodeGroupsInGlobalReplicationGroup Text
dngigrgGlobalReplicationGroupId = lens _dngigrgGlobalReplicationGroupId (\s a -> s {_dngigrgGlobalReplicationGroupId = a})

-- | The number of node groups (shards) that results from the modification of the shard configuration
dngigrgNodeGroupCount :: Lens' DecreaseNodeGroupsInGlobalReplicationGroup Int
dngigrgNodeGroupCount = lens _dngigrgNodeGroupCount (\s a -> s {_dngigrgNodeGroupCount = a})

-- | Indicates that the shard reconfiguration process begins immediately. At present, the only permitted value for this parameter is true.
dngigrgApplyImmediately :: Lens' DecreaseNodeGroupsInGlobalReplicationGroup Bool
dngigrgApplyImmediately = lens _dngigrgApplyImmediately (\s a -> s {_dngigrgApplyImmediately = a})

instance AWSRequest DecreaseNodeGroupsInGlobalReplicationGroup where
  type
    Rs DecreaseNodeGroupsInGlobalReplicationGroup =
      DecreaseNodeGroupsInGlobalReplicationGroupResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "DecreaseNodeGroupsInGlobalReplicationGroupResult"
      ( \s h x ->
          DecreaseNodeGroupsInGlobalReplicationGroupResponse'
            <$> (x .@? "GlobalReplicationGroup") <*> (pure (fromEnum s))
      )

instance Hashable DecreaseNodeGroupsInGlobalReplicationGroup

instance NFData DecreaseNodeGroupsInGlobalReplicationGroup

instance ToHeaders DecreaseNodeGroupsInGlobalReplicationGroup where
  toHeaders = const mempty

instance ToPath DecreaseNodeGroupsInGlobalReplicationGroup where
  toPath = const "/"

instance ToQuery DecreaseNodeGroupsInGlobalReplicationGroup where
  toQuery DecreaseNodeGroupsInGlobalReplicationGroup' {..} =
    mconcat
      [ "Action"
          =: ("DecreaseNodeGroupsInGlobalReplicationGroup" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "GlobalNodeGroupsToRemove"
          =: toQuery
            ( toQueryList "GlobalNodeGroupId"
                <$> _dngigrgGlobalNodeGroupsToRemove
            ),
        "GlobalNodeGroupsToRetain"
          =: toQuery
            ( toQueryList "GlobalNodeGroupId"
                <$> _dngigrgGlobalNodeGroupsToRetain
            ),
        "GlobalReplicationGroupId" =: _dngigrgGlobalReplicationGroupId,
        "NodeGroupCount" =: _dngigrgNodeGroupCount,
        "ApplyImmediately" =: _dngigrgApplyImmediately
      ]

-- | /See:/ 'decreaseNodeGroupsInGlobalReplicationGroupResponse' smart constructor.
data DecreaseNodeGroupsInGlobalReplicationGroupResponse = DecreaseNodeGroupsInGlobalReplicationGroupResponse'
  { _dngigrgrsGlobalReplicationGroup ::
      !( Maybe
           GlobalReplicationGroup
       ),
    _dngigrgrsResponseStatus ::
      !Int
  }
  deriving
    ( Eq,
      Read,
      Show,
      Data,
      Typeable,
      Generic
    )

-- | Creates a value of 'DecreaseNodeGroupsInGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dngigrgrsGlobalReplicationGroup' - Undocumented member.
--
-- * 'dngigrgrsResponseStatus' - -- | The response status code.
decreaseNodeGroupsInGlobalReplicationGroupResponse ::
  -- | 'dngigrgrsResponseStatus'
  Int ->
  DecreaseNodeGroupsInGlobalReplicationGroupResponse
decreaseNodeGroupsInGlobalReplicationGroupResponse pResponseStatus_ =
  DecreaseNodeGroupsInGlobalReplicationGroupResponse'
    { _dngigrgrsGlobalReplicationGroup =
        Nothing,
      _dngigrgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dngigrgrsGlobalReplicationGroup :: Lens' DecreaseNodeGroupsInGlobalReplicationGroupResponse (Maybe GlobalReplicationGroup)
dngigrgrsGlobalReplicationGroup = lens _dngigrgrsGlobalReplicationGroup (\s a -> s {_dngigrgrsGlobalReplicationGroup = a})

-- | -- | The response status code.
dngigrgrsResponseStatus :: Lens' DecreaseNodeGroupsInGlobalReplicationGroupResponse Int
dngigrgrsResponseStatus = lens _dngigrgrsResponseStatus (\s a -> s {_dngigrgrsResponseStatus = a})

instance NFData DecreaseNodeGroupsInGlobalReplicationGroupResponse
