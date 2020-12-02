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
-- Module      : Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Redistribute slots to ensure uniform distribution across existing shards in the cluster.
module Network.AWS.ElastiCache.RebalanceSlotsInGlobalReplicationGroup
  ( -- * Creating a Request
    rebalanceSlotsInGlobalReplicationGroup,
    RebalanceSlotsInGlobalReplicationGroup,

    -- * Request Lenses
    rsigrgGlobalReplicationGroupId,
    rsigrgApplyImmediately,

    -- * Destructuring the Response
    rebalanceSlotsInGlobalReplicationGroupResponse,
    RebalanceSlotsInGlobalReplicationGroupResponse,

    -- * Response Lenses
    rsigrgrsGlobalReplicationGroup,
    rsigrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'rebalanceSlotsInGlobalReplicationGroup' smart constructor.
data RebalanceSlotsInGlobalReplicationGroup = RebalanceSlotsInGlobalReplicationGroup'
  { _rsigrgGlobalReplicationGroupId ::
      !Text,
    _rsigrgApplyImmediately ::
      !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'RebalanceSlotsInGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsigrgGlobalReplicationGroupId' - The name of the Global Datastore
--
-- * 'rsigrgApplyImmediately' - If @True@ , redistribution is applied immediately.
rebalanceSlotsInGlobalReplicationGroup ::
  -- | 'rsigrgGlobalReplicationGroupId'
  Text ->
  -- | 'rsigrgApplyImmediately'
  Bool ->
  RebalanceSlotsInGlobalReplicationGroup
rebalanceSlotsInGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pApplyImmediately_ =
    RebalanceSlotsInGlobalReplicationGroup'
      { _rsigrgGlobalReplicationGroupId =
          pGlobalReplicationGroupId_,
        _rsigrgApplyImmediately = pApplyImmediately_
      }

-- | The name of the Global Datastore
rsigrgGlobalReplicationGroupId :: Lens' RebalanceSlotsInGlobalReplicationGroup Text
rsigrgGlobalReplicationGroupId = lens _rsigrgGlobalReplicationGroupId (\s a -> s {_rsigrgGlobalReplicationGroupId = a})

-- | If @True@ , redistribution is applied immediately.
rsigrgApplyImmediately :: Lens' RebalanceSlotsInGlobalReplicationGroup Bool
rsigrgApplyImmediately = lens _rsigrgApplyImmediately (\s a -> s {_rsigrgApplyImmediately = a})

instance AWSRequest RebalanceSlotsInGlobalReplicationGroup where
  type
    Rs RebalanceSlotsInGlobalReplicationGroup =
      RebalanceSlotsInGlobalReplicationGroupResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "RebalanceSlotsInGlobalReplicationGroupResult"
      ( \s h x ->
          RebalanceSlotsInGlobalReplicationGroupResponse'
            <$> (x .@? "GlobalReplicationGroup") <*> (pure (fromEnum s))
      )

instance Hashable RebalanceSlotsInGlobalReplicationGroup

instance NFData RebalanceSlotsInGlobalReplicationGroup

instance ToHeaders RebalanceSlotsInGlobalReplicationGroup where
  toHeaders = const mempty

instance ToPath RebalanceSlotsInGlobalReplicationGroup where
  toPath = const "/"

instance ToQuery RebalanceSlotsInGlobalReplicationGroup where
  toQuery RebalanceSlotsInGlobalReplicationGroup' {..} =
    mconcat
      [ "Action"
          =: ("RebalanceSlotsInGlobalReplicationGroup" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "GlobalReplicationGroupId" =: _rsigrgGlobalReplicationGroupId,
        "ApplyImmediately" =: _rsigrgApplyImmediately
      ]

-- | /See:/ 'rebalanceSlotsInGlobalReplicationGroupResponse' smart constructor.
data RebalanceSlotsInGlobalReplicationGroupResponse = RebalanceSlotsInGlobalReplicationGroupResponse'
  { _rsigrgrsGlobalReplicationGroup ::
      !( Maybe
           GlobalReplicationGroup
       ),
    _rsigrgrsResponseStatus ::
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

-- | Creates a value of 'RebalanceSlotsInGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'rsigrgrsGlobalReplicationGroup' - Undocumented member.
--
-- * 'rsigrgrsResponseStatus' - -- | The response status code.
rebalanceSlotsInGlobalReplicationGroupResponse ::
  -- | 'rsigrgrsResponseStatus'
  Int ->
  RebalanceSlotsInGlobalReplicationGroupResponse
rebalanceSlotsInGlobalReplicationGroupResponse pResponseStatus_ =
  RebalanceSlotsInGlobalReplicationGroupResponse'
    { _rsigrgrsGlobalReplicationGroup =
        Nothing,
      _rsigrgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
rsigrgrsGlobalReplicationGroup :: Lens' RebalanceSlotsInGlobalReplicationGroupResponse (Maybe GlobalReplicationGroup)
rsigrgrsGlobalReplicationGroup = lens _rsigrgrsGlobalReplicationGroup (\s a -> s {_rsigrgrsGlobalReplicationGroup = a})

-- | -- | The response status code.
rsigrgrsResponseStatus :: Lens' RebalanceSlotsInGlobalReplicationGroupResponse Int
rsigrgrsResponseStatus = lens _rsigrgrsResponseStatus (\s a -> s {_rsigrgrsResponseStatus = a})

instance NFData RebalanceSlotsInGlobalReplicationGroupResponse
