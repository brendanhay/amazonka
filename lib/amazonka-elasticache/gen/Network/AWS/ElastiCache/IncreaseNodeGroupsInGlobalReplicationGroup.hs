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
-- Module      : Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Increase the number of node groups in the Global Datastore
module Network.AWS.ElastiCache.IncreaseNodeGroupsInGlobalReplicationGroup
  ( -- * Creating a Request
    increaseNodeGroupsInGlobalReplicationGroup,
    IncreaseNodeGroupsInGlobalReplicationGroup,

    -- * Request Lenses
    ingigrgRegionalConfigurations,
    ingigrgGlobalReplicationGroupId,
    ingigrgNodeGroupCount,
    ingigrgApplyImmediately,

    -- * Destructuring the Response
    increaseNodeGroupsInGlobalReplicationGroupResponse,
    IncreaseNodeGroupsInGlobalReplicationGroupResponse,

    -- * Response Lenses
    ingigrgrsGlobalReplicationGroup,
    ingigrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'increaseNodeGroupsInGlobalReplicationGroup' smart constructor.
data IncreaseNodeGroupsInGlobalReplicationGroup = IncreaseNodeGroupsInGlobalReplicationGroup'
  { _ingigrgRegionalConfigurations ::
      !( Maybe
           [RegionalConfiguration]
       ),
    _ingigrgGlobalReplicationGroupId ::
      !Text,
    _ingigrgNodeGroupCount ::
      !Int,
    _ingigrgApplyImmediately ::
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

-- | Creates a value of 'IncreaseNodeGroupsInGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ingigrgRegionalConfigurations' - Describes the replication group IDs, the AWS regions where they are stored and the shard configuration for each that comprise the Global Datastore
--
-- * 'ingigrgGlobalReplicationGroupId' - The name of the Global Datastore
--
-- * 'ingigrgNodeGroupCount' - The number of node groups you wish to add
--
-- * 'ingigrgApplyImmediately' - Indicates that the process begins immediately. At present, the only permitted value for this parameter is true.
increaseNodeGroupsInGlobalReplicationGroup ::
  -- | 'ingigrgGlobalReplicationGroupId'
  Text ->
  -- | 'ingigrgNodeGroupCount'
  Int ->
  -- | 'ingigrgApplyImmediately'
  Bool ->
  IncreaseNodeGroupsInGlobalReplicationGroup
increaseNodeGroupsInGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pNodeGroupCount_
  pApplyImmediately_ =
    IncreaseNodeGroupsInGlobalReplicationGroup'
      { _ingigrgRegionalConfigurations =
          Nothing,
        _ingigrgGlobalReplicationGroupId =
          pGlobalReplicationGroupId_,
        _ingigrgNodeGroupCount = pNodeGroupCount_,
        _ingigrgApplyImmediately = pApplyImmediately_
      }

-- | Describes the replication group IDs, the AWS regions where they are stored and the shard configuration for each that comprise the Global Datastore
ingigrgRegionalConfigurations :: Lens' IncreaseNodeGroupsInGlobalReplicationGroup [RegionalConfiguration]
ingigrgRegionalConfigurations = lens _ingigrgRegionalConfigurations (\s a -> s {_ingigrgRegionalConfigurations = a}) . _Default . _Coerce

-- | The name of the Global Datastore
ingigrgGlobalReplicationGroupId :: Lens' IncreaseNodeGroupsInGlobalReplicationGroup Text
ingigrgGlobalReplicationGroupId = lens _ingigrgGlobalReplicationGroupId (\s a -> s {_ingigrgGlobalReplicationGroupId = a})

-- | The number of node groups you wish to add
ingigrgNodeGroupCount :: Lens' IncreaseNodeGroupsInGlobalReplicationGroup Int
ingigrgNodeGroupCount = lens _ingigrgNodeGroupCount (\s a -> s {_ingigrgNodeGroupCount = a})

-- | Indicates that the process begins immediately. At present, the only permitted value for this parameter is true.
ingigrgApplyImmediately :: Lens' IncreaseNodeGroupsInGlobalReplicationGroup Bool
ingigrgApplyImmediately = lens _ingigrgApplyImmediately (\s a -> s {_ingigrgApplyImmediately = a})

instance AWSRequest IncreaseNodeGroupsInGlobalReplicationGroup where
  type
    Rs IncreaseNodeGroupsInGlobalReplicationGroup =
      IncreaseNodeGroupsInGlobalReplicationGroupResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "IncreaseNodeGroupsInGlobalReplicationGroupResult"
      ( \s h x ->
          IncreaseNodeGroupsInGlobalReplicationGroupResponse'
            <$> (x .@? "GlobalReplicationGroup") <*> (pure (fromEnum s))
      )

instance Hashable IncreaseNodeGroupsInGlobalReplicationGroup

instance NFData IncreaseNodeGroupsInGlobalReplicationGroup

instance ToHeaders IncreaseNodeGroupsInGlobalReplicationGroup where
  toHeaders = const mempty

instance ToPath IncreaseNodeGroupsInGlobalReplicationGroup where
  toPath = const "/"

instance ToQuery IncreaseNodeGroupsInGlobalReplicationGroup where
  toQuery IncreaseNodeGroupsInGlobalReplicationGroup' {..} =
    mconcat
      [ "Action"
          =: ("IncreaseNodeGroupsInGlobalReplicationGroup" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "RegionalConfigurations"
          =: toQuery
            ( toQueryList "RegionalConfiguration"
                <$> _ingigrgRegionalConfigurations
            ),
        "GlobalReplicationGroupId" =: _ingigrgGlobalReplicationGroupId,
        "NodeGroupCount" =: _ingigrgNodeGroupCount,
        "ApplyImmediately" =: _ingigrgApplyImmediately
      ]

-- | /See:/ 'increaseNodeGroupsInGlobalReplicationGroupResponse' smart constructor.
data IncreaseNodeGroupsInGlobalReplicationGroupResponse = IncreaseNodeGroupsInGlobalReplicationGroupResponse'
  { _ingigrgrsGlobalReplicationGroup ::
      !( Maybe
           GlobalReplicationGroup
       ),
    _ingigrgrsResponseStatus ::
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

-- | Creates a value of 'IncreaseNodeGroupsInGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'ingigrgrsGlobalReplicationGroup' - Undocumented member.
--
-- * 'ingigrgrsResponseStatus' - -- | The response status code.
increaseNodeGroupsInGlobalReplicationGroupResponse ::
  -- | 'ingigrgrsResponseStatus'
  Int ->
  IncreaseNodeGroupsInGlobalReplicationGroupResponse
increaseNodeGroupsInGlobalReplicationGroupResponse pResponseStatus_ =
  IncreaseNodeGroupsInGlobalReplicationGroupResponse'
    { _ingigrgrsGlobalReplicationGroup =
        Nothing,
      _ingigrgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
ingigrgrsGlobalReplicationGroup :: Lens' IncreaseNodeGroupsInGlobalReplicationGroupResponse (Maybe GlobalReplicationGroup)
ingigrgrsGlobalReplicationGroup = lens _ingigrgrsGlobalReplicationGroup (\s a -> s {_ingigrgrsGlobalReplicationGroup = a})

-- | -- | The response status code.
ingigrgrsResponseStatus :: Lens' IncreaseNodeGroupsInGlobalReplicationGroupResponse Int
ingigrgrsResponseStatus = lens _ingigrgrsResponseStatus (\s a -> s {_ingigrgrsResponseStatus = a})

instance NFData IncreaseNodeGroupsInGlobalReplicationGroupResponse
