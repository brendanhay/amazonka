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
-- Module      : Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deleting a Global Datastore is a two-step process:
--
--
--     * First, you must 'DisassociateGlobalReplicationGroup' to remove the secondary clusters in the Global Datastore.
--
--     * Once the Global Datastore contains only the primary cluster, you can use DeleteGlobalReplicationGroup API to delete the Global Datastore while retainining the primary cluster using Retain…= true.
--
--
--
-- Since the Global Datastore has only a primary cluster, you can delete the Global Datastore while retaining the primary by setting @RetainPrimaryCluster=true@ .
--
-- When you receive a successful response from this operation, Amazon ElastiCache immediately begins deleting the selected resources; you cannot cancel or revert this operation.
module Network.AWS.ElastiCache.DeleteGlobalReplicationGroup
  ( -- * Creating a Request
    deleteGlobalReplicationGroup,
    DeleteGlobalReplicationGroup,

    -- * Request Lenses
    dGlobalReplicationGroupId,
    dRetainPrimaryReplicationGroup,

    -- * Destructuring the Response
    deleteGlobalReplicationGroupResponse,
    DeleteGlobalReplicationGroupResponse,

    -- * Response Lenses
    dgrggrsGlobalReplicationGroup,
    dgrggrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteGlobalReplicationGroup' smart constructor.
data DeleteGlobalReplicationGroup = DeleteGlobalReplicationGroup'
  { _dGlobalReplicationGroupId ::
      !Text,
    _dRetainPrimaryReplicationGroup ::
      !Bool
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dGlobalReplicationGroupId' - The name of the Global Datastore
--
-- * 'dRetainPrimaryReplicationGroup' - The primary replication group is retained as a standalone replication group.
deleteGlobalReplicationGroup ::
  -- | 'dGlobalReplicationGroupId'
  Text ->
  -- | 'dRetainPrimaryReplicationGroup'
  Bool ->
  DeleteGlobalReplicationGroup
deleteGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pRetainPrimaryReplicationGroup_ =
    DeleteGlobalReplicationGroup'
      { _dGlobalReplicationGroupId =
          pGlobalReplicationGroupId_,
        _dRetainPrimaryReplicationGroup = pRetainPrimaryReplicationGroup_
      }

-- | The name of the Global Datastore
dGlobalReplicationGroupId :: Lens' DeleteGlobalReplicationGroup Text
dGlobalReplicationGroupId = lens _dGlobalReplicationGroupId (\s a -> s {_dGlobalReplicationGroupId = a})

-- | The primary replication group is retained as a standalone replication group.
dRetainPrimaryReplicationGroup :: Lens' DeleteGlobalReplicationGroup Bool
dRetainPrimaryReplicationGroup = lens _dRetainPrimaryReplicationGroup (\s a -> s {_dRetainPrimaryReplicationGroup = a})

instance AWSRequest DeleteGlobalReplicationGroup where
  type
    Rs DeleteGlobalReplicationGroup =
      DeleteGlobalReplicationGroupResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "DeleteGlobalReplicationGroupResult"
      ( \s h x ->
          DeleteGlobalReplicationGroupResponse'
            <$> (x .@? "GlobalReplicationGroup") <*> (pure (fromEnum s))
      )

instance Hashable DeleteGlobalReplicationGroup

instance NFData DeleteGlobalReplicationGroup

instance ToHeaders DeleteGlobalReplicationGroup where
  toHeaders = const mempty

instance ToPath DeleteGlobalReplicationGroup where
  toPath = const "/"

instance ToQuery DeleteGlobalReplicationGroup where
  toQuery DeleteGlobalReplicationGroup' {..} =
    mconcat
      [ "Action" =: ("DeleteGlobalReplicationGroup" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "GlobalReplicationGroupId" =: _dGlobalReplicationGroupId,
        "RetainPrimaryReplicationGroup" =: _dRetainPrimaryReplicationGroup
      ]

-- | /See:/ 'deleteGlobalReplicationGroupResponse' smart constructor.
data DeleteGlobalReplicationGroupResponse = DeleteGlobalReplicationGroupResponse'
  { _dgrggrsGlobalReplicationGroup ::
      !( Maybe
           GlobalReplicationGroup
       ),
    _dgrggrsResponseStatus ::
      !Int
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgrggrsGlobalReplicationGroup' - Undocumented member.
--
-- * 'dgrggrsResponseStatus' - -- | The response status code.
deleteGlobalReplicationGroupResponse ::
  -- | 'dgrggrsResponseStatus'
  Int ->
  DeleteGlobalReplicationGroupResponse
deleteGlobalReplicationGroupResponse pResponseStatus_ =
  DeleteGlobalReplicationGroupResponse'
    { _dgrggrsGlobalReplicationGroup =
        Nothing,
      _dgrggrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dgrggrsGlobalReplicationGroup :: Lens' DeleteGlobalReplicationGroupResponse (Maybe GlobalReplicationGroup)
dgrggrsGlobalReplicationGroup = lens _dgrggrsGlobalReplicationGroup (\s a -> s {_dgrggrsGlobalReplicationGroup = a})

-- | -- | The response status code.
dgrggrsResponseStatus :: Lens' DeleteGlobalReplicationGroupResponse Int
dgrggrsResponseStatus = lens _dgrggrsResponseStatus (\s a -> s {_dgrggrsResponseStatus = a})

instance NFData DeleteGlobalReplicationGroupResponse
