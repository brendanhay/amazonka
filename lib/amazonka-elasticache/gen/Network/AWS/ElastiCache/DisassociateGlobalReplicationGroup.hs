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
-- Module      : Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Remove a secondary cluster from the Global Datastore using the Global Datastore name. The secondary cluster will no longer receive updates from the primary cluster, but will remain as a standalone cluster in that AWS region.
module Network.AWS.ElastiCache.DisassociateGlobalReplicationGroup
  ( -- * Creating a Request
    disassociateGlobalReplicationGroup,
    DisassociateGlobalReplicationGroup,

    -- * Request Lenses
    dgrgGlobalReplicationGroupId,
    dgrgReplicationGroupId,
    dgrgReplicationGroupRegion,

    -- * Destructuring the Response
    disassociateGlobalReplicationGroupResponse,
    DisassociateGlobalReplicationGroupResponse,

    -- * Response Lenses
    dgrgrsGlobalReplicationGroup,
    dgrgrsResponseStatus,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'disassociateGlobalReplicationGroup' smart constructor.
data DisassociateGlobalReplicationGroup = DisassociateGlobalReplicationGroup'
  { _dgrgGlobalReplicationGroupId ::
      !Text,
    _dgrgReplicationGroupId ::
      !Text,
    _dgrgReplicationGroupRegion ::
      !Text
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DisassociateGlobalReplicationGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgrgGlobalReplicationGroupId' - The name of the Global Datastore
--
-- * 'dgrgReplicationGroupId' - The name of the secondary cluster you wish to remove from the Global Datastore
--
-- * 'dgrgReplicationGroupRegion' - The AWS region of secondary cluster you wish to remove from the Global Datastore
disassociateGlobalReplicationGroup ::
  -- | 'dgrgGlobalReplicationGroupId'
  Text ->
  -- | 'dgrgReplicationGroupId'
  Text ->
  -- | 'dgrgReplicationGroupRegion'
  Text ->
  DisassociateGlobalReplicationGroup
disassociateGlobalReplicationGroup
  pGlobalReplicationGroupId_
  pReplicationGroupId_
  pReplicationGroupRegion_ =
    DisassociateGlobalReplicationGroup'
      { _dgrgGlobalReplicationGroupId =
          pGlobalReplicationGroupId_,
        _dgrgReplicationGroupId = pReplicationGroupId_,
        _dgrgReplicationGroupRegion = pReplicationGroupRegion_
      }

-- | The name of the Global Datastore
dgrgGlobalReplicationGroupId :: Lens' DisassociateGlobalReplicationGroup Text
dgrgGlobalReplicationGroupId = lens _dgrgGlobalReplicationGroupId (\s a -> s {_dgrgGlobalReplicationGroupId = a})

-- | The name of the secondary cluster you wish to remove from the Global Datastore
dgrgReplicationGroupId :: Lens' DisassociateGlobalReplicationGroup Text
dgrgReplicationGroupId = lens _dgrgReplicationGroupId (\s a -> s {_dgrgReplicationGroupId = a})

-- | The AWS region of secondary cluster you wish to remove from the Global Datastore
dgrgReplicationGroupRegion :: Lens' DisassociateGlobalReplicationGroup Text
dgrgReplicationGroupRegion = lens _dgrgReplicationGroupRegion (\s a -> s {_dgrgReplicationGroupRegion = a})

instance AWSRequest DisassociateGlobalReplicationGroup where
  type
    Rs DisassociateGlobalReplicationGroup =
      DisassociateGlobalReplicationGroupResponse
  request = postQuery elastiCache
  response =
    receiveXMLWrapper
      "DisassociateGlobalReplicationGroupResult"
      ( \s h x ->
          DisassociateGlobalReplicationGroupResponse'
            <$> (x .@? "GlobalReplicationGroup") <*> (pure (fromEnum s))
      )

instance Hashable DisassociateGlobalReplicationGroup

instance NFData DisassociateGlobalReplicationGroup

instance ToHeaders DisassociateGlobalReplicationGroup where
  toHeaders = const mempty

instance ToPath DisassociateGlobalReplicationGroup where
  toPath = const "/"

instance ToQuery DisassociateGlobalReplicationGroup where
  toQuery DisassociateGlobalReplicationGroup' {..} =
    mconcat
      [ "Action" =: ("DisassociateGlobalReplicationGroup" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "GlobalReplicationGroupId" =: _dgrgGlobalReplicationGroupId,
        "ReplicationGroupId" =: _dgrgReplicationGroupId,
        "ReplicationGroupRegion" =: _dgrgReplicationGroupRegion
      ]

-- | /See:/ 'disassociateGlobalReplicationGroupResponse' smart constructor.
data DisassociateGlobalReplicationGroupResponse = DisassociateGlobalReplicationGroupResponse'
  { _dgrgrsGlobalReplicationGroup ::
      !( Maybe
           GlobalReplicationGroup
       ),
    _dgrgrsResponseStatus ::
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

-- | Creates a value of 'DisassociateGlobalReplicationGroupResponse' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dgrgrsGlobalReplicationGroup' - Undocumented member.
--
-- * 'dgrgrsResponseStatus' - -- | The response status code.
disassociateGlobalReplicationGroupResponse ::
  -- | 'dgrgrsResponseStatus'
  Int ->
  DisassociateGlobalReplicationGroupResponse
disassociateGlobalReplicationGroupResponse pResponseStatus_ =
  DisassociateGlobalReplicationGroupResponse'
    { _dgrgrsGlobalReplicationGroup =
        Nothing,
      _dgrgrsResponseStatus = pResponseStatus_
    }

-- | Undocumented member.
dgrgrsGlobalReplicationGroup :: Lens' DisassociateGlobalReplicationGroupResponse (Maybe GlobalReplicationGroup)
dgrgrsGlobalReplicationGroup = lens _dgrgrsGlobalReplicationGroup (\s a -> s {_dgrgrsGlobalReplicationGroup = a})

-- | -- | The response status code.
dgrgrsResponseStatus :: Lens' DisassociateGlobalReplicationGroupResponse Int
dgrgrsResponseStatus = lens _dgrgrsResponseStatus (\s a -> s {_dgrgrsResponseStatus = a})

instance NFData DisassociateGlobalReplicationGroupResponse
