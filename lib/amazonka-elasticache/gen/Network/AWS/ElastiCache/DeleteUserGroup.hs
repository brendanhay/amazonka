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
-- Module      : Network.AWS.ElastiCache.DeleteUserGroup
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Deletes a ser group. The user group must first be disassociated from the replcation group before it can be deleted. For more information, see <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)> .
module Network.AWS.ElastiCache.DeleteUserGroup
  ( -- * Creating a Request
    deleteUserGroup,
    DeleteUserGroup,

    -- * Request Lenses
    dugUserGroupId,

    -- * Destructuring the Response
    userGroup,
    UserGroup,

    -- * Response Lenses
    ugStatus,
    ugUserIds,
    ugARN,
    ugUserGroupId,
    ugEngine,
    ugPendingChanges,
    ugReplicationGroups,
  )
where

import Network.AWS.ElastiCache.Types
import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.Request
import Network.AWS.Response

-- | /See:/ 'deleteUserGroup' smart constructor.
newtype DeleteUserGroup = DeleteUserGroup' {_dugUserGroupId :: Text}
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'DeleteUserGroup' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dugUserGroupId' - The ID of the user group.
deleteUserGroup ::
  -- | 'dugUserGroupId'
  Text ->
  DeleteUserGroup
deleteUserGroup pUserGroupId_ =
  DeleteUserGroup' {_dugUserGroupId = pUserGroupId_}

-- | The ID of the user group.
dugUserGroupId :: Lens' DeleteUserGroup Text
dugUserGroupId = lens _dugUserGroupId (\s a -> s {_dugUserGroupId = a})

instance AWSRequest DeleteUserGroup where
  type Rs DeleteUserGroup = UserGroup
  request = postQuery elastiCache
  response =
    receiveXMLWrapper "DeleteUserGroupResult" (\s h x -> parseXML x)

instance Hashable DeleteUserGroup

instance NFData DeleteUserGroup

instance ToHeaders DeleteUserGroup where
  toHeaders = const mempty

instance ToPath DeleteUserGroup where
  toPath = const "/"

instance ToQuery DeleteUserGroup where
  toQuery DeleteUserGroup' {..} =
    mconcat
      [ "Action" =: ("DeleteUserGroup" :: ByteString),
        "Version" =: ("2015-02-02" :: ByteString),
        "UserGroupId" =: _dugUserGroupId
      ]
