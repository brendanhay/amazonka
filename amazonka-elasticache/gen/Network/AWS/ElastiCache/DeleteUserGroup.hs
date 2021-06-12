{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.ElastiCache.DeleteUserGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.x onwards: Deletes a user group. The user
-- group must first be disassociated from the replication group before it
-- can be deleted. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)>.
module Network.AWS.ElastiCache.DeleteUserGroup
  ( -- * Creating a Request
    DeleteUserGroup (..),
    newDeleteUserGroup,

    -- * Request Lenses
    deleteUserGroup_userGroupId,

    -- * Destructuring the Response
    UserGroup (..),
    newUserGroup,

    -- * Response Lenses
    userGroup_status,
    userGroup_replicationGroups,
    userGroup_arn,
    userGroup_userIds,
    userGroup_engine,
    userGroup_userGroupId,
    userGroup_pendingChanges,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteUserGroup' smart constructor.
data DeleteUserGroup = DeleteUserGroup'
  { -- | The ID of the user group.
    userGroupId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteUserGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userGroupId', 'deleteUserGroup_userGroupId' - The ID of the user group.
newDeleteUserGroup ::
  -- | 'userGroupId'
  Core.Text ->
  DeleteUserGroup
newDeleteUserGroup pUserGroupId_ =
  DeleteUserGroup' {userGroupId = pUserGroupId_}

-- | The ID of the user group.
deleteUserGroup_userGroupId :: Lens.Lens' DeleteUserGroup Core.Text
deleteUserGroup_userGroupId = Lens.lens (\DeleteUserGroup' {userGroupId} -> userGroupId) (\s@DeleteUserGroup' {} a -> s {userGroupId = a} :: DeleteUserGroup)

instance Core.AWSRequest DeleteUserGroup where
  type AWSResponse DeleteUserGroup = UserGroup
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "DeleteUserGroupResult"
      (\s h x -> Core.parseXML x)

instance Core.Hashable DeleteUserGroup

instance Core.NFData DeleteUserGroup

instance Core.ToHeaders DeleteUserGroup where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteUserGroup where
  toPath = Core.const "/"

instance Core.ToQuery DeleteUserGroup where
  toQuery DeleteUserGroup' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteUserGroup" :: Core.ByteString),
        "Version" Core.=: ("2015-02-02" :: Core.ByteString),
        "UserGroupId" Core.=: userGroupId
      ]
