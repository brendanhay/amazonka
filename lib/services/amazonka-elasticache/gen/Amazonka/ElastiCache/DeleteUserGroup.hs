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
-- Module      : Amazonka.ElastiCache.DeleteUserGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- For Redis engine version 6.0 onwards: Deletes a user group. The user
-- group must first be disassociated from the replication group before it
-- can be deleted. For more information, see
-- <http://docs.aws.amazon.com/AmazonElastiCache/latest/red-ug/Clusters.RBAC.html Using Role Based Access Control (RBAC)>.
module Amazonka.ElastiCache.DeleteUserGroup
  ( -- * Creating a Request
    DeleteUserGroup (..),
    newDeleteUserGroup,

    -- * Request Lenses
    deleteUserGroup_userGroupId,

    -- * Destructuring the Response
    UserGroup (..),
    newUserGroup,

    -- * Response Lenses
    userGroup_arn,
    userGroup_engine,
    userGroup_minimumEngineVersion,
    userGroup_pendingChanges,
    userGroup_replicationGroups,
    userGroup_status,
    userGroup_userGroupId,
    userGroup_userIds,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.ElastiCache.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newDeleteUserGroup' smart constructor.
data DeleteUserGroup = DeleteUserGroup'
  { -- | The ID of the user group.
    userGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Text ->
  DeleteUserGroup
newDeleteUserGroup pUserGroupId_ =
  DeleteUserGroup' {userGroupId = pUserGroupId_}

-- | The ID of the user group.
deleteUserGroup_userGroupId :: Lens.Lens' DeleteUserGroup Prelude.Text
deleteUserGroup_userGroupId = Lens.lens (\DeleteUserGroup' {userGroupId} -> userGroupId) (\s@DeleteUserGroup' {} a -> s {userGroupId = a} :: DeleteUserGroup)

instance Core.AWSRequest DeleteUserGroup where
  type AWSResponse DeleteUserGroup = UserGroup
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "DeleteUserGroupResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable DeleteUserGroup where
  hashWithSalt _salt DeleteUserGroup' {..} =
    _salt `Prelude.hashWithSalt` userGroupId

instance Prelude.NFData DeleteUserGroup where
  rnf DeleteUserGroup' {..} = Prelude.rnf userGroupId

instance Data.ToHeaders DeleteUserGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath DeleteUserGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery DeleteUserGroup where
  toQuery DeleteUserGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("DeleteUserGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "UserGroupId" Data.=: userGroupId
      ]
