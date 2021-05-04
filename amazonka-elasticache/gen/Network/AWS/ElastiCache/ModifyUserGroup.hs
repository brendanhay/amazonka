{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.ElastiCache.ModifyUserGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the list of users that belong to the user group.
module Network.AWS.ElastiCache.ModifyUserGroup
  ( -- * Creating a Request
    ModifyUserGroup (..),
    newModifyUserGroup,

    -- * Request Lenses
    modifyUserGroup_userIdsToRemove,
    modifyUserGroup_userIdsToAdd,
    modifyUserGroup_userGroupId,

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

import Network.AWS.ElastiCache.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newModifyUserGroup' smart constructor.
data ModifyUserGroup = ModifyUserGroup'
  { -- | The list of user IDs to remove from the user group.
    userIdsToRemove :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The list of user IDs to add to the user group.
    userIdsToAdd :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID of the user group.
    userGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'ModifyUserGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userIdsToRemove', 'modifyUserGroup_userIdsToRemove' - The list of user IDs to remove from the user group.
--
-- 'userIdsToAdd', 'modifyUserGroup_userIdsToAdd' - The list of user IDs to add to the user group.
--
-- 'userGroupId', 'modifyUserGroup_userGroupId' - The ID of the user group.
newModifyUserGroup ::
  -- | 'userGroupId'
  Prelude.Text ->
  ModifyUserGroup
newModifyUserGroup pUserGroupId_ =
  ModifyUserGroup'
    { userIdsToRemove = Prelude.Nothing,
      userIdsToAdd = Prelude.Nothing,
      userGroupId = pUserGroupId_
    }

-- | The list of user IDs to remove from the user group.
modifyUserGroup_userIdsToRemove :: Lens.Lens' ModifyUserGroup (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
modifyUserGroup_userIdsToRemove = Lens.lens (\ModifyUserGroup' {userIdsToRemove} -> userIdsToRemove) (\s@ModifyUserGroup' {} a -> s {userIdsToRemove = a} :: ModifyUserGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The list of user IDs to add to the user group.
modifyUserGroup_userIdsToAdd :: Lens.Lens' ModifyUserGroup (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
modifyUserGroup_userIdsToAdd = Lens.lens (\ModifyUserGroup' {userIdsToAdd} -> userIdsToAdd) (\s@ModifyUserGroup' {} a -> s {userIdsToAdd = a} :: ModifyUserGroup) Prelude.. Lens.mapping Prelude._Coerce

-- | The ID of the user group.
modifyUserGroup_userGroupId :: Lens.Lens' ModifyUserGroup Prelude.Text
modifyUserGroup_userGroupId = Lens.lens (\ModifyUserGroup' {userGroupId} -> userGroupId) (\s@ModifyUserGroup' {} a -> s {userGroupId = a} :: ModifyUserGroup)

instance Prelude.AWSRequest ModifyUserGroup where
  type Rs ModifyUserGroup = UserGroup
  request = Request.postQuery defaultService
  response =
    Response.receiveXMLWrapper
      "ModifyUserGroupResult"
      (\s h x -> Prelude.parseXML x)

instance Prelude.Hashable ModifyUserGroup

instance Prelude.NFData ModifyUserGroup

instance Prelude.ToHeaders ModifyUserGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath ModifyUserGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery ModifyUserGroup where
  toQuery ModifyUserGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("ModifyUserGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2015-02-02" :: Prelude.ByteString),
        "UserIdsToRemove"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> userIdsToRemove
            ),
        "UserIdsToAdd"
          Prelude.=: Prelude.toQuery
            ( Prelude.toQueryList "member"
                Prelude.<$> userIdsToAdd
            ),
        "UserGroupId" Prelude.=: userGroupId
      ]
