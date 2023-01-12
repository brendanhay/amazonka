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
-- Module      : Amazonka.ElastiCache.ModifyUserGroup
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the list of users that belong to the user group.
module Amazonka.ElastiCache.ModifyUserGroup
  ( -- * Creating a Request
    ModifyUserGroup (..),
    newModifyUserGroup,

    -- * Request Lenses
    modifyUserGroup_userIdsToAdd,
    modifyUserGroup_userIdsToRemove,
    modifyUserGroup_userGroupId,

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

-- | /See:/ 'newModifyUserGroup' smart constructor.
data ModifyUserGroup = ModifyUserGroup'
  { -- | The list of user IDs to add to the user group.
    userIdsToAdd :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The list of user IDs to remove from the user group.
    userIdsToRemove :: Prelude.Maybe (Prelude.NonEmpty Prelude.Text),
    -- | The ID of the user group.
    userGroupId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ModifyUserGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userIdsToAdd', 'modifyUserGroup_userIdsToAdd' - The list of user IDs to add to the user group.
--
-- 'userIdsToRemove', 'modifyUserGroup_userIdsToRemove' - The list of user IDs to remove from the user group.
--
-- 'userGroupId', 'modifyUserGroup_userGroupId' - The ID of the user group.
newModifyUserGroup ::
  -- | 'userGroupId'
  Prelude.Text ->
  ModifyUserGroup
newModifyUserGroup pUserGroupId_ =
  ModifyUserGroup'
    { userIdsToAdd = Prelude.Nothing,
      userIdsToRemove = Prelude.Nothing,
      userGroupId = pUserGroupId_
    }

-- | The list of user IDs to add to the user group.
modifyUserGroup_userIdsToAdd :: Lens.Lens' ModifyUserGroup (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
modifyUserGroup_userIdsToAdd = Lens.lens (\ModifyUserGroup' {userIdsToAdd} -> userIdsToAdd) (\s@ModifyUserGroup' {} a -> s {userIdsToAdd = a} :: ModifyUserGroup) Prelude.. Lens.mapping Lens.coerced

-- | The list of user IDs to remove from the user group.
modifyUserGroup_userIdsToRemove :: Lens.Lens' ModifyUserGroup (Prelude.Maybe (Prelude.NonEmpty Prelude.Text))
modifyUserGroup_userIdsToRemove = Lens.lens (\ModifyUserGroup' {userIdsToRemove} -> userIdsToRemove) (\s@ModifyUserGroup' {} a -> s {userIdsToRemove = a} :: ModifyUserGroup) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the user group.
modifyUserGroup_userGroupId :: Lens.Lens' ModifyUserGroup Prelude.Text
modifyUserGroup_userGroupId = Lens.lens (\ModifyUserGroup' {userGroupId} -> userGroupId) (\s@ModifyUserGroup' {} a -> s {userGroupId = a} :: ModifyUserGroup)

instance Core.AWSRequest ModifyUserGroup where
  type AWSResponse ModifyUserGroup = UserGroup
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveXMLWrapper
      "ModifyUserGroupResult"
      (\s h x -> Data.parseXML x)

instance Prelude.Hashable ModifyUserGroup where
  hashWithSalt _salt ModifyUserGroup' {..} =
    _salt `Prelude.hashWithSalt` userIdsToAdd
      `Prelude.hashWithSalt` userIdsToRemove
      `Prelude.hashWithSalt` userGroupId

instance Prelude.NFData ModifyUserGroup where
  rnf ModifyUserGroup' {..} =
    Prelude.rnf userIdsToAdd
      `Prelude.seq` Prelude.rnf userIdsToRemove
      `Prelude.seq` Prelude.rnf userGroupId

instance Data.ToHeaders ModifyUserGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ModifyUserGroup where
  toPath = Prelude.const "/"

instance Data.ToQuery ModifyUserGroup where
  toQuery ModifyUserGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ModifyUserGroup" :: Prelude.ByteString),
        "Version"
          Data.=: ("2015-02-02" :: Prelude.ByteString),
        "UserIdsToAdd"
          Data.=: Data.toQuery
            (Data.toQueryList "member" Prelude.<$> userIdsToAdd),
        "UserIdsToRemove"
          Data.=: Data.toQuery
            ( Data.toQueryList "member"
                Prelude.<$> userIdsToRemove
            ),
        "UserGroupId" Data.=: userGroupId
      ]
