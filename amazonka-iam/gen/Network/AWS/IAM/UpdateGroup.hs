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
-- Module      : Network.AWS.IAM.UpdateGroup
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and\/or the path of the specified IAM group.
--
-- You should understand the implications of changing a group\'s path or
-- name. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_WorkingWithGroupsAndUsers.html Renaming users and groups>
-- in the /IAM User Guide/.
--
-- The person making the request (the principal), must have permission to
-- change the role group with the old name and the new name. For example,
-- to change the group named @Managers@ to @MGRs@, the principal must have
-- a policy that allows them to update both groups. If the principal has
-- permission to update the @Managers@ group, but not the @MGRs@ group,
-- then the update fails. For more information about permissions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/access.html Access management>.
module Network.AWS.IAM.UpdateGroup
  ( -- * Creating a Request
    UpdateGroup (..),
    newUpdateGroup,

    -- * Request Lenses
    updateGroup_newGroupName,
    updateGroup_newPath,
    updateGroup_groupName,

    -- * Destructuring the Response
    UpdateGroupResponse (..),
    newUpdateGroupResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateGroup' smart constructor.
data UpdateGroup = UpdateGroup'
  { -- | New name for the IAM group. Only include this if changing the group\'s
    -- name.
    --
    -- IAM user, group, role, and policy names must be unique within the
    -- account. Names are not distinguished by case. For example, you cannot
    -- create resources named both \"MyResource\" and \"myresource\".
    newGroupName' :: Prelude.Maybe Prelude.Text,
    -- | New path for the IAM group. Only include this if changing the group\'s
    -- path.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    newPath' :: Prelude.Maybe Prelude.Text,
    -- | Name of the IAM group to update. If you\'re changing the name of the
    -- group, this is the original name.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    groupName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroup' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newGroupName'', 'updateGroup_newGroupName' - New name for the IAM group. Only include this if changing the group\'s
-- name.
--
-- IAM user, group, role, and policy names must be unique within the
-- account. Names are not distinguished by case. For example, you cannot
-- create resources named both \"MyResource\" and \"myresource\".
--
-- 'newPath'', 'updateGroup_newPath' - New path for the IAM group. Only include this if changing the group\'s
-- path.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- 'groupName', 'updateGroup_groupName' - Name of the IAM group to update. If you\'re changing the name of the
-- group, this is the original name.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newUpdateGroup ::
  -- | 'groupName'
  Prelude.Text ->
  UpdateGroup
newUpdateGroup pGroupName_ =
  UpdateGroup'
    { newGroupName' = Prelude.Nothing,
      newPath' = Prelude.Nothing,
      groupName = pGroupName_
    }

-- | New name for the IAM group. Only include this if changing the group\'s
-- name.
--
-- IAM user, group, role, and policy names must be unique within the
-- account. Names are not distinguished by case. For example, you cannot
-- create resources named both \"MyResource\" and \"myresource\".
updateGroup_newGroupName :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_newGroupName = Lens.lens (\UpdateGroup' {newGroupName'} -> newGroupName') (\s@UpdateGroup' {} a -> s {newGroupName' = a} :: UpdateGroup)

-- | New path for the IAM group. Only include this if changing the group\'s
-- path.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
updateGroup_newPath :: Lens.Lens' UpdateGroup (Prelude.Maybe Prelude.Text)
updateGroup_newPath = Lens.lens (\UpdateGroup' {newPath'} -> newPath') (\s@UpdateGroup' {} a -> s {newPath' = a} :: UpdateGroup)

-- | Name of the IAM group to update. If you\'re changing the name of the
-- group, this is the original name.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateGroup_groupName :: Lens.Lens' UpdateGroup Prelude.Text
updateGroup_groupName = Lens.lens (\UpdateGroup' {groupName} -> groupName) (\s@UpdateGroup' {} a -> s {groupName = a} :: UpdateGroup)

instance Prelude.AWSRequest UpdateGroup where
  type Rs UpdateGroup = UpdateGroupResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull UpdateGroupResponse'

instance Prelude.Hashable UpdateGroup

instance Prelude.NFData UpdateGroup

instance Prelude.ToHeaders UpdateGroup where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateGroup where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateGroup where
  toQuery UpdateGroup' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateGroup" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "NewGroupName" Prelude.=: newGroupName',
        "NewPath" Prelude.=: newPath',
        "GroupName" Prelude.=: groupName
      ]

-- | /See:/ 'newUpdateGroupResponse' smart constructor.
data UpdateGroupResponse = UpdateGroupResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateGroupResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateGroupResponse ::
  UpdateGroupResponse
newUpdateGroupResponse = UpdateGroupResponse'

instance Prelude.NFData UpdateGroupResponse
