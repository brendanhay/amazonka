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
-- Module      : Network.AWS.IAM.UpdateUser
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and\/or the path of the specified IAM user.
--
-- You should understand the implications of changing an IAM user\'s path
-- or name. For more information, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_manage.html#id_users_renaming Renaming an IAM user>
-- and
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_groups_manage_rename.html Renaming an IAM group>
-- in the /IAM User Guide/.
--
-- To change a user name, the requester must have appropriate permissions
-- on both the source object and the target object. For example, to change
-- Bob to Robert, the entity making the request must have permission on Bob
-- and Robert, or must have permission on all (*). For more information
-- about permissions, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/PermissionsAndPolicies.html Permissions and policies>.
module Network.AWS.IAM.UpdateUser
  ( -- * Creating a Request
    UpdateUser (..),
    newUpdateUser,

    -- * Request Lenses
    updateUser_newPath,
    updateUser_newUserName,
    updateUser_userName,

    -- * Destructuring the Response
    UpdateUserResponse (..),
    newUpdateUserResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | New path for the IAM user. Include this parameter only if you\'re
    -- changing the user\'s path.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of either a forward slash (\/) by itself or a string that
    -- must begin and end with forward slashes. In addition, it can contain any
    -- ASCII character from the ! (@\\u0021@) through the DEL character
    -- (@\\u007F@), including most punctuation characters, digits, and upper
    -- and lowercased letters.
    newPath' :: Prelude.Maybe Prelude.Text,
    -- | New name for the user. Include this parameter only if you\'re changing
    -- the user\'s name.
    --
    -- IAM user, group, role, and policy names must be unique within the
    -- account. Names are not distinguished by case. For example, you cannot
    -- create resources named both \"MyResource\" and \"myresource\".
    newUserName' :: Prelude.Maybe Prelude.Text,
    -- | Name of the user to update. If you\'re changing the name of the user,
    -- this is the original user name.
    --
    -- This parameter allows (through its
    -- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
    -- consisting of upper and lowercase alphanumeric characters with no
    -- spaces. You can also include any of the following characters: _+=,.\@-
    userName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUser' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'newPath'', 'updateUser_newPath' - New path for the IAM user. Include this parameter only if you\'re
-- changing the user\'s path.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
--
-- 'newUserName'', 'updateUser_newUserName' - New name for the user. Include this parameter only if you\'re changing
-- the user\'s name.
--
-- IAM user, group, role, and policy names must be unique within the
-- account. Names are not distinguished by case. For example, you cannot
-- create resources named both \"MyResource\" and \"myresource\".
--
-- 'userName', 'updateUser_userName' - Name of the user to update. If you\'re changing the name of the user,
-- this is the original user name.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
newUpdateUser ::
  -- | 'userName'
  Prelude.Text ->
  UpdateUser
newUpdateUser pUserName_ =
  UpdateUser'
    { newPath' = Prelude.Nothing,
      newUserName' = Prelude.Nothing,
      userName = pUserName_
    }

-- | New path for the IAM user. Include this parameter only if you\'re
-- changing the user\'s path.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of either a forward slash (\/) by itself or a string that
-- must begin and end with forward slashes. In addition, it can contain any
-- ASCII character from the ! (@\\u0021@) through the DEL character
-- (@\\u007F@), including most punctuation characters, digits, and upper
-- and lowercased letters.
updateUser_newPath :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_newPath = Lens.lens (\UpdateUser' {newPath'} -> newPath') (\s@UpdateUser' {} a -> s {newPath' = a} :: UpdateUser)

-- | New name for the user. Include this parameter only if you\'re changing
-- the user\'s name.
--
-- IAM user, group, role, and policy names must be unique within the
-- account. Names are not distinguished by case. For example, you cannot
-- create resources named both \"MyResource\" and \"myresource\".
updateUser_newUserName :: Lens.Lens' UpdateUser (Prelude.Maybe Prelude.Text)
updateUser_newUserName = Lens.lens (\UpdateUser' {newUserName'} -> newUserName') (\s@UpdateUser' {} a -> s {newUserName' = a} :: UpdateUser)

-- | Name of the user to update. If you\'re changing the name of the user,
-- this is the original user name.
--
-- This parameter allows (through its
-- <http://wikipedia.org/wiki/regex regex pattern>) a string of characters
-- consisting of upper and lowercase alphanumeric characters with no
-- spaces. You can also include any of the following characters: _+=,.\@-
updateUser_userName :: Lens.Lens' UpdateUser Prelude.Text
updateUser_userName = Lens.lens (\UpdateUser' {userName} -> userName) (\s@UpdateUser' {} a -> s {userName = a} :: UpdateUser)

instance Prelude.AWSRequest UpdateUser where
  type Rs UpdateUser = UpdateUserResponse
  request = Request.postQuery defaultService
  response = Response.receiveNull UpdateUserResponse'

instance Prelude.Hashable UpdateUser

instance Prelude.NFData UpdateUser

instance Prelude.ToHeaders UpdateUser where
  toHeaders = Prelude.const Prelude.mempty

instance Prelude.ToPath UpdateUser where
  toPath = Prelude.const "/"

instance Prelude.ToQuery UpdateUser where
  toQuery UpdateUser' {..} =
    Prelude.mconcat
      [ "Action"
          Prelude.=: ("UpdateUser" :: Prelude.ByteString),
        "Version"
          Prelude.=: ("2010-05-08" :: Prelude.ByteString),
        "NewPath" Prelude.=: newPath',
        "NewUserName" Prelude.=: newUserName',
        "UserName" Prelude.=: userName
      ]

-- | /See:/ 'newUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'UpdateUserResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newUpdateUserResponse ::
  UpdateUserResponse
newUpdateUserResponse = UpdateUserResponse'

instance Prelude.NFData UpdateUserResponse
