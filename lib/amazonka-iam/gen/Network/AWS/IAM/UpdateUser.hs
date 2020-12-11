{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.UpdateUser
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name and/or the path of the specified IAM user.
--
-- /Important:/ You should understand the implications of changing an IAM user's path or name. For more information, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_users_manage.html#id_users_renaming Renaming an IAM User> and <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_groups_manage_rename.html Renaming an IAM Group> in the /IAM User Guide/ .
module Network.AWS.IAM.UpdateUser
  ( -- * Creating a request
    UpdateUser (..),
    mkUpdateUser,

    -- ** Request lenses
    updNewUserName,
    updNewPath,
    updUserName,

    -- * Destructuring the response
    UpdateUserResponse (..),
    mkUpdateUserResponse,
  )
where

import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { newUserName :: Lude.Maybe Lude.Text,
    newPath :: Lude.Maybe Lude.Text,
    userName :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUser' with the minimum fields required to make a request.
--
-- * 'newPath' - New path for the IAM user. Include this parameter only if you're changing the user's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
-- * 'newUserName' - New name for the user. Include this parameter only if you're changing the user's name.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
-- * 'userName' - Name of the user to update. If you're changing the name of the user, this is the original user name.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
mkUpdateUser ::
  -- | 'userName'
  Lude.Text ->
  UpdateUser
mkUpdateUser pUserName_ =
  UpdateUser'
    { newUserName = Lude.Nothing,
      newPath = Lude.Nothing,
      userName = pUserName_
    }

-- | New name for the user. Include this parameter only if you're changing the user's name.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'newUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updNewUserName :: Lens.Lens' UpdateUser (Lude.Maybe Lude.Text)
updNewUserName = Lens.lens (newUserName :: UpdateUser -> Lude.Maybe Lude.Text) (\s a -> s {newUserName = a} :: UpdateUser)
{-# DEPRECATED updNewUserName "Use generic-lens or generic-optics with 'newUserName' instead." #-}

-- | New path for the IAM user. Include this parameter only if you're changing the user's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'newPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updNewPath :: Lens.Lens' UpdateUser (Lude.Maybe Lude.Text)
updNewPath = Lens.lens (newPath :: UpdateUser -> Lude.Maybe Lude.Text) (\s a -> s {newPath = a} :: UpdateUser)
{-# DEPRECATED updNewPath "Use generic-lens or generic-optics with 'newPath' instead." #-}

-- | Name of the user to update. If you're changing the name of the user, this is the original user name.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
updUserName :: Lens.Lens' UpdateUser Lude.Text
updUserName = Lens.lens (userName :: UpdateUser -> Lude.Text) (\s a -> s {userName = a} :: UpdateUser)
{-# DEPRECATED updUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

instance Lude.AWSRequest UpdateUser where
  type Rs UpdateUser = UpdateUserResponse
  request = Req.postQuery iamService
  response = Res.receiveNull UpdateUserResponse'

instance Lude.ToHeaders UpdateUser where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath UpdateUser where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateUser where
  toQuery UpdateUser' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("UpdateUser" :: Lude.ByteString),
        "Version" Lude.=: ("2010-05-08" :: Lude.ByteString),
        "NewUserName" Lude.=: newUserName,
        "NewPath" Lude.=: newPath,
        "UserName" Lude.=: userName
      ]

-- | /See:/ 'mkUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateUserResponse' with the minimum fields required to make a request.
mkUpdateUserResponse ::
  UpdateUserResponse
mkUpdateUserResponse = UpdateUserResponse'
