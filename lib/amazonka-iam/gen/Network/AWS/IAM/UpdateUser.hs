{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
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
    uufUserName,
    uufNewPath,
    uufNewUserName,

    -- * Destructuring the response
    UpdateUserResponse (..),
    mkUpdateUserResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkUpdateUser' smart constructor.
data UpdateUser = UpdateUser'
  { -- | Name of the user to update. If you're changing the name of the user, this is the original user name.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
    userName :: Types.UserName,
    -- | New path for the IAM user. Include this parameter only if you're changing the user's path.
    --
    -- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
    newPath :: Core.Maybe Types.NewPath,
    -- | New name for the user. Include this parameter only if you're changing the user's name.
    --
    -- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
    newUserName :: Core.Maybe Types.NewUserName
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUser' value with any optional fields omitted.
mkUpdateUser ::
  -- | 'userName'
  Types.UserName ->
  UpdateUser
mkUpdateUser userName =
  UpdateUser'
    { userName,
      newPath = Core.Nothing,
      newUserName = Core.Nothing
    }

-- | Name of the user to update. If you're changing the name of the user, this is the original user name.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of upper and lowercase alphanumeric characters with no spaces. You can also include any of the following characters: _+=,.@-
--
-- /Note:/ Consider using 'userName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uufUserName :: Lens.Lens' UpdateUser Types.UserName
uufUserName = Lens.field @"userName"
{-# DEPRECATED uufUserName "Use generic-lens or generic-optics with 'userName' instead." #-}

-- | New path for the IAM user. Include this parameter only if you're changing the user's path.
--
-- This parameter allows (through its <http://wikipedia.org/wiki/regex regex pattern> ) a string of characters consisting of either a forward slash (/) by itself or a string that must begin and end with forward slashes. In addition, it can contain any ASCII character from the ! (@\u0021@ ) through the DEL character (@\u007F@ ), including most punctuation characters, digits, and upper and lowercased letters.
--
-- /Note:/ Consider using 'newPath' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uufNewPath :: Lens.Lens' UpdateUser (Core.Maybe Types.NewPath)
uufNewPath = Lens.field @"newPath"
{-# DEPRECATED uufNewPath "Use generic-lens or generic-optics with 'newPath' instead." #-}

-- | New name for the user. Include this parameter only if you're changing the user's name.
--
-- IAM user, group, role, and policy names must be unique within the account. Names are not distinguished by case. For example, you cannot create resources named both "MyResource" and "myresource".
--
-- /Note:/ Consider using 'newUserName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
uufNewUserName :: Lens.Lens' UpdateUser (Core.Maybe Types.NewUserName)
uufNewUserName = Lens.field @"newUserName"
{-# DEPRECATED uufNewUserName "Use generic-lens or generic-optics with 'newUserName' instead." #-}

instance Core.AWSRequest UpdateUser where
  type Rs UpdateUser = UpdateUserResponse
  request x@Core.Request {..} =
    Core.Request
      { Core._rqService = Types.mkServiceConfig,
        Core._rqMethod = Request.POST,
        Core._rqPath = Core.rawPath "/",
        Core._rqQuery = Core.mempty,
        Core._rqHeaders =
          Core.pure
            ( "Content-Type",
              "application/x-www-form-urlencoded; charset=utf-8"
            ),
        Core._rqBody =
          Core.toFormBody
            ( Core.pure ("Action", "UpdateUser")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "UserName" userName)
                Core.<> (Core.toQueryValue "NewPath" Core.<$> newPath)
                Core.<> (Core.toQueryValue "NewUserName" Core.<$> newUserName)
            )
      }
  response = Response.receiveNull UpdateUserResponse'

-- | /See:/ 'mkUpdateUserResponse' smart constructor.
data UpdateUserResponse = UpdateUserResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'UpdateUserResponse' value with any optional fields omitted.
mkUpdateUserResponse ::
  UpdateUserResponse
mkUpdateUserResponse = UpdateUserResponse'
