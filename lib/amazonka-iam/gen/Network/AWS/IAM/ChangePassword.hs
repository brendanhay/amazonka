{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.ChangePassword
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password of the IAM user who is calling this operation. The AWS account root user password is not affected by this operation.
--
-- To change the password for a different user, see 'UpdateLoginProfile' . For more information about modifying passwords, see <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing Passwords> in the /IAM User Guide/ .
module Network.AWS.IAM.ChangePassword
  ( -- * Creating a request
    ChangePassword (..),
    mkChangePassword,

    -- ** Request lenses
    cpOldPassword,
    cpNewPassword,

    -- * Destructuring the response
    ChangePasswordResponse (..),
    mkChangePasswordResponse,
  )
where

import qualified Network.AWS.IAM.Types as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'mkChangePassword' smart constructor.
data ChangePassword = ChangePassword'
  { -- | The IAM user's current password.
    oldPassword :: Types.PasswordType,
    -- | The new password. The new password must conform to the AWS account's password policy, if one exists.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (@\u0020@ ) through the end of the ASCII character range (@\u00FF@ ). You can also include the tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ ) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
    newPassword :: Types.PasswordType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangePassword' value with any optional fields omitted.
mkChangePassword ::
  -- | 'oldPassword'
  Types.PasswordType ->
  -- | 'newPassword'
  Types.PasswordType ->
  ChangePassword
mkChangePassword oldPassword newPassword =
  ChangePassword' {oldPassword, newPassword}

-- | The IAM user's current password.
--
-- /Note:/ Consider using 'oldPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpOldPassword :: Lens.Lens' ChangePassword Types.PasswordType
cpOldPassword = Lens.field @"oldPassword"
{-# DEPRECATED cpOldPassword "Use generic-lens or generic-optics with 'oldPassword' instead." #-}

-- | The new password. The new password must conform to the AWS account's password policy, if one exists.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to validate this parameter is a string of characters. That string can include almost any printable ASCII character from the space (@\u0020@ ) through the end of the ASCII character range (@\u00FF@ ). You can also include the tab (@\u0009@ ), line feed (@\u000A@ ), and carriage return (@\u000D@ ) characters. Any of these characters are valid in a password. However, many tools, such as the AWS Management Console, might restrict the ability to type certain characters because they have special meaning within that tool.
--
-- /Note:/ Consider using 'newPassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cpNewPassword :: Lens.Lens' ChangePassword Types.PasswordType
cpNewPassword = Lens.field @"newPassword"
{-# DEPRECATED cpNewPassword "Use generic-lens or generic-optics with 'newPassword' instead." #-}

instance Core.AWSRequest ChangePassword where
  type Rs ChangePassword = ChangePasswordResponse
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
            ( Core.pure ("Action", "ChangePassword")
                Core.<> (Core.pure ("Version", "2010-05-08"))
                Core.<> (Core.toQueryValue "OldPassword" oldPassword)
                Core.<> (Core.toQueryValue "NewPassword" newPassword)
            )
      }
  response = Response.receiveNull ChangePasswordResponse'

-- | /See:/ 'mkChangePasswordResponse' smart constructor.
data ChangePasswordResponse = ChangePasswordResponse'
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'ChangePasswordResponse' value with any optional fields omitted.
mkChangePasswordResponse ::
  ChangePasswordResponse
mkChangePasswordResponse = ChangePasswordResponse'
