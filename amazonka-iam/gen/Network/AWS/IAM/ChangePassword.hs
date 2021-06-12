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
-- Module      : Network.AWS.IAM.ChangePassword
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password of the IAM user who is calling this operation. This
-- operation can be performed using the AWS CLI, the AWS API, or the __My
-- Security Credentials__ page in the AWS Management Console. The AWS
-- account root user password is not affected by this operation.
--
-- Use UpdateLoginProfile to use the AWS CLI, the AWS API, or the __Users__
-- page in the IAM console to change the password for any IAM user. For
-- more information about modifying passwords, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing passwords>
-- in the /IAM User Guide/.
module Network.AWS.IAM.ChangePassword
  ( -- * Creating a Request
    ChangePassword (..),
    newChangePassword,

    -- * Request Lenses
    changePassword_oldPassword,
    changePassword_newPassword,

    -- * Destructuring the Response
    ChangePasswordResponse (..),
    newChangePasswordResponse,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.IAM.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newChangePassword' smart constructor.
data ChangePassword = ChangePassword'
  { -- | The IAM user\'s current password.
    oldPassword :: Core.Sensitive Core.Text,
    -- | The new password. The new password must conform to the AWS account\'s
    -- password policy, if one exists.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of characters. That string can
    -- include almost any printable ASCII character from the space (@\\u0020@)
    -- through the end of the ASCII character range (@\\u00FF@). You can also
    -- include the tab (@\\u0009@), line feed (@\\u000A@), and carriage return
    -- (@\\u000D@) characters. Any of these characters are valid in a password.
    -- However, many tools, such as the AWS Management Console, might restrict
    -- the ability to type certain characters because they have special meaning
    -- within that tool.
    newPassword' :: Core.Sensitive Core.Text
  }
  deriving (Core.Eq, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChangePassword' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'oldPassword', 'changePassword_oldPassword' - The IAM user\'s current password.
--
-- 'newPassword'', 'changePassword_newPassword' - The new password. The new password must conform to the AWS account\'s
-- password policy, if one exists.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of characters. That string can
-- include almost any printable ASCII character from the space (@\\u0020@)
-- through the end of the ASCII character range (@\\u00FF@). You can also
-- include the tab (@\\u0009@), line feed (@\\u000A@), and carriage return
-- (@\\u000D@) characters. Any of these characters are valid in a password.
-- However, many tools, such as the AWS Management Console, might restrict
-- the ability to type certain characters because they have special meaning
-- within that tool.
newChangePassword ::
  -- | 'oldPassword'
  Core.Text ->
  -- | 'newPassword''
  Core.Text ->
  ChangePassword
newChangePassword pOldPassword_ pNewPassword_ =
  ChangePassword'
    { oldPassword =
        Core._Sensitive Lens.# pOldPassword_,
      newPassword' = Core._Sensitive Lens.# pNewPassword_
    }

-- | The IAM user\'s current password.
changePassword_oldPassword :: Lens.Lens' ChangePassword Core.Text
changePassword_oldPassword = Lens.lens (\ChangePassword' {oldPassword} -> oldPassword) (\s@ChangePassword' {} a -> s {oldPassword = a} :: ChangePassword) Core.. Core._Sensitive

-- | The new password. The new password must conform to the AWS account\'s
-- password policy, if one exists.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of characters. That string can
-- include almost any printable ASCII character from the space (@\\u0020@)
-- through the end of the ASCII character range (@\\u00FF@). You can also
-- include the tab (@\\u0009@), line feed (@\\u000A@), and carriage return
-- (@\\u000D@) characters. Any of these characters are valid in a password.
-- However, many tools, such as the AWS Management Console, might restrict
-- the ability to type certain characters because they have special meaning
-- within that tool.
changePassword_newPassword :: Lens.Lens' ChangePassword Core.Text
changePassword_newPassword = Lens.lens (\ChangePassword' {newPassword'} -> newPassword') (\s@ChangePassword' {} a -> s {newPassword' = a} :: ChangePassword) Core.. Core._Sensitive

instance Core.AWSRequest ChangePassword where
  type
    AWSResponse ChangePassword =
      ChangePasswordResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveNull ChangePasswordResponse'

instance Core.Hashable ChangePassword

instance Core.NFData ChangePassword

instance Core.ToHeaders ChangePassword where
  toHeaders = Core.const Core.mempty

instance Core.ToPath ChangePassword where
  toPath = Core.const "/"

instance Core.ToQuery ChangePassword where
  toQuery ChangePassword' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("ChangePassword" :: Core.ByteString),
        "Version" Core.=: ("2010-05-08" :: Core.ByteString),
        "OldPassword" Core.=: oldPassword,
        "NewPassword" Core.=: newPassword'
      ]

-- | /See:/ 'newChangePasswordResponse' smart constructor.
data ChangePasswordResponse = ChangePasswordResponse'
  {
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'ChangePasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newChangePasswordResponse ::
  ChangePasswordResponse
newChangePasswordResponse = ChangePasswordResponse'

instance Core.NFData ChangePasswordResponse
