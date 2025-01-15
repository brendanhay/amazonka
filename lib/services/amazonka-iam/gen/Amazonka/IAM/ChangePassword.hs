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
-- Module      : Amazonka.IAM.ChangePassword
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Changes the password of the IAM user who is calling this operation. This
-- operation can be performed using the CLI, the Amazon Web Services API,
-- or the __My Security Credentials__ page in the Amazon Web Services
-- Management Console. The Amazon Web Services account root user password
-- is not affected by this operation.
--
-- Use UpdateLoginProfile to use the CLI, the Amazon Web Services API, or
-- the __Users__ page in the IAM console to change the password for any IAM
-- user. For more information about modifying passwords, see
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/Using_ManagingLogins.html Managing passwords>
-- in the /IAM User Guide/.
module Amazonka.IAM.ChangePassword
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.IAM.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newChangePassword' smart constructor.
data ChangePassword = ChangePassword'
  { -- | The IAM user\'s current password.
    oldPassword :: Data.Sensitive Prelude.Text,
    -- | The new password. The new password must conform to the Amazon Web
    -- Services account\'s password policy, if one exists.
    --
    -- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
    -- validate this parameter is a string of characters. That string can
    -- include almost any printable ASCII character from the space (@\\u0020@)
    -- through the end of the ASCII character range (@\\u00FF@). You can also
    -- include the tab (@\\u0009@), line feed (@\\u000A@), and carriage return
    -- (@\\u000D@) characters. Any of these characters are valid in a password.
    -- However, many tools, such as the Amazon Web Services Management Console,
    -- might restrict the ability to type certain characters because they have
    -- special meaning within that tool.
    newPassword' :: Data.Sensitive Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

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
-- 'newPassword'', 'changePassword_newPassword' - The new password. The new password must conform to the Amazon Web
-- Services account\'s password policy, if one exists.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of characters. That string can
-- include almost any printable ASCII character from the space (@\\u0020@)
-- through the end of the ASCII character range (@\\u00FF@). You can also
-- include the tab (@\\u0009@), line feed (@\\u000A@), and carriage return
-- (@\\u000D@) characters. Any of these characters are valid in a password.
-- However, many tools, such as the Amazon Web Services Management Console,
-- might restrict the ability to type certain characters because they have
-- special meaning within that tool.
newChangePassword ::
  -- | 'oldPassword'
  Prelude.Text ->
  -- | 'newPassword''
  Prelude.Text ->
  ChangePassword
newChangePassword pOldPassword_ pNewPassword_ =
  ChangePassword'
    { oldPassword =
        Data._Sensitive Lens.# pOldPassword_,
      newPassword' = Data._Sensitive Lens.# pNewPassword_
    }

-- | The IAM user\'s current password.
changePassword_oldPassword :: Lens.Lens' ChangePassword Prelude.Text
changePassword_oldPassword = Lens.lens (\ChangePassword' {oldPassword} -> oldPassword) (\s@ChangePassword' {} a -> s {oldPassword = a} :: ChangePassword) Prelude.. Data._Sensitive

-- | The new password. The new password must conform to the Amazon Web
-- Services account\'s password policy, if one exists.
--
-- The <http://wikipedia.org/wiki/regex regex pattern> that is used to
-- validate this parameter is a string of characters. That string can
-- include almost any printable ASCII character from the space (@\\u0020@)
-- through the end of the ASCII character range (@\\u00FF@). You can also
-- include the tab (@\\u0009@), line feed (@\\u000A@), and carriage return
-- (@\\u000D@) characters. Any of these characters are valid in a password.
-- However, many tools, such as the Amazon Web Services Management Console,
-- might restrict the ability to type certain characters because they have
-- special meaning within that tool.
changePassword_newPassword :: Lens.Lens' ChangePassword Prelude.Text
changePassword_newPassword = Lens.lens (\ChangePassword' {newPassword'} -> newPassword') (\s@ChangePassword' {} a -> s {newPassword' = a} :: ChangePassword) Prelude.. Data._Sensitive

instance Core.AWSRequest ChangePassword where
  type
    AWSResponse ChangePassword =
      ChangePasswordResponse
  request overrides =
    Request.postQuery (overrides defaultService)
  response =
    Response.receiveNull ChangePasswordResponse'

instance Prelude.Hashable ChangePassword where
  hashWithSalt _salt ChangePassword' {..} =
    _salt
      `Prelude.hashWithSalt` oldPassword
      `Prelude.hashWithSalt` newPassword'

instance Prelude.NFData ChangePassword where
  rnf ChangePassword' {..} =
    Prelude.rnf oldPassword `Prelude.seq`
      Prelude.rnf newPassword'

instance Data.ToHeaders ChangePassword where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath ChangePassword where
  toPath = Prelude.const "/"

instance Data.ToQuery ChangePassword where
  toQuery ChangePassword' {..} =
    Prelude.mconcat
      [ "Action"
          Data.=: ("ChangePassword" :: Prelude.ByteString),
        "Version"
          Data.=: ("2010-05-08" :: Prelude.ByteString),
        "OldPassword" Data.=: oldPassword,
        "NewPassword" Data.=: newPassword'
      ]

-- | /See:/ 'newChangePasswordResponse' smart constructor.
data ChangePasswordResponse = ChangePasswordResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'ChangePasswordResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newChangePasswordResponse ::
  ChangePasswordResponse
newChangePasswordResponse = ChangePasswordResponse'

instance Prelude.NFData ChangePasswordResponse where
  rnf _ = ()
