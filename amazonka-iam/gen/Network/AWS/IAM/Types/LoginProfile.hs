{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.LoginProfile
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.LoginProfile where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains the user name and password create date for a user.
--
-- This data type is used as a response element in the CreateLoginProfile
-- and GetLoginProfile operations.
--
-- /See:/ 'newLoginProfile' smart constructor.
data LoginProfile = LoginProfile'
  { -- | Specifies whether the user is required to set a new password on next
    -- sign-in.
    passwordResetRequired :: Prelude.Maybe Prelude.Bool,
    -- | The name of the user, which can be used for signing in to the AWS
    -- Management Console.
    userName :: Prelude.Text,
    -- | The date when the password for the user was created.
    createDate :: Prelude.ISO8601
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'LoginProfile' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'passwordResetRequired', 'loginProfile_passwordResetRequired' - Specifies whether the user is required to set a new password on next
-- sign-in.
--
-- 'userName', 'loginProfile_userName' - The name of the user, which can be used for signing in to the AWS
-- Management Console.
--
-- 'createDate', 'loginProfile_createDate' - The date when the password for the user was created.
newLoginProfile ::
  -- | 'userName'
  Prelude.Text ->
  -- | 'createDate'
  Prelude.UTCTime ->
  LoginProfile
newLoginProfile pUserName_ pCreateDate_ =
  LoginProfile'
    { passwordResetRequired =
        Prelude.Nothing,
      userName = pUserName_,
      createDate = Prelude._Time Lens.# pCreateDate_
    }

-- | Specifies whether the user is required to set a new password on next
-- sign-in.
loginProfile_passwordResetRequired :: Lens.Lens' LoginProfile (Prelude.Maybe Prelude.Bool)
loginProfile_passwordResetRequired = Lens.lens (\LoginProfile' {passwordResetRequired} -> passwordResetRequired) (\s@LoginProfile' {} a -> s {passwordResetRequired = a} :: LoginProfile)

-- | The name of the user, which can be used for signing in to the AWS
-- Management Console.
loginProfile_userName :: Lens.Lens' LoginProfile Prelude.Text
loginProfile_userName = Lens.lens (\LoginProfile' {userName} -> userName) (\s@LoginProfile' {} a -> s {userName = a} :: LoginProfile)

-- | The date when the password for the user was created.
loginProfile_createDate :: Lens.Lens' LoginProfile Prelude.UTCTime
loginProfile_createDate = Lens.lens (\LoginProfile' {createDate} -> createDate) (\s@LoginProfile' {} a -> s {createDate = a} :: LoginProfile) Prelude.. Prelude._Time

instance Prelude.FromXML LoginProfile where
  parseXML x =
    LoginProfile'
      Prelude.<$> (x Prelude..@? "PasswordResetRequired")
      Prelude.<*> (x Prelude..@ "UserName")
      Prelude.<*> (x Prelude..@ "CreateDate")

instance Prelude.Hashable LoginProfile

instance Prelude.NFData LoginProfile
