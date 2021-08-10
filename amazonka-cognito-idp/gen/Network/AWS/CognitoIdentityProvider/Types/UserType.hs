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
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserType
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserType where

import Network.AWS.CognitoIdentityProvider.Types.AttributeType
import Network.AWS.CognitoIdentityProvider.Types.MFAOptionType
import Network.AWS.CognitoIdentityProvider.Types.UserStatusType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The user type.
--
-- /See:/ 'newUserType' smart constructor.
data UserType = UserType'
  { -- | The creation date of the user.
    userCreateDate :: Prelude.Maybe Core.POSIX,
    -- | The last modified date of the user.
    userLastModifiedDate :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether the user is enabled.
    enabled :: Prelude.Maybe Prelude.Bool,
    -- | A container with information about the user type attributes.
    attributes :: Prelude.Maybe [AttributeType],
    -- | The user name of the user you wish to describe.
    username :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The user status. Can be one of the following:
    --
    -- -   UNCONFIRMED - User has been created but not confirmed.
    --
    -- -   CONFIRMED - User has been confirmed.
    --
    -- -   ARCHIVED - User is no longer active.
    --
    -- -   COMPROMISED - User is disabled due to a potential security threat.
    --
    -- -   UNKNOWN - User status is not known.
    --
    -- -   RESET_REQUIRED - User is confirmed, but the user must request a code
    --     and reset his or her password before he or she can sign in.
    --
    -- -   FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign
    --     in using a temporary password, but on first sign-in, the user must
    --     change his or her password to a new value before doing anything
    --     else.
    userStatus :: Prelude.Maybe UserStatusType,
    -- | The MFA options for the user.
    mfaOptions :: Prelude.Maybe [MFAOptionType]
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UserType' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userCreateDate', 'userType_userCreateDate' - The creation date of the user.
--
-- 'userLastModifiedDate', 'userType_userLastModifiedDate' - The last modified date of the user.
--
-- 'enabled', 'userType_enabled' - Specifies whether the user is enabled.
--
-- 'attributes', 'userType_attributes' - A container with information about the user type attributes.
--
-- 'username', 'userType_username' - The user name of the user you wish to describe.
--
-- 'userStatus', 'userType_userStatus' - The user status. Can be one of the following:
--
-- -   UNCONFIRMED - User has been created but not confirmed.
--
-- -   CONFIRMED - User has been confirmed.
--
-- -   ARCHIVED - User is no longer active.
--
-- -   COMPROMISED - User is disabled due to a potential security threat.
--
-- -   UNKNOWN - User status is not known.
--
-- -   RESET_REQUIRED - User is confirmed, but the user must request a code
--     and reset his or her password before he or she can sign in.
--
-- -   FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign
--     in using a temporary password, but on first sign-in, the user must
--     change his or her password to a new value before doing anything
--     else.
--
-- 'mfaOptions', 'userType_mfaOptions' - The MFA options for the user.
newUserType ::
  UserType
newUserType =
  UserType'
    { userCreateDate = Prelude.Nothing,
      userLastModifiedDate = Prelude.Nothing,
      enabled = Prelude.Nothing,
      attributes = Prelude.Nothing,
      username = Prelude.Nothing,
      userStatus = Prelude.Nothing,
      mfaOptions = Prelude.Nothing
    }

-- | The creation date of the user.
userType_userCreateDate :: Lens.Lens' UserType (Prelude.Maybe Prelude.UTCTime)
userType_userCreateDate = Lens.lens (\UserType' {userCreateDate} -> userCreateDate) (\s@UserType' {} a -> s {userCreateDate = a} :: UserType) Prelude.. Lens.mapping Core._Time

-- | The last modified date of the user.
userType_userLastModifiedDate :: Lens.Lens' UserType (Prelude.Maybe Prelude.UTCTime)
userType_userLastModifiedDate = Lens.lens (\UserType' {userLastModifiedDate} -> userLastModifiedDate) (\s@UserType' {} a -> s {userLastModifiedDate = a} :: UserType) Prelude.. Lens.mapping Core._Time

-- | Specifies whether the user is enabled.
userType_enabled :: Lens.Lens' UserType (Prelude.Maybe Prelude.Bool)
userType_enabled = Lens.lens (\UserType' {enabled} -> enabled) (\s@UserType' {} a -> s {enabled = a} :: UserType)

-- | A container with information about the user type attributes.
userType_attributes :: Lens.Lens' UserType (Prelude.Maybe [AttributeType])
userType_attributes = Lens.lens (\UserType' {attributes} -> attributes) (\s@UserType' {} a -> s {attributes = a} :: UserType) Prelude.. Lens.mapping Lens._Coerce

-- | The user name of the user you wish to describe.
userType_username :: Lens.Lens' UserType (Prelude.Maybe Prelude.Text)
userType_username = Lens.lens (\UserType' {username} -> username) (\s@UserType' {} a -> s {username = a} :: UserType) Prelude.. Lens.mapping Core._Sensitive

-- | The user status. Can be one of the following:
--
-- -   UNCONFIRMED - User has been created but not confirmed.
--
-- -   CONFIRMED - User has been confirmed.
--
-- -   ARCHIVED - User is no longer active.
--
-- -   COMPROMISED - User is disabled due to a potential security threat.
--
-- -   UNKNOWN - User status is not known.
--
-- -   RESET_REQUIRED - User is confirmed, but the user must request a code
--     and reset his or her password before he or she can sign in.
--
-- -   FORCE_CHANGE_PASSWORD - The user is confirmed and the user can sign
--     in using a temporary password, but on first sign-in, the user must
--     change his or her password to a new value before doing anything
--     else.
userType_userStatus :: Lens.Lens' UserType (Prelude.Maybe UserStatusType)
userType_userStatus = Lens.lens (\UserType' {userStatus} -> userStatus) (\s@UserType' {} a -> s {userStatus = a} :: UserType)

-- | The MFA options for the user.
userType_mfaOptions :: Lens.Lens' UserType (Prelude.Maybe [MFAOptionType])
userType_mfaOptions = Lens.lens (\UserType' {mfaOptions} -> mfaOptions) (\s@UserType' {} a -> s {mfaOptions = a} :: UserType) Prelude.. Lens.mapping Lens._Coerce

instance Core.FromJSON UserType where
  parseJSON =
    Core.withObject
      "UserType"
      ( \x ->
          UserType'
            Prelude.<$> (x Core..:? "UserCreateDate")
            Prelude.<*> (x Core..:? "UserLastModifiedDate")
            Prelude.<*> (x Core..:? "Enabled")
            Prelude.<*> (x Core..:? "Attributes" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "Username")
            Prelude.<*> (x Core..:? "UserStatus")
            Prelude.<*> (x Core..:? "MFAOptions" Core..!= Prelude.mempty)
      )

instance Prelude.Hashable UserType

instance Prelude.NFData UserType
