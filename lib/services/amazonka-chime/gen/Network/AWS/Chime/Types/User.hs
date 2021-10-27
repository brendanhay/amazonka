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
-- Module      : Network.AWS.Chime.Types.User
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Chime.Types.User where

import Network.AWS.Chime.Types.AlexaForBusinessMetadata
import Network.AWS.Chime.Types.InviteStatus
import Network.AWS.Chime.Types.License
import Network.AWS.Chime.Types.RegistrationStatus
import Network.AWS.Chime.Types.UserType
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | The user on the Amazon Chime account.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The user invite status.
    userInvitationStatus :: Prelude.Maybe InviteStatus,
    -- | The user\'s personal meeting PIN.
    personalPIN :: Prelude.Maybe Prelude.Text,
    -- | The primary phone number associated with the user.
    primaryProvisionedNumber :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The license type for the user.
    licenseType :: Prelude.Maybe License,
    -- | Date and time when the user is registered, in ISO 8601 format.
    registeredOn :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Chime account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The user registration status.
    userRegistrationStatus :: Prelude.Maybe RegistrationStatus,
    -- | Date and time when the user is invited to the Amazon Chime account, in
    -- ISO 8601 format.
    invitedOn :: Prelude.Maybe Core.POSIX,
    -- | The display name of the user.
    displayName :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The primary email address of the user.
    primaryEmail :: Prelude.Maybe (Core.Sensitive Prelude.Text),
    -- | The user type.
    userType :: Prelude.Maybe UserType,
    -- | The Alexa for Business metadata.
    alexaForBusinessMetadata :: Prelude.Maybe AlexaForBusinessMetadata,
    -- | The user ID.
    userId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'User' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'userInvitationStatus', 'user_userInvitationStatus' - The user invite status.
--
-- 'personalPIN', 'user_personalPIN' - The user\'s personal meeting PIN.
--
-- 'primaryProvisionedNumber', 'user_primaryProvisionedNumber' - The primary phone number associated with the user.
--
-- 'licenseType', 'user_licenseType' - The license type for the user.
--
-- 'registeredOn', 'user_registeredOn' - Date and time when the user is registered, in ISO 8601 format.
--
-- 'accountId', 'user_accountId' - The Amazon Chime account ID.
--
-- 'userRegistrationStatus', 'user_userRegistrationStatus' - The user registration status.
--
-- 'invitedOn', 'user_invitedOn' - Date and time when the user is invited to the Amazon Chime account, in
-- ISO 8601 format.
--
-- 'displayName', 'user_displayName' - The display name of the user.
--
-- 'primaryEmail', 'user_primaryEmail' - The primary email address of the user.
--
-- 'userType', 'user_userType' - The user type.
--
-- 'alexaForBusinessMetadata', 'user_alexaForBusinessMetadata' - The Alexa for Business metadata.
--
-- 'userId', 'user_userId' - The user ID.
newUser ::
  -- | 'userId'
  Prelude.Text ->
  User
newUser pUserId_ =
  User'
    { userInvitationStatus = Prelude.Nothing,
      personalPIN = Prelude.Nothing,
      primaryProvisionedNumber = Prelude.Nothing,
      licenseType = Prelude.Nothing,
      registeredOn = Prelude.Nothing,
      accountId = Prelude.Nothing,
      userRegistrationStatus = Prelude.Nothing,
      invitedOn = Prelude.Nothing,
      displayName = Prelude.Nothing,
      primaryEmail = Prelude.Nothing,
      userType = Prelude.Nothing,
      alexaForBusinessMetadata = Prelude.Nothing,
      userId = pUserId_
    }

-- | The user invite status.
user_userInvitationStatus :: Lens.Lens' User (Prelude.Maybe InviteStatus)
user_userInvitationStatus = Lens.lens (\User' {userInvitationStatus} -> userInvitationStatus) (\s@User' {} a -> s {userInvitationStatus = a} :: User)

-- | The user\'s personal meeting PIN.
user_personalPIN :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_personalPIN = Lens.lens (\User' {personalPIN} -> personalPIN) (\s@User' {} a -> s {personalPIN = a} :: User)

-- | The primary phone number associated with the user.
user_primaryProvisionedNumber :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_primaryProvisionedNumber = Lens.lens (\User' {primaryProvisionedNumber} -> primaryProvisionedNumber) (\s@User' {} a -> s {primaryProvisionedNumber = a} :: User) Prelude.. Lens.mapping Core._Sensitive

-- | The license type for the user.
user_licenseType :: Lens.Lens' User (Prelude.Maybe License)
user_licenseType = Lens.lens (\User' {licenseType} -> licenseType) (\s@User' {} a -> s {licenseType = a} :: User)

-- | Date and time when the user is registered, in ISO 8601 format.
user_registeredOn :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_registeredOn = Lens.lens (\User' {registeredOn} -> registeredOn) (\s@User' {} a -> s {registeredOn = a} :: User) Prelude.. Lens.mapping Core._Time

-- | The Amazon Chime account ID.
user_accountId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_accountId = Lens.lens (\User' {accountId} -> accountId) (\s@User' {} a -> s {accountId = a} :: User)

-- | The user registration status.
user_userRegistrationStatus :: Lens.Lens' User (Prelude.Maybe RegistrationStatus)
user_userRegistrationStatus = Lens.lens (\User' {userRegistrationStatus} -> userRegistrationStatus) (\s@User' {} a -> s {userRegistrationStatus = a} :: User)

-- | Date and time when the user is invited to the Amazon Chime account, in
-- ISO 8601 format.
user_invitedOn :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_invitedOn = Lens.lens (\User' {invitedOn} -> invitedOn) (\s@User' {} a -> s {invitedOn = a} :: User) Prelude.. Lens.mapping Core._Time

-- | The display name of the user.
user_displayName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_displayName = Lens.lens (\User' {displayName} -> displayName) (\s@User' {} a -> s {displayName = a} :: User) Prelude.. Lens.mapping Core._Sensitive

-- | The primary email address of the user.
user_primaryEmail :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_primaryEmail = Lens.lens (\User' {primaryEmail} -> primaryEmail) (\s@User' {} a -> s {primaryEmail = a} :: User) Prelude.. Lens.mapping Core._Sensitive

-- | The user type.
user_userType :: Lens.Lens' User (Prelude.Maybe UserType)
user_userType = Lens.lens (\User' {userType} -> userType) (\s@User' {} a -> s {userType = a} :: User)

-- | The Alexa for Business metadata.
user_alexaForBusinessMetadata :: Lens.Lens' User (Prelude.Maybe AlexaForBusinessMetadata)
user_alexaForBusinessMetadata = Lens.lens (\User' {alexaForBusinessMetadata} -> alexaForBusinessMetadata) (\s@User' {} a -> s {alexaForBusinessMetadata = a} :: User)

-- | The user ID.
user_userId :: Lens.Lens' User Prelude.Text
user_userId = Lens.lens (\User' {userId} -> userId) (\s@User' {} a -> s {userId = a} :: User)

instance Core.FromJSON User where
  parseJSON =
    Core.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Core..:? "UserInvitationStatus")
            Prelude.<*> (x Core..:? "PersonalPIN")
            Prelude.<*> (x Core..:? "PrimaryProvisionedNumber")
            Prelude.<*> (x Core..:? "LicenseType")
            Prelude.<*> (x Core..:? "RegisteredOn")
            Prelude.<*> (x Core..:? "AccountId")
            Prelude.<*> (x Core..:? "UserRegistrationStatus")
            Prelude.<*> (x Core..:? "InvitedOn")
            Prelude.<*> (x Core..:? "DisplayName")
            Prelude.<*> (x Core..:? "PrimaryEmail")
            Prelude.<*> (x Core..:? "UserType")
            Prelude.<*> (x Core..:? "AlexaForBusinessMetadata")
            Prelude.<*> (x Core..: "UserId")
      )

instance Prelude.Hashable User

instance Prelude.NFData User
