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
-- Module      : Amazonka.Chime.Types.User
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.Chime.Types.User where

import Amazonka.Chime.Types.AlexaForBusinessMetadata
import Amazonka.Chime.Types.InviteStatus
import Amazonka.Chime.Types.License
import Amazonka.Chime.Types.RegistrationStatus
import Amazonka.Chime.Types.UserType
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | The user on the Amazon Chime account.
--
-- /See:/ 'newUser' smart constructor.
data User = User'
  { -- | The Amazon Chime account ID.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Alexa for Business metadata.
    alexaForBusinessMetadata :: Prelude.Maybe AlexaForBusinessMetadata,
    -- | The display name of the user.
    displayName :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Date and time when the user is invited to the Amazon Chime account, in
    -- ISO 8601 format.
    invitedOn :: Prelude.Maybe Data.POSIX,
    -- | The license type for the user.
    licenseType :: Prelude.Maybe License,
    -- | The user\'s personal meeting PIN.
    personalPIN :: Prelude.Maybe Prelude.Text,
    -- | The primary email address of the user.
    primaryEmail :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | The primary phone number associated with the user.
    primaryProvisionedNumber :: Prelude.Maybe (Data.Sensitive Prelude.Text),
    -- | Date and time when the user is registered, in ISO 8601 format.
    registeredOn :: Prelude.Maybe Data.POSIX,
    -- | The user invite status.
    userInvitationStatus :: Prelude.Maybe InviteStatus,
    -- | The user registration status.
    userRegistrationStatus :: Prelude.Maybe RegistrationStatus,
    -- | The user type.
    userType :: Prelude.Maybe UserType,
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
-- 'accountId', 'user_accountId' - The Amazon Chime account ID.
--
-- 'alexaForBusinessMetadata', 'user_alexaForBusinessMetadata' - The Alexa for Business metadata.
--
-- 'displayName', 'user_displayName' - The display name of the user.
--
-- 'invitedOn', 'user_invitedOn' - Date and time when the user is invited to the Amazon Chime account, in
-- ISO 8601 format.
--
-- 'licenseType', 'user_licenseType' - The license type for the user.
--
-- 'personalPIN', 'user_personalPIN' - The user\'s personal meeting PIN.
--
-- 'primaryEmail', 'user_primaryEmail' - The primary email address of the user.
--
-- 'primaryProvisionedNumber', 'user_primaryProvisionedNumber' - The primary phone number associated with the user.
--
-- 'registeredOn', 'user_registeredOn' - Date and time when the user is registered, in ISO 8601 format.
--
-- 'userInvitationStatus', 'user_userInvitationStatus' - The user invite status.
--
-- 'userRegistrationStatus', 'user_userRegistrationStatus' - The user registration status.
--
-- 'userType', 'user_userType' - The user type.
--
-- 'userId', 'user_userId' - The user ID.
newUser ::
  -- | 'userId'
  Prelude.Text ->
  User
newUser pUserId_ =
  User'
    { accountId = Prelude.Nothing,
      alexaForBusinessMetadata = Prelude.Nothing,
      displayName = Prelude.Nothing,
      invitedOn = Prelude.Nothing,
      licenseType = Prelude.Nothing,
      personalPIN = Prelude.Nothing,
      primaryEmail = Prelude.Nothing,
      primaryProvisionedNumber = Prelude.Nothing,
      registeredOn = Prelude.Nothing,
      userInvitationStatus = Prelude.Nothing,
      userRegistrationStatus = Prelude.Nothing,
      userType = Prelude.Nothing,
      userId = pUserId_
    }

-- | The Amazon Chime account ID.
user_accountId :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_accountId = Lens.lens (\User' {accountId} -> accountId) (\s@User' {} a -> s {accountId = a} :: User)

-- | The Alexa for Business metadata.
user_alexaForBusinessMetadata :: Lens.Lens' User (Prelude.Maybe AlexaForBusinessMetadata)
user_alexaForBusinessMetadata = Lens.lens (\User' {alexaForBusinessMetadata} -> alexaForBusinessMetadata) (\s@User' {} a -> s {alexaForBusinessMetadata = a} :: User)

-- | The display name of the user.
user_displayName :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_displayName = Lens.lens (\User' {displayName} -> displayName) (\s@User' {} a -> s {displayName = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | Date and time when the user is invited to the Amazon Chime account, in
-- ISO 8601 format.
user_invitedOn :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_invitedOn = Lens.lens (\User' {invitedOn} -> invitedOn) (\s@User' {} a -> s {invitedOn = a} :: User) Prelude.. Lens.mapping Data._Time

-- | The license type for the user.
user_licenseType :: Lens.Lens' User (Prelude.Maybe License)
user_licenseType = Lens.lens (\User' {licenseType} -> licenseType) (\s@User' {} a -> s {licenseType = a} :: User)

-- | The user\'s personal meeting PIN.
user_personalPIN :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_personalPIN = Lens.lens (\User' {personalPIN} -> personalPIN) (\s@User' {} a -> s {personalPIN = a} :: User)

-- | The primary email address of the user.
user_primaryEmail :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_primaryEmail = Lens.lens (\User' {primaryEmail} -> primaryEmail) (\s@User' {} a -> s {primaryEmail = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | The primary phone number associated with the user.
user_primaryProvisionedNumber :: Lens.Lens' User (Prelude.Maybe Prelude.Text)
user_primaryProvisionedNumber = Lens.lens (\User' {primaryProvisionedNumber} -> primaryProvisionedNumber) (\s@User' {} a -> s {primaryProvisionedNumber = a} :: User) Prelude.. Lens.mapping Data._Sensitive

-- | Date and time when the user is registered, in ISO 8601 format.
user_registeredOn :: Lens.Lens' User (Prelude.Maybe Prelude.UTCTime)
user_registeredOn = Lens.lens (\User' {registeredOn} -> registeredOn) (\s@User' {} a -> s {registeredOn = a} :: User) Prelude.. Lens.mapping Data._Time

-- | The user invite status.
user_userInvitationStatus :: Lens.Lens' User (Prelude.Maybe InviteStatus)
user_userInvitationStatus = Lens.lens (\User' {userInvitationStatus} -> userInvitationStatus) (\s@User' {} a -> s {userInvitationStatus = a} :: User)

-- | The user registration status.
user_userRegistrationStatus :: Lens.Lens' User (Prelude.Maybe RegistrationStatus)
user_userRegistrationStatus = Lens.lens (\User' {userRegistrationStatus} -> userRegistrationStatus) (\s@User' {} a -> s {userRegistrationStatus = a} :: User)

-- | The user type.
user_userType :: Lens.Lens' User (Prelude.Maybe UserType)
user_userType = Lens.lens (\User' {userType} -> userType) (\s@User' {} a -> s {userType = a} :: User)

-- | The user ID.
user_userId :: Lens.Lens' User Prelude.Text
user_userId = Lens.lens (\User' {userId} -> userId) (\s@User' {} a -> s {userId = a} :: User)

instance Data.FromJSON User where
  parseJSON =
    Data.withObject
      "User"
      ( \x ->
          User'
            Prelude.<$> (x Data..:? "AccountId")
            Prelude.<*> (x Data..:? "AlexaForBusinessMetadata")
            Prelude.<*> (x Data..:? "DisplayName")
            Prelude.<*> (x Data..:? "InvitedOn")
            Prelude.<*> (x Data..:? "LicenseType")
            Prelude.<*> (x Data..:? "PersonalPIN")
            Prelude.<*> (x Data..:? "PrimaryEmail")
            Prelude.<*> (x Data..:? "PrimaryProvisionedNumber")
            Prelude.<*> (x Data..:? "RegisteredOn")
            Prelude.<*> (x Data..:? "UserInvitationStatus")
            Prelude.<*> (x Data..:? "UserRegistrationStatus")
            Prelude.<*> (x Data..:? "UserType")
            Prelude.<*> (x Data..: "UserId")
      )

instance Prelude.Hashable User where
  hashWithSalt _salt User' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` alexaForBusinessMetadata
      `Prelude.hashWithSalt` displayName
      `Prelude.hashWithSalt` invitedOn
      `Prelude.hashWithSalt` licenseType
      `Prelude.hashWithSalt` personalPIN
      `Prelude.hashWithSalt` primaryEmail
      `Prelude.hashWithSalt` primaryProvisionedNumber
      `Prelude.hashWithSalt` registeredOn
      `Prelude.hashWithSalt` userInvitationStatus
      `Prelude.hashWithSalt` userRegistrationStatus
      `Prelude.hashWithSalt` userType
      `Prelude.hashWithSalt` userId

instance Prelude.NFData User where
  rnf User' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf alexaForBusinessMetadata
      `Prelude.seq` Prelude.rnf displayName
      `Prelude.seq` Prelude.rnf invitedOn
      `Prelude.seq` Prelude.rnf licenseType
      `Prelude.seq` Prelude.rnf personalPIN
      `Prelude.seq` Prelude.rnf primaryEmail
      `Prelude.seq` Prelude.rnf primaryProvisionedNumber
      `Prelude.seq` Prelude.rnf registeredOn
      `Prelude.seq` Prelude.rnf userInvitationStatus
      `Prelude.seq` Prelude.rnf userRegistrationStatus
      `Prelude.seq` Prelude.rnf userType
      `Prelude.seq` Prelude.rnf userId
