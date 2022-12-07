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
-- Module      : Amazonka.IAM.Types.PasswordPolicy
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.IAM.Types.PasswordPolicy where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Contains information about the account password policy.
--
-- This data type is used as a response element in the
-- GetAccountPasswordPolicy operation.
--
-- /See:/ 'newPasswordPolicy' smart constructor.
data PasswordPolicy = PasswordPolicy'
  { -- | The number of days that an IAM user password is valid.
    maxPasswordAge :: Prelude.Maybe Prelude.Natural,
    -- | Minimum length to require for IAM user passwords.
    minimumPasswordLength :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether IAM users are allowed to change their own password.
    -- Gives IAM users permissions to @iam:ChangePassword@ for only their user
    -- and to the @iam:GetAccountPasswordPolicy@ action. This option does not
    -- attach a permissions policy to each user, rather the permissions are
    -- applied at the account-level for all users by IAM.
    allowUsersToChangePassword :: Prelude.Maybe Prelude.Bool,
    -- | Specifies the number of previous passwords that IAM users are prevented
    -- from reusing.
    passwordReusePrevention :: Prelude.Maybe Prelude.Natural,
    -- | Specifies whether IAM user passwords must contain at least one numeric
    -- character (0 to 9).
    requireNumbers :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one lowercase
    -- character (a to z).
    requireLowercaseCharacters :: Prelude.Maybe Prelude.Bool,
    -- | Indicates whether passwords in the account expire. Returns true if
    -- @MaxPasswordAge@ contains a value greater than 0. Returns false if
    -- MaxPasswordAge is 0 or not present.
    expirePasswords :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one of the
    -- following symbols:
    --
    -- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
    requireSymbols :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one uppercase
    -- character (A to Z).
    requireUppercaseCharacters :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM users are prevented from setting a new password
    -- via the Amazon Web Services Management Console after their password has
    -- expired. The IAM user cannot access the console until an administrator
    -- resets the password. IAM users with @iam:ChangePassword@ permission and
    -- active access keys can reset their own expired console password using
    -- the CLI or API.
    hardExpiry :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PasswordPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'maxPasswordAge', 'passwordPolicy_maxPasswordAge' - The number of days that an IAM user password is valid.
--
-- 'minimumPasswordLength', 'passwordPolicy_minimumPasswordLength' - Minimum length to require for IAM user passwords.
--
-- 'allowUsersToChangePassword', 'passwordPolicy_allowUsersToChangePassword' - Specifies whether IAM users are allowed to change their own password.
-- Gives IAM users permissions to @iam:ChangePassword@ for only their user
-- and to the @iam:GetAccountPasswordPolicy@ action. This option does not
-- attach a permissions policy to each user, rather the permissions are
-- applied at the account-level for all users by IAM.
--
-- 'passwordReusePrevention', 'passwordPolicy_passwordReusePrevention' - Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
--
-- 'requireNumbers', 'passwordPolicy_requireNumbers' - Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
--
-- 'requireLowercaseCharacters', 'passwordPolicy_requireLowercaseCharacters' - Specifies whether IAM user passwords must contain at least one lowercase
-- character (a to z).
--
-- 'expirePasswords', 'passwordPolicy_expirePasswords' - Indicates whether passwords in the account expire. Returns true if
-- @MaxPasswordAge@ contains a value greater than 0. Returns false if
-- MaxPasswordAge is 0 or not present.
--
-- 'requireSymbols', 'passwordPolicy_requireSymbols' - Specifies whether IAM user passwords must contain at least one of the
-- following symbols:
--
-- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
--
-- 'requireUppercaseCharacters', 'passwordPolicy_requireUppercaseCharacters' - Specifies whether IAM user passwords must contain at least one uppercase
-- character (A to Z).
--
-- 'hardExpiry', 'passwordPolicy_hardExpiry' - Specifies whether IAM users are prevented from setting a new password
-- via the Amazon Web Services Management Console after their password has
-- expired. The IAM user cannot access the console until an administrator
-- resets the password. IAM users with @iam:ChangePassword@ permission and
-- active access keys can reset their own expired console password using
-- the CLI or API.
newPasswordPolicy ::
  PasswordPolicy
newPasswordPolicy =
  PasswordPolicy'
    { maxPasswordAge = Prelude.Nothing,
      minimumPasswordLength = Prelude.Nothing,
      allowUsersToChangePassword = Prelude.Nothing,
      passwordReusePrevention = Prelude.Nothing,
      requireNumbers = Prelude.Nothing,
      requireLowercaseCharacters = Prelude.Nothing,
      expirePasswords = Prelude.Nothing,
      requireSymbols = Prelude.Nothing,
      requireUppercaseCharacters = Prelude.Nothing,
      hardExpiry = Prelude.Nothing
    }

-- | The number of days that an IAM user password is valid.
passwordPolicy_maxPasswordAge :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Natural)
passwordPolicy_maxPasswordAge = Lens.lens (\PasswordPolicy' {maxPasswordAge} -> maxPasswordAge) (\s@PasswordPolicy' {} a -> s {maxPasswordAge = a} :: PasswordPolicy)

-- | Minimum length to require for IAM user passwords.
passwordPolicy_minimumPasswordLength :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Natural)
passwordPolicy_minimumPasswordLength = Lens.lens (\PasswordPolicy' {minimumPasswordLength} -> minimumPasswordLength) (\s@PasswordPolicy' {} a -> s {minimumPasswordLength = a} :: PasswordPolicy)

-- | Specifies whether IAM users are allowed to change their own password.
-- Gives IAM users permissions to @iam:ChangePassword@ for only their user
-- and to the @iam:GetAccountPasswordPolicy@ action. This option does not
-- attach a permissions policy to each user, rather the permissions are
-- applied at the account-level for all users by IAM.
passwordPolicy_allowUsersToChangePassword :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_allowUsersToChangePassword = Lens.lens (\PasswordPolicy' {allowUsersToChangePassword} -> allowUsersToChangePassword) (\s@PasswordPolicy' {} a -> s {allowUsersToChangePassword = a} :: PasswordPolicy)

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
passwordPolicy_passwordReusePrevention :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Natural)
passwordPolicy_passwordReusePrevention = Lens.lens (\PasswordPolicy' {passwordReusePrevention} -> passwordReusePrevention) (\s@PasswordPolicy' {} a -> s {passwordReusePrevention = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
passwordPolicy_requireNumbers :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_requireNumbers = Lens.lens (\PasswordPolicy' {requireNumbers} -> requireNumbers) (\s@PasswordPolicy' {} a -> s {requireNumbers = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character (a to z).
passwordPolicy_requireLowercaseCharacters :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_requireLowercaseCharacters = Lens.lens (\PasswordPolicy' {requireLowercaseCharacters} -> requireLowercaseCharacters) (\s@PasswordPolicy' {} a -> s {requireLowercaseCharacters = a} :: PasswordPolicy)

-- | Indicates whether passwords in the account expire. Returns true if
-- @MaxPasswordAge@ contains a value greater than 0. Returns false if
-- MaxPasswordAge is 0 or not present.
passwordPolicy_expirePasswords :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_expirePasswords = Lens.lens (\PasswordPolicy' {expirePasswords} -> expirePasswords) (\s@PasswordPolicy' {} a -> s {expirePasswords = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one of the
-- following symbols:
--
-- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
passwordPolicy_requireSymbols :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_requireSymbols = Lens.lens (\PasswordPolicy' {requireSymbols} -> requireSymbols) (\s@PasswordPolicy' {} a -> s {requireSymbols = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one uppercase
-- character (A to Z).
passwordPolicy_requireUppercaseCharacters :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_requireUppercaseCharacters = Lens.lens (\PasswordPolicy' {requireUppercaseCharacters} -> requireUppercaseCharacters) (\s@PasswordPolicy' {} a -> s {requireUppercaseCharacters = a} :: PasswordPolicy)

-- | Specifies whether IAM users are prevented from setting a new password
-- via the Amazon Web Services Management Console after their password has
-- expired. The IAM user cannot access the console until an administrator
-- resets the password. IAM users with @iam:ChangePassword@ permission and
-- active access keys can reset their own expired console password using
-- the CLI or API.
passwordPolicy_hardExpiry :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_hardExpiry = Lens.lens (\PasswordPolicy' {hardExpiry} -> hardExpiry) (\s@PasswordPolicy' {} a -> s {hardExpiry = a} :: PasswordPolicy)

instance Data.FromXML PasswordPolicy where
  parseXML x =
    PasswordPolicy'
      Prelude.<$> (x Data..@? "MaxPasswordAge")
      Prelude.<*> (x Data..@? "MinimumPasswordLength")
      Prelude.<*> (x Data..@? "AllowUsersToChangePassword")
      Prelude.<*> (x Data..@? "PasswordReusePrevention")
      Prelude.<*> (x Data..@? "RequireNumbers")
      Prelude.<*> (x Data..@? "RequireLowercaseCharacters")
      Prelude.<*> (x Data..@? "ExpirePasswords")
      Prelude.<*> (x Data..@? "RequireSymbols")
      Prelude.<*> (x Data..@? "RequireUppercaseCharacters")
      Prelude.<*> (x Data..@? "HardExpiry")

instance Prelude.Hashable PasswordPolicy where
  hashWithSalt _salt PasswordPolicy' {..} =
    _salt `Prelude.hashWithSalt` maxPasswordAge
      `Prelude.hashWithSalt` minimumPasswordLength
      `Prelude.hashWithSalt` allowUsersToChangePassword
      `Prelude.hashWithSalt` passwordReusePrevention
      `Prelude.hashWithSalt` requireNumbers
      `Prelude.hashWithSalt` requireLowercaseCharacters
      `Prelude.hashWithSalt` expirePasswords
      `Prelude.hashWithSalt` requireSymbols
      `Prelude.hashWithSalt` requireUppercaseCharacters
      `Prelude.hashWithSalt` hardExpiry

instance Prelude.NFData PasswordPolicy where
  rnf PasswordPolicy' {..} =
    Prelude.rnf maxPasswordAge
      `Prelude.seq` Prelude.rnf minimumPasswordLength
      `Prelude.seq` Prelude.rnf allowUsersToChangePassword
      `Prelude.seq` Prelude.rnf passwordReusePrevention
      `Prelude.seq` Prelude.rnf requireNumbers
      `Prelude.seq` Prelude.rnf requireLowercaseCharacters
      `Prelude.seq` Prelude.rnf expirePasswords
      `Prelude.seq` Prelude.rnf requireSymbols
      `Prelude.seq` Prelude.rnf requireUppercaseCharacters
      `Prelude.seq` Prelude.rnf hardExpiry
