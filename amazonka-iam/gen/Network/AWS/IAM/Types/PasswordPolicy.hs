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
-- Module      : Network.AWS.IAM.Types.PasswordPolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PasswordPolicy where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Contains information about the account password policy.
--
-- This data type is used as a response element in the
-- GetAccountPasswordPolicy operation.
--
-- /See:/ 'newPasswordPolicy' smart constructor.
data PasswordPolicy = PasswordPolicy'
  { -- | Specifies whether IAM user passwords must contain at least one lowercase
    -- character (a to z).
    requireLowercaseCharacters :: Prelude.Maybe Prelude.Bool,
    -- | The number of days that an IAM user password is valid.
    maxPasswordAge :: Prelude.Maybe Prelude.Natural,
    -- | Specifies the number of previous passwords that IAM users are prevented
    -- from reusing.
    passwordReusePrevention :: Prelude.Maybe Prelude.Natural,
    -- | Minimum length to require for IAM user passwords.
    minimumPasswordLength :: Prelude.Maybe Prelude.Natural,
    -- | Indicates whether passwords in the account expire. Returns true if
    -- @MaxPasswordAge@ contains a value greater than 0. Returns false if
    -- MaxPasswordAge is 0 or not present.
    expirePasswords :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one uppercase
    -- character (A to Z).
    requireUppercaseCharacters :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM users are allowed to change their own password.
    allowUsersToChangePassword :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM users are prevented from setting a new password
    -- after their password has expired.
    hardExpiry :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one of the
    -- following symbols:
    --
    -- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
    requireSymbols :: Prelude.Maybe Prelude.Bool,
    -- | Specifies whether IAM user passwords must contain at least one numeric
    -- character (0 to 9).
    requireNumbers :: Prelude.Maybe Prelude.Bool
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
-- 'requireLowercaseCharacters', 'passwordPolicy_requireLowercaseCharacters' - Specifies whether IAM user passwords must contain at least one lowercase
-- character (a to z).
--
-- 'maxPasswordAge', 'passwordPolicy_maxPasswordAge' - The number of days that an IAM user password is valid.
--
-- 'passwordReusePrevention', 'passwordPolicy_passwordReusePrevention' - Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
--
-- 'minimumPasswordLength', 'passwordPolicy_minimumPasswordLength' - Minimum length to require for IAM user passwords.
--
-- 'expirePasswords', 'passwordPolicy_expirePasswords' - Indicates whether passwords in the account expire. Returns true if
-- @MaxPasswordAge@ contains a value greater than 0. Returns false if
-- MaxPasswordAge is 0 or not present.
--
-- 'requireUppercaseCharacters', 'passwordPolicy_requireUppercaseCharacters' - Specifies whether IAM user passwords must contain at least one uppercase
-- character (A to Z).
--
-- 'allowUsersToChangePassword', 'passwordPolicy_allowUsersToChangePassword' - Specifies whether IAM users are allowed to change their own password.
--
-- 'hardExpiry', 'passwordPolicy_hardExpiry' - Specifies whether IAM users are prevented from setting a new password
-- after their password has expired.
--
-- 'requireSymbols', 'passwordPolicy_requireSymbols' - Specifies whether IAM user passwords must contain at least one of the
-- following symbols:
--
-- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
--
-- 'requireNumbers', 'passwordPolicy_requireNumbers' - Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
newPasswordPolicy ::
  PasswordPolicy
newPasswordPolicy =
  PasswordPolicy'
    { requireLowercaseCharacters =
        Prelude.Nothing,
      maxPasswordAge = Prelude.Nothing,
      passwordReusePrevention = Prelude.Nothing,
      minimumPasswordLength = Prelude.Nothing,
      expirePasswords = Prelude.Nothing,
      requireUppercaseCharacters = Prelude.Nothing,
      allowUsersToChangePassword = Prelude.Nothing,
      hardExpiry = Prelude.Nothing,
      requireSymbols = Prelude.Nothing,
      requireNumbers = Prelude.Nothing
    }

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character (a to z).
passwordPolicy_requireLowercaseCharacters :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_requireLowercaseCharacters = Lens.lens (\PasswordPolicy' {requireLowercaseCharacters} -> requireLowercaseCharacters) (\s@PasswordPolicy' {} a -> s {requireLowercaseCharacters = a} :: PasswordPolicy)

-- | The number of days that an IAM user password is valid.
passwordPolicy_maxPasswordAge :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Natural)
passwordPolicy_maxPasswordAge = Lens.lens (\PasswordPolicy' {maxPasswordAge} -> maxPasswordAge) (\s@PasswordPolicy' {} a -> s {maxPasswordAge = a} :: PasswordPolicy)

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
passwordPolicy_passwordReusePrevention :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Natural)
passwordPolicy_passwordReusePrevention = Lens.lens (\PasswordPolicy' {passwordReusePrevention} -> passwordReusePrevention) (\s@PasswordPolicy' {} a -> s {passwordReusePrevention = a} :: PasswordPolicy)

-- | Minimum length to require for IAM user passwords.
passwordPolicy_minimumPasswordLength :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Natural)
passwordPolicy_minimumPasswordLength = Lens.lens (\PasswordPolicy' {minimumPasswordLength} -> minimumPasswordLength) (\s@PasswordPolicy' {} a -> s {minimumPasswordLength = a} :: PasswordPolicy)

-- | Indicates whether passwords in the account expire. Returns true if
-- @MaxPasswordAge@ contains a value greater than 0. Returns false if
-- MaxPasswordAge is 0 or not present.
passwordPolicy_expirePasswords :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_expirePasswords = Lens.lens (\PasswordPolicy' {expirePasswords} -> expirePasswords) (\s@PasswordPolicy' {} a -> s {expirePasswords = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one uppercase
-- character (A to Z).
passwordPolicy_requireUppercaseCharacters :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_requireUppercaseCharacters = Lens.lens (\PasswordPolicy' {requireUppercaseCharacters} -> requireUppercaseCharacters) (\s@PasswordPolicy' {} a -> s {requireUppercaseCharacters = a} :: PasswordPolicy)

-- | Specifies whether IAM users are allowed to change their own password.
passwordPolicy_allowUsersToChangePassword :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_allowUsersToChangePassword = Lens.lens (\PasswordPolicy' {allowUsersToChangePassword} -> allowUsersToChangePassword) (\s@PasswordPolicy' {} a -> s {allowUsersToChangePassword = a} :: PasswordPolicy)

-- | Specifies whether IAM users are prevented from setting a new password
-- after their password has expired.
passwordPolicy_hardExpiry :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_hardExpiry = Lens.lens (\PasswordPolicy' {hardExpiry} -> hardExpiry) (\s@PasswordPolicy' {} a -> s {hardExpiry = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one of the
-- following symbols:
--
-- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
passwordPolicy_requireSymbols :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_requireSymbols = Lens.lens (\PasswordPolicy' {requireSymbols} -> requireSymbols) (\s@PasswordPolicy' {} a -> s {requireSymbols = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
passwordPolicy_requireNumbers :: Lens.Lens' PasswordPolicy (Prelude.Maybe Prelude.Bool)
passwordPolicy_requireNumbers = Lens.lens (\PasswordPolicy' {requireNumbers} -> requireNumbers) (\s@PasswordPolicy' {} a -> s {requireNumbers = a} :: PasswordPolicy)

instance Core.FromXML PasswordPolicy where
  parseXML x =
    PasswordPolicy'
      Prelude.<$> (x Core..@? "RequireLowercaseCharacters")
      Prelude.<*> (x Core..@? "MaxPasswordAge")
      Prelude.<*> (x Core..@? "PasswordReusePrevention")
      Prelude.<*> (x Core..@? "MinimumPasswordLength")
      Prelude.<*> (x Core..@? "ExpirePasswords")
      Prelude.<*> (x Core..@? "RequireUppercaseCharacters")
      Prelude.<*> (x Core..@? "AllowUsersToChangePassword")
      Prelude.<*> (x Core..@? "HardExpiry")
      Prelude.<*> (x Core..@? "RequireSymbols")
      Prelude.<*> (x Core..@? "RequireNumbers")

instance Prelude.Hashable PasswordPolicy

instance Prelude.NFData PasswordPolicy
