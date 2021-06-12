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

-- | Contains information about the account password policy.
--
-- This data type is used as a response element in the
-- GetAccountPasswordPolicy operation.
--
-- /See:/ 'newPasswordPolicy' smart constructor.
data PasswordPolicy = PasswordPolicy'
  { -- | The number of days that an IAM user password is valid.
    maxPasswordAge :: Core.Maybe Core.Natural,
    -- | Specifies whether IAM user passwords must contain at least one lowercase
    -- character (a to z).
    requireLowercaseCharacters :: Core.Maybe Core.Bool,
    -- | Minimum length to require for IAM user passwords.
    minimumPasswordLength :: Core.Maybe Core.Natural,
    -- | Specifies the number of previous passwords that IAM users are prevented
    -- from reusing.
    passwordReusePrevention :: Core.Maybe Core.Natural,
    -- | Indicates whether passwords in the account expire. Returns true if
    -- @MaxPasswordAge@ contains a value greater than 0. Returns false if
    -- MaxPasswordAge is 0 or not present.
    expirePasswords :: Core.Maybe Core.Bool,
    -- | Specifies whether IAM user passwords must contain at least one uppercase
    -- character (A to Z).
    requireUppercaseCharacters :: Core.Maybe Core.Bool,
    -- | Specifies whether IAM users are allowed to change their own password.
    allowUsersToChangePassword :: Core.Maybe Core.Bool,
    -- | Specifies whether IAM users are prevented from setting a new password
    -- after their password has expired.
    hardExpiry :: Core.Maybe Core.Bool,
    -- | Specifies whether IAM user passwords must contain at least one of the
    -- following symbols:
    --
    -- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
    requireSymbols :: Core.Maybe Core.Bool,
    -- | Specifies whether IAM user passwords must contain at least one numeric
    -- character (0 to 9).
    requireNumbers :: Core.Maybe Core.Bool
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- 'requireLowercaseCharacters', 'passwordPolicy_requireLowercaseCharacters' - Specifies whether IAM user passwords must contain at least one lowercase
-- character (a to z).
--
-- 'minimumPasswordLength', 'passwordPolicy_minimumPasswordLength' - Minimum length to require for IAM user passwords.
--
-- 'passwordReusePrevention', 'passwordPolicy_passwordReusePrevention' - Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
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
    { maxPasswordAge = Core.Nothing,
      requireLowercaseCharacters = Core.Nothing,
      minimumPasswordLength = Core.Nothing,
      passwordReusePrevention = Core.Nothing,
      expirePasswords = Core.Nothing,
      requireUppercaseCharacters = Core.Nothing,
      allowUsersToChangePassword = Core.Nothing,
      hardExpiry = Core.Nothing,
      requireSymbols = Core.Nothing,
      requireNumbers = Core.Nothing
    }

-- | The number of days that an IAM user password is valid.
passwordPolicy_maxPasswordAge :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Natural)
passwordPolicy_maxPasswordAge = Lens.lens (\PasswordPolicy' {maxPasswordAge} -> maxPasswordAge) (\s@PasswordPolicy' {} a -> s {maxPasswordAge = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one lowercase
-- character (a to z).
passwordPolicy_requireLowercaseCharacters :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
passwordPolicy_requireLowercaseCharacters = Lens.lens (\PasswordPolicy' {requireLowercaseCharacters} -> requireLowercaseCharacters) (\s@PasswordPolicy' {} a -> s {requireLowercaseCharacters = a} :: PasswordPolicy)

-- | Minimum length to require for IAM user passwords.
passwordPolicy_minimumPasswordLength :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Natural)
passwordPolicy_minimumPasswordLength = Lens.lens (\PasswordPolicy' {minimumPasswordLength} -> minimumPasswordLength) (\s@PasswordPolicy' {} a -> s {minimumPasswordLength = a} :: PasswordPolicy)

-- | Specifies the number of previous passwords that IAM users are prevented
-- from reusing.
passwordPolicy_passwordReusePrevention :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Natural)
passwordPolicy_passwordReusePrevention = Lens.lens (\PasswordPolicy' {passwordReusePrevention} -> passwordReusePrevention) (\s@PasswordPolicy' {} a -> s {passwordReusePrevention = a} :: PasswordPolicy)

-- | Indicates whether passwords in the account expire. Returns true if
-- @MaxPasswordAge@ contains a value greater than 0. Returns false if
-- MaxPasswordAge is 0 or not present.
passwordPolicy_expirePasswords :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
passwordPolicy_expirePasswords = Lens.lens (\PasswordPolicy' {expirePasswords} -> expirePasswords) (\s@PasswordPolicy' {} a -> s {expirePasswords = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one uppercase
-- character (A to Z).
passwordPolicy_requireUppercaseCharacters :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
passwordPolicy_requireUppercaseCharacters = Lens.lens (\PasswordPolicy' {requireUppercaseCharacters} -> requireUppercaseCharacters) (\s@PasswordPolicy' {} a -> s {requireUppercaseCharacters = a} :: PasswordPolicy)

-- | Specifies whether IAM users are allowed to change their own password.
passwordPolicy_allowUsersToChangePassword :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
passwordPolicy_allowUsersToChangePassword = Lens.lens (\PasswordPolicy' {allowUsersToChangePassword} -> allowUsersToChangePassword) (\s@PasswordPolicy' {} a -> s {allowUsersToChangePassword = a} :: PasswordPolicy)

-- | Specifies whether IAM users are prevented from setting a new password
-- after their password has expired.
passwordPolicy_hardExpiry :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
passwordPolicy_hardExpiry = Lens.lens (\PasswordPolicy' {hardExpiry} -> hardExpiry) (\s@PasswordPolicy' {} a -> s {hardExpiry = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one of the
-- following symbols:
--
-- ! \@ # $ % ^ & * ( ) _ + - = [ ] { } | \'
passwordPolicy_requireSymbols :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
passwordPolicy_requireSymbols = Lens.lens (\PasswordPolicy' {requireSymbols} -> requireSymbols) (\s@PasswordPolicy' {} a -> s {requireSymbols = a} :: PasswordPolicy)

-- | Specifies whether IAM user passwords must contain at least one numeric
-- character (0 to 9).
passwordPolicy_requireNumbers :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
passwordPolicy_requireNumbers = Lens.lens (\PasswordPolicy' {requireNumbers} -> requireNumbers) (\s@PasswordPolicy' {} a -> s {requireNumbers = a} :: PasswordPolicy)

instance Core.FromXML PasswordPolicy where
  parseXML x =
    PasswordPolicy'
      Core.<$> (x Core..@? "MaxPasswordAge")
      Core.<*> (x Core..@? "RequireLowercaseCharacters")
      Core.<*> (x Core..@? "MinimumPasswordLength")
      Core.<*> (x Core..@? "PasswordReusePrevention")
      Core.<*> (x Core..@? "ExpirePasswords")
      Core.<*> (x Core..@? "RequireUppercaseCharacters")
      Core.<*> (x Core..@? "AllowUsersToChangePassword")
      Core.<*> (x Core..@? "HardExpiry")
      Core.<*> (x Core..@? "RequireSymbols")
      Core.<*> (x Core..@? "RequireNumbers")

instance Core.Hashable PasswordPolicy

instance Core.NFData PasswordPolicy
