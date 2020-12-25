{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IAM.Types.PasswordPolicy
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IAM.Types.PasswordPolicy
  ( PasswordPolicy (..),

    -- * Smart constructor
    mkPasswordPolicy,

    -- * Lenses
    ppAllowUsersToChangePassword,
    ppExpirePasswords,
    ppHardExpiry,
    ppMaxPasswordAge,
    ppMinimumPasswordLength,
    ppPasswordReusePrevention,
    ppRequireLowercaseCharacters,
    ppRequireNumbers,
    ppRequireSymbols,
    ppRequireUppercaseCharacters,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains information about the account password policy.
--
-- This data type is used as a response element in the 'GetAccountPasswordPolicy' operation.
--
-- /See:/ 'mkPasswordPolicy' smart constructor.
data PasswordPolicy = PasswordPolicy'
  { -- | Specifies whether IAM users are allowed to change their own password.
    allowUsersToChangePassword :: Core.Maybe Core.Bool,
    -- | Indicates whether passwords in the account expire. Returns true if @MaxPasswordAge@ contains a value greater than 0. Returns false if MaxPasswordAge is 0 or not present.
    expirePasswords :: Core.Maybe Core.Bool,
    -- | Specifies whether IAM users are prevented from setting a new password after their password has expired.
    hardExpiry :: Core.Maybe Core.Bool,
    -- | The number of days that an IAM user password is valid.
    maxPasswordAge :: Core.Maybe Core.Natural,
    -- | Minimum length to require for IAM user passwords.
    minimumPasswordLength :: Core.Maybe Core.Natural,
    -- | Specifies the number of previous passwords that IAM users are prevented from reusing.
    passwordReusePrevention :: Core.Maybe Core.Natural,
    -- | Specifies whether to require lowercase characters for IAM user passwords.
    requireLowercaseCharacters :: Core.Maybe Core.Bool,
    -- | Specifies whether to require numbers for IAM user passwords.
    requireNumbers :: Core.Maybe Core.Bool,
    -- | Specifies whether to require symbols for IAM user passwords.
    requireSymbols :: Core.Maybe Core.Bool,
    -- | Specifies whether to require uppercase characters for IAM user passwords.
    requireUppercaseCharacters :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PasswordPolicy' value with any optional fields omitted.
mkPasswordPolicy ::
  PasswordPolicy
mkPasswordPolicy =
  PasswordPolicy'
    { allowUsersToChangePassword = Core.Nothing,
      expirePasswords = Core.Nothing,
      hardExpiry = Core.Nothing,
      maxPasswordAge = Core.Nothing,
      minimumPasswordLength = Core.Nothing,
      passwordReusePrevention = Core.Nothing,
      requireLowercaseCharacters = Core.Nothing,
      requireNumbers = Core.Nothing,
      requireSymbols = Core.Nothing,
      requireUppercaseCharacters = Core.Nothing
    }

-- | Specifies whether IAM users are allowed to change their own password.
--
-- /Note:/ Consider using 'allowUsersToChangePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppAllowUsersToChangePassword :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
ppAllowUsersToChangePassword = Lens.field @"allowUsersToChangePassword"
{-# DEPRECATED ppAllowUsersToChangePassword "Use generic-lens or generic-optics with 'allowUsersToChangePassword' instead." #-}

-- | Indicates whether passwords in the account expire. Returns true if @MaxPasswordAge@ contains a value greater than 0. Returns false if MaxPasswordAge is 0 or not present.
--
-- /Note:/ Consider using 'expirePasswords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppExpirePasswords :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
ppExpirePasswords = Lens.field @"expirePasswords"
{-# DEPRECATED ppExpirePasswords "Use generic-lens or generic-optics with 'expirePasswords' instead." #-}

-- | Specifies whether IAM users are prevented from setting a new password after their password has expired.
--
-- /Note:/ Consider using 'hardExpiry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppHardExpiry :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
ppHardExpiry = Lens.field @"hardExpiry"
{-# DEPRECATED ppHardExpiry "Use generic-lens or generic-optics with 'hardExpiry' instead." #-}

-- | The number of days that an IAM user password is valid.
--
-- /Note:/ Consider using 'maxPasswordAge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppMaxPasswordAge :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Natural)
ppMaxPasswordAge = Lens.field @"maxPasswordAge"
{-# DEPRECATED ppMaxPasswordAge "Use generic-lens or generic-optics with 'maxPasswordAge' instead." #-}

-- | Minimum length to require for IAM user passwords.
--
-- /Note:/ Consider using 'minimumPasswordLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppMinimumPasswordLength :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Natural)
ppMinimumPasswordLength = Lens.field @"minimumPasswordLength"
{-# DEPRECATED ppMinimumPasswordLength "Use generic-lens or generic-optics with 'minimumPasswordLength' instead." #-}

-- | Specifies the number of previous passwords that IAM users are prevented from reusing.
--
-- /Note:/ Consider using 'passwordReusePrevention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPasswordReusePrevention :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Natural)
ppPasswordReusePrevention = Lens.field @"passwordReusePrevention"
{-# DEPRECATED ppPasswordReusePrevention "Use generic-lens or generic-optics with 'passwordReusePrevention' instead." #-}

-- | Specifies whether to require lowercase characters for IAM user passwords.
--
-- /Note:/ Consider using 'requireLowercaseCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppRequireLowercaseCharacters :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
ppRequireLowercaseCharacters = Lens.field @"requireLowercaseCharacters"
{-# DEPRECATED ppRequireLowercaseCharacters "Use generic-lens or generic-optics with 'requireLowercaseCharacters' instead." #-}

-- | Specifies whether to require numbers for IAM user passwords.
--
-- /Note:/ Consider using 'requireNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppRequireNumbers :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
ppRequireNumbers = Lens.field @"requireNumbers"
{-# DEPRECATED ppRequireNumbers "Use generic-lens or generic-optics with 'requireNumbers' instead." #-}

-- | Specifies whether to require symbols for IAM user passwords.
--
-- /Note:/ Consider using 'requireSymbols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppRequireSymbols :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
ppRequireSymbols = Lens.field @"requireSymbols"
{-# DEPRECATED ppRequireSymbols "Use generic-lens or generic-optics with 'requireSymbols' instead." #-}

-- | Specifies whether to require uppercase characters for IAM user passwords.
--
-- /Note:/ Consider using 'requireUppercaseCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppRequireUppercaseCharacters :: Lens.Lens' PasswordPolicy (Core.Maybe Core.Bool)
ppRequireUppercaseCharacters = Lens.field @"requireUppercaseCharacters"
{-# DEPRECATED ppRequireUppercaseCharacters "Use generic-lens or generic-optics with 'requireUppercaseCharacters' instead." #-}

instance Core.FromXML PasswordPolicy where
  parseXML x =
    PasswordPolicy'
      Core.<$> (x Core..@? "AllowUsersToChangePassword")
      Core.<*> (x Core..@? "ExpirePasswords")
      Core.<*> (x Core..@? "HardExpiry")
      Core.<*> (x Core..@? "MaxPasswordAge")
      Core.<*> (x Core..@? "MinimumPasswordLength")
      Core.<*> (x Core..@? "PasswordReusePrevention")
      Core.<*> (x Core..@? "RequireLowercaseCharacters")
      Core.<*> (x Core..@? "RequireNumbers")
      Core.<*> (x Core..@? "RequireSymbols")
      Core.<*> (x Core..@? "RequireUppercaseCharacters")
