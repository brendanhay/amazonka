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
    ppExpirePasswords,
    ppMinimumPasswordLength,
    ppRequireNumbers,
    ppPasswordReusePrevention,
    ppRequireLowercaseCharacters,
    ppMaxPasswordAge,
    ppHardExpiry,
    ppRequireSymbols,
    ppRequireUppercaseCharacters,
    ppAllowUsersToChangePassword,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains information about the account password policy.
--
-- This data type is used as a response element in the 'GetAccountPasswordPolicy' operation.
--
-- /See:/ 'mkPasswordPolicy' smart constructor.
data PasswordPolicy = PasswordPolicy'
  { expirePasswords ::
      Lude.Maybe Lude.Bool,
    minimumPasswordLength :: Lude.Maybe Lude.Natural,
    requireNumbers :: Lude.Maybe Lude.Bool,
    passwordReusePrevention :: Lude.Maybe Lude.Natural,
    requireLowercaseCharacters :: Lude.Maybe Lude.Bool,
    maxPasswordAge :: Lude.Maybe Lude.Natural,
    hardExpiry :: Lude.Maybe Lude.Bool,
    requireSymbols :: Lude.Maybe Lude.Bool,
    requireUppercaseCharacters :: Lude.Maybe Lude.Bool,
    allowUsersToChangePassword :: Lude.Maybe Lude.Bool
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PasswordPolicy' with the minimum fields required to make a request.
--
-- * 'allowUsersToChangePassword' - Specifies whether IAM users are allowed to change their own password.
-- * 'expirePasswords' - Indicates whether passwords in the account expire. Returns true if @MaxPasswordAge@ contains a value greater than 0. Returns false if MaxPasswordAge is 0 or not present.
-- * 'hardExpiry' - Specifies whether IAM users are prevented from setting a new password after their password has expired.
-- * 'maxPasswordAge' - The number of days that an IAM user password is valid.
-- * 'minimumPasswordLength' - Minimum length to require for IAM user passwords.
-- * 'passwordReusePrevention' - Specifies the number of previous passwords that IAM users are prevented from reusing.
-- * 'requireLowercaseCharacters' - Specifies whether to require lowercase characters for IAM user passwords.
-- * 'requireNumbers' - Specifies whether to require numbers for IAM user passwords.
-- * 'requireSymbols' - Specifies whether to require symbols for IAM user passwords.
-- * 'requireUppercaseCharacters' - Specifies whether to require uppercase characters for IAM user passwords.
mkPasswordPolicy ::
  PasswordPolicy
mkPasswordPolicy =
  PasswordPolicy'
    { expirePasswords = Lude.Nothing,
      minimumPasswordLength = Lude.Nothing,
      requireNumbers = Lude.Nothing,
      passwordReusePrevention = Lude.Nothing,
      requireLowercaseCharacters = Lude.Nothing,
      maxPasswordAge = Lude.Nothing,
      hardExpiry = Lude.Nothing,
      requireSymbols = Lude.Nothing,
      requireUppercaseCharacters = Lude.Nothing,
      allowUsersToChangePassword = Lude.Nothing
    }

-- | Indicates whether passwords in the account expire. Returns true if @MaxPasswordAge@ contains a value greater than 0. Returns false if MaxPasswordAge is 0 or not present.
--
-- /Note:/ Consider using 'expirePasswords' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppExpirePasswords :: Lens.Lens' PasswordPolicy (Lude.Maybe Lude.Bool)
ppExpirePasswords = Lens.lens (expirePasswords :: PasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {expirePasswords = a} :: PasswordPolicy)
{-# DEPRECATED ppExpirePasswords "Use generic-lens or generic-optics with 'expirePasswords' instead." #-}

-- | Minimum length to require for IAM user passwords.
--
-- /Note:/ Consider using 'minimumPasswordLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppMinimumPasswordLength :: Lens.Lens' PasswordPolicy (Lude.Maybe Lude.Natural)
ppMinimumPasswordLength = Lens.lens (minimumPasswordLength :: PasswordPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {minimumPasswordLength = a} :: PasswordPolicy)
{-# DEPRECATED ppMinimumPasswordLength "Use generic-lens or generic-optics with 'minimumPasswordLength' instead." #-}

-- | Specifies whether to require numbers for IAM user passwords.
--
-- /Note:/ Consider using 'requireNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppRequireNumbers :: Lens.Lens' PasswordPolicy (Lude.Maybe Lude.Bool)
ppRequireNumbers = Lens.lens (requireNumbers :: PasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {requireNumbers = a} :: PasswordPolicy)
{-# DEPRECATED ppRequireNumbers "Use generic-lens or generic-optics with 'requireNumbers' instead." #-}

-- | Specifies the number of previous passwords that IAM users are prevented from reusing.
--
-- /Note:/ Consider using 'passwordReusePrevention' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppPasswordReusePrevention :: Lens.Lens' PasswordPolicy (Lude.Maybe Lude.Natural)
ppPasswordReusePrevention = Lens.lens (passwordReusePrevention :: PasswordPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {passwordReusePrevention = a} :: PasswordPolicy)
{-# DEPRECATED ppPasswordReusePrevention "Use generic-lens or generic-optics with 'passwordReusePrevention' instead." #-}

-- | Specifies whether to require lowercase characters for IAM user passwords.
--
-- /Note:/ Consider using 'requireLowercaseCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppRequireLowercaseCharacters :: Lens.Lens' PasswordPolicy (Lude.Maybe Lude.Bool)
ppRequireLowercaseCharacters = Lens.lens (requireLowercaseCharacters :: PasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {requireLowercaseCharacters = a} :: PasswordPolicy)
{-# DEPRECATED ppRequireLowercaseCharacters "Use generic-lens or generic-optics with 'requireLowercaseCharacters' instead." #-}

-- | The number of days that an IAM user password is valid.
--
-- /Note:/ Consider using 'maxPasswordAge' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppMaxPasswordAge :: Lens.Lens' PasswordPolicy (Lude.Maybe Lude.Natural)
ppMaxPasswordAge = Lens.lens (maxPasswordAge :: PasswordPolicy -> Lude.Maybe Lude.Natural) (\s a -> s {maxPasswordAge = a} :: PasswordPolicy)
{-# DEPRECATED ppMaxPasswordAge "Use generic-lens or generic-optics with 'maxPasswordAge' instead." #-}

-- | Specifies whether IAM users are prevented from setting a new password after their password has expired.
--
-- /Note:/ Consider using 'hardExpiry' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppHardExpiry :: Lens.Lens' PasswordPolicy (Lude.Maybe Lude.Bool)
ppHardExpiry = Lens.lens (hardExpiry :: PasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {hardExpiry = a} :: PasswordPolicy)
{-# DEPRECATED ppHardExpiry "Use generic-lens or generic-optics with 'hardExpiry' instead." #-}

-- | Specifies whether to require symbols for IAM user passwords.
--
-- /Note:/ Consider using 'requireSymbols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppRequireSymbols :: Lens.Lens' PasswordPolicy (Lude.Maybe Lude.Bool)
ppRequireSymbols = Lens.lens (requireSymbols :: PasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {requireSymbols = a} :: PasswordPolicy)
{-# DEPRECATED ppRequireSymbols "Use generic-lens or generic-optics with 'requireSymbols' instead." #-}

-- | Specifies whether to require uppercase characters for IAM user passwords.
--
-- /Note:/ Consider using 'requireUppercaseCharacters' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppRequireUppercaseCharacters :: Lens.Lens' PasswordPolicy (Lude.Maybe Lude.Bool)
ppRequireUppercaseCharacters = Lens.lens (requireUppercaseCharacters :: PasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {requireUppercaseCharacters = a} :: PasswordPolicy)
{-# DEPRECATED ppRequireUppercaseCharacters "Use generic-lens or generic-optics with 'requireUppercaseCharacters' instead." #-}

-- | Specifies whether IAM users are allowed to change their own password.
--
-- /Note:/ Consider using 'allowUsersToChangePassword' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ppAllowUsersToChangePassword :: Lens.Lens' PasswordPolicy (Lude.Maybe Lude.Bool)
ppAllowUsersToChangePassword = Lens.lens (allowUsersToChangePassword :: PasswordPolicy -> Lude.Maybe Lude.Bool) (\s a -> s {allowUsersToChangePassword = a} :: PasswordPolicy)
{-# DEPRECATED ppAllowUsersToChangePassword "Use generic-lens or generic-optics with 'allowUsersToChangePassword' instead." #-}

instance Lude.FromXML PasswordPolicy where
  parseXML x =
    PasswordPolicy'
      Lude.<$> (x Lude..@? "ExpirePasswords")
      Lude.<*> (x Lude..@? "MinimumPasswordLength")
      Lude.<*> (x Lude..@? "RequireNumbers")
      Lude.<*> (x Lude..@? "PasswordReusePrevention")
      Lude.<*> (x Lude..@? "RequireLowercaseCharacters")
      Lude.<*> (x Lude..@? "MaxPasswordAge")
      Lude.<*> (x Lude..@? "HardExpiry")
      Lude.<*> (x Lude..@? "RequireSymbols")
      Lude.<*> (x Lude..@? "RequireUppercaseCharacters")
      Lude.<*> (x Lude..@? "AllowUsersToChangePassword")
