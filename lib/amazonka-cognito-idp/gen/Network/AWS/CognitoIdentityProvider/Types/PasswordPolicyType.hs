-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
  ( PasswordPolicyType (..),

    -- * Smart constructor
    mkPasswordPolicyType,

    -- * Lenses
    pptRequireNumbers,
    pptRequireUppercase,
    pptRequireLowercase,
    pptMinimumLength,
    pptRequireSymbols,
    pptTemporaryPasswordValidityDays,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The password policy type.
--
-- /See:/ 'mkPasswordPolicyType' smart constructor.
data PasswordPolicyType = PasswordPolicyType'
  { requireNumbers ::
      Lude.Maybe Lude.Bool,
    requireUppercase :: Lude.Maybe Lude.Bool,
    requireLowercase :: Lude.Maybe Lude.Bool,
    minimumLength :: Lude.Maybe Lude.Natural,
    requireSymbols :: Lude.Maybe Lude.Bool,
    temporaryPasswordValidityDays ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PasswordPolicyType' with the minimum fields required to make a request.
--
-- * 'minimumLength' - The minimum length of the password policy that you have set. Cannot be less than 6.
-- * 'requireLowercase' - In the password policy that you have set, refers to whether you have required users to use at least one lowercase letter in their password.
-- * 'requireNumbers' - In the password policy that you have set, refers to whether you have required users to use at least one number in their password.
-- * 'requireSymbols' - In the password policy that you have set, refers to whether you have required users to use at least one symbol in their password.
-- * 'requireUppercase' - In the password policy that you have set, refers to whether you have required users to use at least one uppercase letter in their password.
-- * 'temporaryPasswordValidityDays' - In the password policy you have set, refers to the number of days a temporary password is valid. If the user does not sign-in during this time, their password will need to be reset by an administrator.
mkPasswordPolicyType ::
  PasswordPolicyType
mkPasswordPolicyType =
  PasswordPolicyType'
    { requireNumbers = Lude.Nothing,
      requireUppercase = Lude.Nothing,
      requireLowercase = Lude.Nothing,
      minimumLength = Lude.Nothing,
      requireSymbols = Lude.Nothing,
      temporaryPasswordValidityDays = Lude.Nothing
    }

-- | In the password policy that you have set, refers to whether you have required users to use at least one number in their password.
--
-- /Note:/ Consider using 'requireNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptRequireNumbers :: Lens.Lens' PasswordPolicyType (Lude.Maybe Lude.Bool)
pptRequireNumbers = Lens.lens (requireNumbers :: PasswordPolicyType -> Lude.Maybe Lude.Bool) (\s a -> s {requireNumbers = a} :: PasswordPolicyType)
{-# DEPRECATED pptRequireNumbers "Use generic-lens or generic-optics with 'requireNumbers' instead." #-}

-- | In the password policy that you have set, refers to whether you have required users to use at least one uppercase letter in their password.
--
-- /Note:/ Consider using 'requireUppercase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptRequireUppercase :: Lens.Lens' PasswordPolicyType (Lude.Maybe Lude.Bool)
pptRequireUppercase = Lens.lens (requireUppercase :: PasswordPolicyType -> Lude.Maybe Lude.Bool) (\s a -> s {requireUppercase = a} :: PasswordPolicyType)
{-# DEPRECATED pptRequireUppercase "Use generic-lens or generic-optics with 'requireUppercase' instead." #-}

-- | In the password policy that you have set, refers to whether you have required users to use at least one lowercase letter in their password.
--
-- /Note:/ Consider using 'requireLowercase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptRequireLowercase :: Lens.Lens' PasswordPolicyType (Lude.Maybe Lude.Bool)
pptRequireLowercase = Lens.lens (requireLowercase :: PasswordPolicyType -> Lude.Maybe Lude.Bool) (\s a -> s {requireLowercase = a} :: PasswordPolicyType)
{-# DEPRECATED pptRequireLowercase "Use generic-lens or generic-optics with 'requireLowercase' instead." #-}

-- | The minimum length of the password policy that you have set. Cannot be less than 6.
--
-- /Note:/ Consider using 'minimumLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptMinimumLength :: Lens.Lens' PasswordPolicyType (Lude.Maybe Lude.Natural)
pptMinimumLength = Lens.lens (minimumLength :: PasswordPolicyType -> Lude.Maybe Lude.Natural) (\s a -> s {minimumLength = a} :: PasswordPolicyType)
{-# DEPRECATED pptMinimumLength "Use generic-lens or generic-optics with 'minimumLength' instead." #-}

-- | In the password policy that you have set, refers to whether you have required users to use at least one symbol in their password.
--
-- /Note:/ Consider using 'requireSymbols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptRequireSymbols :: Lens.Lens' PasswordPolicyType (Lude.Maybe Lude.Bool)
pptRequireSymbols = Lens.lens (requireSymbols :: PasswordPolicyType -> Lude.Maybe Lude.Bool) (\s a -> s {requireSymbols = a} :: PasswordPolicyType)
{-# DEPRECATED pptRequireSymbols "Use generic-lens or generic-optics with 'requireSymbols' instead." #-}

-- | In the password policy you have set, refers to the number of days a temporary password is valid. If the user does not sign-in during this time, their password will need to be reset by an administrator.
--
-- /Note:/ Consider using 'temporaryPasswordValidityDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptTemporaryPasswordValidityDays :: Lens.Lens' PasswordPolicyType (Lude.Maybe Lude.Natural)
pptTemporaryPasswordValidityDays = Lens.lens (temporaryPasswordValidityDays :: PasswordPolicyType -> Lude.Maybe Lude.Natural) (\s a -> s {temporaryPasswordValidityDays = a} :: PasswordPolicyType)
{-# DEPRECATED pptTemporaryPasswordValidityDays "Use generic-lens or generic-optics with 'temporaryPasswordValidityDays' instead." #-}

instance Lude.FromJSON PasswordPolicyType where
  parseJSON =
    Lude.withObject
      "PasswordPolicyType"
      ( \x ->
          PasswordPolicyType'
            Lude.<$> (x Lude..:? "RequireNumbers")
            Lude.<*> (x Lude..:? "RequireUppercase")
            Lude.<*> (x Lude..:? "RequireLowercase")
            Lude.<*> (x Lude..:? "MinimumLength")
            Lude.<*> (x Lude..:? "RequireSymbols")
            Lude.<*> (x Lude..:? "TemporaryPasswordValidityDays")
      )

instance Lude.ToJSON PasswordPolicyType where
  toJSON PasswordPolicyType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("RequireNumbers" Lude..=) Lude.<$> requireNumbers,
            ("RequireUppercase" Lude..=) Lude.<$> requireUppercase,
            ("RequireLowercase" Lude..=) Lude.<$> requireLowercase,
            ("MinimumLength" Lude..=) Lude.<$> minimumLength,
            ("RequireSymbols" Lude..=) Lude.<$> requireSymbols,
            ("TemporaryPasswordValidityDays" Lude..=)
              Lude.<$> temporaryPasswordValidityDays
          ]
      )
