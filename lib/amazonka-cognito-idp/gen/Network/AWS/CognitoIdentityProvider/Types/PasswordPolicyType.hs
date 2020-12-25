{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    pptMinimumLength,
    pptRequireLowercase,
    pptRequireNumbers,
    pptRequireSymbols,
    pptRequireUppercase,
    pptTemporaryPasswordValidityDays,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The password policy type.
--
-- /See:/ 'mkPasswordPolicyType' smart constructor.
data PasswordPolicyType = PasswordPolicyType'
  { -- | The minimum length of the password policy that you have set. Cannot be less than 6.
    minimumLength :: Core.Maybe Core.Natural,
    -- | In the password policy that you have set, refers to whether you have required users to use at least one lowercase letter in their password.
    requireLowercase :: Core.Maybe Core.Bool,
    -- | In the password policy that you have set, refers to whether you have required users to use at least one number in their password.
    requireNumbers :: Core.Maybe Core.Bool,
    -- | In the password policy that you have set, refers to whether you have required users to use at least one symbol in their password.
    requireSymbols :: Core.Maybe Core.Bool,
    -- | In the password policy that you have set, refers to whether you have required users to use at least one uppercase letter in their password.
    requireUppercase :: Core.Maybe Core.Bool,
    -- | In the password policy you have set, refers to the number of days a temporary password is valid. If the user does not sign-in during this time, their password will need to be reset by an administrator.
    temporaryPasswordValidityDays :: Core.Maybe Core.Natural
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PasswordPolicyType' value with any optional fields omitted.
mkPasswordPolicyType ::
  PasswordPolicyType
mkPasswordPolicyType =
  PasswordPolicyType'
    { minimumLength = Core.Nothing,
      requireLowercase = Core.Nothing,
      requireNumbers = Core.Nothing,
      requireSymbols = Core.Nothing,
      requireUppercase = Core.Nothing,
      temporaryPasswordValidityDays = Core.Nothing
    }

-- | The minimum length of the password policy that you have set. Cannot be less than 6.
--
-- /Note:/ Consider using 'minimumLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptMinimumLength :: Lens.Lens' PasswordPolicyType (Core.Maybe Core.Natural)
pptMinimumLength = Lens.field @"minimumLength"
{-# DEPRECATED pptMinimumLength "Use generic-lens or generic-optics with 'minimumLength' instead." #-}

-- | In the password policy that you have set, refers to whether you have required users to use at least one lowercase letter in their password.
--
-- /Note:/ Consider using 'requireLowercase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptRequireLowercase :: Lens.Lens' PasswordPolicyType (Core.Maybe Core.Bool)
pptRequireLowercase = Lens.field @"requireLowercase"
{-# DEPRECATED pptRequireLowercase "Use generic-lens or generic-optics with 'requireLowercase' instead." #-}

-- | In the password policy that you have set, refers to whether you have required users to use at least one number in their password.
--
-- /Note:/ Consider using 'requireNumbers' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptRequireNumbers :: Lens.Lens' PasswordPolicyType (Core.Maybe Core.Bool)
pptRequireNumbers = Lens.field @"requireNumbers"
{-# DEPRECATED pptRequireNumbers "Use generic-lens or generic-optics with 'requireNumbers' instead." #-}

-- | In the password policy that you have set, refers to whether you have required users to use at least one symbol in their password.
--
-- /Note:/ Consider using 'requireSymbols' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptRequireSymbols :: Lens.Lens' PasswordPolicyType (Core.Maybe Core.Bool)
pptRequireSymbols = Lens.field @"requireSymbols"
{-# DEPRECATED pptRequireSymbols "Use generic-lens or generic-optics with 'requireSymbols' instead." #-}

-- | In the password policy that you have set, refers to whether you have required users to use at least one uppercase letter in their password.
--
-- /Note:/ Consider using 'requireUppercase' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptRequireUppercase :: Lens.Lens' PasswordPolicyType (Core.Maybe Core.Bool)
pptRequireUppercase = Lens.field @"requireUppercase"
{-# DEPRECATED pptRequireUppercase "Use generic-lens or generic-optics with 'requireUppercase' instead." #-}

-- | In the password policy you have set, refers to the number of days a temporary password is valid. If the user does not sign-in during this time, their password will need to be reset by an administrator.
--
-- /Note:/ Consider using 'temporaryPasswordValidityDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pptTemporaryPasswordValidityDays :: Lens.Lens' PasswordPolicyType (Core.Maybe Core.Natural)
pptTemporaryPasswordValidityDays = Lens.field @"temporaryPasswordValidityDays"
{-# DEPRECATED pptTemporaryPasswordValidityDays "Use generic-lens or generic-optics with 'temporaryPasswordValidityDays' instead." #-}

instance Core.FromJSON PasswordPolicyType where
  toJSON PasswordPolicyType {..} =
    Core.object
      ( Core.catMaybes
          [ ("MinimumLength" Core..=) Core.<$> minimumLength,
            ("RequireLowercase" Core..=) Core.<$> requireLowercase,
            ("RequireNumbers" Core..=) Core.<$> requireNumbers,
            ("RequireSymbols" Core..=) Core.<$> requireSymbols,
            ("RequireUppercase" Core..=) Core.<$> requireUppercase,
            ("TemporaryPasswordValidityDays" Core..=)
              Core.<$> temporaryPasswordValidityDays
          ]
      )

instance Core.FromJSON PasswordPolicyType where
  parseJSON =
    Core.withObject "PasswordPolicyType" Core.$
      \x ->
        PasswordPolicyType'
          Core.<$> (x Core..:? "MinimumLength")
          Core.<*> (x Core..:? "RequireLowercase")
          Core.<*> (x Core..:? "RequireNumbers")
          Core.<*> (x Core..:? "RequireSymbols")
          Core.<*> (x Core..:? "RequireUppercase")
          Core.<*> (x Core..:? "TemporaryPasswordValidityDays")
