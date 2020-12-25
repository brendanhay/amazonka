{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.UserPoolPolicyType
  ( UserPoolPolicyType (..),

    -- * Smart constructor
    mkUserPoolPolicyType,

    -- * Lenses
    upptPasswordPolicy,
  )
where

import qualified Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The policy associated with a user pool.
--
-- /See:/ 'mkUserPoolPolicyType' smart constructor.
newtype UserPoolPolicyType = UserPoolPolicyType'
  { -- | The password policy.
    passwordPolicy :: Core.Maybe Types.PasswordPolicyType
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving newtype (Core.Hashable, Core.NFData)

-- | Creates a 'UserPoolPolicyType' value with any optional fields omitted.
mkUserPoolPolicyType ::
  UserPoolPolicyType
mkUserPoolPolicyType =
  UserPoolPolicyType' {passwordPolicy = Core.Nothing}

-- | The password policy.
--
-- /Note:/ Consider using 'passwordPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upptPasswordPolicy :: Lens.Lens' UserPoolPolicyType (Core.Maybe Types.PasswordPolicyType)
upptPasswordPolicy = Lens.field @"passwordPolicy"
{-# DEPRECATED upptPasswordPolicy "Use generic-lens or generic-optics with 'passwordPolicy' instead." #-}

instance Core.FromJSON UserPoolPolicyType where
  toJSON UserPoolPolicyType {..} =
    Core.object
      ( Core.catMaybes
          [("PasswordPolicy" Core..=) Core.<$> passwordPolicy]
      )

instance Core.FromJSON UserPoolPolicyType where
  parseJSON =
    Core.withObject "UserPoolPolicyType" Core.$
      \x -> UserPoolPolicyType' Core.<$> (x Core..:? "PasswordPolicy")
