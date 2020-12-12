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

import Network.AWS.CognitoIdentityProvider.Types.PasswordPolicyType
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The policy associated with a user pool.
--
-- /See:/ 'mkUserPoolPolicyType' smart constructor.
newtype UserPoolPolicyType = UserPoolPolicyType'
  { passwordPolicy ::
      Lude.Maybe PasswordPolicyType
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UserPoolPolicyType' with the minimum fields required to make a request.
--
-- * 'passwordPolicy' - The password policy.
mkUserPoolPolicyType ::
  UserPoolPolicyType
mkUserPoolPolicyType =
  UserPoolPolicyType' {passwordPolicy = Lude.Nothing}

-- | The password policy.
--
-- /Note:/ Consider using 'passwordPolicy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
upptPasswordPolicy :: Lens.Lens' UserPoolPolicyType (Lude.Maybe PasswordPolicyType)
upptPasswordPolicy = Lens.lens (passwordPolicy :: UserPoolPolicyType -> Lude.Maybe PasswordPolicyType) (\s a -> s {passwordPolicy = a} :: UserPoolPolicyType)
{-# DEPRECATED upptPasswordPolicy "Use generic-lens or generic-optics with 'passwordPolicy' instead." #-}

instance Lude.FromJSON UserPoolPolicyType where
  parseJSON =
    Lude.withObject
      "UserPoolPolicyType"
      (\x -> UserPoolPolicyType' Lude.<$> (x Lude..:? "PasswordPolicy"))

instance Lude.ToJSON UserPoolPolicyType where
  toJSON UserPoolPolicyType' {..} =
    Lude.object
      ( Lude.catMaybes
          [("PasswordPolicy" Lude..=) Lude.<$> passwordPolicy]
      )
