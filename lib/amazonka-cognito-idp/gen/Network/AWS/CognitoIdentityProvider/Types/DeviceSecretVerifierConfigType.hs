{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CognitoIdentityProvider.Types.DeviceSecretVerifierConfigType
  ( DeviceSecretVerifierConfigType (..),

    -- * Smart constructor
    mkDeviceSecretVerifierConfigType,

    -- * Lenses
    dsvctPasswordVerifier,
    dsvctSalt,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The device verifier against which it will be authenticated.
--
-- /See:/ 'mkDeviceSecretVerifierConfigType' smart constructor.
data DeviceSecretVerifierConfigType = DeviceSecretVerifierConfigType'
  { -- | The password verifier.
    passwordVerifier :: Lude.Maybe Lude.Text,
    -- | The salt.
    salt :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeviceSecretVerifierConfigType' with the minimum fields required to make a request.
--
-- * 'passwordVerifier' - The password verifier.
-- * 'salt' - The salt.
mkDeviceSecretVerifierConfigType ::
  DeviceSecretVerifierConfigType
mkDeviceSecretVerifierConfigType =
  DeviceSecretVerifierConfigType'
    { passwordVerifier = Lude.Nothing,
      salt = Lude.Nothing
    }

-- | The password verifier.
--
-- /Note:/ Consider using 'passwordVerifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvctPasswordVerifier :: Lens.Lens' DeviceSecretVerifierConfigType (Lude.Maybe Lude.Text)
dsvctPasswordVerifier = Lens.lens (passwordVerifier :: DeviceSecretVerifierConfigType -> Lude.Maybe Lude.Text) (\s a -> s {passwordVerifier = a} :: DeviceSecretVerifierConfigType)
{-# DEPRECATED dsvctPasswordVerifier "Use generic-lens or generic-optics with 'passwordVerifier' instead." #-}

-- | The salt.
--
-- /Note:/ Consider using 'salt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvctSalt :: Lens.Lens' DeviceSecretVerifierConfigType (Lude.Maybe Lude.Text)
dsvctSalt = Lens.lens (salt :: DeviceSecretVerifierConfigType -> Lude.Maybe Lude.Text) (\s a -> s {salt = a} :: DeviceSecretVerifierConfigType)
{-# DEPRECATED dsvctSalt "Use generic-lens or generic-optics with 'salt' instead." #-}

instance Lude.ToJSON DeviceSecretVerifierConfigType where
  toJSON DeviceSecretVerifierConfigType' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PasswordVerifier" Lude..=) Lude.<$> passwordVerifier,
            ("Salt" Lude..=) Lude.<$> salt
          ]
      )
