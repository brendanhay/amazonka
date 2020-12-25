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

import qualified Network.AWS.CognitoIdentityProvider.Types.PasswordVerifier as Types
import qualified Network.AWS.CognitoIdentityProvider.Types.Salt as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The device verifier against which it will be authenticated.
--
-- /See:/ 'mkDeviceSecretVerifierConfigType' smart constructor.
data DeviceSecretVerifierConfigType = DeviceSecretVerifierConfigType'
  { -- | The password verifier.
    passwordVerifier :: Core.Maybe Types.PasswordVerifier,
    -- | The salt.
    salt :: Core.Maybe Types.Salt
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'DeviceSecretVerifierConfigType' value with any optional fields omitted.
mkDeviceSecretVerifierConfigType ::
  DeviceSecretVerifierConfigType
mkDeviceSecretVerifierConfigType =
  DeviceSecretVerifierConfigType'
    { passwordVerifier = Core.Nothing,
      salt = Core.Nothing
    }

-- | The password verifier.
--
-- /Note:/ Consider using 'passwordVerifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvctPasswordVerifier :: Lens.Lens' DeviceSecretVerifierConfigType (Core.Maybe Types.PasswordVerifier)
dsvctPasswordVerifier = Lens.field @"passwordVerifier"
{-# DEPRECATED dsvctPasswordVerifier "Use generic-lens or generic-optics with 'passwordVerifier' instead." #-}

-- | The salt.
--
-- /Note:/ Consider using 'salt' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dsvctSalt :: Lens.Lens' DeviceSecretVerifierConfigType (Core.Maybe Types.Salt)
dsvctSalt = Lens.field @"salt"
{-# DEPRECATED dsvctSalt "Use generic-lens or generic-optics with 'salt' instead." #-}

instance Core.FromJSON DeviceSecretVerifierConfigType where
  toJSON DeviceSecretVerifierConfigType {..} =
    Core.object
      ( Core.catMaybes
          [ ("PasswordVerifier" Core..=) Core.<$> passwordVerifier,
            ("Salt" Core..=) Core.<$> salt
          ]
      )
