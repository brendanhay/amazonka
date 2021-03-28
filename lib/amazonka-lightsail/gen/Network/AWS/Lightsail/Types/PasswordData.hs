{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PasswordData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.Lightsail.Types.PasswordData
  ( PasswordData (..)
  -- * Smart constructor
  , mkPasswordData
  -- * Lenses
  , pdCiphertext
  , pdKeyPairName
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Lightsail.Types.ResourceName as Types
import qualified Network.AWS.Prelude as Core

-- | The password data for the Windows Server-based instance, including the ciphertext and the key pair name.
--
-- /See:/ 'mkPasswordData' smart constructor.
data PasswordData = PasswordData'
  { ciphertext :: Core.Maybe Core.Text
    -- ^ The encrypted password. Ciphertext will be an empty string if access to your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
  , keyPairName :: Core.Maybe Types.ResourceName
    -- ^ The name of the key pair that you used when creating your instance. If no key pair name was specified when creating the instance, Lightsail uses the default key pair (@LightsailDefaultKeyPair@ ).
--
-- If you are using a custom key pair, you need to use your own means of decrypting your password using the @ciphertext@ . Lightsail creates the ciphertext by encrypting your password with the public key part of this key pair.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PasswordData' value with any optional fields omitted.
mkPasswordData
    :: PasswordData
mkPasswordData
  = PasswordData'{ciphertext = Core.Nothing,
                  keyPairName = Core.Nothing}

-- | The encrypted password. Ciphertext will be an empty string if access to your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
--
-- /Note:/ Consider using 'ciphertext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCiphertext :: Lens.Lens' PasswordData (Core.Maybe Core.Text)
pdCiphertext = Lens.field @"ciphertext"
{-# INLINEABLE pdCiphertext #-}
{-# DEPRECATED ciphertext "Use generic-lens or generic-optics with 'ciphertext' instead"  #-}

-- | The name of the key pair that you used when creating your instance. If no key pair name was specified when creating the instance, Lightsail uses the default key pair (@LightsailDefaultKeyPair@ ).
--
-- If you are using a custom key pair, you need to use your own means of decrypting your password using the @ciphertext@ . Lightsail creates the ciphertext by encrypting your password with the public key part of this key pair.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdKeyPairName :: Lens.Lens' PasswordData (Core.Maybe Types.ResourceName)
pdKeyPairName = Lens.field @"keyPairName"
{-# INLINEABLE pdKeyPairName #-}
{-# DEPRECATED keyPairName "Use generic-lens or generic-optics with 'keyPairName' instead"  #-}

instance Core.FromJSON PasswordData where
        parseJSON
          = Core.withObject "PasswordData" Core.$
              \ x ->
                PasswordData' Core.<$>
                  (x Core..:? "ciphertext") Core.<*> x Core..:? "keyPairName"
