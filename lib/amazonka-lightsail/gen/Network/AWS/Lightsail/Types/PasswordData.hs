{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.Lightsail.Types.PasswordData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.Lightsail.Types.PasswordData
  ( PasswordData (..),

    -- * Smart constructor
    mkPasswordData,

    -- * Lenses
    pdKeyPairName,
    pdCiphertext,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The password data for the Windows Server-based instance, including the ciphertext and the key pair name.
--
-- /See:/ 'mkPasswordData' smart constructor.
data PasswordData = PasswordData'
  { keyPairName ::
      Lude.Maybe Lude.Text,
    ciphertext :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PasswordData' with the minimum fields required to make a request.
--
-- * 'ciphertext' - The encrypted password. Ciphertext will be an empty string if access to your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
-- * 'keyPairName' - The name of the key pair that you used when creating your instance. If no key pair name was specified when creating the instance, Lightsail uses the default key pair (@LightsailDefaultKeyPair@ ).
--
-- If you are using a custom key pair, you need to use your own means of decrypting your password using the @ciphertext@ . Lightsail creates the ciphertext by encrypting your password with the public key part of this key pair.
mkPasswordData ::
  PasswordData
mkPasswordData =
  PasswordData'
    { keyPairName = Lude.Nothing,
      ciphertext = Lude.Nothing
    }

-- | The name of the key pair that you used when creating your instance. If no key pair name was specified when creating the instance, Lightsail uses the default key pair (@LightsailDefaultKeyPair@ ).
--
-- If you are using a custom key pair, you need to use your own means of decrypting your password using the @ciphertext@ . Lightsail creates the ciphertext by encrypting your password with the public key part of this key pair.
--
-- /Note:/ Consider using 'keyPairName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdKeyPairName :: Lens.Lens' PasswordData (Lude.Maybe Lude.Text)
pdKeyPairName = Lens.lens (keyPairName :: PasswordData -> Lude.Maybe Lude.Text) (\s a -> s {keyPairName = a} :: PasswordData)
{-# DEPRECATED pdKeyPairName "Use generic-lens or generic-optics with 'keyPairName' instead." #-}

-- | The encrypted password. Ciphertext will be an empty string if access to your new instance is not ready yet. When you create an instance, it can take up to 15 minutes for the instance to be ready.
--
-- /Note:/ Consider using 'ciphertext' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pdCiphertext :: Lens.Lens' PasswordData (Lude.Maybe Lude.Text)
pdCiphertext = Lens.lens (ciphertext :: PasswordData -> Lude.Maybe Lude.Text) (\s a -> s {ciphertext = a} :: PasswordData)
{-# DEPRECATED pdCiphertext "Use generic-lens or generic-optics with 'ciphertext' instead." #-}

instance Lude.FromJSON PasswordData where
  parseJSON =
    Lude.withObject
      "PasswordData"
      ( \x ->
          PasswordData'
            Lude.<$> (x Lude..:? "keyPairName") Lude.<*> (x Lude..:? "ciphertext")
      )
