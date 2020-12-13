{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.KeyPair
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.KeyPair
  ( KeyPair (..),

    -- * Smart constructor
    mkKeyPair,

    -- * Lenses
    kpPrivateKey,
    kpPublicKey,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a key pair.
--
-- /See:/ 'mkKeyPair' smart constructor.
data KeyPair = KeyPair'
  { -- | The private key.
    privateKey :: Lude.Maybe (Lude.Sensitive Lude.Text),
    -- | The public key.
    publicKey :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'KeyPair' with the minimum fields required to make a request.
--
-- * 'privateKey' - The private key.
-- * 'publicKey' - The public key.
mkKeyPair ::
  KeyPair
mkKeyPair =
  KeyPair' {privateKey = Lude.Nothing, publicKey = Lude.Nothing}

-- | The private key.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpPrivateKey :: Lens.Lens' KeyPair (Lude.Maybe (Lude.Sensitive Lude.Text))
kpPrivateKey = Lens.lens (privateKey :: KeyPair -> Lude.Maybe (Lude.Sensitive Lude.Text)) (\s a -> s {privateKey = a} :: KeyPair)
{-# DEPRECATED kpPrivateKey "Use generic-lens or generic-optics with 'privateKey' instead." #-}

-- | The public key.
--
-- /Note:/ Consider using 'publicKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
kpPublicKey :: Lens.Lens' KeyPair (Lude.Maybe Lude.Text)
kpPublicKey = Lens.lens (publicKey :: KeyPair -> Lude.Maybe Lude.Text) (\s a -> s {publicKey = a} :: KeyPair)
{-# DEPRECATED kpPublicKey "Use generic-lens or generic-optics with 'publicKey' instead." #-}

instance Lude.FromJSON KeyPair where
  parseJSON =
    Lude.withObject
      "KeyPair"
      ( \x ->
          KeyPair'
            Lude.<$> (x Lude..:? "PrivateKey") Lude.<*> (x Lude..:? "PublicKey")
      )
