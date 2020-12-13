{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.OpsWorks.Types.SSLConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.OpsWorks.Types.SSLConfiguration
  ( SSLConfiguration (..),

    -- * Smart constructor
    mkSSLConfiguration,

    -- * Lenses
    scPrivateKey,
    scCertificate,
    scChain,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes an app's SSL configuration.
--
-- /See:/ 'mkSSLConfiguration' smart constructor.
data SSLConfiguration = SSLConfiguration'
  { -- | The private key; the contents of the certificate's domain.kex file.
    privateKey :: Lude.Maybe Lude.Text,
    -- | The contents of the certificate's domain.crt file.
    certificate :: Lude.Maybe Lude.Text,
    -- | Optional. Can be used to specify an intermediate certificate authority key or client authentication.
    chain :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'SSLConfiguration' with the minimum fields required to make a request.
--
-- * 'privateKey' - The private key; the contents of the certificate's domain.kex file.
-- * 'certificate' - The contents of the certificate's domain.crt file.
-- * 'chain' - Optional. Can be used to specify an intermediate certificate authority key or client authentication.
mkSSLConfiguration ::
  SSLConfiguration
mkSSLConfiguration =
  SSLConfiguration'
    { privateKey = Lude.Nothing,
      certificate = Lude.Nothing,
      chain = Lude.Nothing
    }

-- | The private key; the contents of the certificate's domain.kex file.
--
-- /Note:/ Consider using 'privateKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scPrivateKey :: Lens.Lens' SSLConfiguration (Lude.Maybe Lude.Text)
scPrivateKey = Lens.lens (privateKey :: SSLConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {privateKey = a} :: SSLConfiguration)
{-# DEPRECATED scPrivateKey "Use generic-lens or generic-optics with 'privateKey' instead." #-}

-- | The contents of the certificate's domain.crt file.
--
-- /Note:/ Consider using 'certificate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scCertificate :: Lens.Lens' SSLConfiguration (Lude.Maybe Lude.Text)
scCertificate = Lens.lens (certificate :: SSLConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {certificate = a} :: SSLConfiguration)
{-# DEPRECATED scCertificate "Use generic-lens or generic-optics with 'certificate' instead." #-}

-- | Optional. Can be used to specify an intermediate certificate authority key or client authentication.
--
-- /Note:/ Consider using 'chain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
scChain :: Lens.Lens' SSLConfiguration (Lude.Maybe Lude.Text)
scChain = Lens.lens (chain :: SSLConfiguration -> Lude.Maybe Lude.Text) (\s a -> s {chain = a} :: SSLConfiguration)
{-# DEPRECATED scChain "Use generic-lens or generic-optics with 'chain' instead." #-}

instance Lude.FromJSON SSLConfiguration where
  parseJSON =
    Lude.withObject
      "SSLConfiguration"
      ( \x ->
          SSLConfiguration'
            Lude.<$> (x Lude..:? "PrivateKey")
            Lude.<*> (x Lude..:? "Certificate")
            Lude.<*> (x Lude..:? "Chain")
      )

instance Lude.ToJSON SSLConfiguration where
  toJSON SSLConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("PrivateKey" Lude..=) Lude.<$> privateKey,
            ("Certificate" Lude..=) Lude.<$> certificate,
            ("Chain" Lude..=) Lude.<$> chain
          ]
      )
