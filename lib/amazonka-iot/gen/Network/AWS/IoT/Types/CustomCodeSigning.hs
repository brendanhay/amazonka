-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CustomCodeSigning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CustomCodeSigning
  ( CustomCodeSigning (..),

    -- * Smart constructor
    mkCustomCodeSigning,

    -- * Lenses
    ccsSignature,
    ccsHashAlgorithm,
    ccsCertificateChain,
    ccsSignatureAlgorithm,
  )
where

import Network.AWS.IoT.Types.CodeSigningCertificateChain
import Network.AWS.IoT.Types.CodeSigningSignature
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a custom method used to code sign a file.
--
-- /See:/ 'mkCustomCodeSigning' smart constructor.
data CustomCodeSigning = CustomCodeSigning'
  { signature ::
      Lude.Maybe CodeSigningSignature,
    hashAlgorithm :: Lude.Maybe Lude.Text,
    certificateChain ::
      Lude.Maybe CodeSigningCertificateChain,
    signatureAlgorithm :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CustomCodeSigning' with the minimum fields required to make a request.
--
-- * 'certificateChain' - The certificate chain.
-- * 'hashAlgorithm' - The hash algorithm used to code sign the file.
-- * 'signature' - The signature for the file.
-- * 'signatureAlgorithm' - The signature algorithm used to code sign the file.
mkCustomCodeSigning ::
  CustomCodeSigning
mkCustomCodeSigning =
  CustomCodeSigning'
    { signature = Lude.Nothing,
      hashAlgorithm = Lude.Nothing,
      certificateChain = Lude.Nothing,
      signatureAlgorithm = Lude.Nothing
    }

-- | The signature for the file.
--
-- /Note:/ Consider using 'signature' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsSignature :: Lens.Lens' CustomCodeSigning (Lude.Maybe CodeSigningSignature)
ccsSignature = Lens.lens (signature :: CustomCodeSigning -> Lude.Maybe CodeSigningSignature) (\s a -> s {signature = a} :: CustomCodeSigning)
{-# DEPRECATED ccsSignature "Use generic-lens or generic-optics with 'signature' instead." #-}

-- | The hash algorithm used to code sign the file.
--
-- /Note:/ Consider using 'hashAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsHashAlgorithm :: Lens.Lens' CustomCodeSigning (Lude.Maybe Lude.Text)
ccsHashAlgorithm = Lens.lens (hashAlgorithm :: CustomCodeSigning -> Lude.Maybe Lude.Text) (\s a -> s {hashAlgorithm = a} :: CustomCodeSigning)
{-# DEPRECATED ccsHashAlgorithm "Use generic-lens or generic-optics with 'hashAlgorithm' instead." #-}

-- | The certificate chain.
--
-- /Note:/ Consider using 'certificateChain' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsCertificateChain :: Lens.Lens' CustomCodeSigning (Lude.Maybe CodeSigningCertificateChain)
ccsCertificateChain = Lens.lens (certificateChain :: CustomCodeSigning -> Lude.Maybe CodeSigningCertificateChain) (\s a -> s {certificateChain = a} :: CustomCodeSigning)
{-# DEPRECATED ccsCertificateChain "Use generic-lens or generic-optics with 'certificateChain' instead." #-}

-- | The signature algorithm used to code sign the file.
--
-- /Note:/ Consider using 'signatureAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ccsSignatureAlgorithm :: Lens.Lens' CustomCodeSigning (Lude.Maybe Lude.Text)
ccsSignatureAlgorithm = Lens.lens (signatureAlgorithm :: CustomCodeSigning -> Lude.Maybe Lude.Text) (\s a -> s {signatureAlgorithm = a} :: CustomCodeSigning)
{-# DEPRECATED ccsSignatureAlgorithm "Use generic-lens or generic-optics with 'signatureAlgorithm' instead." #-}

instance Lude.FromJSON CustomCodeSigning where
  parseJSON =
    Lude.withObject
      "CustomCodeSigning"
      ( \x ->
          CustomCodeSigning'
            Lude.<$> (x Lude..:? "signature")
            Lude.<*> (x Lude..:? "hashAlgorithm")
            Lude.<*> (x Lude..:? "certificateChain")
            Lude.<*> (x Lude..:? "signatureAlgorithm")
      )

instance Lude.ToJSON CustomCodeSigning where
  toJSON CustomCodeSigning' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("signature" Lude..=) Lude.<$> signature,
            ("hashAlgorithm" Lude..=) Lude.<$> hashAlgorithm,
            ("certificateChain" Lude..=) Lude.<$> certificateChain,
            ("signatureAlgorithm" Lude..=) Lude.<$> signatureAlgorithm
          ]
      )
