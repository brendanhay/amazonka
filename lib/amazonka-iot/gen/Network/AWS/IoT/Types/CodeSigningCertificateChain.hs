{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CodeSigningCertificateChain
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CodeSigningCertificateChain
  ( CodeSigningCertificateChain (..),

    -- * Smart constructor
    mkCodeSigningCertificateChain,

    -- * Lenses
    csccCertificateName,
    csccInlineDocument,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes the certificate chain being used when code signing a file.
--
-- /See:/ 'mkCodeSigningCertificateChain' smart constructor.
data CodeSigningCertificateChain = CodeSigningCertificateChain'
  { certificateName ::
      Lude.Maybe Lude.Text,
    inlineDocument ::
      Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CodeSigningCertificateChain' with the minimum fields required to make a request.
--
-- * 'certificateName' - The name of the certificate.
-- * 'inlineDocument' - A base64 encoded binary representation of the code signing certificate chain.
mkCodeSigningCertificateChain ::
  CodeSigningCertificateChain
mkCodeSigningCertificateChain =
  CodeSigningCertificateChain'
    { certificateName = Lude.Nothing,
      inlineDocument = Lude.Nothing
    }

-- | The name of the certificate.
--
-- /Note:/ Consider using 'certificateName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csccCertificateName :: Lens.Lens' CodeSigningCertificateChain (Lude.Maybe Lude.Text)
csccCertificateName = Lens.lens (certificateName :: CodeSigningCertificateChain -> Lude.Maybe Lude.Text) (\s a -> s {certificateName = a} :: CodeSigningCertificateChain)
{-# DEPRECATED csccCertificateName "Use generic-lens or generic-optics with 'certificateName' instead." #-}

-- | A base64 encoded binary representation of the code signing certificate chain.
--
-- /Note:/ Consider using 'inlineDocument' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
csccInlineDocument :: Lens.Lens' CodeSigningCertificateChain (Lude.Maybe Lude.Text)
csccInlineDocument = Lens.lens (inlineDocument :: CodeSigningCertificateChain -> Lude.Maybe Lude.Text) (\s a -> s {inlineDocument = a} :: CodeSigningCertificateChain)
{-# DEPRECATED csccInlineDocument "Use generic-lens or generic-optics with 'inlineDocument' instead." #-}

instance Lude.FromJSON CodeSigningCertificateChain where
  parseJSON =
    Lude.withObject
      "CodeSigningCertificateChain"
      ( \x ->
          CodeSigningCertificateChain'
            Lude.<$> (x Lude..:? "certificateName")
            Lude.<*> (x Lude..:? "inlineDocument")
      )

instance Lude.ToJSON CodeSigningCertificateChain where
  toJSON CodeSigningCertificateChain' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("certificateName" Lude..=) Lude.<$> certificateName,
            ("inlineDocument" Lude..=) Lude.<$> inlineDocument
          ]
      )
