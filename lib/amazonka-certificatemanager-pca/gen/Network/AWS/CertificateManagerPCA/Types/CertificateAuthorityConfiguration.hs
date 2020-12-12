{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.CertificateManagerPCA.Types.CertificateAuthorityConfiguration
  ( CertificateAuthorityConfiguration (..),

    -- * Smart constructor
    mkCertificateAuthorityConfiguration,

    -- * Lenses
    cacKeyAlgorithm,
    cacSigningAlgorithm,
    cacSubject,
  )
where

import Network.AWS.CertificateManagerPCA.Types.ASN1Subject
import Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm
import Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains configuration information for your private certificate authority (CA). This includes information about the class of public key algorithm and the key pair that your private CA creates when it issues a certificate. It also includes the signature algorithm that it uses when issuing certificates, and its X.500 distinguished name. You must specify this information when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action.
--
-- /See:/ 'mkCertificateAuthorityConfiguration' smart constructor.
data CertificateAuthorityConfiguration = CertificateAuthorityConfiguration'
  { keyAlgorithm ::
      KeyAlgorithm,
    signingAlgorithm ::
      SigningAlgorithm,
    subject :: ASN1Subject
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateAuthorityConfiguration' with the minimum fields required to make a request.
--
-- * 'keyAlgorithm' - Type of the public key algorithm and size, in bits, of the key pair that your CA creates when it issues a certificate. When you create a subordinate CA, you must use a key algorithm supported by the parent CA.
-- * 'signingAlgorithm' - Name of the algorithm your private CA uses to sign certificate requests.
--
-- This parameter should not be confused with the @SigningAlgorithm@ parameter used to sign certificates when they are issued.
-- * 'subject' - Structure that contains X.500 distinguished name information for your private CA.
mkCertificateAuthorityConfiguration ::
  -- | 'keyAlgorithm'
  KeyAlgorithm ->
  -- | 'signingAlgorithm'
  SigningAlgorithm ->
  -- | 'subject'
  ASN1Subject ->
  CertificateAuthorityConfiguration
mkCertificateAuthorityConfiguration
  pKeyAlgorithm_
  pSigningAlgorithm_
  pSubject_ =
    CertificateAuthorityConfiguration'
      { keyAlgorithm = pKeyAlgorithm_,
        signingAlgorithm = pSigningAlgorithm_,
        subject = pSubject_
      }

-- | Type of the public key algorithm and size, in bits, of the key pair that your CA creates when it issues a certificate. When you create a subordinate CA, you must use a key algorithm supported by the parent CA.
--
-- /Note:/ Consider using 'keyAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacKeyAlgorithm :: Lens.Lens' CertificateAuthorityConfiguration KeyAlgorithm
cacKeyAlgorithm = Lens.lens (keyAlgorithm :: CertificateAuthorityConfiguration -> KeyAlgorithm) (\s a -> s {keyAlgorithm = a} :: CertificateAuthorityConfiguration)
{-# DEPRECATED cacKeyAlgorithm "Use generic-lens or generic-optics with 'keyAlgorithm' instead." #-}

-- | Name of the algorithm your private CA uses to sign certificate requests.
--
-- This parameter should not be confused with the @SigningAlgorithm@ parameter used to sign certificates when they are issued.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacSigningAlgorithm :: Lens.Lens' CertificateAuthorityConfiguration SigningAlgorithm
cacSigningAlgorithm = Lens.lens (signingAlgorithm :: CertificateAuthorityConfiguration -> SigningAlgorithm) (\s a -> s {signingAlgorithm = a} :: CertificateAuthorityConfiguration)
{-# DEPRECATED cacSigningAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead." #-}

-- | Structure that contains X.500 distinguished name information for your private CA.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacSubject :: Lens.Lens' CertificateAuthorityConfiguration ASN1Subject
cacSubject = Lens.lens (subject :: CertificateAuthorityConfiguration -> ASN1Subject) (\s a -> s {subject = a} :: CertificateAuthorityConfiguration)
{-# DEPRECATED cacSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

instance Lude.FromJSON CertificateAuthorityConfiguration where
  parseJSON =
    Lude.withObject
      "CertificateAuthorityConfiguration"
      ( \x ->
          CertificateAuthorityConfiguration'
            Lude.<$> (x Lude..: "KeyAlgorithm")
            Lude.<*> (x Lude..: "SigningAlgorithm")
            Lude.<*> (x Lude..: "Subject")
      )

instance Lude.ToJSON CertificateAuthorityConfiguration where
  toJSON CertificateAuthorityConfiguration' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("KeyAlgorithm" Lude..= keyAlgorithm),
            Lude.Just ("SigningAlgorithm" Lude..= signingAlgorithm),
            Lude.Just ("Subject" Lude..= subject)
          ]
      )
