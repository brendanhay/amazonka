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

import qualified Network.AWS.CertificateManagerPCA.Types.ASN1Subject as Types
import qualified Network.AWS.CertificateManagerPCA.Types.KeyAlgorithm as Types
import qualified Network.AWS.CertificateManagerPCA.Types.SigningAlgorithm as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Contains configuration information for your private certificate authority (CA). This includes information about the class of public key algorithm and the key pair that your private CA creates when it issues a certificate. It also includes the signature algorithm that it uses when issuing certificates, and its X.500 distinguished name. You must specify this information when you call the <https://docs.aws.amazon.com/acm-pca/latest/APIReference/API_CreateCertificateAuthority.html CreateCertificateAuthority> action.
--
-- /See:/ 'mkCertificateAuthorityConfiguration' smart constructor.
data CertificateAuthorityConfiguration = CertificateAuthorityConfiguration'
  { -- | Type of the public key algorithm and size, in bits, of the key pair that your CA creates when it issues a certificate. When you create a subordinate CA, you must use a key algorithm supported by the parent CA.
    keyAlgorithm :: Types.KeyAlgorithm,
    -- | Name of the algorithm your private CA uses to sign certificate requests.
    --
    -- This parameter should not be confused with the @SigningAlgorithm@ parameter used to sign certificates when they are issued.
    signingAlgorithm :: Types.SigningAlgorithm,
    -- | Structure that contains X.500 distinguished name information for your private CA.
    subject :: Types.ASN1Subject
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'CertificateAuthorityConfiguration' value with any optional fields omitted.
mkCertificateAuthorityConfiguration ::
  -- | 'keyAlgorithm'
  Types.KeyAlgorithm ->
  -- | 'signingAlgorithm'
  Types.SigningAlgorithm ->
  -- | 'subject'
  Types.ASN1Subject ->
  CertificateAuthorityConfiguration
mkCertificateAuthorityConfiguration
  keyAlgorithm
  signingAlgorithm
  subject =
    CertificateAuthorityConfiguration'
      { keyAlgorithm,
        signingAlgorithm,
        subject
      }

-- | Type of the public key algorithm and size, in bits, of the key pair that your CA creates when it issues a certificate. When you create a subordinate CA, you must use a key algorithm supported by the parent CA.
--
-- /Note:/ Consider using 'keyAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacKeyAlgorithm :: Lens.Lens' CertificateAuthorityConfiguration Types.KeyAlgorithm
cacKeyAlgorithm = Lens.field @"keyAlgorithm"
{-# DEPRECATED cacKeyAlgorithm "Use generic-lens or generic-optics with 'keyAlgorithm' instead." #-}

-- | Name of the algorithm your private CA uses to sign certificate requests.
--
-- This parameter should not be confused with the @SigningAlgorithm@ parameter used to sign certificates when they are issued.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacSigningAlgorithm :: Lens.Lens' CertificateAuthorityConfiguration Types.SigningAlgorithm
cacSigningAlgorithm = Lens.field @"signingAlgorithm"
{-# DEPRECATED cacSigningAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead." #-}

-- | Structure that contains X.500 distinguished name information for your private CA.
--
-- /Note:/ Consider using 'subject' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacSubject :: Lens.Lens' CertificateAuthorityConfiguration Types.ASN1Subject
cacSubject = Lens.field @"subject"
{-# DEPRECATED cacSubject "Use generic-lens or generic-optics with 'subject' instead." #-}

instance Core.FromJSON CertificateAuthorityConfiguration where
  toJSON CertificateAuthorityConfiguration {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("KeyAlgorithm" Core..= keyAlgorithm),
            Core.Just ("SigningAlgorithm" Core..= signingAlgorithm),
            Core.Just ("Subject" Core..= subject)
          ]
      )

instance Core.FromJSON CertificateAuthorityConfiguration where
  parseJSON =
    Core.withObject "CertificateAuthorityConfiguration" Core.$
      \x ->
        CertificateAuthorityConfiguration'
          Core.<$> (x Core..: "KeyAlgorithm")
          Core.<*> (x Core..: "SigningAlgorithm")
          Core.<*> (x Core..: "Subject")
