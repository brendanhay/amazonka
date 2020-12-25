{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DMS.Types.Certificate
  ( Certificate (..),

    -- * Smart constructor
    mkCertificate,

    -- * Lenses
    cCertificateArn,
    cCertificateCreationDate,
    cCertificateIdentifier,
    cCertificateOwner,
    cCertificatePem,
    cCertificateWallet,
    cKeyLength,
    cSigningAlgorithm,
    cValidFromDate,
    cValidToDate,
  )
where

import qualified Network.AWS.DMS.Types.String as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The SSL certificate that can be used to encrypt connections between the endpoints and the replication instance.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { -- | The Amazon Resource Name (ARN) for the certificate.
    certificateArn :: Core.Maybe Types.String,
    -- | The date that the certificate was created.
    certificateCreationDate :: Core.Maybe Core.NominalDiffTime,
    -- | A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
    certificateIdentifier :: Core.Maybe Types.String,
    -- | The owner of the certificate.
    certificateOwner :: Core.Maybe Types.String,
    -- | The contents of a @.pem@ file, which contains an X.509 certificate.
    certificatePem :: Core.Maybe Types.String,
    -- | The location of an imported Oracle Wallet certificate for use with SSL.
    certificateWallet :: Core.Maybe Core.Base64,
    -- | The key length of the cryptographic algorithm being used.
    keyLength :: Core.Maybe Core.Int,
    -- | The signing algorithm for the certificate.
    signingAlgorithm :: Core.Maybe Types.String,
    -- | The beginning date that the certificate is valid.
    validFromDate :: Core.Maybe Core.NominalDiffTime,
    -- | The final date that the certificate is valid.
    validToDate :: Core.Maybe Core.NominalDiffTime
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'Certificate' value with any optional fields omitted.
mkCertificate ::
  Certificate
mkCertificate =
  Certificate'
    { certificateArn = Core.Nothing,
      certificateCreationDate = Core.Nothing,
      certificateIdentifier = Core.Nothing,
      certificateOwner = Core.Nothing,
      certificatePem = Core.Nothing,
      certificateWallet = Core.Nothing,
      keyLength = Core.Nothing,
      signingAlgorithm = Core.Nothing,
      validFromDate = Core.Nothing,
      validToDate = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) for the certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateArn :: Lens.Lens' Certificate (Core.Maybe Types.String)
cCertificateArn = Lens.field @"certificateArn"
{-# DEPRECATED cCertificateArn "Use generic-lens or generic-optics with 'certificateArn' instead." #-}

-- | The date that the certificate was created.
--
-- /Note:/ Consider using 'certificateCreationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateCreationDate :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cCertificateCreationDate = Lens.field @"certificateCreationDate"
{-# DEPRECATED cCertificateCreationDate "Use generic-lens or generic-optics with 'certificateCreationDate' instead." #-}

-- | A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateIdentifier :: Lens.Lens' Certificate (Core.Maybe Types.String)
cCertificateIdentifier = Lens.field @"certificateIdentifier"
{-# DEPRECATED cCertificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead." #-}

-- | The owner of the certificate.
--
-- /Note:/ Consider using 'certificateOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateOwner :: Lens.Lens' Certificate (Core.Maybe Types.String)
cCertificateOwner = Lens.field @"certificateOwner"
{-# DEPRECATED cCertificateOwner "Use generic-lens or generic-optics with 'certificateOwner' instead." #-}

-- | The contents of a @.pem@ file, which contains an X.509 certificate.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificatePem :: Lens.Lens' Certificate (Core.Maybe Types.String)
cCertificatePem = Lens.field @"certificatePem"
{-# DEPRECATED cCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The location of an imported Oracle Wallet certificate for use with SSL.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificateWallet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateWallet :: Lens.Lens' Certificate (Core.Maybe Core.Base64)
cCertificateWallet = Lens.field @"certificateWallet"
{-# DEPRECATED cCertificateWallet "Use generic-lens or generic-optics with 'certificateWallet' instead." #-}

-- | The key length of the cryptographic algorithm being used.
--
-- /Note:/ Consider using 'keyLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKeyLength :: Lens.Lens' Certificate (Core.Maybe Core.Int)
cKeyLength = Lens.field @"keyLength"
{-# DEPRECATED cKeyLength "Use generic-lens or generic-optics with 'keyLength' instead." #-}

-- | The signing algorithm for the certificate.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSigningAlgorithm :: Lens.Lens' Certificate (Core.Maybe Types.String)
cSigningAlgorithm = Lens.field @"signingAlgorithm"
{-# DEPRECATED cSigningAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead." #-}

-- | The beginning date that the certificate is valid.
--
-- /Note:/ Consider using 'validFromDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValidFromDate :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cValidFromDate = Lens.field @"validFromDate"
{-# DEPRECATED cValidFromDate "Use generic-lens or generic-optics with 'validFromDate' instead." #-}

-- | The final date that the certificate is valid.
--
-- /Note:/ Consider using 'validToDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValidToDate :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cValidToDate = Lens.field @"validToDate"
{-# DEPRECATED cValidToDate "Use generic-lens or generic-optics with 'validToDate' instead." #-}

instance Core.FromJSON Certificate where
  parseJSON =
    Core.withObject "Certificate" Core.$
      \x ->
        Certificate'
          Core.<$> (x Core..:? "CertificateArn")
          Core.<*> (x Core..:? "CertificateCreationDate")
          Core.<*> (x Core..:? "CertificateIdentifier")
          Core.<*> (x Core..:? "CertificateOwner")
          Core.<*> (x Core..:? "CertificatePem")
          Core.<*> (x Core..:? "CertificateWallet")
          Core.<*> (x Core..:? "KeyLength")
          Core.<*> (x Core..:? "SigningAlgorithm")
          Core.<*> (x Core..:? "ValidFromDate")
          Core.<*> (x Core..:? "ValidToDate")
