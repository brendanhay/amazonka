{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DMS.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.DMS.Types.Certificate
  ( Certificate (..)
  -- * Smart constructor
  , mkCertificate
  -- * Lenses
  , cCertificateArn
  , cCertificateCreationDate
  , cCertificateIdentifier
  , cCertificateOwner
  , cCertificatePem
  , cCertificateWallet
  , cKeyLength
  , cSigningAlgorithm
  , cValidFromDate
  , cValidToDate
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | The SSL certificate that can be used to encrypt connections between the endpoints and the replication instance.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { certificateArn :: Core.Maybe Core.Text
    -- ^ The Amazon Resource Name (ARN) for the certificate.
  , certificateCreationDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the certificate was created.
  , certificateIdentifier :: Core.Maybe Core.Text
    -- ^ A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
  , certificateOwner :: Core.Maybe Core.Text
    -- ^ The owner of the certificate.
  , certificatePem :: Core.Maybe Core.Text
    -- ^ The contents of a @.pem@ file, which contains an X.509 certificate.
  , certificateWallet :: Core.Maybe Core.Base64
    -- ^ The location of an imported Oracle Wallet certificate for use with SSL.
  , keyLength :: Core.Maybe Core.Int
    -- ^ The key length of the cryptographic algorithm being used.
  , signingAlgorithm :: Core.Maybe Core.Text
    -- ^ The signing algorithm for the certificate.
  , validFromDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The beginning date that the certificate is valid.
  , validToDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The final date that the certificate is valid.
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Certificate' value with any optional fields omitted.
mkCertificate
    :: Certificate
mkCertificate
  = Certificate'{certificateArn = Core.Nothing,
                 certificateCreationDate = Core.Nothing,
                 certificateIdentifier = Core.Nothing,
                 certificateOwner = Core.Nothing, certificatePem = Core.Nothing,
                 certificateWallet = Core.Nothing, keyLength = Core.Nothing,
                 signingAlgorithm = Core.Nothing, validFromDate = Core.Nothing,
                 validToDate = Core.Nothing}

-- | The Amazon Resource Name (ARN) for the certificate.
--
-- /Note:/ Consider using 'certificateArn' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateArn :: Lens.Lens' Certificate (Core.Maybe Core.Text)
cCertificateArn = Lens.field @"certificateArn"
{-# INLINEABLE cCertificateArn #-}
{-# DEPRECATED certificateArn "Use generic-lens or generic-optics with 'certificateArn' instead"  #-}

-- | The date that the certificate was created.
--
-- /Note:/ Consider using 'certificateCreationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateCreationDate :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cCertificateCreationDate = Lens.field @"certificateCreationDate"
{-# INLINEABLE cCertificateCreationDate #-}
{-# DEPRECATED certificateCreationDate "Use generic-lens or generic-optics with 'certificateCreationDate' instead"  #-}

-- | A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateIdentifier :: Lens.Lens' Certificate (Core.Maybe Core.Text)
cCertificateIdentifier = Lens.field @"certificateIdentifier"
{-# INLINEABLE cCertificateIdentifier #-}
{-# DEPRECATED certificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead"  #-}

-- | The owner of the certificate.
--
-- /Note:/ Consider using 'certificateOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateOwner :: Lens.Lens' Certificate (Core.Maybe Core.Text)
cCertificateOwner = Lens.field @"certificateOwner"
{-# INLINEABLE cCertificateOwner #-}
{-# DEPRECATED certificateOwner "Use generic-lens or generic-optics with 'certificateOwner' instead"  #-}

-- | The contents of a @.pem@ file, which contains an X.509 certificate.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificatePem :: Lens.Lens' Certificate (Core.Maybe Core.Text)
cCertificatePem = Lens.field @"certificatePem"
{-# INLINEABLE cCertificatePem #-}
{-# DEPRECATED certificatePem "Use generic-lens or generic-optics with 'certificatePem' instead"  #-}

-- | The location of an imported Oracle Wallet certificate for use with SSL.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificateWallet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateWallet :: Lens.Lens' Certificate (Core.Maybe Core.Base64)
cCertificateWallet = Lens.field @"certificateWallet"
{-# INLINEABLE cCertificateWallet #-}
{-# DEPRECATED certificateWallet "Use generic-lens or generic-optics with 'certificateWallet' instead"  #-}

-- | The key length of the cryptographic algorithm being used.
--
-- /Note:/ Consider using 'keyLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKeyLength :: Lens.Lens' Certificate (Core.Maybe Core.Int)
cKeyLength = Lens.field @"keyLength"
{-# INLINEABLE cKeyLength #-}
{-# DEPRECATED keyLength "Use generic-lens or generic-optics with 'keyLength' instead"  #-}

-- | The signing algorithm for the certificate.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSigningAlgorithm :: Lens.Lens' Certificate (Core.Maybe Core.Text)
cSigningAlgorithm = Lens.field @"signingAlgorithm"
{-# INLINEABLE cSigningAlgorithm #-}
{-# DEPRECATED signingAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead"  #-}

-- | The beginning date that the certificate is valid.
--
-- /Note:/ Consider using 'validFromDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValidFromDate :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cValidFromDate = Lens.field @"validFromDate"
{-# INLINEABLE cValidFromDate #-}
{-# DEPRECATED validFromDate "Use generic-lens or generic-optics with 'validFromDate' instead"  #-}

-- | The final date that the certificate is valid.
--
-- /Note:/ Consider using 'validToDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValidToDate :: Lens.Lens' Certificate (Core.Maybe Core.NominalDiffTime)
cValidToDate = Lens.field @"validToDate"
{-# INLINEABLE cValidToDate #-}
{-# DEPRECATED validToDate "Use generic-lens or generic-optics with 'validToDate' instead"  #-}

instance Core.FromJSON Certificate where
        parseJSON
          = Core.withObject "Certificate" Core.$
              \ x ->
                Certificate' Core.<$>
                  (x Core..:? "CertificateArn") Core.<*>
                    x Core..:? "CertificateCreationDate"
                    Core.<*> x Core..:? "CertificateIdentifier"
                    Core.<*> x Core..:? "CertificateOwner"
                    Core.<*> x Core..:? "CertificatePem"
                    Core.<*> x Core..:? "CertificateWallet"
                    Core.<*> x Core..:? "KeyLength"
                    Core.<*> x Core..:? "SigningAlgorithm"
                    Core.<*> x Core..:? "ValidFromDate"
                    Core.<*> x Core..:? "ValidToDate"
