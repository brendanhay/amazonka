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
    cCertificateOwner,
    cSigningAlgorithm,
    cValidFromDate,
    cCertificatePem,
    cCertificateARN,
    cCertificateCreationDate,
    cCertificateIdentifier,
    cCertificateWallet,
    cKeyLength,
    cValidToDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | The SSL certificate that can be used to encrypt connections between the endpoints and the replication instance.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { certificateOwner ::
      Lude.Maybe Lude.Text,
    signingAlgorithm :: Lude.Maybe Lude.Text,
    validFromDate :: Lude.Maybe Lude.Timestamp,
    certificatePem :: Lude.Maybe Lude.Text,
    certificateARN :: Lude.Maybe Lude.Text,
    certificateCreationDate :: Lude.Maybe Lude.Timestamp,
    certificateIdentifier :: Lude.Maybe Lude.Text,
    certificateWallet :: Lude.Maybe Lude.Base64,
    keyLength :: Lude.Maybe Lude.Int,
    validToDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Certificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The Amazon Resource Name (ARN) for the certificate.
-- * 'certificateCreationDate' - The date that the certificate was created.
-- * 'certificateIdentifier' - A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
-- * 'certificateOwner' - The owner of the certificate.
-- * 'certificatePem' - The contents of a @.pem@ file, which contains an X.509 certificate.
-- * 'certificateWallet' - The location of an imported Oracle Wallet certificate for use with SSL.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
-- * 'keyLength' - The key length of the cryptographic algorithm being used.
-- * 'signingAlgorithm' - The signing algorithm for the certificate.
-- * 'validFromDate' - The beginning date that the certificate is valid.
-- * 'validToDate' - The final date that the certificate is valid.
mkCertificate ::
  Certificate
mkCertificate =
  Certificate'
    { certificateOwner = Lude.Nothing,
      signingAlgorithm = Lude.Nothing,
      validFromDate = Lude.Nothing,
      certificatePem = Lude.Nothing,
      certificateARN = Lude.Nothing,
      certificateCreationDate = Lude.Nothing,
      certificateIdentifier = Lude.Nothing,
      certificateWallet = Lude.Nothing,
      keyLength = Lude.Nothing,
      validToDate = Lude.Nothing
    }

-- | The owner of the certificate.
--
-- /Note:/ Consider using 'certificateOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateOwner :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificateOwner = Lens.lens (certificateOwner :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateOwner = a} :: Certificate)
{-# DEPRECATED cCertificateOwner "Use generic-lens or generic-optics with 'certificateOwner' instead." #-}

-- | The signing algorithm for the certificate.
--
-- /Note:/ Consider using 'signingAlgorithm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cSigningAlgorithm :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cSigningAlgorithm = Lens.lens (signingAlgorithm :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {signingAlgorithm = a} :: Certificate)
{-# DEPRECATED cSigningAlgorithm "Use generic-lens or generic-optics with 'signingAlgorithm' instead." #-}

-- | The beginning date that the certificate is valid.
--
-- /Note:/ Consider using 'validFromDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValidFromDate :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cValidFromDate = Lens.lens (validFromDate :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {validFromDate = a} :: Certificate)
{-# DEPRECATED cValidFromDate "Use generic-lens or generic-optics with 'validFromDate' instead." #-}

-- | The contents of a @.pem@ file, which contains an X.509 certificate.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificatePem :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificatePem = Lens.lens (certificatePem :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificatePem = a} :: Certificate)
{-# DEPRECATED cCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The Amazon Resource Name (ARN) for the certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateARN :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificateARN = Lens.lens (certificateARN :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: Certificate)
{-# DEPRECATED cCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The date that the certificate was created.
--
-- /Note:/ Consider using 'certificateCreationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateCreationDate :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cCertificateCreationDate = Lens.lens (certificateCreationDate :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {certificateCreationDate = a} :: Certificate)
{-# DEPRECATED cCertificateCreationDate "Use generic-lens or generic-optics with 'certificateCreationDate' instead." #-}

-- | A customer-assigned name for the certificate. Identifiers must begin with a letter and must contain only ASCII letters, digits, and hyphens. They can't end with a hyphen or contain two consecutive hyphens.
--
-- /Note:/ Consider using 'certificateIdentifier' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateIdentifier :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificateIdentifier = Lens.lens (certificateIdentifier :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateIdentifier = a} :: Certificate)
{-# DEPRECATED cCertificateIdentifier "Use generic-lens or generic-optics with 'certificateIdentifier' instead." #-}

-- | The location of an imported Oracle Wallet certificate for use with SSL.--
-- /Note:/ This 'Lens' automatically encodes and decodes Base64 data.
-- The underlying isomorphism will encode to Base64 representation during
-- serialisation, and decode from Base64 representation during deserialisation.
-- This 'Lens' accepts and returns only raw unencoded data.
--
-- /Note:/ Consider using 'certificateWallet' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateWallet :: Lens.Lens' Certificate (Lude.Maybe Lude.Base64)
cCertificateWallet = Lens.lens (certificateWallet :: Certificate -> Lude.Maybe Lude.Base64) (\s a -> s {certificateWallet = a} :: Certificate)
{-# DEPRECATED cCertificateWallet "Use generic-lens or generic-optics with 'certificateWallet' instead." #-}

-- | The key length of the cryptographic algorithm being used.
--
-- /Note:/ Consider using 'keyLength' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cKeyLength :: Lens.Lens' Certificate (Lude.Maybe Lude.Int)
cKeyLength = Lens.lens (keyLength :: Certificate -> Lude.Maybe Lude.Int) (\s a -> s {keyLength = a} :: Certificate)
{-# DEPRECATED cKeyLength "Use generic-lens or generic-optics with 'keyLength' instead." #-}

-- | The final date that the certificate is valid.
--
-- /Note:/ Consider using 'validToDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cValidToDate :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cValidToDate = Lens.lens (validToDate :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {validToDate = a} :: Certificate)
{-# DEPRECATED cValidToDate "Use generic-lens or generic-optics with 'validToDate' instead." #-}

instance Lude.FromJSON Certificate where
  parseJSON =
    Lude.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Lude.<$> (x Lude..:? "CertificateOwner")
            Lude.<*> (x Lude..:? "SigningAlgorithm")
            Lude.<*> (x Lude..:? "ValidFromDate")
            Lude.<*> (x Lude..:? "CertificatePem")
            Lude.<*> (x Lude..:? "CertificateArn")
            Lude.<*> (x Lude..:? "CertificateCreationDate")
            Lude.<*> (x Lude..:? "CertificateIdentifier")
            Lude.<*> (x Lude..:? "CertificateWallet")
            Lude.<*> (x Lude..:? "KeyLength")
            Lude.<*> (x Lude..:? "ValidToDate")
      )
