-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CertificateDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CertificateDescription
  ( CertificateDescription (..),

    -- * Smart constructor
    mkCertificateDescription,

    -- * Lenses
    cdStatus,
    cdOwnedBy,
    cdLastModifiedDate,
    cdCaCertificateId,
    cdPreviousOwnedBy,
    cdCertificatePem,
    cdCertificateARN,
    cdCertificateId,
    cdCertificateMode,
    cdValidity,
    cdCreationDate,
    cdGenerationId,
    cdTransferData,
    cdCustomerVersion,
  )
where

import Network.AWS.IoT.Types.CertificateMode
import Network.AWS.IoT.Types.CertificateStatus
import Network.AWS.IoT.Types.CertificateValidity
import Network.AWS.IoT.Types.TransferData
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a certificate.
--
-- /See:/ 'mkCertificateDescription' smart constructor.
data CertificateDescription = CertificateDescription'
  { status ::
      Lude.Maybe CertificateStatus,
    ownedBy :: Lude.Maybe Lude.Text,
    lastModifiedDate :: Lude.Maybe Lude.Timestamp,
    caCertificateId :: Lude.Maybe Lude.Text,
    previousOwnedBy :: Lude.Maybe Lude.Text,
    certificatePem :: Lude.Maybe Lude.Text,
    certificateARN :: Lude.Maybe Lude.Text,
    certificateId :: Lude.Maybe Lude.Text,
    certificateMode :: Lude.Maybe CertificateMode,
    validity :: Lude.Maybe CertificateValidity,
    creationDate :: Lude.Maybe Lude.Timestamp,
    generationId :: Lude.Maybe Lude.Text,
    transferData :: Lude.Maybe TransferData,
    customerVersion :: Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateDescription' with the minimum fields required to make a request.
--
-- * 'caCertificateId' - The certificate ID of the CA certificate used to sign this certificate.
-- * 'certificateARN' - The ARN of the certificate.
-- * 'certificateId' - The ID of the certificate.
-- * 'certificateMode' - The mode of the certificate.
-- * 'certificatePem' - The certificate data, in PEM format.
-- * 'creationDate' - The date and time the certificate was created.
-- * 'customerVersion' - The customer version of the certificate.
-- * 'generationId' - The generation ID of the certificate.
-- * 'lastModifiedDate' - The date and time the certificate was last modified.
-- * 'ownedBy' - The ID of the AWS account that owns the certificate.
-- * 'previousOwnedBy' - The ID of the AWS account of the previous owner of the certificate.
-- * 'status' - The status of the certificate.
-- * 'transferData' - The transfer data.
-- * 'validity' - When the certificate is valid.
mkCertificateDescription ::
  CertificateDescription
mkCertificateDescription =
  CertificateDescription'
    { status = Lude.Nothing,
      ownedBy = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      caCertificateId = Lude.Nothing,
      previousOwnedBy = Lude.Nothing,
      certificatePem = Lude.Nothing,
      certificateARN = Lude.Nothing,
      certificateId = Lude.Nothing,
      certificateMode = Lude.Nothing,
      validity = Lude.Nothing,
      creationDate = Lude.Nothing,
      generationId = Lude.Nothing,
      transferData = Lude.Nothing,
      customerVersion = Lude.Nothing
    }

-- | The status of the certificate.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdStatus :: Lens.Lens' CertificateDescription (Lude.Maybe CertificateStatus)
cdStatus = Lens.lens (status :: CertificateDescription -> Lude.Maybe CertificateStatus) (\s a -> s {status = a} :: CertificateDescription)
{-# DEPRECATED cdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ID of the AWS account that owns the certificate.
--
-- /Note:/ Consider using 'ownedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdOwnedBy :: Lens.Lens' CertificateDescription (Lude.Maybe Lude.Text)
cdOwnedBy = Lens.lens (ownedBy :: CertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {ownedBy = a} :: CertificateDescription)
{-# DEPRECATED cdOwnedBy "Use generic-lens or generic-optics with 'ownedBy' instead." #-}

-- | The date and time the certificate was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdLastModifiedDate :: Lens.Lens' CertificateDescription (Lude.Maybe Lude.Timestamp)
cdLastModifiedDate = Lens.lens (lastModifiedDate :: CertificateDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: CertificateDescription)
{-# DEPRECATED cdLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The certificate ID of the CA certificate used to sign this certificate.
--
-- /Note:/ Consider using 'caCertificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCaCertificateId :: Lens.Lens' CertificateDescription (Lude.Maybe Lude.Text)
cdCaCertificateId = Lens.lens (caCertificateId :: CertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {caCertificateId = a} :: CertificateDescription)
{-# DEPRECATED cdCaCertificateId "Use generic-lens or generic-optics with 'caCertificateId' instead." #-}

-- | The ID of the AWS account of the previous owner of the certificate.
--
-- /Note:/ Consider using 'previousOwnedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdPreviousOwnedBy :: Lens.Lens' CertificateDescription (Lude.Maybe Lude.Text)
cdPreviousOwnedBy = Lens.lens (previousOwnedBy :: CertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {previousOwnedBy = a} :: CertificateDescription)
{-# DEPRECATED cdPreviousOwnedBy "Use generic-lens or generic-optics with 'previousOwnedBy' instead." #-}

-- | The certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCertificatePem :: Lens.Lens' CertificateDescription (Lude.Maybe Lude.Text)
cdCertificatePem = Lens.lens (certificatePem :: CertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {certificatePem = a} :: CertificateDescription)
{-# DEPRECATED cdCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The ARN of the certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCertificateARN :: Lens.Lens' CertificateDescription (Lude.Maybe Lude.Text)
cdCertificateARN = Lens.lens (certificateARN :: CertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CertificateDescription)
{-# DEPRECATED cdCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The ID of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCertificateId :: Lens.Lens' CertificateDescription (Lude.Maybe Lude.Text)
cdCertificateId = Lens.lens (certificateId :: CertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: CertificateDescription)
{-# DEPRECATED cdCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The mode of the certificate.
--
-- /Note:/ Consider using 'certificateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCertificateMode :: Lens.Lens' CertificateDescription (Lude.Maybe CertificateMode)
cdCertificateMode = Lens.lens (certificateMode :: CertificateDescription -> Lude.Maybe CertificateMode) (\s a -> s {certificateMode = a} :: CertificateDescription)
{-# DEPRECATED cdCertificateMode "Use generic-lens or generic-optics with 'certificateMode' instead." #-}

-- | When the certificate is valid.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdValidity :: Lens.Lens' CertificateDescription (Lude.Maybe CertificateValidity)
cdValidity = Lens.lens (validity :: CertificateDescription -> Lude.Maybe CertificateValidity) (\s a -> s {validity = a} :: CertificateDescription)
{-# DEPRECATED cdValidity "Use generic-lens or generic-optics with 'validity' instead." #-}

-- | The date and time the certificate was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCreationDate :: Lens.Lens' CertificateDescription (Lude.Maybe Lude.Timestamp)
cdCreationDate = Lens.lens (creationDate :: CertificateDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: CertificateDescription)
{-# DEPRECATED cdCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The generation ID of the certificate.
--
-- /Note:/ Consider using 'generationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdGenerationId :: Lens.Lens' CertificateDescription (Lude.Maybe Lude.Text)
cdGenerationId = Lens.lens (generationId :: CertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {generationId = a} :: CertificateDescription)
{-# DEPRECATED cdGenerationId "Use generic-lens or generic-optics with 'generationId' instead." #-}

-- | The transfer data.
--
-- /Note:/ Consider using 'transferData' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdTransferData :: Lens.Lens' CertificateDescription (Lude.Maybe TransferData)
cdTransferData = Lens.lens (transferData :: CertificateDescription -> Lude.Maybe TransferData) (\s a -> s {transferData = a} :: CertificateDescription)
{-# DEPRECATED cdTransferData "Use generic-lens or generic-optics with 'transferData' instead." #-}

-- | The customer version of the certificate.
--
-- /Note:/ Consider using 'customerVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdCustomerVersion :: Lens.Lens' CertificateDescription (Lude.Maybe Lude.Natural)
cdCustomerVersion = Lens.lens (customerVersion :: CertificateDescription -> Lude.Maybe Lude.Natural) (\s a -> s {customerVersion = a} :: CertificateDescription)
{-# DEPRECATED cdCustomerVersion "Use generic-lens or generic-optics with 'customerVersion' instead." #-}

instance Lude.FromJSON CertificateDescription where
  parseJSON =
    Lude.withObject
      "CertificateDescription"
      ( \x ->
          CertificateDescription'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "ownedBy")
            Lude.<*> (x Lude..:? "lastModifiedDate")
            Lude.<*> (x Lude..:? "caCertificateId")
            Lude.<*> (x Lude..:? "previousOwnedBy")
            Lude.<*> (x Lude..:? "certificatePem")
            Lude.<*> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..:? "certificateId")
            Lude.<*> (x Lude..:? "certificateMode")
            Lude.<*> (x Lude..:? "validity")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "generationId")
            Lude.<*> (x Lude..:? "transferData")
            Lude.<*> (x Lude..:? "customerVersion")
      )
