-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CACertificateDescription
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CACertificateDescription
  ( CACertificateDescription (..),

    -- * Smart constructor
    mkCACertificateDescription,

    -- * Lenses
    cacdStatus,
    cacdOwnedBy,
    cacdLastModifiedDate,
    cacdCertificatePem,
    cacdCertificateARN,
    cacdCertificateId,
    cacdValidity,
    cacdAutoRegistrationStatus,
    cacdCreationDate,
    cacdGenerationId,
    cacdCustomerVersion,
  )
where

import Network.AWS.IoT.Types.AutoRegistrationStatus
import Network.AWS.IoT.Types.CACertificateStatus
import Network.AWS.IoT.Types.CertificateValidity
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a CA certificate.
--
-- /See:/ 'mkCACertificateDescription' smart constructor.
data CACertificateDescription = CACertificateDescription'
  { status ::
      Lude.Maybe CACertificateStatus,
    ownedBy :: Lude.Maybe Lude.Text,
    lastModifiedDate ::
      Lude.Maybe Lude.Timestamp,
    certificatePem :: Lude.Maybe Lude.Text,
    certificateARN :: Lude.Maybe Lude.Text,
    certificateId :: Lude.Maybe Lude.Text,
    validity ::
      Lude.Maybe CertificateValidity,
    autoRegistrationStatus ::
      Lude.Maybe AutoRegistrationStatus,
    creationDate :: Lude.Maybe Lude.Timestamp,
    generationId :: Lude.Maybe Lude.Text,
    customerVersion ::
      Lude.Maybe Lude.Natural
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CACertificateDescription' with the minimum fields required to make a request.
--
-- * 'autoRegistrationStatus' - Whether the CA certificate configured for auto registration of device certificates. Valid values are "ENABLE" and "DISABLE"
-- * 'certificateARN' - The CA certificate ARN.
-- * 'certificateId' - The CA certificate ID.
-- * 'certificatePem' - The CA certificate data, in PEM format.
-- * 'creationDate' - The date the CA certificate was created.
-- * 'customerVersion' - The customer version of the CA certificate.
-- * 'generationId' - The generation ID of the CA certificate.
-- * 'lastModifiedDate' - The date the CA certificate was last modified.
-- * 'ownedBy' - The owner of the CA certificate.
-- * 'status' - The status of a CA certificate.
-- * 'validity' - When the CA certificate is valid.
mkCACertificateDescription ::
  CACertificateDescription
mkCACertificateDescription =
  CACertificateDescription'
    { status = Lude.Nothing,
      ownedBy = Lude.Nothing,
      lastModifiedDate = Lude.Nothing,
      certificatePem = Lude.Nothing,
      certificateARN = Lude.Nothing,
      certificateId = Lude.Nothing,
      validity = Lude.Nothing,
      autoRegistrationStatus = Lude.Nothing,
      creationDate = Lude.Nothing,
      generationId = Lude.Nothing,
      customerVersion = Lude.Nothing
    }

-- | The status of a CA certificate.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdStatus :: Lens.Lens' CACertificateDescription (Lude.Maybe CACertificateStatus)
cacdStatus = Lens.lens (status :: CACertificateDescription -> Lude.Maybe CACertificateStatus) (\s a -> s {status = a} :: CACertificateDescription)
{-# DEPRECATED cacdStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The owner of the CA certificate.
--
-- /Note:/ Consider using 'ownedBy' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdOwnedBy :: Lens.Lens' CACertificateDescription (Lude.Maybe Lude.Text)
cacdOwnedBy = Lens.lens (ownedBy :: CACertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {ownedBy = a} :: CACertificateDescription)
{-# DEPRECATED cacdOwnedBy "Use generic-lens or generic-optics with 'ownedBy' instead." #-}

-- | The date the CA certificate was last modified.
--
-- /Note:/ Consider using 'lastModifiedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdLastModifiedDate :: Lens.Lens' CACertificateDescription (Lude.Maybe Lude.Timestamp)
cacdLastModifiedDate = Lens.lens (lastModifiedDate :: CACertificateDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {lastModifiedDate = a} :: CACertificateDescription)
{-# DEPRECATED cacdLastModifiedDate "Use generic-lens or generic-optics with 'lastModifiedDate' instead." #-}

-- | The CA certificate data, in PEM format.
--
-- /Note:/ Consider using 'certificatePem' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdCertificatePem :: Lens.Lens' CACertificateDescription (Lude.Maybe Lude.Text)
cacdCertificatePem = Lens.lens (certificatePem :: CACertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {certificatePem = a} :: CACertificateDescription)
{-# DEPRECATED cacdCertificatePem "Use generic-lens or generic-optics with 'certificatePem' instead." #-}

-- | The CA certificate ARN.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdCertificateARN :: Lens.Lens' CACertificateDescription (Lude.Maybe Lude.Text)
cacdCertificateARN = Lens.lens (certificateARN :: CACertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CACertificateDescription)
{-# DEPRECATED cacdCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The CA certificate ID.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdCertificateId :: Lens.Lens' CACertificateDescription (Lude.Maybe Lude.Text)
cacdCertificateId = Lens.lens (certificateId :: CACertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: CACertificateDescription)
{-# DEPRECATED cacdCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | When the CA certificate is valid.
--
-- /Note:/ Consider using 'validity' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdValidity :: Lens.Lens' CACertificateDescription (Lude.Maybe CertificateValidity)
cacdValidity = Lens.lens (validity :: CACertificateDescription -> Lude.Maybe CertificateValidity) (\s a -> s {validity = a} :: CACertificateDescription)
{-# DEPRECATED cacdValidity "Use generic-lens or generic-optics with 'validity' instead." #-}

-- | Whether the CA certificate configured for auto registration of device certificates. Valid values are "ENABLE" and "DISABLE"
--
-- /Note:/ Consider using 'autoRegistrationStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdAutoRegistrationStatus :: Lens.Lens' CACertificateDescription (Lude.Maybe AutoRegistrationStatus)
cacdAutoRegistrationStatus = Lens.lens (autoRegistrationStatus :: CACertificateDescription -> Lude.Maybe AutoRegistrationStatus) (\s a -> s {autoRegistrationStatus = a} :: CACertificateDescription)
{-# DEPRECATED cacdAutoRegistrationStatus "Use generic-lens or generic-optics with 'autoRegistrationStatus' instead." #-}

-- | The date the CA certificate was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdCreationDate :: Lens.Lens' CACertificateDescription (Lude.Maybe Lude.Timestamp)
cacdCreationDate = Lens.lens (creationDate :: CACertificateDescription -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: CACertificateDescription)
{-# DEPRECATED cacdCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The generation ID of the CA certificate.
--
-- /Note:/ Consider using 'generationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdGenerationId :: Lens.Lens' CACertificateDescription (Lude.Maybe Lude.Text)
cacdGenerationId = Lens.lens (generationId :: CACertificateDescription -> Lude.Maybe Lude.Text) (\s a -> s {generationId = a} :: CACertificateDescription)
{-# DEPRECATED cacdGenerationId "Use generic-lens or generic-optics with 'generationId' instead." #-}

-- | The customer version of the CA certificate.
--
-- /Note:/ Consider using 'customerVersion' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacdCustomerVersion :: Lens.Lens' CACertificateDescription (Lude.Maybe Lude.Natural)
cacdCustomerVersion = Lens.lens (customerVersion :: CACertificateDescription -> Lude.Maybe Lude.Natural) (\s a -> s {customerVersion = a} :: CACertificateDescription)
{-# DEPRECATED cacdCustomerVersion "Use generic-lens or generic-optics with 'customerVersion' instead." #-}

instance Lude.FromJSON CACertificateDescription where
  parseJSON =
    Lude.withObject
      "CACertificateDescription"
      ( \x ->
          CACertificateDescription'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "ownedBy")
            Lude.<*> (x Lude..:? "lastModifiedDate")
            Lude.<*> (x Lude..:? "certificatePem")
            Lude.<*> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..:? "certificateId")
            Lude.<*> (x Lude..:? "validity")
            Lude.<*> (x Lude..:? "autoRegistrationStatus")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "generationId")
            Lude.<*> (x Lude..:? "customerVersion")
      )
