{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.Certificate
  ( Certificate (..),

    -- * Smart constructor
    mkCertificate,

    -- * Lenses
    cStatus,
    cCertificateARN,
    cCertificateId,
    cCertificateMode,
    cCreationDate,
  )
where

import Network.AWS.IoT.Types.CertificateMode
import Network.AWS.IoT.Types.CertificateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about a certificate.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { status ::
      Lude.Maybe CertificateStatus,
    certificateARN :: Lude.Maybe Lude.Text,
    certificateId :: Lude.Maybe Lude.Text,
    certificateMode :: Lude.Maybe CertificateMode,
    creationDate :: Lude.Maybe Lude.Timestamp
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
-- * 'certificateARN' - The ARN of the certificate.
-- * 'certificateId' - The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
-- * 'certificateMode' - The mode of the certificate.
-- * 'creationDate' - The date and time the certificate was created.
-- * 'status' - The status of the certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
mkCertificate ::
  Certificate
mkCertificate =
  Certificate'
    { status = Lude.Nothing,
      certificateARN = Lude.Nothing,
      certificateId = Lude.Nothing,
      certificateMode = Lude.Nothing,
      creationDate = Lude.Nothing
    }

-- | The status of the certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStatus :: Lens.Lens' Certificate (Lude.Maybe CertificateStatus)
cStatus = Lens.lens (status :: Certificate -> Lude.Maybe CertificateStatus) (\s a -> s {status = a} :: Certificate)
{-# DEPRECATED cStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateARN :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificateARN = Lens.lens (certificateARN :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: Certificate)
{-# DEPRECATED cCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The ID of the certificate. (The last part of the certificate ARN contains the certificate ID.)
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateId :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificateId = Lens.lens (certificateId :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: Certificate)
{-# DEPRECATED cCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The mode of the certificate.
--
-- /Note:/ Consider using 'certificateMode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateMode :: Lens.Lens' Certificate (Lude.Maybe CertificateMode)
cCertificateMode = Lens.lens (certificateMode :: Certificate -> Lude.Maybe CertificateMode) (\s a -> s {certificateMode = a} :: Certificate)
{-# DEPRECATED cCertificateMode "Use generic-lens or generic-optics with 'certificateMode' instead." #-}

-- | The date and time the certificate was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCreationDate :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cCreationDate = Lens.lens (creationDate :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: Certificate)
{-# DEPRECATED cCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromJSON Certificate where
  parseJSON =
    Lude.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..:? "certificateId")
            Lude.<*> (x Lude..:? "certificateMode")
            Lude.<*> (x Lude..:? "creationDate")
      )
