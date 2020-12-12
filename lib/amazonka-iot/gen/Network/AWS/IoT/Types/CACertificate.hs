{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.CACertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.CACertificate
  ( CACertificate (..),

    -- * Smart constructor
    mkCACertificate,

    -- * Lenses
    cacStatus,
    cacCertificateARN,
    cacCertificateId,
    cacCreationDate,
  )
where

import Network.AWS.IoT.Types.CACertificateStatus
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A CA certificate.
--
-- /See:/ 'mkCACertificate' smart constructor.
data CACertificate = CACertificate'
  { status ::
      Lude.Maybe CACertificateStatus,
    certificateARN :: Lude.Maybe Lude.Text,
    certificateId :: Lude.Maybe Lude.Text,
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

-- | Creates a value of 'CACertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The ARN of the CA certificate.
-- * 'certificateId' - The ID of the CA certificate.
-- * 'creationDate' - The date the CA certificate was created.
-- * 'status' - The status of the CA certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
mkCACertificate ::
  CACertificate
mkCACertificate =
  CACertificate'
    { status = Lude.Nothing,
      certificateARN = Lude.Nothing,
      certificateId = Lude.Nothing,
      creationDate = Lude.Nothing
    }

-- | The status of the CA certificate.
--
-- The status value REGISTER_INACTIVE is deprecated and should not be used.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacStatus :: Lens.Lens' CACertificate (Lude.Maybe CACertificateStatus)
cacStatus = Lens.lens (status :: CACertificate -> Lude.Maybe CACertificateStatus) (\s a -> s {status = a} :: CACertificate)
{-# DEPRECATED cacStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The ARN of the CA certificate.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacCertificateARN :: Lens.Lens' CACertificate (Lude.Maybe Lude.Text)
cacCertificateARN = Lens.lens (certificateARN :: CACertificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: CACertificate)
{-# DEPRECATED cacCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The ID of the CA certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacCertificateId :: Lens.Lens' CACertificate (Lude.Maybe Lude.Text)
cacCertificateId = Lens.lens (certificateId :: CACertificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: CACertificate)
{-# DEPRECATED cacCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The date the CA certificate was created.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cacCreationDate :: Lens.Lens' CACertificate (Lude.Maybe Lude.Timestamp)
cacCreationDate = Lens.lens (creationDate :: CACertificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: CACertificate)
{-# DEPRECATED cacCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

instance Lude.FromJSON CACertificate where
  parseJSON =
    Lude.withObject
      "CACertificate"
      ( \x ->
          CACertificate'
            Lude.<$> (x Lude..:? "status")
            Lude.<*> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..:? "certificateId")
            Lude.<*> (x Lude..:? "creationDate")
      )
