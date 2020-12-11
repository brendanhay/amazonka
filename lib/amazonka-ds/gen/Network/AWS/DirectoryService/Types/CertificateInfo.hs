-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.CertificateInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.CertificateInfo
  ( CertificateInfo (..),

    -- * Smart constructor
    mkCertificateInfo,

    -- * Lenses
    ciState,
    ciCommonName,
    ciCertificateId,
    ciExpiryDateTime,
  )
where

import Network.AWS.DirectoryService.Types.CertificateState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Contains general information about a certificate.
--
-- /See:/ 'mkCertificateInfo' smart constructor.
data CertificateInfo = CertificateInfo'
  { state ::
      Lude.Maybe CertificateState,
    commonName :: Lude.Maybe Lude.Text,
    certificateId :: Lude.Maybe Lude.Text,
    expiryDateTime :: Lude.Maybe Lude.Timestamp
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CertificateInfo' with the minimum fields required to make a request.
--
-- * 'certificateId' - The identifier of the certificate.
-- * 'commonName' - The common name for the certificate.
-- * 'expiryDateTime' - The date and time when the certificate will expire.
-- * 'state' - The state of the certificate.
mkCertificateInfo ::
  CertificateInfo
mkCertificateInfo =
  CertificateInfo'
    { state = Lude.Nothing,
      commonName = Lude.Nothing,
      certificateId = Lude.Nothing,
      expiryDateTime = Lude.Nothing
    }

-- | The state of the certificate.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciState :: Lens.Lens' CertificateInfo (Lude.Maybe CertificateState)
ciState = Lens.lens (state :: CertificateInfo -> Lude.Maybe CertificateState) (\s a -> s {state = a} :: CertificateInfo)
{-# DEPRECATED ciState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The common name for the certificate.
--
-- /Note:/ Consider using 'commonName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCommonName :: Lens.Lens' CertificateInfo (Lude.Maybe Lude.Text)
ciCommonName = Lens.lens (commonName :: CertificateInfo -> Lude.Maybe Lude.Text) (\s a -> s {commonName = a} :: CertificateInfo)
{-# DEPRECATED ciCommonName "Use generic-lens or generic-optics with 'commonName' instead." #-}

-- | The identifier of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciCertificateId :: Lens.Lens' CertificateInfo (Lude.Maybe Lude.Text)
ciCertificateId = Lens.lens (certificateId :: CertificateInfo -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: CertificateInfo)
{-# DEPRECATED ciCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The date and time when the certificate will expire.
--
-- /Note:/ Consider using 'expiryDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ciExpiryDateTime :: Lens.Lens' CertificateInfo (Lude.Maybe Lude.Timestamp)
ciExpiryDateTime = Lens.lens (expiryDateTime :: CertificateInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiryDateTime = a} :: CertificateInfo)
{-# DEPRECATED ciExpiryDateTime "Use generic-lens or generic-optics with 'expiryDateTime' instead." #-}

instance Lude.FromJSON CertificateInfo where
  parseJSON =
    Lude.withObject
      "CertificateInfo"
      ( \x ->
          CertificateInfo'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "CommonName")
            Lude.<*> (x Lude..:? "CertificateId")
            Lude.<*> (x Lude..:? "ExpiryDateTime")
      )
