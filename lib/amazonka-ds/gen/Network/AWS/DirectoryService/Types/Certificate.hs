{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectoryService.Types.Certificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.DirectoryService.Types.Certificate
  ( Certificate (..),

    -- * Smart constructor
    mkCertificate,

    -- * Lenses
    cState,
    cCommonName,
    cCertificateId,
    cExpiryDateTime,
    cRegisteredDateTime,
    cStateReason,
  )
where

import Network.AWS.DirectoryService.Types.CertificateState
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Information about the certificate.
--
-- /See:/ 'mkCertificate' smart constructor.
data Certificate = Certificate'
  { state ::
      Lude.Maybe CertificateState,
    commonName :: Lude.Maybe Lude.Text,
    certificateId :: Lude.Maybe Lude.Text,
    expiryDateTime :: Lude.Maybe Lude.Timestamp,
    registeredDateTime :: Lude.Maybe Lude.Timestamp,
    stateReason :: Lude.Maybe Lude.Text
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
-- * 'certificateId' - The identifier of the certificate.
-- * 'commonName' - The common name for the certificate.
-- * 'expiryDateTime' - The date and time when the certificate will expire.
-- * 'registeredDateTime' - The date and time that the certificate was registered.
-- * 'state' - The state of the certificate.
-- * 'stateReason' - Describes a state change for the certificate.
mkCertificate ::
  Certificate
mkCertificate =
  Certificate'
    { state = Lude.Nothing,
      commonName = Lude.Nothing,
      certificateId = Lude.Nothing,
      expiryDateTime = Lude.Nothing,
      registeredDateTime = Lude.Nothing,
      stateReason = Lude.Nothing
    }

-- | The state of the certificate.
--
-- /Note:/ Consider using 'state' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cState :: Lens.Lens' Certificate (Lude.Maybe CertificateState)
cState = Lens.lens (state :: Certificate -> Lude.Maybe CertificateState) (\s a -> s {state = a} :: Certificate)
{-# DEPRECATED cState "Use generic-lens or generic-optics with 'state' instead." #-}

-- | The common name for the certificate.
--
-- /Note:/ Consider using 'commonName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCommonName :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCommonName = Lens.lens (commonName :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {commonName = a} :: Certificate)
{-# DEPRECATED cCommonName "Use generic-lens or generic-optics with 'commonName' instead." #-}

-- | The identifier of the certificate.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cCertificateId :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cCertificateId = Lens.lens (certificateId :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: Certificate)
{-# DEPRECATED cCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The date and time when the certificate will expire.
--
-- /Note:/ Consider using 'expiryDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cExpiryDateTime :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cExpiryDateTime = Lens.lens (expiryDateTime :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {expiryDateTime = a} :: Certificate)
{-# DEPRECATED cExpiryDateTime "Use generic-lens or generic-optics with 'expiryDateTime' instead." #-}

-- | The date and time that the certificate was registered.
--
-- /Note:/ Consider using 'registeredDateTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cRegisteredDateTime :: Lens.Lens' Certificate (Lude.Maybe Lude.Timestamp)
cRegisteredDateTime = Lens.lens (registeredDateTime :: Certificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {registeredDateTime = a} :: Certificate)
{-# DEPRECATED cRegisteredDateTime "Use generic-lens or generic-optics with 'registeredDateTime' instead." #-}

-- | Describes a state change for the certificate.
--
-- /Note:/ Consider using 'stateReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cStateReason :: Lens.Lens' Certificate (Lude.Maybe Lude.Text)
cStateReason = Lens.lens (stateReason :: Certificate -> Lude.Maybe Lude.Text) (\s a -> s {stateReason = a} :: Certificate)
{-# DEPRECATED cStateReason "Use generic-lens or generic-optics with 'stateReason' instead." #-}

instance Lude.FromJSON Certificate where
  parseJSON =
    Lude.withObject
      "Certificate"
      ( \x ->
          Certificate'
            Lude.<$> (x Lude..:? "State")
            Lude.<*> (x Lude..:? "CommonName")
            Lude.<*> (x Lude..:? "CertificateId")
            Lude.<*> (x Lude..:? "ExpiryDateTime")
            Lude.<*> (x Lude..:? "RegisteredDateTime")
            Lude.<*> (x Lude..:? "StateReason")
      )
