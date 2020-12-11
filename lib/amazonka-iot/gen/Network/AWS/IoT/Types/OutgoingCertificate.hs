-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.OutgoingCertificate
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.OutgoingCertificate
  ( OutgoingCertificate (..),

    -- * Smart constructor
    mkOutgoingCertificate,

    -- * Lenses
    ocTransferDate,
    ocCertificateARN,
    ocCertificateId,
    ocTransferredTo,
    ocCreationDate,
    ocTransferMessage,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | A certificate that has been transferred but not yet accepted.
--
-- /See:/ 'mkOutgoingCertificate' smart constructor.
data OutgoingCertificate = OutgoingCertificate'
  { transferDate ::
      Lude.Maybe Lude.Timestamp,
    certificateARN :: Lude.Maybe Lude.Text,
    certificateId :: Lude.Maybe Lude.Text,
    transferredTo :: Lude.Maybe Lude.Text,
    creationDate :: Lude.Maybe Lude.Timestamp,
    transferMessage :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'OutgoingCertificate' with the minimum fields required to make a request.
--
-- * 'certificateARN' - The certificate ARN.
-- * 'certificateId' - The certificate ID.
-- * 'creationDate' - The certificate creation date.
-- * 'transferDate' - The date the transfer was initiated.
-- * 'transferMessage' - The transfer message.
-- * 'transferredTo' - The AWS account to which the transfer was made.
mkOutgoingCertificate ::
  OutgoingCertificate
mkOutgoingCertificate =
  OutgoingCertificate'
    { transferDate = Lude.Nothing,
      certificateARN = Lude.Nothing,
      certificateId = Lude.Nothing,
      transferredTo = Lude.Nothing,
      creationDate = Lude.Nothing,
      transferMessage = Lude.Nothing
    }

-- | The date the transfer was initiated.
--
-- /Note:/ Consider using 'transferDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTransferDate :: Lens.Lens' OutgoingCertificate (Lude.Maybe Lude.Timestamp)
ocTransferDate = Lens.lens (transferDate :: OutgoingCertificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {transferDate = a} :: OutgoingCertificate)
{-# DEPRECATED ocTransferDate "Use generic-lens or generic-optics with 'transferDate' instead." #-}

-- | The certificate ARN.
--
-- /Note:/ Consider using 'certificateARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocCertificateARN :: Lens.Lens' OutgoingCertificate (Lude.Maybe Lude.Text)
ocCertificateARN = Lens.lens (certificateARN :: OutgoingCertificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateARN = a} :: OutgoingCertificate)
{-# DEPRECATED ocCertificateARN "Use generic-lens or generic-optics with 'certificateARN' instead." #-}

-- | The certificate ID.
--
-- /Note:/ Consider using 'certificateId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocCertificateId :: Lens.Lens' OutgoingCertificate (Lude.Maybe Lude.Text)
ocCertificateId = Lens.lens (certificateId :: OutgoingCertificate -> Lude.Maybe Lude.Text) (\s a -> s {certificateId = a} :: OutgoingCertificate)
{-# DEPRECATED ocCertificateId "Use generic-lens or generic-optics with 'certificateId' instead." #-}

-- | The AWS account to which the transfer was made.
--
-- /Note:/ Consider using 'transferredTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTransferredTo :: Lens.Lens' OutgoingCertificate (Lude.Maybe Lude.Text)
ocTransferredTo = Lens.lens (transferredTo :: OutgoingCertificate -> Lude.Maybe Lude.Text) (\s a -> s {transferredTo = a} :: OutgoingCertificate)
{-# DEPRECATED ocTransferredTo "Use generic-lens or generic-optics with 'transferredTo' instead." #-}

-- | The certificate creation date.
--
-- /Note:/ Consider using 'creationDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocCreationDate :: Lens.Lens' OutgoingCertificate (Lude.Maybe Lude.Timestamp)
ocCreationDate = Lens.lens (creationDate :: OutgoingCertificate -> Lude.Maybe Lude.Timestamp) (\s a -> s {creationDate = a} :: OutgoingCertificate)
{-# DEPRECATED ocCreationDate "Use generic-lens or generic-optics with 'creationDate' instead." #-}

-- | The transfer message.
--
-- /Note:/ Consider using 'transferMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ocTransferMessage :: Lens.Lens' OutgoingCertificate (Lude.Maybe Lude.Text)
ocTransferMessage = Lens.lens (transferMessage :: OutgoingCertificate -> Lude.Maybe Lude.Text) (\s a -> s {transferMessage = a} :: OutgoingCertificate)
{-# DEPRECATED ocTransferMessage "Use generic-lens or generic-optics with 'transferMessage' instead." #-}

instance Lude.FromJSON OutgoingCertificate where
  parseJSON =
    Lude.withObject
      "OutgoingCertificate"
      ( \x ->
          OutgoingCertificate'
            Lude.<$> (x Lude..:? "transferDate")
            Lude.<*> (x Lude..:? "certificateArn")
            Lude.<*> (x Lude..:? "certificateId")
            Lude.<*> (x Lude..:? "transferredTo")
            Lude.<*> (x Lude..:? "creationDate")
            Lude.<*> (x Lude..:? "transferMessage")
      )
