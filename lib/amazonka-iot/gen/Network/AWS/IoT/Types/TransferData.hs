{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.IoT.Types.TransferData
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.IoT.Types.TransferData
  ( TransferData (..),

    -- * Smart constructor
    mkTransferData,

    -- * Lenses
    tdTransferDate,
    tdAcceptDate,
    tdTransferMessage,
    tdRejectDate,
    tdRejectReason,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Data used to transfer a certificate to an AWS account.
--
-- /See:/ 'mkTransferData' smart constructor.
data TransferData = TransferData'
  { transferDate ::
      Lude.Maybe Lude.Timestamp,
    acceptDate :: Lude.Maybe Lude.Timestamp,
    transferMessage :: Lude.Maybe Lude.Text,
    rejectDate :: Lude.Maybe Lude.Timestamp,
    rejectReason :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TransferData' with the minimum fields required to make a request.
--
-- * 'acceptDate' - The date the transfer was accepted.
-- * 'rejectDate' - The date the transfer was rejected.
-- * 'rejectReason' - The reason why the transfer was rejected.
-- * 'transferDate' - The date the transfer took place.
-- * 'transferMessage' - The transfer message.
mkTransferData ::
  TransferData
mkTransferData =
  TransferData'
    { transferDate = Lude.Nothing,
      acceptDate = Lude.Nothing,
      transferMessage = Lude.Nothing,
      rejectDate = Lude.Nothing,
      rejectReason = Lude.Nothing
    }

-- | The date the transfer took place.
--
-- /Note:/ Consider using 'transferDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTransferDate :: Lens.Lens' TransferData (Lude.Maybe Lude.Timestamp)
tdTransferDate = Lens.lens (transferDate :: TransferData -> Lude.Maybe Lude.Timestamp) (\s a -> s {transferDate = a} :: TransferData)
{-# DEPRECATED tdTransferDate "Use generic-lens or generic-optics with 'transferDate' instead." #-}

-- | The date the transfer was accepted.
--
-- /Note:/ Consider using 'acceptDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAcceptDate :: Lens.Lens' TransferData (Lude.Maybe Lude.Timestamp)
tdAcceptDate = Lens.lens (acceptDate :: TransferData -> Lude.Maybe Lude.Timestamp) (\s a -> s {acceptDate = a} :: TransferData)
{-# DEPRECATED tdAcceptDate "Use generic-lens or generic-optics with 'acceptDate' instead." #-}

-- | The transfer message.
--
-- /Note:/ Consider using 'transferMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTransferMessage :: Lens.Lens' TransferData (Lude.Maybe Lude.Text)
tdTransferMessage = Lens.lens (transferMessage :: TransferData -> Lude.Maybe Lude.Text) (\s a -> s {transferMessage = a} :: TransferData)
{-# DEPRECATED tdTransferMessage "Use generic-lens or generic-optics with 'transferMessage' instead." #-}

-- | The date the transfer was rejected.
--
-- /Note:/ Consider using 'rejectDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRejectDate :: Lens.Lens' TransferData (Lude.Maybe Lude.Timestamp)
tdRejectDate = Lens.lens (rejectDate :: TransferData -> Lude.Maybe Lude.Timestamp) (\s a -> s {rejectDate = a} :: TransferData)
{-# DEPRECATED tdRejectDate "Use generic-lens or generic-optics with 'rejectDate' instead." #-}

-- | The reason why the transfer was rejected.
--
-- /Note:/ Consider using 'rejectReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRejectReason :: Lens.Lens' TransferData (Lude.Maybe Lude.Text)
tdRejectReason = Lens.lens (rejectReason :: TransferData -> Lude.Maybe Lude.Text) (\s a -> s {rejectReason = a} :: TransferData)
{-# DEPRECATED tdRejectReason "Use generic-lens or generic-optics with 'rejectReason' instead." #-}

instance Lude.FromJSON TransferData where
  parseJSON =
    Lude.withObject
      "TransferData"
      ( \x ->
          TransferData'
            Lude.<$> (x Lude..:? "transferDate")
            Lude.<*> (x Lude..:? "acceptDate")
            Lude.<*> (x Lude..:? "transferMessage")
            Lude.<*> (x Lude..:? "rejectDate")
            Lude.<*> (x Lude..:? "rejectReason")
      )
