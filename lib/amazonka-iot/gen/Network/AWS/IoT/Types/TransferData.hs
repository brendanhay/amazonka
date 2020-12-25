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
    tdAcceptDate,
    tdRejectDate,
    tdRejectReason,
    tdTransferDate,
    tdTransferMessage,
  )
where

import qualified Network.AWS.IoT.Types.RejectReason as Types
import qualified Network.AWS.IoT.Types.TransferMessage as Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core

-- | Data used to transfer a certificate to an AWS account.
--
-- /See:/ 'mkTransferData' smart constructor.
data TransferData = TransferData'
  { -- | The date the transfer was accepted.
    acceptDate :: Core.Maybe Core.NominalDiffTime,
    -- | The date the transfer was rejected.
    rejectDate :: Core.Maybe Core.NominalDiffTime,
    -- | The reason why the transfer was rejected.
    rejectReason :: Core.Maybe Types.RejectReason,
    -- | The date the transfer took place.
    transferDate :: Core.Maybe Core.NominalDiffTime,
    -- | The transfer message.
    transferMessage :: Core.Maybe Types.TransferMessage
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TransferData' value with any optional fields omitted.
mkTransferData ::
  TransferData
mkTransferData =
  TransferData'
    { acceptDate = Core.Nothing,
      rejectDate = Core.Nothing,
      rejectReason = Core.Nothing,
      transferDate = Core.Nothing,
      transferMessage = Core.Nothing
    }

-- | The date the transfer was accepted.
--
-- /Note:/ Consider using 'acceptDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdAcceptDate :: Lens.Lens' TransferData (Core.Maybe Core.NominalDiffTime)
tdAcceptDate = Lens.field @"acceptDate"
{-# DEPRECATED tdAcceptDate "Use generic-lens or generic-optics with 'acceptDate' instead." #-}

-- | The date the transfer was rejected.
--
-- /Note:/ Consider using 'rejectDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRejectDate :: Lens.Lens' TransferData (Core.Maybe Core.NominalDiffTime)
tdRejectDate = Lens.field @"rejectDate"
{-# DEPRECATED tdRejectDate "Use generic-lens or generic-optics with 'rejectDate' instead." #-}

-- | The reason why the transfer was rejected.
--
-- /Note:/ Consider using 'rejectReason' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdRejectReason :: Lens.Lens' TransferData (Core.Maybe Types.RejectReason)
tdRejectReason = Lens.field @"rejectReason"
{-# DEPRECATED tdRejectReason "Use generic-lens or generic-optics with 'rejectReason' instead." #-}

-- | The date the transfer took place.
--
-- /Note:/ Consider using 'transferDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTransferDate :: Lens.Lens' TransferData (Core.Maybe Core.NominalDiffTime)
tdTransferDate = Lens.field @"transferDate"
{-# DEPRECATED tdTransferDate "Use generic-lens or generic-optics with 'transferDate' instead." #-}

-- | The transfer message.
--
-- /Note:/ Consider using 'transferMessage' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tdTransferMessage :: Lens.Lens' TransferData (Core.Maybe Types.TransferMessage)
tdTransferMessage = Lens.field @"transferMessage"
{-# DEPRECATED tdTransferMessage "Use generic-lens or generic-optics with 'transferMessage' instead." #-}

instance Core.FromJSON TransferData where
  parseJSON =
    Core.withObject "TransferData" Core.$
      \x ->
        TransferData'
          Core.<$> (x Core..:? "acceptDate")
          Core.<*> (x Core..:? "rejectDate")
          Core.<*> (x Core..:? "rejectReason")
          Core.<*> (x Core..:? "transferDate")
          Core.<*> (x Core..:? "transferMessage")
