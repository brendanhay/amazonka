{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo
  ( TapeRecoveryPointInfo (..),

    -- * Smart constructor
    mkTapeRecoveryPointInfo,

    -- * Lenses
    trpiTapeARN,
    trpiTapeRecoveryPointTime,
    trpiTapeSizeInBytes,
    trpiTapeStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.TapeARN as Types
import qualified Network.AWS.StorageGateway.Types.TapeRecoveryPointStatus as Types

-- | Describes a recovery point.
--
-- /See:/ 'mkTapeRecoveryPointInfo' smart constructor.
data TapeRecoveryPointInfo = TapeRecoveryPointInfo'
  { -- | The Amazon Resource Name (ARN) of the virtual tape.
    tapeARN :: Core.Maybe Types.TapeARN,
    -- | The time when the point-in-time view of the virtual tape was replicated for later recovery.
    --
    -- The default timestamp format of the tape recovery point time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
    tapeRecoveryPointTime :: Core.Maybe Core.NominalDiffTime,
    -- | The size, in bytes, of the virtual tapes to recover.
    tapeSizeInBytes :: Core.Maybe Core.Integer,
    -- | The status of the virtual tapes.
    tapeStatus :: Core.Maybe Types.TapeRecoveryPointStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TapeRecoveryPointInfo' value with any optional fields omitted.
mkTapeRecoveryPointInfo ::
  TapeRecoveryPointInfo
mkTapeRecoveryPointInfo =
  TapeRecoveryPointInfo'
    { tapeARN = Core.Nothing,
      tapeRecoveryPointTime = Core.Nothing,
      tapeSizeInBytes = Core.Nothing,
      tapeStatus = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpiTapeARN :: Lens.Lens' TapeRecoveryPointInfo (Core.Maybe Types.TapeARN)
trpiTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED trpiTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The time when the point-in-time view of the virtual tape was replicated for later recovery.
--
-- The default timestamp format of the tape recovery point time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
--
-- /Note:/ Consider using 'tapeRecoveryPointTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpiTapeRecoveryPointTime :: Lens.Lens' TapeRecoveryPointInfo (Core.Maybe Core.NominalDiffTime)
trpiTapeRecoveryPointTime = Lens.field @"tapeRecoveryPointTime"
{-# DEPRECATED trpiTapeRecoveryPointTime "Use generic-lens or generic-optics with 'tapeRecoveryPointTime' instead." #-}

-- | The size, in bytes, of the virtual tapes to recover.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpiTapeSizeInBytes :: Lens.Lens' TapeRecoveryPointInfo (Core.Maybe Core.Integer)
trpiTapeSizeInBytes = Lens.field @"tapeSizeInBytes"
{-# DEPRECATED trpiTapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead." #-}

-- | The status of the virtual tapes.
--
-- /Note:/ Consider using 'tapeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpiTapeStatus :: Lens.Lens' TapeRecoveryPointInfo (Core.Maybe Types.TapeRecoveryPointStatus)
trpiTapeStatus = Lens.field @"tapeStatus"
{-# DEPRECATED trpiTapeStatus "Use generic-lens or generic-optics with 'tapeStatus' instead." #-}

instance Core.FromJSON TapeRecoveryPointInfo where
  parseJSON =
    Core.withObject "TapeRecoveryPointInfo" Core.$
      \x ->
        TapeRecoveryPointInfo'
          Core.<$> (x Core..:? "TapeARN")
          Core.<*> (x Core..:? "TapeRecoveryPointTime")
          Core.<*> (x Core..:? "TapeSizeInBytes")
          Core.<*> (x Core..:? "TapeStatus")
