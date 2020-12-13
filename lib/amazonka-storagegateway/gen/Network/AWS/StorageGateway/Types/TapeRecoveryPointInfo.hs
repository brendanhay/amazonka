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
    trpiTapeStatus,
    trpiTapeRecoveryPointTime,
    trpiTapeARN,
    trpiTapeSizeInBytes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a recovery point.
--
-- /See:/ 'mkTapeRecoveryPointInfo' smart constructor.
data TapeRecoveryPointInfo = TapeRecoveryPointInfo'
  { -- | The status of the virtual tapes.
    tapeStatus :: Lude.Maybe Lude.Text,
    -- | The time when the point-in-time view of the virtual tape was replicated for later recovery.
    --
    -- The default timestamp format of the tape recovery point time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
    tapeRecoveryPointTime :: Lude.Maybe Lude.Timestamp,
    -- | The Amazon Resource Name (ARN) of the virtual tape.
    tapeARN :: Lude.Maybe Lude.Text,
    -- | The size, in bytes, of the virtual tapes to recover.
    tapeSizeInBytes :: Lude.Maybe Lude.Integer
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TapeRecoveryPointInfo' with the minimum fields required to make a request.
--
-- * 'tapeStatus' - The status of the virtual tapes.
-- * 'tapeRecoveryPointTime' - The time when the point-in-time view of the virtual tape was replicated for later recovery.
--
-- The default timestamp format of the tape recovery point time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape.
-- * 'tapeSizeInBytes' - The size, in bytes, of the virtual tapes to recover.
mkTapeRecoveryPointInfo ::
  TapeRecoveryPointInfo
mkTapeRecoveryPointInfo =
  TapeRecoveryPointInfo'
    { tapeStatus = Lude.Nothing,
      tapeRecoveryPointTime = Lude.Nothing,
      tapeARN = Lude.Nothing,
      tapeSizeInBytes = Lude.Nothing
    }

-- | The status of the virtual tapes.
--
-- /Note:/ Consider using 'tapeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpiTapeStatus :: Lens.Lens' TapeRecoveryPointInfo (Lude.Maybe Lude.Text)
trpiTapeStatus = Lens.lens (tapeStatus :: TapeRecoveryPointInfo -> Lude.Maybe Lude.Text) (\s a -> s {tapeStatus = a} :: TapeRecoveryPointInfo)
{-# DEPRECATED trpiTapeStatus "Use generic-lens or generic-optics with 'tapeStatus' instead." #-}

-- | The time when the point-in-time view of the virtual tape was replicated for later recovery.
--
-- The default timestamp format of the tape recovery point time is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
--
-- /Note:/ Consider using 'tapeRecoveryPointTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpiTapeRecoveryPointTime :: Lens.Lens' TapeRecoveryPointInfo (Lude.Maybe Lude.Timestamp)
trpiTapeRecoveryPointTime = Lens.lens (tapeRecoveryPointTime :: TapeRecoveryPointInfo -> Lude.Maybe Lude.Timestamp) (\s a -> s {tapeRecoveryPointTime = a} :: TapeRecoveryPointInfo)
{-# DEPRECATED trpiTapeRecoveryPointTime "Use generic-lens or generic-optics with 'tapeRecoveryPointTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpiTapeARN :: Lens.Lens' TapeRecoveryPointInfo (Lude.Maybe Lude.Text)
trpiTapeARN = Lens.lens (tapeARN :: TapeRecoveryPointInfo -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: TapeRecoveryPointInfo)
{-# DEPRECATED trpiTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The size, in bytes, of the virtual tapes to recover.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
trpiTapeSizeInBytes :: Lens.Lens' TapeRecoveryPointInfo (Lude.Maybe Lude.Integer)
trpiTapeSizeInBytes = Lens.lens (tapeSizeInBytes :: TapeRecoveryPointInfo -> Lude.Maybe Lude.Integer) (\s a -> s {tapeSizeInBytes = a} :: TapeRecoveryPointInfo)
{-# DEPRECATED trpiTapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead." #-}

instance Lude.FromJSON TapeRecoveryPointInfo where
  parseJSON =
    Lude.withObject
      "TapeRecoveryPointInfo"
      ( \x ->
          TapeRecoveryPointInfo'
            Lude.<$> (x Lude..:? "TapeStatus")
            Lude.<*> (x Lude..:? "TapeRecoveryPointTime")
            Lude.<*> (x Lude..:? "TapeARN")
            Lude.<*> (x Lude..:? "TapeSizeInBytes")
      )
