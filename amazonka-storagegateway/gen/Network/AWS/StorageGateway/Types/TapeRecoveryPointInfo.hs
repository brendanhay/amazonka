{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeRecoveryPointInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a recovery point.
--
-- /See:/ 'newTapeRecoveryPointInfo' smart constructor.
data TapeRecoveryPointInfo = TapeRecoveryPointInfo'
  { -- | The status of the virtual tapes.
    tapeStatus :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of the virtual tapes to recover.
    tapeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The time when the point-in-time view of the virtual tape was replicated
    -- for later recovery.
    --
    -- The default timestamp format of the tape recovery point time is in the
    -- ISO8601 extended YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
    tapeRecoveryPointTime :: Prelude.Maybe Prelude.POSIX
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'TapeRecoveryPointInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeStatus', 'tapeRecoveryPointInfo_tapeStatus' - The status of the virtual tapes.
--
-- 'tapeARN', 'tapeRecoveryPointInfo_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape.
--
-- 'tapeSizeInBytes', 'tapeRecoveryPointInfo_tapeSizeInBytes' - The size, in bytes, of the virtual tapes to recover.
--
-- 'tapeRecoveryPointTime', 'tapeRecoveryPointInfo_tapeRecoveryPointTime' - The time when the point-in-time view of the virtual tape was replicated
-- for later recovery.
--
-- The default timestamp format of the tape recovery point time is in the
-- ISO8601 extended YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
newTapeRecoveryPointInfo ::
  TapeRecoveryPointInfo
newTapeRecoveryPointInfo =
  TapeRecoveryPointInfo'
    { tapeStatus =
        Prelude.Nothing,
      tapeARN = Prelude.Nothing,
      tapeSizeInBytes = Prelude.Nothing,
      tapeRecoveryPointTime = Prelude.Nothing
    }

-- | The status of the virtual tapes.
tapeRecoveryPointInfo_tapeStatus :: Lens.Lens' TapeRecoveryPointInfo (Prelude.Maybe Prelude.Text)
tapeRecoveryPointInfo_tapeStatus = Lens.lens (\TapeRecoveryPointInfo' {tapeStatus} -> tapeStatus) (\s@TapeRecoveryPointInfo' {} a -> s {tapeStatus = a} :: TapeRecoveryPointInfo)

-- | The Amazon Resource Name (ARN) of the virtual tape.
tapeRecoveryPointInfo_tapeARN :: Lens.Lens' TapeRecoveryPointInfo (Prelude.Maybe Prelude.Text)
tapeRecoveryPointInfo_tapeARN = Lens.lens (\TapeRecoveryPointInfo' {tapeARN} -> tapeARN) (\s@TapeRecoveryPointInfo' {} a -> s {tapeARN = a} :: TapeRecoveryPointInfo)

-- | The size, in bytes, of the virtual tapes to recover.
tapeRecoveryPointInfo_tapeSizeInBytes :: Lens.Lens' TapeRecoveryPointInfo (Prelude.Maybe Prelude.Integer)
tapeRecoveryPointInfo_tapeSizeInBytes = Lens.lens (\TapeRecoveryPointInfo' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@TapeRecoveryPointInfo' {} a -> s {tapeSizeInBytes = a} :: TapeRecoveryPointInfo)

-- | The time when the point-in-time view of the virtual tape was replicated
-- for later recovery.
--
-- The default timestamp format of the tape recovery point time is in the
-- ISO8601 extended YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
tapeRecoveryPointInfo_tapeRecoveryPointTime :: Lens.Lens' TapeRecoveryPointInfo (Prelude.Maybe Prelude.UTCTime)
tapeRecoveryPointInfo_tapeRecoveryPointTime = Lens.lens (\TapeRecoveryPointInfo' {tapeRecoveryPointTime} -> tapeRecoveryPointTime) (\s@TapeRecoveryPointInfo' {} a -> s {tapeRecoveryPointTime = a} :: TapeRecoveryPointInfo) Prelude.. Lens.mapping Prelude._Time

instance Prelude.FromJSON TapeRecoveryPointInfo where
  parseJSON =
    Prelude.withObject
      "TapeRecoveryPointInfo"
      ( \x ->
          TapeRecoveryPointInfo'
            Prelude.<$> (x Prelude..:? "TapeStatus")
            Prelude.<*> (x Prelude..:? "TapeARN")
            Prelude.<*> (x Prelude..:? "TapeSizeInBytes")
            Prelude.<*> (x Prelude..:? "TapeRecoveryPointTime")
      )

instance Prelude.Hashable TapeRecoveryPointInfo

instance Prelude.NFData TapeRecoveryPointInfo
