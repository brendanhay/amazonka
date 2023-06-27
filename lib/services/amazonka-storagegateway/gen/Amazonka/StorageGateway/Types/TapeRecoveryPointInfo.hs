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
-- Module      : Amazonka.StorageGateway.Types.TapeRecoveryPointInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.TapeRecoveryPointInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a recovery point.
--
-- /See:/ 'newTapeRecoveryPointInfo' smart constructor.
data TapeRecoveryPointInfo = TapeRecoveryPointInfo'
  { -- | The Amazon Resource Name (ARN) of the virtual tape.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The time when the point-in-time view of the virtual tape was replicated
    -- for later recovery.
    --
    -- The default timestamp format of the tape recovery point time is in the
    -- ISO8601 extended YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
    tapeRecoveryPointTime :: Prelude.Maybe Data.POSIX,
    -- | The size, in bytes, of the virtual tapes to recover.
    tapeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The status of the virtual tapes.
    tapeStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TapeRecoveryPointInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeARN', 'tapeRecoveryPointInfo_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape.
--
-- 'tapeRecoveryPointTime', 'tapeRecoveryPointInfo_tapeRecoveryPointTime' - The time when the point-in-time view of the virtual tape was replicated
-- for later recovery.
--
-- The default timestamp format of the tape recovery point time is in the
-- ISO8601 extended YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
--
-- 'tapeSizeInBytes', 'tapeRecoveryPointInfo_tapeSizeInBytes' - The size, in bytes, of the virtual tapes to recover.
--
-- 'tapeStatus', 'tapeRecoveryPointInfo_tapeStatus' - The status of the virtual tapes.
newTapeRecoveryPointInfo ::
  TapeRecoveryPointInfo
newTapeRecoveryPointInfo =
  TapeRecoveryPointInfo'
    { tapeARN = Prelude.Nothing,
      tapeRecoveryPointTime = Prelude.Nothing,
      tapeSizeInBytes = Prelude.Nothing,
      tapeStatus = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the virtual tape.
tapeRecoveryPointInfo_tapeARN :: Lens.Lens' TapeRecoveryPointInfo (Prelude.Maybe Prelude.Text)
tapeRecoveryPointInfo_tapeARN = Lens.lens (\TapeRecoveryPointInfo' {tapeARN} -> tapeARN) (\s@TapeRecoveryPointInfo' {} a -> s {tapeARN = a} :: TapeRecoveryPointInfo)

-- | The time when the point-in-time view of the virtual tape was replicated
-- for later recovery.
--
-- The default timestamp format of the tape recovery point time is in the
-- ISO8601 extended YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
tapeRecoveryPointInfo_tapeRecoveryPointTime :: Lens.Lens' TapeRecoveryPointInfo (Prelude.Maybe Prelude.UTCTime)
tapeRecoveryPointInfo_tapeRecoveryPointTime = Lens.lens (\TapeRecoveryPointInfo' {tapeRecoveryPointTime} -> tapeRecoveryPointTime) (\s@TapeRecoveryPointInfo' {} a -> s {tapeRecoveryPointTime = a} :: TapeRecoveryPointInfo) Prelude.. Lens.mapping Data._Time

-- | The size, in bytes, of the virtual tapes to recover.
tapeRecoveryPointInfo_tapeSizeInBytes :: Lens.Lens' TapeRecoveryPointInfo (Prelude.Maybe Prelude.Integer)
tapeRecoveryPointInfo_tapeSizeInBytes = Lens.lens (\TapeRecoveryPointInfo' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@TapeRecoveryPointInfo' {} a -> s {tapeSizeInBytes = a} :: TapeRecoveryPointInfo)

-- | The status of the virtual tapes.
tapeRecoveryPointInfo_tapeStatus :: Lens.Lens' TapeRecoveryPointInfo (Prelude.Maybe Prelude.Text)
tapeRecoveryPointInfo_tapeStatus = Lens.lens (\TapeRecoveryPointInfo' {tapeStatus} -> tapeStatus) (\s@TapeRecoveryPointInfo' {} a -> s {tapeStatus = a} :: TapeRecoveryPointInfo)

instance Data.FromJSON TapeRecoveryPointInfo where
  parseJSON =
    Data.withObject
      "TapeRecoveryPointInfo"
      ( \x ->
          TapeRecoveryPointInfo'
            Prelude.<$> (x Data..:? "TapeARN")
            Prelude.<*> (x Data..:? "TapeRecoveryPointTime")
            Prelude.<*> (x Data..:? "TapeSizeInBytes")
            Prelude.<*> (x Data..:? "TapeStatus")
      )

instance Prelude.Hashable TapeRecoveryPointInfo where
  hashWithSalt _salt TapeRecoveryPointInfo' {..} =
    _salt
      `Prelude.hashWithSalt` tapeARN
      `Prelude.hashWithSalt` tapeRecoveryPointTime
      `Prelude.hashWithSalt` tapeSizeInBytes
      `Prelude.hashWithSalt` tapeStatus

instance Prelude.NFData TapeRecoveryPointInfo where
  rnf TapeRecoveryPointInfo' {..} =
    Prelude.rnf tapeARN
      `Prelude.seq` Prelude.rnf tapeRecoveryPointTime
      `Prelude.seq` Prelude.rnf tapeSizeInBytes
      `Prelude.seq` Prelude.rnf tapeStatus
