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
-- Module      : Network.AWS.StorageGateway.Types.Tape
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.Tape where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a virtual tape object.
--
-- /See:/ 'newTape' smart constructor.
data Tape = Tape'
  { -- | The date that the tape enters a custom tape pool.
    poolEntryDate :: Core.Maybe Core.POSIX,
    -- | The current state of the virtual tape.
    tapeStatus :: Core.Maybe Core.Text,
    -- | The date the virtual tape was created.
    tapeCreatedDate :: Core.Maybe Core.POSIX,
    -- | The ID of the pool that contains tapes that will be archived. The tapes
    -- in this pool are archived in the S3 storage class that is associated
    -- with the pool. When you use your backup application to eject the tape,
    -- the tape is archived directly into the storage class (S3 Glacier or S3
    -- Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Core.Maybe Core.Text,
    -- | The virtual tape library (VTL) device that the virtual tape is
    -- associated with.
    vTLDevice :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape.
    tapeARN :: Core.Maybe Core.Text,
    kmsKey :: Core.Maybe Core.Text,
    -- | If the tape is archived as write-once-read-many (WORM), this value is
    -- @true@.
    worm :: Core.Maybe Core.Bool,
    -- | The barcode that identifies a specific virtual tape.
    tapeBarcode :: Core.Maybe Core.Text,
    -- | The size, in bytes, of data stored on the virtual tape.
    --
    -- This value is not available for tapes created prior to May 13, 2015.
    tapeUsedInBytes :: Core.Maybe Core.Integer,
    -- | The size, in bytes, of the virtual tape capacity.
    tapeSizeInBytes :: Core.Maybe Core.Integer,
    -- | The date that the tape is first archived with tape retention lock
    -- enabled.
    retentionStartDate :: Core.Maybe Core.POSIX,
    -- | For archiving virtual tapes, indicates how much data remains to be
    -- uploaded before archiving is complete.
    --
    -- Range: 0 (not started) to 100 (complete).
    progress :: Core.Maybe Core.Double
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'Tape' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolEntryDate', 'tape_poolEntryDate' - The date that the tape enters a custom tape pool.
--
-- 'tapeStatus', 'tape_tapeStatus' - The current state of the virtual tape.
--
-- 'tapeCreatedDate', 'tape_tapeCreatedDate' - The date the virtual tape was created.
--
-- 'poolId', 'tape_poolId' - The ID of the pool that contains tapes that will be archived. The tapes
-- in this pool are archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- 'vTLDevice', 'tape_vTLDevice' - The virtual tape library (VTL) device that the virtual tape is
-- associated with.
--
-- 'tapeARN', 'tape_tapeARN' - The Amazon Resource Name (ARN) of the virtual tape.
--
-- 'kmsKey', 'tape_kmsKey' - Undocumented member.
--
-- 'worm', 'tape_worm' - If the tape is archived as write-once-read-many (WORM), this value is
-- @true@.
--
-- 'tapeBarcode', 'tape_tapeBarcode' - The barcode that identifies a specific virtual tape.
--
-- 'tapeUsedInBytes', 'tape_tapeUsedInBytes' - The size, in bytes, of data stored on the virtual tape.
--
-- This value is not available for tapes created prior to May 13, 2015.
--
-- 'tapeSizeInBytes', 'tape_tapeSizeInBytes' - The size, in bytes, of the virtual tape capacity.
--
-- 'retentionStartDate', 'tape_retentionStartDate' - The date that the tape is first archived with tape retention lock
-- enabled.
--
-- 'progress', 'tape_progress' - For archiving virtual tapes, indicates how much data remains to be
-- uploaded before archiving is complete.
--
-- Range: 0 (not started) to 100 (complete).
newTape ::
  Tape
newTape =
  Tape'
    { poolEntryDate = Core.Nothing,
      tapeStatus = Core.Nothing,
      tapeCreatedDate = Core.Nothing,
      poolId = Core.Nothing,
      vTLDevice = Core.Nothing,
      tapeARN = Core.Nothing,
      kmsKey = Core.Nothing,
      worm = Core.Nothing,
      tapeBarcode = Core.Nothing,
      tapeUsedInBytes = Core.Nothing,
      tapeSizeInBytes = Core.Nothing,
      retentionStartDate = Core.Nothing,
      progress = Core.Nothing
    }

-- | The date that the tape enters a custom tape pool.
tape_poolEntryDate :: Lens.Lens' Tape (Core.Maybe Core.UTCTime)
tape_poolEntryDate = Lens.lens (\Tape' {poolEntryDate} -> poolEntryDate) (\s@Tape' {} a -> s {poolEntryDate = a} :: Tape) Core.. Lens.mapping Core._Time

-- | The current state of the virtual tape.
tape_tapeStatus :: Lens.Lens' Tape (Core.Maybe Core.Text)
tape_tapeStatus = Lens.lens (\Tape' {tapeStatus} -> tapeStatus) (\s@Tape' {} a -> s {tapeStatus = a} :: Tape)

-- | The date the virtual tape was created.
tape_tapeCreatedDate :: Lens.Lens' Tape (Core.Maybe Core.UTCTime)
tape_tapeCreatedDate = Lens.lens (\Tape' {tapeCreatedDate} -> tapeCreatedDate) (\s@Tape' {} a -> s {tapeCreatedDate = a} :: Tape) Core.. Lens.mapping Core._Time

-- | The ID of the pool that contains tapes that will be archived. The tapes
-- in this pool are archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
tape_poolId :: Lens.Lens' Tape (Core.Maybe Core.Text)
tape_poolId = Lens.lens (\Tape' {poolId} -> poolId) (\s@Tape' {} a -> s {poolId = a} :: Tape)

-- | The virtual tape library (VTL) device that the virtual tape is
-- associated with.
tape_vTLDevice :: Lens.Lens' Tape (Core.Maybe Core.Text)
tape_vTLDevice = Lens.lens (\Tape' {vTLDevice} -> vTLDevice) (\s@Tape' {} a -> s {vTLDevice = a} :: Tape)

-- | The Amazon Resource Name (ARN) of the virtual tape.
tape_tapeARN :: Lens.Lens' Tape (Core.Maybe Core.Text)
tape_tapeARN = Lens.lens (\Tape' {tapeARN} -> tapeARN) (\s@Tape' {} a -> s {tapeARN = a} :: Tape)

-- | Undocumented member.
tape_kmsKey :: Lens.Lens' Tape (Core.Maybe Core.Text)
tape_kmsKey = Lens.lens (\Tape' {kmsKey} -> kmsKey) (\s@Tape' {} a -> s {kmsKey = a} :: Tape)

-- | If the tape is archived as write-once-read-many (WORM), this value is
-- @true@.
tape_worm :: Lens.Lens' Tape (Core.Maybe Core.Bool)
tape_worm = Lens.lens (\Tape' {worm} -> worm) (\s@Tape' {} a -> s {worm = a} :: Tape)

-- | The barcode that identifies a specific virtual tape.
tape_tapeBarcode :: Lens.Lens' Tape (Core.Maybe Core.Text)
tape_tapeBarcode = Lens.lens (\Tape' {tapeBarcode} -> tapeBarcode) (\s@Tape' {} a -> s {tapeBarcode = a} :: Tape)

-- | The size, in bytes, of data stored on the virtual tape.
--
-- This value is not available for tapes created prior to May 13, 2015.
tape_tapeUsedInBytes :: Lens.Lens' Tape (Core.Maybe Core.Integer)
tape_tapeUsedInBytes = Lens.lens (\Tape' {tapeUsedInBytes} -> tapeUsedInBytes) (\s@Tape' {} a -> s {tapeUsedInBytes = a} :: Tape)

-- | The size, in bytes, of the virtual tape capacity.
tape_tapeSizeInBytes :: Lens.Lens' Tape (Core.Maybe Core.Integer)
tape_tapeSizeInBytes = Lens.lens (\Tape' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@Tape' {} a -> s {tapeSizeInBytes = a} :: Tape)

-- | The date that the tape is first archived with tape retention lock
-- enabled.
tape_retentionStartDate :: Lens.Lens' Tape (Core.Maybe Core.UTCTime)
tape_retentionStartDate = Lens.lens (\Tape' {retentionStartDate} -> retentionStartDate) (\s@Tape' {} a -> s {retentionStartDate = a} :: Tape) Core.. Lens.mapping Core._Time

-- | For archiving virtual tapes, indicates how much data remains to be
-- uploaded before archiving is complete.
--
-- Range: 0 (not started) to 100 (complete).
tape_progress :: Lens.Lens' Tape (Core.Maybe Core.Double)
tape_progress = Lens.lens (\Tape' {progress} -> progress) (\s@Tape' {} a -> s {progress = a} :: Tape)

instance Core.FromJSON Tape where
  parseJSON =
    Core.withObject
      "Tape"
      ( \x ->
          Tape'
            Core.<$> (x Core..:? "PoolEntryDate")
            Core.<*> (x Core..:? "TapeStatus")
            Core.<*> (x Core..:? "TapeCreatedDate")
            Core.<*> (x Core..:? "PoolId")
            Core.<*> (x Core..:? "VTLDevice")
            Core.<*> (x Core..:? "TapeARN")
            Core.<*> (x Core..:? "KMSKey")
            Core.<*> (x Core..:? "Worm")
            Core.<*> (x Core..:? "TapeBarcode")
            Core.<*> (x Core..:? "TapeUsedInBytes")
            Core.<*> (x Core..:? "TapeSizeInBytes")
            Core.<*> (x Core..:? "RetentionStartDate")
            Core.<*> (x Core..:? "Progress")
      )

instance Core.Hashable Tape

instance Core.NFData Tape
