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
-- Module      : Network.AWS.StorageGateway.Types.TapeArchive
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeArchive where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Represents a virtual tape that is archived in the virtual tape shelf
-- (VTS).
--
-- /See:/ 'newTapeArchive' smart constructor.
data TapeArchive = TapeArchive'
  { -- | The time that the tape entered the custom tape pool.
    --
    -- The default timestamp format is in the ISO8601 extended
    -- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
    poolEntryDate :: Core.Maybe Core.POSIX,
    -- | The current state of the archived virtual tape.
    tapeStatus :: Core.Maybe Core.Text,
    -- | The date the virtual tape was created.
    tapeCreatedDate :: Core.Maybe Core.POSIX,
    -- | The ID of the pool that was used to archive the tape. The tapes in this
    -- pool are archived in the S3 storage class that is associated with the
    -- pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Core.Maybe Core.Text,
    -- | The time that the archiving of the virtual tape was completed.
    --
    -- The default timestamp format is in the ISO8601 extended
    -- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
    completionTime :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the tape gateway that the virtual tape
    -- is being retrieved to.
    --
    -- The virtual tape is retrieved from the virtual tape shelf (VTS).
    retrievedTo :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of an archived virtual tape.
    tapeARN :: Core.Maybe Core.Text,
    kmsKey :: Core.Maybe Core.Text,
    -- | Set to @true@ if the archived tape is stored as write-once-read-many
    -- (WORM).
    worm :: Core.Maybe Core.Bool,
    -- | The barcode that identifies the archived virtual tape.
    tapeBarcode :: Core.Maybe Core.Text,
    -- | The size, in bytes, of data stored on the virtual tape.
    --
    -- This value is not available for tapes created prior to May 13, 2015.
    tapeUsedInBytes :: Core.Maybe Core.Integer,
    -- | The size, in bytes, of the archived virtual tape.
    tapeSizeInBytes :: Core.Maybe Core.Integer,
    -- | If the archived tape is subject to tape retention lock, the date that
    -- the archived tape started being retained.
    retentionStartDate :: Core.Maybe Core.POSIX
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TapeArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolEntryDate', 'tapeArchive_poolEntryDate' - The time that the tape entered the custom tape pool.
--
-- The default timestamp format is in the ISO8601 extended
-- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
--
-- 'tapeStatus', 'tapeArchive_tapeStatus' - The current state of the archived virtual tape.
--
-- 'tapeCreatedDate', 'tapeArchive_tapeCreatedDate' - The date the virtual tape was created.
--
-- 'poolId', 'tapeArchive_poolId' - The ID of the pool that was used to archive the tape. The tapes in this
-- pool are archived in the S3 storage class that is associated with the
-- pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- 'completionTime', 'tapeArchive_completionTime' - The time that the archiving of the virtual tape was completed.
--
-- The default timestamp format is in the ISO8601 extended
-- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
--
-- 'retrievedTo', 'tapeArchive_retrievedTo' - The Amazon Resource Name (ARN) of the tape gateway that the virtual tape
-- is being retrieved to.
--
-- The virtual tape is retrieved from the virtual tape shelf (VTS).
--
-- 'tapeARN', 'tapeArchive_tapeARN' - The Amazon Resource Name (ARN) of an archived virtual tape.
--
-- 'kmsKey', 'tapeArchive_kmsKey' - Undocumented member.
--
-- 'worm', 'tapeArchive_worm' - Set to @true@ if the archived tape is stored as write-once-read-many
-- (WORM).
--
-- 'tapeBarcode', 'tapeArchive_tapeBarcode' - The barcode that identifies the archived virtual tape.
--
-- 'tapeUsedInBytes', 'tapeArchive_tapeUsedInBytes' - The size, in bytes, of data stored on the virtual tape.
--
-- This value is not available for tapes created prior to May 13, 2015.
--
-- 'tapeSizeInBytes', 'tapeArchive_tapeSizeInBytes' - The size, in bytes, of the archived virtual tape.
--
-- 'retentionStartDate', 'tapeArchive_retentionStartDate' - If the archived tape is subject to tape retention lock, the date that
-- the archived tape started being retained.
newTapeArchive ::
  TapeArchive
newTapeArchive =
  TapeArchive'
    { poolEntryDate = Core.Nothing,
      tapeStatus = Core.Nothing,
      tapeCreatedDate = Core.Nothing,
      poolId = Core.Nothing,
      completionTime = Core.Nothing,
      retrievedTo = Core.Nothing,
      tapeARN = Core.Nothing,
      kmsKey = Core.Nothing,
      worm = Core.Nothing,
      tapeBarcode = Core.Nothing,
      tapeUsedInBytes = Core.Nothing,
      tapeSizeInBytes = Core.Nothing,
      retentionStartDate = Core.Nothing
    }

-- | The time that the tape entered the custom tape pool.
--
-- The default timestamp format is in the ISO8601 extended
-- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
tapeArchive_poolEntryDate :: Lens.Lens' TapeArchive (Core.Maybe Core.UTCTime)
tapeArchive_poolEntryDate = Lens.lens (\TapeArchive' {poolEntryDate} -> poolEntryDate) (\s@TapeArchive' {} a -> s {poolEntryDate = a} :: TapeArchive) Core.. Lens.mapping Core._Time

-- | The current state of the archived virtual tape.
tapeArchive_tapeStatus :: Lens.Lens' TapeArchive (Core.Maybe Core.Text)
tapeArchive_tapeStatus = Lens.lens (\TapeArchive' {tapeStatus} -> tapeStatus) (\s@TapeArchive' {} a -> s {tapeStatus = a} :: TapeArchive)

-- | The date the virtual tape was created.
tapeArchive_tapeCreatedDate :: Lens.Lens' TapeArchive (Core.Maybe Core.UTCTime)
tapeArchive_tapeCreatedDate = Lens.lens (\TapeArchive' {tapeCreatedDate} -> tapeCreatedDate) (\s@TapeArchive' {} a -> s {tapeCreatedDate = a} :: TapeArchive) Core.. Lens.mapping Core._Time

-- | The ID of the pool that was used to archive the tape. The tapes in this
-- pool are archived in the S3 storage class that is associated with the
-- pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
tapeArchive_poolId :: Lens.Lens' TapeArchive (Core.Maybe Core.Text)
tapeArchive_poolId = Lens.lens (\TapeArchive' {poolId} -> poolId) (\s@TapeArchive' {} a -> s {poolId = a} :: TapeArchive)

-- | The time that the archiving of the virtual tape was completed.
--
-- The default timestamp format is in the ISO8601 extended
-- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
tapeArchive_completionTime :: Lens.Lens' TapeArchive (Core.Maybe Core.UTCTime)
tapeArchive_completionTime = Lens.lens (\TapeArchive' {completionTime} -> completionTime) (\s@TapeArchive' {} a -> s {completionTime = a} :: TapeArchive) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the tape gateway that the virtual tape
-- is being retrieved to.
--
-- The virtual tape is retrieved from the virtual tape shelf (VTS).
tapeArchive_retrievedTo :: Lens.Lens' TapeArchive (Core.Maybe Core.Text)
tapeArchive_retrievedTo = Lens.lens (\TapeArchive' {retrievedTo} -> retrievedTo) (\s@TapeArchive' {} a -> s {retrievedTo = a} :: TapeArchive)

-- | The Amazon Resource Name (ARN) of an archived virtual tape.
tapeArchive_tapeARN :: Lens.Lens' TapeArchive (Core.Maybe Core.Text)
tapeArchive_tapeARN = Lens.lens (\TapeArchive' {tapeARN} -> tapeARN) (\s@TapeArchive' {} a -> s {tapeARN = a} :: TapeArchive)

-- | Undocumented member.
tapeArchive_kmsKey :: Lens.Lens' TapeArchive (Core.Maybe Core.Text)
tapeArchive_kmsKey = Lens.lens (\TapeArchive' {kmsKey} -> kmsKey) (\s@TapeArchive' {} a -> s {kmsKey = a} :: TapeArchive)

-- | Set to @true@ if the archived tape is stored as write-once-read-many
-- (WORM).
tapeArchive_worm :: Lens.Lens' TapeArchive (Core.Maybe Core.Bool)
tapeArchive_worm = Lens.lens (\TapeArchive' {worm} -> worm) (\s@TapeArchive' {} a -> s {worm = a} :: TapeArchive)

-- | The barcode that identifies the archived virtual tape.
tapeArchive_tapeBarcode :: Lens.Lens' TapeArchive (Core.Maybe Core.Text)
tapeArchive_tapeBarcode = Lens.lens (\TapeArchive' {tapeBarcode} -> tapeBarcode) (\s@TapeArchive' {} a -> s {tapeBarcode = a} :: TapeArchive)

-- | The size, in bytes, of data stored on the virtual tape.
--
-- This value is not available for tapes created prior to May 13, 2015.
tapeArchive_tapeUsedInBytes :: Lens.Lens' TapeArchive (Core.Maybe Core.Integer)
tapeArchive_tapeUsedInBytes = Lens.lens (\TapeArchive' {tapeUsedInBytes} -> tapeUsedInBytes) (\s@TapeArchive' {} a -> s {tapeUsedInBytes = a} :: TapeArchive)

-- | The size, in bytes, of the archived virtual tape.
tapeArchive_tapeSizeInBytes :: Lens.Lens' TapeArchive (Core.Maybe Core.Integer)
tapeArchive_tapeSizeInBytes = Lens.lens (\TapeArchive' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@TapeArchive' {} a -> s {tapeSizeInBytes = a} :: TapeArchive)

-- | If the archived tape is subject to tape retention lock, the date that
-- the archived tape started being retained.
tapeArchive_retentionStartDate :: Lens.Lens' TapeArchive (Core.Maybe Core.UTCTime)
tapeArchive_retentionStartDate = Lens.lens (\TapeArchive' {retentionStartDate} -> retentionStartDate) (\s@TapeArchive' {} a -> s {retentionStartDate = a} :: TapeArchive) Core.. Lens.mapping Core._Time

instance Core.FromJSON TapeArchive where
  parseJSON =
    Core.withObject
      "TapeArchive"
      ( \x ->
          TapeArchive'
            Core.<$> (x Core..:? "PoolEntryDate")
            Core.<*> (x Core..:? "TapeStatus")
            Core.<*> (x Core..:? "TapeCreatedDate")
            Core.<*> (x Core..:? "PoolId")
            Core.<*> (x Core..:? "CompletionTime")
            Core.<*> (x Core..:? "RetrievedTo")
            Core.<*> (x Core..:? "TapeARN")
            Core.<*> (x Core..:? "KMSKey")
            Core.<*> (x Core..:? "Worm")
            Core.<*> (x Core..:? "TapeBarcode")
            Core.<*> (x Core..:? "TapeUsedInBytes")
            Core.<*> (x Core..:? "TapeSizeInBytes")
            Core.<*> (x Core..:? "RetentionStartDate")
      )

instance Core.Hashable TapeArchive

instance Core.NFData TapeArchive
