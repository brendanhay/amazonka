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
import qualified Network.AWS.Prelude as Prelude

-- | Represents a virtual tape that is archived in the virtual tape shelf
-- (VTS).
--
-- /See:/ 'newTapeArchive' smart constructor.
data TapeArchive = TapeArchive'
  { -- | The barcode that identifies the archived virtual tape.
    tapeBarcode :: Prelude.Maybe Prelude.Text,
    -- | The current state of the archived virtual tape.
    tapeStatus :: Prelude.Maybe Prelude.Text,
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of an archived virtual tape.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of the archived virtual tape.
    tapeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The time that the archiving of the virtual tape was completed.
    --
    -- The default timestamp format is in the ISO8601 extended
    -- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
    completionTime :: Prelude.Maybe Core.POSIX,
    -- | The ID of the pool that was used to archive the tape. The tapes in this
    -- pool are archived in the S3 storage class that is associated with the
    -- pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of data stored on the virtual tape.
    --
    -- This value is not available for tapes created prior to May 13, 2015.
    tapeUsedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The date the virtual tape was created.
    tapeCreatedDate :: Prelude.Maybe Core.POSIX,
    -- | The time that the tape entered the custom tape pool.
    --
    -- The default timestamp format is in the ISO8601 extended
    -- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
    poolEntryDate :: Prelude.Maybe Core.POSIX,
    -- | Set to @true@ if the archived tape is stored as write-once-read-many
    -- (WORM).
    worm :: Prelude.Maybe Prelude.Bool,
    -- | If the archived tape is subject to tape retention lock, the date that
    -- the archived tape started being retained.
    retentionStartDate :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the tape gateway that the virtual tape
    -- is being retrieved to.
    --
    -- The virtual tape is retrieved from the virtual tape shelf (VTS).
    retrievedTo :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TapeArchive' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tapeBarcode', 'tapeArchive_tapeBarcode' - The barcode that identifies the archived virtual tape.
--
-- 'tapeStatus', 'tapeArchive_tapeStatus' - The current state of the archived virtual tape.
--
-- 'kmsKey', 'tapeArchive_kmsKey' - Undocumented member.
--
-- 'tapeARN', 'tapeArchive_tapeARN' - The Amazon Resource Name (ARN) of an archived virtual tape.
--
-- 'tapeSizeInBytes', 'tapeArchive_tapeSizeInBytes' - The size, in bytes, of the archived virtual tape.
--
-- 'completionTime', 'tapeArchive_completionTime' - The time that the archiving of the virtual tape was completed.
--
-- The default timestamp format is in the ISO8601 extended
-- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
--
-- 'poolId', 'tapeArchive_poolId' - The ID of the pool that was used to archive the tape. The tapes in this
-- pool are archived in the S3 storage class that is associated with the
-- pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- 'tapeUsedInBytes', 'tapeArchive_tapeUsedInBytes' - The size, in bytes, of data stored on the virtual tape.
--
-- This value is not available for tapes created prior to May 13, 2015.
--
-- 'tapeCreatedDate', 'tapeArchive_tapeCreatedDate' - The date the virtual tape was created.
--
-- 'poolEntryDate', 'tapeArchive_poolEntryDate' - The time that the tape entered the custom tape pool.
--
-- The default timestamp format is in the ISO8601 extended
-- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
--
-- 'worm', 'tapeArchive_worm' - Set to @true@ if the archived tape is stored as write-once-read-many
-- (WORM).
--
-- 'retentionStartDate', 'tapeArchive_retentionStartDate' - If the archived tape is subject to tape retention lock, the date that
-- the archived tape started being retained.
--
-- 'retrievedTo', 'tapeArchive_retrievedTo' - The Amazon Resource Name (ARN) of the tape gateway that the virtual tape
-- is being retrieved to.
--
-- The virtual tape is retrieved from the virtual tape shelf (VTS).
newTapeArchive ::
  TapeArchive
newTapeArchive =
  TapeArchive'
    { tapeBarcode = Prelude.Nothing,
      tapeStatus = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      tapeARN = Prelude.Nothing,
      tapeSizeInBytes = Prelude.Nothing,
      completionTime = Prelude.Nothing,
      poolId = Prelude.Nothing,
      tapeUsedInBytes = Prelude.Nothing,
      tapeCreatedDate = Prelude.Nothing,
      poolEntryDate = Prelude.Nothing,
      worm = Prelude.Nothing,
      retentionStartDate = Prelude.Nothing,
      retrievedTo = Prelude.Nothing
    }

-- | The barcode that identifies the archived virtual tape.
tapeArchive_tapeBarcode :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.Text)
tapeArchive_tapeBarcode = Lens.lens (\TapeArchive' {tapeBarcode} -> tapeBarcode) (\s@TapeArchive' {} a -> s {tapeBarcode = a} :: TapeArchive)

-- | The current state of the archived virtual tape.
tapeArchive_tapeStatus :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.Text)
tapeArchive_tapeStatus = Lens.lens (\TapeArchive' {tapeStatus} -> tapeStatus) (\s@TapeArchive' {} a -> s {tapeStatus = a} :: TapeArchive)

-- | Undocumented member.
tapeArchive_kmsKey :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.Text)
tapeArchive_kmsKey = Lens.lens (\TapeArchive' {kmsKey} -> kmsKey) (\s@TapeArchive' {} a -> s {kmsKey = a} :: TapeArchive)

-- | The Amazon Resource Name (ARN) of an archived virtual tape.
tapeArchive_tapeARN :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.Text)
tapeArchive_tapeARN = Lens.lens (\TapeArchive' {tapeARN} -> tapeARN) (\s@TapeArchive' {} a -> s {tapeARN = a} :: TapeArchive)

-- | The size, in bytes, of the archived virtual tape.
tapeArchive_tapeSizeInBytes :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.Integer)
tapeArchive_tapeSizeInBytes = Lens.lens (\TapeArchive' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@TapeArchive' {} a -> s {tapeSizeInBytes = a} :: TapeArchive)

-- | The time that the archiving of the virtual tape was completed.
--
-- The default timestamp format is in the ISO8601 extended
-- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
tapeArchive_completionTime :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.UTCTime)
tapeArchive_completionTime = Lens.lens (\TapeArchive' {completionTime} -> completionTime) (\s@TapeArchive' {} a -> s {completionTime = a} :: TapeArchive) Prelude.. Lens.mapping Core._Time

-- | The ID of the pool that was used to archive the tape. The tapes in this
-- pool are archived in the S3 storage class that is associated with the
-- pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
tapeArchive_poolId :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.Text)
tapeArchive_poolId = Lens.lens (\TapeArchive' {poolId} -> poolId) (\s@TapeArchive' {} a -> s {poolId = a} :: TapeArchive)

-- | The size, in bytes, of data stored on the virtual tape.
--
-- This value is not available for tapes created prior to May 13, 2015.
tapeArchive_tapeUsedInBytes :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.Integer)
tapeArchive_tapeUsedInBytes = Lens.lens (\TapeArchive' {tapeUsedInBytes} -> tapeUsedInBytes) (\s@TapeArchive' {} a -> s {tapeUsedInBytes = a} :: TapeArchive)

-- | The date the virtual tape was created.
tapeArchive_tapeCreatedDate :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.UTCTime)
tapeArchive_tapeCreatedDate = Lens.lens (\TapeArchive' {tapeCreatedDate} -> tapeCreatedDate) (\s@TapeArchive' {} a -> s {tapeCreatedDate = a} :: TapeArchive) Prelude.. Lens.mapping Core._Time

-- | The time that the tape entered the custom tape pool.
--
-- The default timestamp format is in the ISO8601 extended
-- YYYY-MM-DD\'T\'HH:MM:SS\'Z\' format.
tapeArchive_poolEntryDate :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.UTCTime)
tapeArchive_poolEntryDate = Lens.lens (\TapeArchive' {poolEntryDate} -> poolEntryDate) (\s@TapeArchive' {} a -> s {poolEntryDate = a} :: TapeArchive) Prelude.. Lens.mapping Core._Time

-- | Set to @true@ if the archived tape is stored as write-once-read-many
-- (WORM).
tapeArchive_worm :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.Bool)
tapeArchive_worm = Lens.lens (\TapeArchive' {worm} -> worm) (\s@TapeArchive' {} a -> s {worm = a} :: TapeArchive)

-- | If the archived tape is subject to tape retention lock, the date that
-- the archived tape started being retained.
tapeArchive_retentionStartDate :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.UTCTime)
tapeArchive_retentionStartDate = Lens.lens (\TapeArchive' {retentionStartDate} -> retentionStartDate) (\s@TapeArchive' {} a -> s {retentionStartDate = a} :: TapeArchive) Prelude.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the tape gateway that the virtual tape
-- is being retrieved to.
--
-- The virtual tape is retrieved from the virtual tape shelf (VTS).
tapeArchive_retrievedTo :: Lens.Lens' TapeArchive (Prelude.Maybe Prelude.Text)
tapeArchive_retrievedTo = Lens.lens (\TapeArchive' {retrievedTo} -> retrievedTo) (\s@TapeArchive' {} a -> s {retrievedTo = a} :: TapeArchive)

instance Core.FromJSON TapeArchive where
  parseJSON =
    Core.withObject
      "TapeArchive"
      ( \x ->
          TapeArchive'
            Prelude.<$> (x Core..:? "TapeBarcode")
            Prelude.<*> (x Core..:? "TapeStatus")
            Prelude.<*> (x Core..:? "KMSKey")
            Prelude.<*> (x Core..:? "TapeARN")
            Prelude.<*> (x Core..:? "TapeSizeInBytes")
            Prelude.<*> (x Core..:? "CompletionTime")
            Prelude.<*> (x Core..:? "PoolId")
            Prelude.<*> (x Core..:? "TapeUsedInBytes")
            Prelude.<*> (x Core..:? "TapeCreatedDate")
            Prelude.<*> (x Core..:? "PoolEntryDate")
            Prelude.<*> (x Core..:? "Worm")
            Prelude.<*> (x Core..:? "RetentionStartDate")
            Prelude.<*> (x Core..:? "RetrievedTo")
      )

instance Prelude.Hashable TapeArchive

instance Prelude.NFData TapeArchive
