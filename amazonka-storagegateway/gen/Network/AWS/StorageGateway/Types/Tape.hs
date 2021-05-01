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
-- Module      : Network.AWS.StorageGateway.Types.Tape
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.Tape where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a virtual tape object.
--
-- /See:/ 'newTape' smart constructor.
data Tape = Tape'
  { -- | The date that the tape enters a custom tape pool.
    poolEntryDate :: Prelude.Maybe Prelude.POSIX,
    -- | The current state of the virtual tape.
    tapeStatus :: Prelude.Maybe Prelude.Text,
    -- | The date the virtual tape was created.
    tapeCreatedDate :: Prelude.Maybe Prelude.POSIX,
    -- | The ID of the pool that contains tapes that will be archived. The tapes
    -- in this pool are archived in the S3 storage class that is associated
    -- with the pool. When you use your backup application to eject the tape,
    -- the tape is archived directly into the storage class (S3 Glacier or S3
    -- Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The virtual tape library (VTL) device that the virtual tape is
    -- associated with.
    vTLDevice :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape.
    tapeARN :: Prelude.Maybe Prelude.Text,
    kmsKey :: Prelude.Maybe Prelude.Text,
    -- | If the tape is archived as write-once-read-many (WORM), this value is
    -- @true@.
    worm :: Prelude.Maybe Prelude.Bool,
    -- | The barcode that identifies a specific virtual tape.
    tapeBarcode :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of data stored on the virtual tape.
    --
    -- This value is not available for tapes created prior to May 13, 2015.
    tapeUsedInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The size, in bytes, of the virtual tape capacity.
    tapeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The date that the tape is first archived with tape retention lock
    -- enabled.
    retentionStartDate :: Prelude.Maybe Prelude.POSIX,
    -- | For archiving virtual tapes, indicates how much data remains to be
    -- uploaded before archiving is complete.
    --
    -- Range: 0 (not started) to 100 (complete).
    progress :: Prelude.Maybe Prelude.Double
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { poolEntryDate = Prelude.Nothing,
      tapeStatus = Prelude.Nothing,
      tapeCreatedDate = Prelude.Nothing,
      poolId = Prelude.Nothing,
      vTLDevice = Prelude.Nothing,
      tapeARN = Prelude.Nothing,
      kmsKey = Prelude.Nothing,
      worm = Prelude.Nothing,
      tapeBarcode = Prelude.Nothing,
      tapeUsedInBytes = Prelude.Nothing,
      tapeSizeInBytes = Prelude.Nothing,
      retentionStartDate = Prelude.Nothing,
      progress = Prelude.Nothing
    }

-- | The date that the tape enters a custom tape pool.
tape_poolEntryDate :: Lens.Lens' Tape (Prelude.Maybe Prelude.UTCTime)
tape_poolEntryDate = Lens.lens (\Tape' {poolEntryDate} -> poolEntryDate) (\s@Tape' {} a -> s {poolEntryDate = a} :: Tape) Prelude.. Lens.mapping Prelude._Time

-- | The current state of the virtual tape.
tape_tapeStatus :: Lens.Lens' Tape (Prelude.Maybe Prelude.Text)
tape_tapeStatus = Lens.lens (\Tape' {tapeStatus} -> tapeStatus) (\s@Tape' {} a -> s {tapeStatus = a} :: Tape)

-- | The date the virtual tape was created.
tape_tapeCreatedDate :: Lens.Lens' Tape (Prelude.Maybe Prelude.UTCTime)
tape_tapeCreatedDate = Lens.lens (\Tape' {tapeCreatedDate} -> tapeCreatedDate) (\s@Tape' {} a -> s {tapeCreatedDate = a} :: Tape) Prelude.. Lens.mapping Prelude._Time

-- | The ID of the pool that contains tapes that will be archived. The tapes
-- in this pool are archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
tape_poolId :: Lens.Lens' Tape (Prelude.Maybe Prelude.Text)
tape_poolId = Lens.lens (\Tape' {poolId} -> poolId) (\s@Tape' {} a -> s {poolId = a} :: Tape)

-- | The virtual tape library (VTL) device that the virtual tape is
-- associated with.
tape_vTLDevice :: Lens.Lens' Tape (Prelude.Maybe Prelude.Text)
tape_vTLDevice = Lens.lens (\Tape' {vTLDevice} -> vTLDevice) (\s@Tape' {} a -> s {vTLDevice = a} :: Tape)

-- | The Amazon Resource Name (ARN) of the virtual tape.
tape_tapeARN :: Lens.Lens' Tape (Prelude.Maybe Prelude.Text)
tape_tapeARN = Lens.lens (\Tape' {tapeARN} -> tapeARN) (\s@Tape' {} a -> s {tapeARN = a} :: Tape)

-- | Undocumented member.
tape_kmsKey :: Lens.Lens' Tape (Prelude.Maybe Prelude.Text)
tape_kmsKey = Lens.lens (\Tape' {kmsKey} -> kmsKey) (\s@Tape' {} a -> s {kmsKey = a} :: Tape)

-- | If the tape is archived as write-once-read-many (WORM), this value is
-- @true@.
tape_worm :: Lens.Lens' Tape (Prelude.Maybe Prelude.Bool)
tape_worm = Lens.lens (\Tape' {worm} -> worm) (\s@Tape' {} a -> s {worm = a} :: Tape)

-- | The barcode that identifies a specific virtual tape.
tape_tapeBarcode :: Lens.Lens' Tape (Prelude.Maybe Prelude.Text)
tape_tapeBarcode = Lens.lens (\Tape' {tapeBarcode} -> tapeBarcode) (\s@Tape' {} a -> s {tapeBarcode = a} :: Tape)

-- | The size, in bytes, of data stored on the virtual tape.
--
-- This value is not available for tapes created prior to May 13, 2015.
tape_tapeUsedInBytes :: Lens.Lens' Tape (Prelude.Maybe Prelude.Integer)
tape_tapeUsedInBytes = Lens.lens (\Tape' {tapeUsedInBytes} -> tapeUsedInBytes) (\s@Tape' {} a -> s {tapeUsedInBytes = a} :: Tape)

-- | The size, in bytes, of the virtual tape capacity.
tape_tapeSizeInBytes :: Lens.Lens' Tape (Prelude.Maybe Prelude.Integer)
tape_tapeSizeInBytes = Lens.lens (\Tape' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@Tape' {} a -> s {tapeSizeInBytes = a} :: Tape)

-- | The date that the tape is first archived with tape retention lock
-- enabled.
tape_retentionStartDate :: Lens.Lens' Tape (Prelude.Maybe Prelude.UTCTime)
tape_retentionStartDate = Lens.lens (\Tape' {retentionStartDate} -> retentionStartDate) (\s@Tape' {} a -> s {retentionStartDate = a} :: Tape) Prelude.. Lens.mapping Prelude._Time

-- | For archiving virtual tapes, indicates how much data remains to be
-- uploaded before archiving is complete.
--
-- Range: 0 (not started) to 100 (complete).
tape_progress :: Lens.Lens' Tape (Prelude.Maybe Prelude.Double)
tape_progress = Lens.lens (\Tape' {progress} -> progress) (\s@Tape' {} a -> s {progress = a} :: Tape)

instance Prelude.FromJSON Tape where
  parseJSON =
    Prelude.withObject
      "Tape"
      ( \x ->
          Tape'
            Prelude.<$> (x Prelude..:? "PoolEntryDate")
            Prelude.<*> (x Prelude..:? "TapeStatus")
            Prelude.<*> (x Prelude..:? "TapeCreatedDate")
            Prelude.<*> (x Prelude..:? "PoolId")
            Prelude.<*> (x Prelude..:? "VTLDevice")
            Prelude.<*> (x Prelude..:? "TapeARN")
            Prelude.<*> (x Prelude..:? "KMSKey")
            Prelude.<*> (x Prelude..:? "Worm")
            Prelude.<*> (x Prelude..:? "TapeBarcode")
            Prelude.<*> (x Prelude..:? "TapeUsedInBytes")
            Prelude.<*> (x Prelude..:? "TapeSizeInBytes")
            Prelude.<*> (x Prelude..:? "RetentionStartDate")
            Prelude.<*> (x Prelude..:? "Progress")
      )

instance Prelude.Hashable Tape

instance Prelude.NFData Tape
