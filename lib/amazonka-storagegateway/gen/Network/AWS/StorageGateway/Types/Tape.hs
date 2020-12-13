{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.Tape
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.Tape
  ( Tape (..),

    -- * Smart constructor
    mkTape,

    -- * Lenses
    tTapeBarcode,
    tTapeStatus,
    tKMSKey,
    tTapeARN,
    tProgress,
    tTapeSizeInBytes,
    tVTLDevice,
    tPoolId,
    tTapeUsedInBytes,
    tTapeCreatedDate,
    tPoolEntryDate,
    tWorm,
    tRetentionStartDate,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Describes a virtual tape object.
--
-- /See:/ 'mkTape' smart constructor.
data Tape = Tape'
  { -- | The barcode that identifies a specific virtual tape.
    tapeBarcode :: Lude.Maybe Lude.Text,
    -- | The current state of the virtual tape.
    tapeStatus :: Lude.Maybe Lude.Text,
    kmsKey :: Lude.Maybe Lude.Text,
    -- | The Amazon Resource Name (ARN) of the virtual tape.
    tapeARN :: Lude.Maybe Lude.Text,
    -- | For archiving virtual tapes, indicates how much data remains to be uploaded before archiving is complete.
    --
    -- Range: 0 (not started) to 100 (complete).
    progress :: Lude.Maybe Lude.Double,
    -- | The size, in bytes, of the virtual tape capacity.
    tapeSizeInBytes :: Lude.Maybe Lude.Integer,
    -- | The virtual tape library (VTL) device that the virtual tape is associated with.
    vTLDevice :: Lude.Maybe Lude.Text,
    -- | The ID of the pool that contains tapes that will be archived. The tapes in this pool are archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Lude.Maybe Lude.Text,
    -- | The size, in bytes, of data stored on the virtual tape.
    tapeUsedInBytes :: Lude.Maybe Lude.Integer,
    -- | The date the virtual tape was created.
    tapeCreatedDate :: Lude.Maybe Lude.Timestamp,
    -- | The date that the tape enters a custom tape pool.
    poolEntryDate :: Lude.Maybe Lude.Timestamp,
    -- | If the tape is archived as write-once-read-many (WORM), this value is @true@ .
    worm :: Lude.Maybe Lude.Bool,
    -- | The date that the tape is first archived with tape retention lock enabled.
    retentionStartDate :: Lude.Maybe Lude.Timestamp
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Tape' with the minimum fields required to make a request.
--
-- * 'tapeBarcode' - The barcode that identifies a specific virtual tape.
-- * 'tapeStatus' - The current state of the virtual tape.
-- * 'kmsKey' -
-- * 'tapeARN' - The Amazon Resource Name (ARN) of the virtual tape.
-- * 'progress' - For archiving virtual tapes, indicates how much data remains to be uploaded before archiving is complete.
--
-- Range: 0 (not started) to 100 (complete).
-- * 'tapeSizeInBytes' - The size, in bytes, of the virtual tape capacity.
-- * 'vTLDevice' - The virtual tape library (VTL) device that the virtual tape is associated with.
-- * 'poolId' - The ID of the pool that contains tapes that will be archived. The tapes in this pool are archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
-- * 'tapeUsedInBytes' - The size, in bytes, of data stored on the virtual tape.
-- * 'tapeCreatedDate' - The date the virtual tape was created.
-- * 'poolEntryDate' - The date that the tape enters a custom tape pool.
-- * 'worm' - If the tape is archived as write-once-read-many (WORM), this value is @true@ .
-- * 'retentionStartDate' - The date that the tape is first archived with tape retention lock enabled.
mkTape ::
  Tape
mkTape =
  Tape'
    { tapeBarcode = Lude.Nothing,
      tapeStatus = Lude.Nothing,
      kmsKey = Lude.Nothing,
      tapeARN = Lude.Nothing,
      progress = Lude.Nothing,
      tapeSizeInBytes = Lude.Nothing,
      vTLDevice = Lude.Nothing,
      poolId = Lude.Nothing,
      tapeUsedInBytes = Lude.Nothing,
      tapeCreatedDate = Lude.Nothing,
      poolEntryDate = Lude.Nothing,
      worm = Lude.Nothing,
      retentionStartDate = Lude.Nothing
    }

-- | The barcode that identifies a specific virtual tape.
--
-- /Note:/ Consider using 'tapeBarcode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeBarcode :: Lens.Lens' Tape (Lude.Maybe Lude.Text)
tTapeBarcode = Lens.lens (tapeBarcode :: Tape -> Lude.Maybe Lude.Text) (\s a -> s {tapeBarcode = a} :: Tape)
{-# DEPRECATED tTapeBarcode "Use generic-lens or generic-optics with 'tapeBarcode' instead." #-}

-- | The current state of the virtual tape.
--
-- /Note:/ Consider using 'tapeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeStatus :: Lens.Lens' Tape (Lude.Maybe Lude.Text)
tTapeStatus = Lens.lens (tapeStatus :: Tape -> Lude.Maybe Lude.Text) (\s a -> s {tapeStatus = a} :: Tape)
{-# DEPRECATED tTapeStatus "Use generic-lens or generic-optics with 'tapeStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKMSKey :: Lens.Lens' Tape (Lude.Maybe Lude.Text)
tKMSKey = Lens.lens (kmsKey :: Tape -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: Tape)
{-# DEPRECATED tKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | The Amazon Resource Name (ARN) of the virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeARN :: Lens.Lens' Tape (Lude.Maybe Lude.Text)
tTapeARN = Lens.lens (tapeARN :: Tape -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: Tape)
{-# DEPRECATED tTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | For archiving virtual tapes, indicates how much data remains to be uploaded before archiving is complete.
--
-- Range: 0 (not started) to 100 (complete).
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tProgress :: Lens.Lens' Tape (Lude.Maybe Lude.Double)
tProgress = Lens.lens (progress :: Tape -> Lude.Maybe Lude.Double) (\s a -> s {progress = a} :: Tape)
{-# DEPRECATED tProgress "Use generic-lens or generic-optics with 'progress' instead." #-}

-- | The size, in bytes, of the virtual tape capacity.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeSizeInBytes :: Lens.Lens' Tape (Lude.Maybe Lude.Integer)
tTapeSizeInBytes = Lens.lens (tapeSizeInBytes :: Tape -> Lude.Maybe Lude.Integer) (\s a -> s {tapeSizeInBytes = a} :: Tape)
{-# DEPRECATED tTapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead." #-}

-- | The virtual tape library (VTL) device that the virtual tape is associated with.
--
-- /Note:/ Consider using 'vTLDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tVTLDevice :: Lens.Lens' Tape (Lude.Maybe Lude.Text)
tVTLDevice = Lens.lens (vTLDevice :: Tape -> Lude.Maybe Lude.Text) (\s a -> s {vTLDevice = a} :: Tape)
{-# DEPRECATED tVTLDevice "Use generic-lens or generic-optics with 'vTLDevice' instead." #-}

-- | The ID of the pool that contains tapes that will be archived. The tapes in this pool are archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPoolId :: Lens.Lens' Tape (Lude.Maybe Lude.Text)
tPoolId = Lens.lens (poolId :: Tape -> Lude.Maybe Lude.Text) (\s a -> s {poolId = a} :: Tape)
{-# DEPRECATED tPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | The size, in bytes, of data stored on the virtual tape.
--
-- /Note:/ Consider using 'tapeUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeUsedInBytes :: Lens.Lens' Tape (Lude.Maybe Lude.Integer)
tTapeUsedInBytes = Lens.lens (tapeUsedInBytes :: Tape -> Lude.Maybe Lude.Integer) (\s a -> s {tapeUsedInBytes = a} :: Tape)
{-# DEPRECATED tTapeUsedInBytes "Use generic-lens or generic-optics with 'tapeUsedInBytes' instead." #-}

-- | The date the virtual tape was created.
--
-- /Note:/ Consider using 'tapeCreatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeCreatedDate :: Lens.Lens' Tape (Lude.Maybe Lude.Timestamp)
tTapeCreatedDate = Lens.lens (tapeCreatedDate :: Tape -> Lude.Maybe Lude.Timestamp) (\s a -> s {tapeCreatedDate = a} :: Tape)
{-# DEPRECATED tTapeCreatedDate "Use generic-lens or generic-optics with 'tapeCreatedDate' instead." #-}

-- | The date that the tape enters a custom tape pool.
--
-- /Note:/ Consider using 'poolEntryDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPoolEntryDate :: Lens.Lens' Tape (Lude.Maybe Lude.Timestamp)
tPoolEntryDate = Lens.lens (poolEntryDate :: Tape -> Lude.Maybe Lude.Timestamp) (\s a -> s {poolEntryDate = a} :: Tape)
{-# DEPRECATED tPoolEntryDate "Use generic-lens or generic-optics with 'poolEntryDate' instead." #-}

-- | If the tape is archived as write-once-read-many (WORM), this value is @true@ .
--
-- /Note:/ Consider using 'worm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tWorm :: Lens.Lens' Tape (Lude.Maybe Lude.Bool)
tWorm = Lens.lens (worm :: Tape -> Lude.Maybe Lude.Bool) (\s a -> s {worm = a} :: Tape)
{-# DEPRECATED tWorm "Use generic-lens or generic-optics with 'worm' instead." #-}

-- | The date that the tape is first archived with tape retention lock enabled.
--
-- /Note:/ Consider using 'retentionStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRetentionStartDate :: Lens.Lens' Tape (Lude.Maybe Lude.Timestamp)
tRetentionStartDate = Lens.lens (retentionStartDate :: Tape -> Lude.Maybe Lude.Timestamp) (\s a -> s {retentionStartDate = a} :: Tape)
{-# DEPRECATED tRetentionStartDate "Use generic-lens or generic-optics with 'retentionStartDate' instead." #-}

instance Lude.FromJSON Tape where
  parseJSON =
    Lude.withObject
      "Tape"
      ( \x ->
          Tape'
            Lude.<$> (x Lude..:? "TapeBarcode")
            Lude.<*> (x Lude..:? "TapeStatus")
            Lude.<*> (x Lude..:? "KMSKey")
            Lude.<*> (x Lude..:? "TapeARN")
            Lude.<*> (x Lude..:? "Progress")
            Lude.<*> (x Lude..:? "TapeSizeInBytes")
            Lude.<*> (x Lude..:? "VTLDevice")
            Lude.<*> (x Lude..:? "PoolId")
            Lude.<*> (x Lude..:? "TapeUsedInBytes")
            Lude.<*> (x Lude..:? "TapeCreatedDate")
            Lude.<*> (x Lude..:? "PoolEntryDate")
            Lude.<*> (x Lude..:? "Worm")
            Lude.<*> (x Lude..:? "RetentionStartDate")
      )
