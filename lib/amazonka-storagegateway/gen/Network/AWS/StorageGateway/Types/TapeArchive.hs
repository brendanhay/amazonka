-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeArchive
  ( TapeArchive (..),

    -- * Smart constructor
    mkTapeArchive,

    -- * Lenses
    taTapeBarcode,
    taTapeStatus,
    taKMSKey,
    taTapeARN,
    taTapeSizeInBytes,
    taCompletionTime,
    taPoolId,
    taTapeUsedInBytes,
    taTapeCreatedDate,
    taPoolEntryDate,
    taWorm,
    taRetentionStartDate,
    taRetrievedTo,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | Represents a virtual tape that is archived in the virtual tape shelf (VTS).
--
-- /See:/ 'mkTapeArchive' smart constructor.
data TapeArchive = TapeArchive'
  { tapeBarcode ::
      Lude.Maybe Lude.Text,
    tapeStatus :: Lude.Maybe Lude.Text,
    kmsKey :: Lude.Maybe Lude.Text,
    tapeARN :: Lude.Maybe Lude.Text,
    tapeSizeInBytes :: Lude.Maybe Lude.Integer,
    completionTime :: Lude.Maybe Lude.Timestamp,
    poolId :: Lude.Maybe Lude.Text,
    tapeUsedInBytes :: Lude.Maybe Lude.Integer,
    tapeCreatedDate :: Lude.Maybe Lude.Timestamp,
    poolEntryDate :: Lude.Maybe Lude.Timestamp,
    worm :: Lude.Maybe Lude.Bool,
    retentionStartDate :: Lude.Maybe Lude.Timestamp,
    retrievedTo :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'TapeArchive' with the minimum fields required to make a request.
--
-- * 'completionTime' - The time that the archiving of the virtual tape was completed.
--
-- The default timestamp format is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
-- * 'kmsKey' - Undocumented field.
-- * 'poolEntryDate' - The time that the tape entered the custom tape pool.
--
-- The default timestamp format is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
-- * 'poolId' - The ID of the pool that was used to archive the tape. The tapes in this pool are archived in the S3 storage class that is associated with the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
-- * 'retentionStartDate' - If the archived tape is subject to tape retention lock, the date that the archived tape started being retained.
-- * 'retrievedTo' - The Amazon Resource Name (ARN) of the tape gateway that the virtual tape is being retrieved to.
--
-- The virtual tape is retrieved from the virtual tape shelf (VTS).
-- * 'tapeARN' - The Amazon Resource Name (ARN) of an archived virtual tape.
-- * 'tapeBarcode' - The barcode that identifies the archived virtual tape.
-- * 'tapeCreatedDate' - The date the virtual tape was created.
-- * 'tapeSizeInBytes' - The size, in bytes, of the archived virtual tape.
-- * 'tapeStatus' - The current state of the archived virtual tape.
-- * 'tapeUsedInBytes' - The size, in bytes, of data stored on the virtual tape.
-- * 'worm' - Set to @true@ if the archived tape is stored as write-once-read-many (WORM).
mkTapeArchive ::
  TapeArchive
mkTapeArchive =
  TapeArchive'
    { tapeBarcode = Lude.Nothing,
      tapeStatus = Lude.Nothing,
      kmsKey = Lude.Nothing,
      tapeARN = Lude.Nothing,
      tapeSizeInBytes = Lude.Nothing,
      completionTime = Lude.Nothing,
      poolId = Lude.Nothing,
      tapeUsedInBytes = Lude.Nothing,
      tapeCreatedDate = Lude.Nothing,
      poolEntryDate = Lude.Nothing,
      worm = Lude.Nothing,
      retentionStartDate = Lude.Nothing,
      retrievedTo = Lude.Nothing
    }

-- | The barcode that identifies the archived virtual tape.
--
-- /Note:/ Consider using 'tapeBarcode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeBarcode :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Text)
taTapeBarcode = Lens.lens (tapeBarcode :: TapeArchive -> Lude.Maybe Lude.Text) (\s a -> s {tapeBarcode = a} :: TapeArchive)
{-# DEPRECATED taTapeBarcode "Use generic-lens or generic-optics with 'tapeBarcode' instead." #-}

-- | The current state of the archived virtual tape.
--
-- /Note:/ Consider using 'tapeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeStatus :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Text)
taTapeStatus = Lens.lens (tapeStatus :: TapeArchive -> Lude.Maybe Lude.Text) (\s a -> s {tapeStatus = a} :: TapeArchive)
{-# DEPRECATED taTapeStatus "Use generic-lens or generic-optics with 'tapeStatus' instead." #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kmsKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taKMSKey :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Text)
taKMSKey = Lens.lens (kmsKey :: TapeArchive -> Lude.Maybe Lude.Text) (\s a -> s {kmsKey = a} :: TapeArchive)
{-# DEPRECATED taKMSKey "Use generic-lens or generic-optics with 'kmsKey' instead." #-}

-- | The Amazon Resource Name (ARN) of an archived virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeARN :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Text)
taTapeARN = Lens.lens (tapeARN :: TapeArchive -> Lude.Maybe Lude.Text) (\s a -> s {tapeARN = a} :: TapeArchive)
{-# DEPRECATED taTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The size, in bytes, of the archived virtual tape.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeSizeInBytes :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Integer)
taTapeSizeInBytes = Lens.lens (tapeSizeInBytes :: TapeArchive -> Lude.Maybe Lude.Integer) (\s a -> s {tapeSizeInBytes = a} :: TapeArchive)
{-# DEPRECATED taTapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead." #-}

-- | The time that the archiving of the virtual tape was completed.
--
-- The default timestamp format is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taCompletionTime :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Timestamp)
taCompletionTime = Lens.lens (completionTime :: TapeArchive -> Lude.Maybe Lude.Timestamp) (\s a -> s {completionTime = a} :: TapeArchive)
{-# DEPRECATED taCompletionTime "Use generic-lens or generic-optics with 'completionTime' instead." #-}

-- | The ID of the pool that was used to archive the tape. The tapes in this pool are archived in the S3 storage class that is associated with the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taPoolId :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Text)
taPoolId = Lens.lens (poolId :: TapeArchive -> Lude.Maybe Lude.Text) (\s a -> s {poolId = a} :: TapeArchive)
{-# DEPRECATED taPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | The size, in bytes, of data stored on the virtual tape.
--
-- /Note:/ Consider using 'tapeUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeUsedInBytes :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Integer)
taTapeUsedInBytes = Lens.lens (tapeUsedInBytes :: TapeArchive -> Lude.Maybe Lude.Integer) (\s a -> s {tapeUsedInBytes = a} :: TapeArchive)
{-# DEPRECATED taTapeUsedInBytes "Use generic-lens or generic-optics with 'tapeUsedInBytes' instead." #-}

-- | The date the virtual tape was created.
--
-- /Note:/ Consider using 'tapeCreatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeCreatedDate :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Timestamp)
taTapeCreatedDate = Lens.lens (tapeCreatedDate :: TapeArchive -> Lude.Maybe Lude.Timestamp) (\s a -> s {tapeCreatedDate = a} :: TapeArchive)
{-# DEPRECATED taTapeCreatedDate "Use generic-lens or generic-optics with 'tapeCreatedDate' instead." #-}

-- | The time that the tape entered the custom tape pool.
--
-- The default timestamp format is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
--
-- /Note:/ Consider using 'poolEntryDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taPoolEntryDate :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Timestamp)
taPoolEntryDate = Lens.lens (poolEntryDate :: TapeArchive -> Lude.Maybe Lude.Timestamp) (\s a -> s {poolEntryDate = a} :: TapeArchive)
{-# DEPRECATED taPoolEntryDate "Use generic-lens or generic-optics with 'poolEntryDate' instead." #-}

-- | Set to @true@ if the archived tape is stored as write-once-read-many (WORM).
--
-- /Note:/ Consider using 'worm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taWorm :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Bool)
taWorm = Lens.lens (worm :: TapeArchive -> Lude.Maybe Lude.Bool) (\s a -> s {worm = a} :: TapeArchive)
{-# DEPRECATED taWorm "Use generic-lens or generic-optics with 'worm' instead." #-}

-- | If the archived tape is subject to tape retention lock, the date that the archived tape started being retained.
--
-- /Note:/ Consider using 'retentionStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taRetentionStartDate :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Timestamp)
taRetentionStartDate = Lens.lens (retentionStartDate :: TapeArchive -> Lude.Maybe Lude.Timestamp) (\s a -> s {retentionStartDate = a} :: TapeArchive)
{-# DEPRECATED taRetentionStartDate "Use generic-lens or generic-optics with 'retentionStartDate' instead." #-}

-- | The Amazon Resource Name (ARN) of the tape gateway that the virtual tape is being retrieved to.
--
-- The virtual tape is retrieved from the virtual tape shelf (VTS).
--
-- /Note:/ Consider using 'retrievedTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taRetrievedTo :: Lens.Lens' TapeArchive (Lude.Maybe Lude.Text)
taRetrievedTo = Lens.lens (retrievedTo :: TapeArchive -> Lude.Maybe Lude.Text) (\s a -> s {retrievedTo = a} :: TapeArchive)
{-# DEPRECATED taRetrievedTo "Use generic-lens or generic-optics with 'retrievedTo' instead." #-}

instance Lude.FromJSON TapeArchive where
  parseJSON =
    Lude.withObject
      "TapeArchive"
      ( \x ->
          TapeArchive'
            Lude.<$> (x Lude..:? "TapeBarcode")
            Lude.<*> (x Lude..:? "TapeStatus")
            Lude.<*> (x Lude..:? "KMSKey")
            Lude.<*> (x Lude..:? "TapeARN")
            Lude.<*> (x Lude..:? "TapeSizeInBytes")
            Lude.<*> (x Lude..:? "CompletionTime")
            Lude.<*> (x Lude..:? "PoolId")
            Lude.<*> (x Lude..:? "TapeUsedInBytes")
            Lude.<*> (x Lude..:? "TapeCreatedDate")
            Lude.<*> (x Lude..:? "PoolEntryDate")
            Lude.<*> (x Lude..:? "Worm")
            Lude.<*> (x Lude..:? "RetentionStartDate")
            Lude.<*> (x Lude..:? "RetrievedTo")
      )
