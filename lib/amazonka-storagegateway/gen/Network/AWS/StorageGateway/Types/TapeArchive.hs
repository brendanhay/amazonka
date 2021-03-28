{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeArchive
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.TapeArchive
  ( TapeArchive (..)
  -- * Smart constructor
  , mkTapeArchive
  -- * Lenses
  , taCompletionTime
  , taKMSKey
  , taPoolEntryDate
  , taPoolId
  , taRetentionStartDate
  , taRetrievedTo
  , taTapeARN
  , taTapeBarcode
  , taTapeCreatedDate
  , taTapeSizeInBytes
  , taTapeStatus
  , taTapeUsedInBytes
  , taWorm
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.GatewayARN as Types
import qualified Network.AWS.StorageGateway.Types.KMSKey as Types
import qualified Network.AWS.StorageGateway.Types.PoolId as Types
import qualified Network.AWS.StorageGateway.Types.TapeARN as Types
import qualified Network.AWS.StorageGateway.Types.TapeArchiveStatus as Types
import qualified Network.AWS.StorageGateway.Types.TapeBarcode as Types

-- | Represents a virtual tape that is archived in the virtual tape shelf (VTS).
--
-- /See:/ 'mkTapeArchive' smart constructor.
data TapeArchive = TapeArchive'
  { completionTime :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the archiving of the virtual tape was completed.
--
-- The default timestamp format is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
  , kMSKey :: Core.Maybe Types.KMSKey
  , poolEntryDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The time that the tape entered the custom tape pool.
--
-- The default timestamp format is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
  , poolId :: Core.Maybe Types.PoolId
    -- ^ The ID of the pool that was used to archive the tape. The tapes in this pool are archived in the S3 storage class that is associated with the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
  , retentionStartDate :: Core.Maybe Core.NominalDiffTime
    -- ^ If the archived tape is subject to tape retention lock, the date that the archived tape started being retained.
  , retrievedTo :: Core.Maybe Types.GatewayARN
    -- ^ The Amazon Resource Name (ARN) of the tape gateway that the virtual tape is being retrieved to.
--
-- The virtual tape is retrieved from the virtual tape shelf (VTS).
  , tapeARN :: Core.Maybe Types.TapeARN
    -- ^ The Amazon Resource Name (ARN) of an archived virtual tape.
  , tapeBarcode :: Core.Maybe Types.TapeBarcode
    -- ^ The barcode that identifies the archived virtual tape.
  , tapeCreatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the virtual tape was created.
  , tapeSizeInBytes :: Core.Maybe Core.Integer
    -- ^ The size, in bytes, of the archived virtual tape.
  , tapeStatus :: Core.Maybe Types.TapeArchiveStatus
    -- ^ The current state of the archived virtual tape.
  , tapeUsedInBytes :: Core.Maybe Core.Integer
    -- ^ The size, in bytes, of data stored on the virtual tape.
  , worm :: Core.Maybe Core.Bool
    -- ^ Set to @true@ if the archived tape is stored as write-once-read-many (WORM).
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'TapeArchive' value with any optional fields omitted.
mkTapeArchive
    :: TapeArchive
mkTapeArchive
  = TapeArchive'{completionTime = Core.Nothing,
                 kMSKey = Core.Nothing, poolEntryDate = Core.Nothing,
                 poolId = Core.Nothing, retentionStartDate = Core.Nothing,
                 retrievedTo = Core.Nothing, tapeARN = Core.Nothing,
                 tapeBarcode = Core.Nothing, tapeCreatedDate = Core.Nothing,
                 tapeSizeInBytes = Core.Nothing, tapeStatus = Core.Nothing,
                 tapeUsedInBytes = Core.Nothing, worm = Core.Nothing}

-- | The time that the archiving of the virtual tape was completed.
--
-- The default timestamp format is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
--
-- /Note:/ Consider using 'completionTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taCompletionTime :: Lens.Lens' TapeArchive (Core.Maybe Core.NominalDiffTime)
taCompletionTime = Lens.field @"completionTime"
{-# INLINEABLE taCompletionTime #-}
{-# DEPRECATED completionTime "Use generic-lens or generic-optics with 'completionTime' instead"  #-}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taKMSKey :: Lens.Lens' TapeArchive (Core.Maybe Types.KMSKey)
taKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE taKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | The time that the tape entered the custom tape pool.
--
-- The default timestamp format is in the ISO8601 extended YYYY-MM-DD'T'HH:MM:SS'Z' format.
--
-- /Note:/ Consider using 'poolEntryDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taPoolEntryDate :: Lens.Lens' TapeArchive (Core.Maybe Core.NominalDiffTime)
taPoolEntryDate = Lens.field @"poolEntryDate"
{-# INLINEABLE taPoolEntryDate #-}
{-# DEPRECATED poolEntryDate "Use generic-lens or generic-optics with 'poolEntryDate' instead"  #-}

-- | The ID of the pool that was used to archive the tape. The tapes in this pool are archived in the S3 storage class that is associated with the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taPoolId :: Lens.Lens' TapeArchive (Core.Maybe Types.PoolId)
taPoolId = Lens.field @"poolId"
{-# INLINEABLE taPoolId #-}
{-# DEPRECATED poolId "Use generic-lens or generic-optics with 'poolId' instead"  #-}

-- | If the archived tape is subject to tape retention lock, the date that the archived tape started being retained.
--
-- /Note:/ Consider using 'retentionStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taRetentionStartDate :: Lens.Lens' TapeArchive (Core.Maybe Core.NominalDiffTime)
taRetentionStartDate = Lens.field @"retentionStartDate"
{-# INLINEABLE taRetentionStartDate #-}
{-# DEPRECATED retentionStartDate "Use generic-lens or generic-optics with 'retentionStartDate' instead"  #-}

-- | The Amazon Resource Name (ARN) of the tape gateway that the virtual tape is being retrieved to.
--
-- The virtual tape is retrieved from the virtual tape shelf (VTS).
--
-- /Note:/ Consider using 'retrievedTo' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taRetrievedTo :: Lens.Lens' TapeArchive (Core.Maybe Types.GatewayARN)
taRetrievedTo = Lens.field @"retrievedTo"
{-# INLINEABLE taRetrievedTo #-}
{-# DEPRECATED retrievedTo "Use generic-lens or generic-optics with 'retrievedTo' instead"  #-}

-- | The Amazon Resource Name (ARN) of an archived virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeARN :: Lens.Lens' TapeArchive (Core.Maybe Types.TapeARN)
taTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE taTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

-- | The barcode that identifies the archived virtual tape.
--
-- /Note:/ Consider using 'tapeBarcode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeBarcode :: Lens.Lens' TapeArchive (Core.Maybe Types.TapeBarcode)
taTapeBarcode = Lens.field @"tapeBarcode"
{-# INLINEABLE taTapeBarcode #-}
{-# DEPRECATED tapeBarcode "Use generic-lens or generic-optics with 'tapeBarcode' instead"  #-}

-- | The date the virtual tape was created.
--
-- /Note:/ Consider using 'tapeCreatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeCreatedDate :: Lens.Lens' TapeArchive (Core.Maybe Core.NominalDiffTime)
taTapeCreatedDate = Lens.field @"tapeCreatedDate"
{-# INLINEABLE taTapeCreatedDate #-}
{-# DEPRECATED tapeCreatedDate "Use generic-lens or generic-optics with 'tapeCreatedDate' instead"  #-}

-- | The size, in bytes, of the archived virtual tape.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeSizeInBytes :: Lens.Lens' TapeArchive (Core.Maybe Core.Integer)
taTapeSizeInBytes = Lens.field @"tapeSizeInBytes"
{-# INLINEABLE taTapeSizeInBytes #-}
{-# DEPRECATED tapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead"  #-}

-- | The current state of the archived virtual tape.
--
-- /Note:/ Consider using 'tapeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeStatus :: Lens.Lens' TapeArchive (Core.Maybe Types.TapeArchiveStatus)
taTapeStatus = Lens.field @"tapeStatus"
{-# INLINEABLE taTapeStatus #-}
{-# DEPRECATED tapeStatus "Use generic-lens or generic-optics with 'tapeStatus' instead"  #-}

-- | The size, in bytes, of data stored on the virtual tape.
--
-- /Note:/ Consider using 'tapeUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taTapeUsedInBytes :: Lens.Lens' TapeArchive (Core.Maybe Core.Integer)
taTapeUsedInBytes = Lens.field @"tapeUsedInBytes"
{-# INLINEABLE taTapeUsedInBytes #-}
{-# DEPRECATED tapeUsedInBytes "Use generic-lens or generic-optics with 'tapeUsedInBytes' instead"  #-}

-- | Set to @true@ if the archived tape is stored as write-once-read-many (WORM).
--
-- /Note:/ Consider using 'worm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
taWorm :: Lens.Lens' TapeArchive (Core.Maybe Core.Bool)
taWorm = Lens.field @"worm"
{-# INLINEABLE taWorm #-}
{-# DEPRECATED worm "Use generic-lens or generic-optics with 'worm' instead"  #-}

instance Core.FromJSON TapeArchive where
        parseJSON
          = Core.withObject "TapeArchive" Core.$
              \ x ->
                TapeArchive' Core.<$>
                  (x Core..:? "CompletionTime") Core.<*> x Core..:? "KMSKey" Core.<*>
                    x Core..:? "PoolEntryDate"
                    Core.<*> x Core..:? "PoolId"
                    Core.<*> x Core..:? "RetentionStartDate"
                    Core.<*> x Core..:? "RetrievedTo"
                    Core.<*> x Core..:? "TapeARN"
                    Core.<*> x Core..:? "TapeBarcode"
                    Core.<*> x Core..:? "TapeCreatedDate"
                    Core.<*> x Core..:? "TapeSizeInBytes"
                    Core.<*> x Core..:? "TapeStatus"
                    Core.<*> x Core..:? "TapeUsedInBytes"
                    Core.<*> x Core..:? "Worm"
