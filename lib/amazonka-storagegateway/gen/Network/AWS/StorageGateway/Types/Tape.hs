{-# OPTIONS_GHC -fno-warn-unused-matches #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.Tape
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
module Network.AWS.StorageGateway.Types.Tape
  ( Tape (..)
  -- * Smart constructor
  , mkTape
  -- * Lenses
  , tKMSKey
  , tPoolEntryDate
  , tPoolId
  , tProgress
  , tRetentionStartDate
  , tTapeARN
  , tTapeBarcode
  , tTapeCreatedDate
  , tTapeSizeInBytes
  , tTapeStatus
  , tTapeUsedInBytes
  , tVTLDevice
  , tWorm
  ) where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.KMSKey as Types
import qualified Network.AWS.StorageGateway.Types.PoolId as Types
import qualified Network.AWS.StorageGateway.Types.TapeARN as Types
import qualified Network.AWS.StorageGateway.Types.TapeBarcode as Types
import qualified Network.AWS.StorageGateway.Types.TapeStatus as Types
import qualified Network.AWS.StorageGateway.Types.VTLDeviceARN as Types

-- | Describes a virtual tape object.
--
-- /See:/ 'mkTape' smart constructor.
data Tape = Tape'
  { kMSKey :: Core.Maybe Types.KMSKey
  , poolEntryDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the tape enters a custom tape pool.
  , poolId :: Core.Maybe Types.PoolId
    -- ^ The ID of the pool that contains tapes that will be archived. The tapes in this pool are archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
  , progress :: Core.Maybe Core.Double
    -- ^ For archiving virtual tapes, indicates how much data remains to be uploaded before archiving is complete.
--
-- Range: 0 (not started) to 100 (complete).
  , retentionStartDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date that the tape is first archived with tape retention lock enabled.
  , tapeARN :: Core.Maybe Types.TapeARN
    -- ^ The Amazon Resource Name (ARN) of the virtual tape.
  , tapeBarcode :: Core.Maybe Types.TapeBarcode
    -- ^ The barcode that identifies a specific virtual tape.
  , tapeCreatedDate :: Core.Maybe Core.NominalDiffTime
    -- ^ The date the virtual tape was created.
  , tapeSizeInBytes :: Core.Maybe Core.Integer
    -- ^ The size, in bytes, of the virtual tape capacity.
  , tapeStatus :: Core.Maybe Types.TapeStatus
    -- ^ The current state of the virtual tape.
  , tapeUsedInBytes :: Core.Maybe Core.Integer
    -- ^ The size, in bytes, of data stored on the virtual tape.
  , vTLDevice :: Core.Maybe Types.VTLDeviceARN
    -- ^ The virtual tape library (VTL) device that the virtual tape is associated with.
  , worm :: Core.Maybe Core.Bool
    -- ^ If the tape is archived as write-once-read-many (WORM), this value is @true@ .
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass Core.NFData

-- | Creates a 'Tape' value with any optional fields omitted.
mkTape
    :: Tape
mkTape
  = Tape'{kMSKey = Core.Nothing, poolEntryDate = Core.Nothing,
          poolId = Core.Nothing, progress = Core.Nothing,
          retentionStartDate = Core.Nothing, tapeARN = Core.Nothing,
          tapeBarcode = Core.Nothing, tapeCreatedDate = Core.Nothing,
          tapeSizeInBytes = Core.Nothing, tapeStatus = Core.Nothing,
          tapeUsedInBytes = Core.Nothing, vTLDevice = Core.Nothing,
          worm = Core.Nothing}

-- | Undocumented field.
--
-- /Note:/ Consider using 'kMSKey' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tKMSKey :: Lens.Lens' Tape (Core.Maybe Types.KMSKey)
tKMSKey = Lens.field @"kMSKey"
{-# INLINEABLE tKMSKey #-}
{-# DEPRECATED kMSKey "Use generic-lens or generic-optics with 'kMSKey' instead"  #-}

-- | The date that the tape enters a custom tape pool.
--
-- /Note:/ Consider using 'poolEntryDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPoolEntryDate :: Lens.Lens' Tape (Core.Maybe Core.NominalDiffTime)
tPoolEntryDate = Lens.field @"poolEntryDate"
{-# INLINEABLE tPoolEntryDate #-}
{-# DEPRECATED poolEntryDate "Use generic-lens or generic-optics with 'poolEntryDate' instead"  #-}

-- | The ID of the pool that contains tapes that will be archived. The tapes in this pool are archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@ 
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tPoolId :: Lens.Lens' Tape (Core.Maybe Types.PoolId)
tPoolId = Lens.field @"poolId"
{-# INLINEABLE tPoolId #-}
{-# DEPRECATED poolId "Use generic-lens or generic-optics with 'poolId' instead"  #-}

-- | For archiving virtual tapes, indicates how much data remains to be uploaded before archiving is complete.
--
-- Range: 0 (not started) to 100 (complete).
--
-- /Note:/ Consider using 'progress' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tProgress :: Lens.Lens' Tape (Core.Maybe Core.Double)
tProgress = Lens.field @"progress"
{-# INLINEABLE tProgress #-}
{-# DEPRECATED progress "Use generic-lens or generic-optics with 'progress' instead"  #-}

-- | The date that the tape is first archived with tape retention lock enabled.
--
-- /Note:/ Consider using 'retentionStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tRetentionStartDate :: Lens.Lens' Tape (Core.Maybe Core.NominalDiffTime)
tRetentionStartDate = Lens.field @"retentionStartDate"
{-# INLINEABLE tRetentionStartDate #-}
{-# DEPRECATED retentionStartDate "Use generic-lens or generic-optics with 'retentionStartDate' instead"  #-}

-- | The Amazon Resource Name (ARN) of the virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeARN :: Lens.Lens' Tape (Core.Maybe Types.TapeARN)
tTapeARN = Lens.field @"tapeARN"
{-# INLINEABLE tTapeARN #-}
{-# DEPRECATED tapeARN "Use generic-lens or generic-optics with 'tapeARN' instead"  #-}

-- | The barcode that identifies a specific virtual tape.
--
-- /Note:/ Consider using 'tapeBarcode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeBarcode :: Lens.Lens' Tape (Core.Maybe Types.TapeBarcode)
tTapeBarcode = Lens.field @"tapeBarcode"
{-# INLINEABLE tTapeBarcode #-}
{-# DEPRECATED tapeBarcode "Use generic-lens or generic-optics with 'tapeBarcode' instead"  #-}

-- | The date the virtual tape was created.
--
-- /Note:/ Consider using 'tapeCreatedDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeCreatedDate :: Lens.Lens' Tape (Core.Maybe Core.NominalDiffTime)
tTapeCreatedDate = Lens.field @"tapeCreatedDate"
{-# INLINEABLE tTapeCreatedDate #-}
{-# DEPRECATED tapeCreatedDate "Use generic-lens or generic-optics with 'tapeCreatedDate' instead"  #-}

-- | The size, in bytes, of the virtual tape capacity.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeSizeInBytes :: Lens.Lens' Tape (Core.Maybe Core.Integer)
tTapeSizeInBytes = Lens.field @"tapeSizeInBytes"
{-# INLINEABLE tTapeSizeInBytes #-}
{-# DEPRECATED tapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead"  #-}

-- | The current state of the virtual tape.
--
-- /Note:/ Consider using 'tapeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeStatus :: Lens.Lens' Tape (Core.Maybe Types.TapeStatus)
tTapeStatus = Lens.field @"tapeStatus"
{-# INLINEABLE tTapeStatus #-}
{-# DEPRECATED tapeStatus "Use generic-lens or generic-optics with 'tapeStatus' instead"  #-}

-- | The size, in bytes, of data stored on the virtual tape.
--
-- /Note:/ Consider using 'tapeUsedInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tTapeUsedInBytes :: Lens.Lens' Tape (Core.Maybe Core.Integer)
tTapeUsedInBytes = Lens.field @"tapeUsedInBytes"
{-# INLINEABLE tTapeUsedInBytes #-}
{-# DEPRECATED tapeUsedInBytes "Use generic-lens or generic-optics with 'tapeUsedInBytes' instead"  #-}

-- | The virtual tape library (VTL) device that the virtual tape is associated with.
--
-- /Note:/ Consider using 'vTLDevice' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tVTLDevice :: Lens.Lens' Tape (Core.Maybe Types.VTLDeviceARN)
tVTLDevice = Lens.field @"vTLDevice"
{-# INLINEABLE tVTLDevice #-}
{-# DEPRECATED vTLDevice "Use generic-lens or generic-optics with 'vTLDevice' instead"  #-}

-- | If the tape is archived as write-once-read-many (WORM), this value is @true@ .
--
-- /Note:/ Consider using 'worm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tWorm :: Lens.Lens' Tape (Core.Maybe Core.Bool)
tWorm = Lens.field @"worm"
{-# INLINEABLE tWorm #-}
{-# DEPRECATED worm "Use generic-lens or generic-optics with 'worm' instead"  #-}

instance Core.FromJSON Tape where
        parseJSON
          = Core.withObject "Tape" Core.$
              \ x ->
                Tape' Core.<$>
                  (x Core..:? "KMSKey") Core.<*> x Core..:? "PoolEntryDate" Core.<*>
                    x Core..:? "PoolId"
                    Core.<*> x Core..:? "Progress"
                    Core.<*> x Core..:? "RetentionStartDate"
                    Core.<*> x Core..:? "TapeARN"
                    Core.<*> x Core..:? "TapeBarcode"
                    Core.<*> x Core..:? "TapeCreatedDate"
                    Core.<*> x Core..:? "TapeSizeInBytes"
                    Core.<*> x Core..:? "TapeStatus"
                    Core.<*> x Core..:? "TapeUsedInBytes"
                    Core.<*> x Core..:? "VTLDevice"
                    Core.<*> x Core..:? "Worm"
