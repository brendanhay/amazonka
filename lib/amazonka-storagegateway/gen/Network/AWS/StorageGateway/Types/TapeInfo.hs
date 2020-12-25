{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.TapeInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeInfo
  ( TapeInfo (..),

    -- * Smart constructor
    mkTapeInfo,

    -- * Lenses
    tiGatewayARN,
    tiPoolEntryDate,
    tiPoolId,
    tiRetentionStartDate,
    tiTapeARN,
    tiTapeBarcode,
    tiTapeSizeInBytes,
    tiTapeStatus,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.GatewayARN as Types
import qualified Network.AWS.StorageGateway.Types.PoolId as Types
import qualified Network.AWS.StorageGateway.Types.TapeARN as Types
import qualified Network.AWS.StorageGateway.Types.TapeBarcode as Types
import qualified Network.AWS.StorageGateway.Types.TapeStatus as Types

-- | Describes a virtual tape.
--
-- /See:/ 'mkTapeInfo' smart constructor.
data TapeInfo = TapeInfo'
  { -- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Core.Maybe Types.GatewayARN,
    -- | The date that the tape entered the custom tape pool with tape retention lock enabled.
    poolEntryDate :: Core.Maybe Core.NominalDiffTime,
    -- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Core.Maybe Types.PoolId,
    -- | The date that the tape became subject to tape retention lock.
    retentionStartDate :: Core.Maybe Core.NominalDiffTime,
    -- | The Amazon Resource Name (ARN) of a virtual tape.
    tapeARN :: Core.Maybe Types.TapeARN,
    -- | The barcode that identifies a specific virtual tape.
    tapeBarcode :: Core.Maybe Types.TapeBarcode,
    -- | The size, in bytes, of a virtual tape.
    tapeSizeInBytes :: Core.Maybe Core.Integer,
    -- | The status of the tape.
    tapeStatus :: Core.Maybe Types.TapeStatus
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.NFData)

-- | Creates a 'TapeInfo' value with any optional fields omitted.
mkTapeInfo ::
  TapeInfo
mkTapeInfo =
  TapeInfo'
    { gatewayARN = Core.Nothing,
      poolEntryDate = Core.Nothing,
      poolId = Core.Nothing,
      retentionStartDate = Core.Nothing,
      tapeARN = Core.Nothing,
      tapeBarcode = Core.Nothing,
      tapeSizeInBytes = Core.Nothing,
      tapeStatus = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the 'ListGateways' operation to return a list of gateways for your account and AWS Region.
--
-- /Note:/ Consider using 'gatewayARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiGatewayARN :: Lens.Lens' TapeInfo (Core.Maybe Types.GatewayARN)
tiGatewayARN = Lens.field @"gatewayARN"
{-# DEPRECATED tiGatewayARN "Use generic-lens or generic-optics with 'gatewayARN' instead." #-}

-- | The date that the tape entered the custom tape pool with tape retention lock enabled.
--
-- /Note:/ Consider using 'poolEntryDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiPoolEntryDate :: Lens.Lens' TapeInfo (Core.Maybe Core.NominalDiffTime)
tiPoolEntryDate = Lens.field @"poolEntryDate"
{-# DEPRECATED tiPoolEntryDate "Use generic-lens or generic-optics with 'poolEntryDate' instead." #-}

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiPoolId :: Lens.Lens' TapeInfo (Core.Maybe Types.PoolId)
tiPoolId = Lens.field @"poolId"
{-# DEPRECATED tiPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | The date that the tape became subject to tape retention lock.
--
-- /Note:/ Consider using 'retentionStartDate' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiRetentionStartDate :: Lens.Lens' TapeInfo (Core.Maybe Core.NominalDiffTime)
tiRetentionStartDate = Lens.field @"retentionStartDate"
{-# DEPRECATED tiRetentionStartDate "Use generic-lens or generic-optics with 'retentionStartDate' instead." #-}

-- | The Amazon Resource Name (ARN) of a virtual tape.
--
-- /Note:/ Consider using 'tapeARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTapeARN :: Lens.Lens' TapeInfo (Core.Maybe Types.TapeARN)
tiTapeARN = Lens.field @"tapeARN"
{-# DEPRECATED tiTapeARN "Use generic-lens or generic-optics with 'tapeARN' instead." #-}

-- | The barcode that identifies a specific virtual tape.
--
-- /Note:/ Consider using 'tapeBarcode' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTapeBarcode :: Lens.Lens' TapeInfo (Core.Maybe Types.TapeBarcode)
tiTapeBarcode = Lens.field @"tapeBarcode"
{-# DEPRECATED tiTapeBarcode "Use generic-lens or generic-optics with 'tapeBarcode' instead." #-}

-- | The size, in bytes, of a virtual tape.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTapeSizeInBytes :: Lens.Lens' TapeInfo (Core.Maybe Core.Integer)
tiTapeSizeInBytes = Lens.field @"tapeSizeInBytes"
{-# DEPRECATED tiTapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead." #-}

-- | The status of the tape.
--
-- /Note:/ Consider using 'tapeStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
tiTapeStatus :: Lens.Lens' TapeInfo (Core.Maybe Types.TapeStatus)
tiTapeStatus = Lens.field @"tapeStatus"
{-# DEPRECATED tiTapeStatus "Use generic-lens or generic-optics with 'tapeStatus' instead." #-}

instance Core.FromJSON TapeInfo where
  parseJSON =
    Core.withObject "TapeInfo" Core.$
      \x ->
        TapeInfo'
          Core.<$> (x Core..:? "GatewayARN")
          Core.<*> (x Core..:? "PoolEntryDate")
          Core.<*> (x Core..:? "PoolId")
          Core.<*> (x Core..:? "RetentionStartDate")
          Core.<*> (x Core..:? "TapeARN")
          Core.<*> (x Core..:? "TapeBarcode")
          Core.<*> (x Core..:? "TapeSizeInBytes")
          Core.<*> (x Core..:? "TapeStatus")
