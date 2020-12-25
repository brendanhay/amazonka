{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule
  ( AutomaticTapeCreationRule (..),

    -- * Smart constructor
    mkAutomaticTapeCreationRule,

    -- * Lenses
    atcrTapeBarcodePrefix,
    atcrPoolId,
    atcrTapeSizeInBytes,
    atcrMinimumNumTapes,
    atcrWorm,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.PoolId as Types
import qualified Network.AWS.StorageGateway.Types.TapeBarcodePrefix as Types

-- | An automatic tape creation policy consists of automatic tape creation rules where each rule defines when and how to create new tapes. For more information about automatic tape creation, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically> .
--
-- /See:/ 'mkAutomaticTapeCreationRule' smart constructor.
data AutomaticTapeCreationRule = AutomaticTapeCreationRule'
  { -- | A prefix that you append to the barcode of the virtual tape that you are creating. This prefix makes the barcode unique.
    tapeBarcodePrefix :: Types.TapeBarcodePrefix,
    -- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the Amazon S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Types.PoolId,
    -- | The size, in bytes, of the virtual tape capacity.
    tapeSizeInBytes :: Core.Integer,
    -- | The minimum number of available virtual tapes that the gateway maintains at all times. If the number of tapes on the gateway goes below this value, the gateway creates as many new tapes as are needed to have @MinimumNumTapes@ on the gateway. For more information about automatic tape creation, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically> .
    minimumNumTapes :: Core.Natural,
    -- | Set to @true@ to indicate that tapes are to be archived as write-once-read-many (WORM). Set to @false@ when WORM is not enabled for tapes.
    worm :: Core.Maybe Core.Bool
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'AutomaticTapeCreationRule' value with any optional fields omitted.
mkAutomaticTapeCreationRule ::
  -- | 'tapeBarcodePrefix'
  Types.TapeBarcodePrefix ->
  -- | 'poolId'
  Types.PoolId ->
  -- | 'tapeSizeInBytes'
  Core.Integer ->
  -- | 'minimumNumTapes'
  Core.Natural ->
  AutomaticTapeCreationRule
mkAutomaticTapeCreationRule
  tapeBarcodePrefix
  poolId
  tapeSizeInBytes
  minimumNumTapes =
    AutomaticTapeCreationRule'
      { tapeBarcodePrefix,
        poolId,
        tapeSizeInBytes,
        minimumNumTapes,
        worm = Core.Nothing
      }

-- | A prefix that you append to the barcode of the virtual tape that you are creating. This prefix makes the barcode unique.
--
-- /Note:/ Consider using 'tapeBarcodePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrTapeBarcodePrefix :: Lens.Lens' AutomaticTapeCreationRule Types.TapeBarcodePrefix
atcrTapeBarcodePrefix = Lens.field @"tapeBarcodePrefix"
{-# DEPRECATED atcrTapeBarcodePrefix "Use generic-lens or generic-optics with 'tapeBarcodePrefix' instead." #-}

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the Amazon S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrPoolId :: Lens.Lens' AutomaticTapeCreationRule Types.PoolId
atcrPoolId = Lens.field @"poolId"
{-# DEPRECATED atcrPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | The size, in bytes, of the virtual tape capacity.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrTapeSizeInBytes :: Lens.Lens' AutomaticTapeCreationRule Core.Integer
atcrTapeSizeInBytes = Lens.field @"tapeSizeInBytes"
{-# DEPRECATED atcrTapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead." #-}

-- | The minimum number of available virtual tapes that the gateway maintains at all times. If the number of tapes on the gateway goes below this value, the gateway creates as many new tapes as are needed to have @MinimumNumTapes@ on the gateway. For more information about automatic tape creation, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically> .
--
-- /Note:/ Consider using 'minimumNumTapes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrMinimumNumTapes :: Lens.Lens' AutomaticTapeCreationRule Core.Natural
atcrMinimumNumTapes = Lens.field @"minimumNumTapes"
{-# DEPRECATED atcrMinimumNumTapes "Use generic-lens or generic-optics with 'minimumNumTapes' instead." #-}

-- | Set to @true@ to indicate that tapes are to be archived as write-once-read-many (WORM). Set to @false@ when WORM is not enabled for tapes.
--
-- /Note:/ Consider using 'worm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrWorm :: Lens.Lens' AutomaticTapeCreationRule (Core.Maybe Core.Bool)
atcrWorm = Lens.field @"worm"
{-# DEPRECATED atcrWorm "Use generic-lens or generic-optics with 'worm' instead." #-}

instance Core.FromJSON AutomaticTapeCreationRule where
  toJSON AutomaticTapeCreationRule {..} =
    Core.object
      ( Core.catMaybes
          [ Core.Just ("TapeBarcodePrefix" Core..= tapeBarcodePrefix),
            Core.Just ("PoolId" Core..= poolId),
            Core.Just ("TapeSizeInBytes" Core..= tapeSizeInBytes),
            Core.Just ("MinimumNumTapes" Core..= minimumNumTapes),
            ("Worm" Core..=) Core.<$> worm
          ]
      )

instance Core.FromJSON AutomaticTapeCreationRule where
  parseJSON =
    Core.withObject "AutomaticTapeCreationRule" Core.$
      \x ->
        AutomaticTapeCreationRule'
          Core.<$> (x Core..: "TapeBarcodePrefix")
          Core.<*> (x Core..: "PoolId")
          Core.<*> (x Core..: "TapeSizeInBytes")
          Core.<*> (x Core..: "MinimumNumTapes")
          Core.<*> (x Core..:? "Worm")
