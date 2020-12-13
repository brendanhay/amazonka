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
    atcrTapeSizeInBytes,
    atcrPoolId,
    atcrWorm,
    atcrMinimumNumTapes,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude

-- | An automatic tape creation policy consists of automatic tape creation rules where each rule defines when and how to create new tapes. For more information about automatic tape creation, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically> .
--
-- /See:/ 'mkAutomaticTapeCreationRule' smart constructor.
data AutomaticTapeCreationRule = AutomaticTapeCreationRule'
  { -- | A prefix that you append to the barcode of the virtual tape that you are creating. This prefix makes the barcode unique.
    tapeBarcodePrefix :: Lude.Text,
    -- | The size, in bytes, of the virtual tape capacity.
    tapeSizeInBytes :: Lude.Integer,
    -- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the Amazon S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Lude.Text,
    -- | Set to @true@ to indicate that tapes are to be archived as write-once-read-many (WORM). Set to @false@ when WORM is not enabled for tapes.
    worm :: Lude.Maybe Lude.Bool,
    -- | The minimum number of available virtual tapes that the gateway maintains at all times. If the number of tapes on the gateway goes below this value, the gateway creates as many new tapes as are needed to have @MinimumNumTapes@ on the gateway. For more information about automatic tape creation, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically> .
    minimumNumTapes :: Lude.Natural
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'AutomaticTapeCreationRule' with the minimum fields required to make a request.
--
-- * 'tapeBarcodePrefix' - A prefix that you append to the barcode of the virtual tape that you are creating. This prefix makes the barcode unique.
-- * 'tapeSizeInBytes' - The size, in bytes, of the virtual tape capacity.
-- * 'poolId' - The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the Amazon S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
-- * 'worm' - Set to @true@ to indicate that tapes are to be archived as write-once-read-many (WORM). Set to @false@ when WORM is not enabled for tapes.
-- * 'minimumNumTapes' - The minimum number of available virtual tapes that the gateway maintains at all times. If the number of tapes on the gateway goes below this value, the gateway creates as many new tapes as are needed to have @MinimumNumTapes@ on the gateway. For more information about automatic tape creation, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically> .
mkAutomaticTapeCreationRule ::
  -- | 'tapeBarcodePrefix'
  Lude.Text ->
  -- | 'tapeSizeInBytes'
  Lude.Integer ->
  -- | 'poolId'
  Lude.Text ->
  -- | 'minimumNumTapes'
  Lude.Natural ->
  AutomaticTapeCreationRule
mkAutomaticTapeCreationRule
  pTapeBarcodePrefix_
  pTapeSizeInBytes_
  pPoolId_
  pMinimumNumTapes_ =
    AutomaticTapeCreationRule'
      { tapeBarcodePrefix =
          pTapeBarcodePrefix_,
        tapeSizeInBytes = pTapeSizeInBytes_,
        poolId = pPoolId_,
        worm = Lude.Nothing,
        minimumNumTapes = pMinimumNumTapes_
      }

-- | A prefix that you append to the barcode of the virtual tape that you are creating. This prefix makes the barcode unique.
--
-- /Note:/ Consider using 'tapeBarcodePrefix' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrTapeBarcodePrefix :: Lens.Lens' AutomaticTapeCreationRule Lude.Text
atcrTapeBarcodePrefix = Lens.lens (tapeBarcodePrefix :: AutomaticTapeCreationRule -> Lude.Text) (\s a -> s {tapeBarcodePrefix = a} :: AutomaticTapeCreationRule)
{-# DEPRECATED atcrTapeBarcodePrefix "Use generic-lens or generic-optics with 'tapeBarcodePrefix' instead." #-}

-- | The size, in bytes, of the virtual tape capacity.
--
-- /Note:/ Consider using 'tapeSizeInBytes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrTapeSizeInBytes :: Lens.Lens' AutomaticTapeCreationRule Lude.Integer
atcrTapeSizeInBytes = Lens.lens (tapeSizeInBytes :: AutomaticTapeCreationRule -> Lude.Integer) (\s a -> s {tapeSizeInBytes = a} :: AutomaticTapeCreationRule)
{-# DEPRECATED atcrTapeSizeInBytes "Use generic-lens or generic-optics with 'tapeSizeInBytes' instead." #-}

-- | The ID of the pool that you want to add your tape to for archiving. The tape in this pool is archived in the Amazon S3 storage class that is associated with the pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- /Note:/ Consider using 'poolId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrPoolId :: Lens.Lens' AutomaticTapeCreationRule Lude.Text
atcrPoolId = Lens.lens (poolId :: AutomaticTapeCreationRule -> Lude.Text) (\s a -> s {poolId = a} :: AutomaticTapeCreationRule)
{-# DEPRECATED atcrPoolId "Use generic-lens or generic-optics with 'poolId' instead." #-}

-- | Set to @true@ to indicate that tapes are to be archived as write-once-read-many (WORM). Set to @false@ when WORM is not enabled for tapes.
--
-- /Note:/ Consider using 'worm' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrWorm :: Lens.Lens' AutomaticTapeCreationRule (Lude.Maybe Lude.Bool)
atcrWorm = Lens.lens (worm :: AutomaticTapeCreationRule -> Lude.Maybe Lude.Bool) (\s a -> s {worm = a} :: AutomaticTapeCreationRule)
{-# DEPRECATED atcrWorm "Use generic-lens or generic-optics with 'worm' instead." #-}

-- | The minimum number of available virtual tapes that the gateway maintains at all times. If the number of tapes on the gateway goes below this value, the gateway creates as many new tapes as are needed to have @MinimumNumTapes@ on the gateway. For more information about automatic tape creation, see <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically> .
--
-- /Note:/ Consider using 'minimumNumTapes' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
atcrMinimumNumTapes :: Lens.Lens' AutomaticTapeCreationRule Lude.Natural
atcrMinimumNumTapes = Lens.lens (minimumNumTapes :: AutomaticTapeCreationRule -> Lude.Natural) (\s a -> s {minimumNumTapes = a} :: AutomaticTapeCreationRule)
{-# DEPRECATED atcrMinimumNumTapes "Use generic-lens or generic-optics with 'minimumNumTapes' instead." #-}

instance Lude.FromJSON AutomaticTapeCreationRule where
  parseJSON =
    Lude.withObject
      "AutomaticTapeCreationRule"
      ( \x ->
          AutomaticTapeCreationRule'
            Lude.<$> (x Lude..: "TapeBarcodePrefix")
            Lude.<*> (x Lude..: "TapeSizeInBytes")
            Lude.<*> (x Lude..: "PoolId")
            Lude.<*> (x Lude..:? "Worm")
            Lude.<*> (x Lude..: "MinimumNumTapes")
      )

instance Lude.ToJSON AutomaticTapeCreationRule where
  toJSON AutomaticTapeCreationRule' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just ("TapeBarcodePrefix" Lude..= tapeBarcodePrefix),
            Lude.Just ("TapeSizeInBytes" Lude..= tapeSizeInBytes),
            Lude.Just ("PoolId" Lude..= poolId),
            ("Worm" Lude..=) Lude.<$> worm,
            Lude.Just ("MinimumNumTapes" Lude..= minimumNumTapes)
          ]
      )
