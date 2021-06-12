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
-- Module      : Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.AutomaticTapeCreationRule where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | An automatic tape creation policy consists of automatic tape creation
-- rules where each rule defines when and how to create new tapes. For more
-- information about automatic tape creation, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically>.
--
-- /See:/ 'newAutomaticTapeCreationRule' smart constructor.
data AutomaticTapeCreationRule = AutomaticTapeCreationRule'
  { -- | Set to @true@ to indicate that tapes are to be archived as
    -- write-once-read-many (WORM). Set to @false@ when WORM is not enabled for
    -- tapes.
    worm :: Core.Maybe Core.Bool,
    -- | A prefix that you append to the barcode of the virtual tape that you are
    -- creating. This prefix makes the barcode unique.
    --
    -- The prefix must be 1-4 characters in length and must be one of the
    -- uppercase letters from A to Z.
    tapeBarcodePrefix :: Core.Text,
    -- | The ID of the pool that you want to add your tape to for archiving. The
    -- tape in this pool is archived in the Amazon S3 storage class that is
    -- associated with the pool. When you use your backup application to eject
    -- the tape, the tape is archived directly into the storage class (S3
    -- Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Core.Text,
    -- | The size, in bytes, of the virtual tape capacity.
    tapeSizeInBytes :: Core.Integer,
    -- | The minimum number of available virtual tapes that the gateway maintains
    -- at all times. If the number of tapes on the gateway goes below this
    -- value, the gateway creates as many new tapes as are needed to have
    -- @MinimumNumTapes@ on the gateway. For more information about automatic
    -- tape creation, see
    -- <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically>.
    minimumNumTapes :: Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AutomaticTapeCreationRule' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'worm', 'automaticTapeCreationRule_worm' - Set to @true@ to indicate that tapes are to be archived as
-- write-once-read-many (WORM). Set to @false@ when WORM is not enabled for
-- tapes.
--
-- 'tapeBarcodePrefix', 'automaticTapeCreationRule_tapeBarcodePrefix' - A prefix that you append to the barcode of the virtual tape that you are
-- creating. This prefix makes the barcode unique.
--
-- The prefix must be 1-4 characters in length and must be one of the
-- uppercase letters from A to Z.
--
-- 'poolId', 'automaticTapeCreationRule_poolId' - The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the Amazon S3 storage class that is
-- associated with the pool. When you use your backup application to eject
-- the tape, the tape is archived directly into the storage class (S3
-- Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- 'tapeSizeInBytes', 'automaticTapeCreationRule_tapeSizeInBytes' - The size, in bytes, of the virtual tape capacity.
--
-- 'minimumNumTapes', 'automaticTapeCreationRule_minimumNumTapes' - The minimum number of available virtual tapes that the gateway maintains
-- at all times. If the number of tapes on the gateway goes below this
-- value, the gateway creates as many new tapes as are needed to have
-- @MinimumNumTapes@ on the gateway. For more information about automatic
-- tape creation, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically>.
newAutomaticTapeCreationRule ::
  -- | 'tapeBarcodePrefix'
  Core.Text ->
  -- | 'poolId'
  Core.Text ->
  -- | 'tapeSizeInBytes'
  Core.Integer ->
  -- | 'minimumNumTapes'
  Core.Natural ->
  AutomaticTapeCreationRule
newAutomaticTapeCreationRule
  pTapeBarcodePrefix_
  pPoolId_
  pTapeSizeInBytes_
  pMinimumNumTapes_ =
    AutomaticTapeCreationRule'
      { worm = Core.Nothing,
        tapeBarcodePrefix = pTapeBarcodePrefix_,
        poolId = pPoolId_,
        tapeSizeInBytes = pTapeSizeInBytes_,
        minimumNumTapes = pMinimumNumTapes_
      }

-- | Set to @true@ to indicate that tapes are to be archived as
-- write-once-read-many (WORM). Set to @false@ when WORM is not enabled for
-- tapes.
automaticTapeCreationRule_worm :: Lens.Lens' AutomaticTapeCreationRule (Core.Maybe Core.Bool)
automaticTapeCreationRule_worm = Lens.lens (\AutomaticTapeCreationRule' {worm} -> worm) (\s@AutomaticTapeCreationRule' {} a -> s {worm = a} :: AutomaticTapeCreationRule)

-- | A prefix that you append to the barcode of the virtual tape that you are
-- creating. This prefix makes the barcode unique.
--
-- The prefix must be 1-4 characters in length and must be one of the
-- uppercase letters from A to Z.
automaticTapeCreationRule_tapeBarcodePrefix :: Lens.Lens' AutomaticTapeCreationRule Core.Text
automaticTapeCreationRule_tapeBarcodePrefix = Lens.lens (\AutomaticTapeCreationRule' {tapeBarcodePrefix} -> tapeBarcodePrefix) (\s@AutomaticTapeCreationRule' {} a -> s {tapeBarcodePrefix = a} :: AutomaticTapeCreationRule)

-- | The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the Amazon S3 storage class that is
-- associated with the pool. When you use your backup application to eject
-- the tape, the tape is archived directly into the storage class (S3
-- Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
automaticTapeCreationRule_poolId :: Lens.Lens' AutomaticTapeCreationRule Core.Text
automaticTapeCreationRule_poolId = Lens.lens (\AutomaticTapeCreationRule' {poolId} -> poolId) (\s@AutomaticTapeCreationRule' {} a -> s {poolId = a} :: AutomaticTapeCreationRule)

-- | The size, in bytes, of the virtual tape capacity.
automaticTapeCreationRule_tapeSizeInBytes :: Lens.Lens' AutomaticTapeCreationRule Core.Integer
automaticTapeCreationRule_tapeSizeInBytes = Lens.lens (\AutomaticTapeCreationRule' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@AutomaticTapeCreationRule' {} a -> s {tapeSizeInBytes = a} :: AutomaticTapeCreationRule)

-- | The minimum number of available virtual tapes that the gateway maintains
-- at all times. If the number of tapes on the gateway goes below this
-- value, the gateway creates as many new tapes as are needed to have
-- @MinimumNumTapes@ on the gateway. For more information about automatic
-- tape creation, see
-- <https://docs.aws.amazon.com/storagegateway/latest/userguide/GettingStartedCreateTapes.html#CreateTapesAutomatically Creating Tapes Automatically>.
automaticTapeCreationRule_minimumNumTapes :: Lens.Lens' AutomaticTapeCreationRule Core.Natural
automaticTapeCreationRule_minimumNumTapes = Lens.lens (\AutomaticTapeCreationRule' {minimumNumTapes} -> minimumNumTapes) (\s@AutomaticTapeCreationRule' {} a -> s {minimumNumTapes = a} :: AutomaticTapeCreationRule)

instance Core.FromJSON AutomaticTapeCreationRule where
  parseJSON =
    Core.withObject
      "AutomaticTapeCreationRule"
      ( \x ->
          AutomaticTapeCreationRule'
            Core.<$> (x Core..:? "Worm")
            Core.<*> (x Core..: "TapeBarcodePrefix")
            Core.<*> (x Core..: "PoolId")
            Core.<*> (x Core..: "TapeSizeInBytes")
            Core.<*> (x Core..: "MinimumNumTapes")
      )

instance Core.Hashable AutomaticTapeCreationRule

instance Core.NFData AutomaticTapeCreationRule

instance Core.ToJSON AutomaticTapeCreationRule where
  toJSON AutomaticTapeCreationRule' {..} =
    Core.object
      ( Core.catMaybes
          [ ("Worm" Core..=) Core.<$> worm,
            Core.Just
              ("TapeBarcodePrefix" Core..= tapeBarcodePrefix),
            Core.Just ("PoolId" Core..= poolId),
            Core.Just
              ("TapeSizeInBytes" Core..= tapeSizeInBytes),
            Core.Just
              ("MinimumNumTapes" Core..= minimumNumTapes)
          ]
      )
