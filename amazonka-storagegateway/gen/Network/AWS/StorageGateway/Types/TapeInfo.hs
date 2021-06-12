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
-- Module      : Network.AWS.StorageGateway.Types.TapeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens

-- | Describes a virtual tape.
--
-- /See:/ 'newTapeInfo' smart constructor.
data TapeInfo = TapeInfo'
  { -- | The date that the tape entered the custom tape pool with tape retention
    -- lock enabled.
    poolEntryDate :: Core.Maybe Core.POSIX,
    -- | The status of the tape.
    tapeStatus :: Core.Maybe Core.Text,
    -- | The ID of the pool that you want to add your tape to for archiving. The
    -- tape in this pool is archived in the S3 storage class that is associated
    -- with the pool. When you use your backup application to eject the tape,
    -- the tape is archived directly into the storage class (S3 Glacier or S3
    -- Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Core.Maybe Core.Text,
    -- | The Amazon Resource Name (ARN) of a virtual tape.
    tapeARN :: Core.Maybe Core.Text,
    -- | The barcode that identifies a specific virtual tape.
    tapeBarcode :: Core.Maybe Core.Text,
    -- | The size, in bytes, of a virtual tape.
    tapeSizeInBytes :: Core.Maybe Core.Integer,
    -- | The date that the tape became subject to tape retention lock.
    retentionStartDate :: Core.Maybe Core.POSIX,
    -- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
    -- operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'TapeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolEntryDate', 'tapeInfo_poolEntryDate' - The date that the tape entered the custom tape pool with tape retention
-- lock enabled.
--
-- 'tapeStatus', 'tapeInfo_tapeStatus' - The status of the tape.
--
-- 'poolId', 'tapeInfo_poolId' - The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
--
-- 'tapeARN', 'tapeInfo_tapeARN' - The Amazon Resource Name (ARN) of a virtual tape.
--
-- 'tapeBarcode', 'tapeInfo_tapeBarcode' - The barcode that identifies a specific virtual tape.
--
-- 'tapeSizeInBytes', 'tapeInfo_tapeSizeInBytes' - The size, in bytes, of a virtual tape.
--
-- 'retentionStartDate', 'tapeInfo_retentionStartDate' - The date that the tape became subject to tape retention lock.
--
-- 'gatewayARN', 'tapeInfo_gatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and AWS Region.
newTapeInfo ::
  TapeInfo
newTapeInfo =
  TapeInfo'
    { poolEntryDate = Core.Nothing,
      tapeStatus = Core.Nothing,
      poolId = Core.Nothing,
      tapeARN = Core.Nothing,
      tapeBarcode = Core.Nothing,
      tapeSizeInBytes = Core.Nothing,
      retentionStartDate = Core.Nothing,
      gatewayARN = Core.Nothing
    }

-- | The date that the tape entered the custom tape pool with tape retention
-- lock enabled.
tapeInfo_poolEntryDate :: Lens.Lens' TapeInfo (Core.Maybe Core.UTCTime)
tapeInfo_poolEntryDate = Lens.lens (\TapeInfo' {poolEntryDate} -> poolEntryDate) (\s@TapeInfo' {} a -> s {poolEntryDate = a} :: TapeInfo) Core.. Lens.mapping Core._Time

-- | The status of the tape.
tapeInfo_tapeStatus :: Lens.Lens' TapeInfo (Core.Maybe Core.Text)
tapeInfo_tapeStatus = Lens.lens (\TapeInfo' {tapeStatus} -> tapeStatus) (\s@TapeInfo' {} a -> s {tapeStatus = a} :: TapeInfo)

-- | The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
tapeInfo_poolId :: Lens.Lens' TapeInfo (Core.Maybe Core.Text)
tapeInfo_poolId = Lens.lens (\TapeInfo' {poolId} -> poolId) (\s@TapeInfo' {} a -> s {poolId = a} :: TapeInfo)

-- | The Amazon Resource Name (ARN) of a virtual tape.
tapeInfo_tapeARN :: Lens.Lens' TapeInfo (Core.Maybe Core.Text)
tapeInfo_tapeARN = Lens.lens (\TapeInfo' {tapeARN} -> tapeARN) (\s@TapeInfo' {} a -> s {tapeARN = a} :: TapeInfo)

-- | The barcode that identifies a specific virtual tape.
tapeInfo_tapeBarcode :: Lens.Lens' TapeInfo (Core.Maybe Core.Text)
tapeInfo_tapeBarcode = Lens.lens (\TapeInfo' {tapeBarcode} -> tapeBarcode) (\s@TapeInfo' {} a -> s {tapeBarcode = a} :: TapeInfo)

-- | The size, in bytes, of a virtual tape.
tapeInfo_tapeSizeInBytes :: Lens.Lens' TapeInfo (Core.Maybe Core.Integer)
tapeInfo_tapeSizeInBytes = Lens.lens (\TapeInfo' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@TapeInfo' {} a -> s {tapeSizeInBytes = a} :: TapeInfo)

-- | The date that the tape became subject to tape retention lock.
tapeInfo_retentionStartDate :: Lens.Lens' TapeInfo (Core.Maybe Core.UTCTime)
tapeInfo_retentionStartDate = Lens.lens (\TapeInfo' {retentionStartDate} -> retentionStartDate) (\s@TapeInfo' {} a -> s {retentionStartDate = a} :: TapeInfo) Core.. Lens.mapping Core._Time

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and AWS Region.
tapeInfo_gatewayARN :: Lens.Lens' TapeInfo (Core.Maybe Core.Text)
tapeInfo_gatewayARN = Lens.lens (\TapeInfo' {gatewayARN} -> gatewayARN) (\s@TapeInfo' {} a -> s {gatewayARN = a} :: TapeInfo)

instance Core.FromJSON TapeInfo where
  parseJSON =
    Core.withObject
      "TapeInfo"
      ( \x ->
          TapeInfo'
            Core.<$> (x Core..:? "PoolEntryDate")
            Core.<*> (x Core..:? "TapeStatus")
            Core.<*> (x Core..:? "PoolId")
            Core.<*> (x Core..:? "TapeARN")
            Core.<*> (x Core..:? "TapeBarcode")
            Core.<*> (x Core..:? "TapeSizeInBytes")
            Core.<*> (x Core..:? "RetentionStartDate")
            Core.<*> (x Core..:? "GatewayARN")
      )

instance Core.Hashable TapeInfo

instance Core.NFData TapeInfo
