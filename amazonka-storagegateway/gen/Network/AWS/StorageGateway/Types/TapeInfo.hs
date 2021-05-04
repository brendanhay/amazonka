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
-- Module      : Network.AWS.StorageGateway.Types.TapeInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.TapeInfo where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude

-- | Describes a virtual tape.
--
-- /See:/ 'newTapeInfo' smart constructor.
data TapeInfo = TapeInfo'
  { -- | The date that the tape entered the custom tape pool with tape retention
    -- lock enabled.
    poolEntryDate :: Prelude.Maybe Prelude.POSIX,
    -- | The status of the tape.
    tapeStatus :: Prelude.Maybe Prelude.Text,
    -- | The ID of the pool that you want to add your tape to for archiving. The
    -- tape in this pool is archived in the S3 storage class that is associated
    -- with the pool. When you use your backup application to eject the tape,
    -- the tape is archived directly into the storage class (S3 Glacier or S3
    -- Glacier Deep Archive) that corresponds to the pool.
    --
    -- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of a virtual tape.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The barcode that identifies a specific virtual tape.
    tapeBarcode :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of a virtual tape.
    tapeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The date that the tape became subject to tape retention lock.
    retentionStartDate :: Prelude.Maybe Prelude.POSIX,
    -- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
    -- operation to return a list of gateways for your account and AWS Region.
    gatewayARN :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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
    { poolEntryDate = Prelude.Nothing,
      tapeStatus = Prelude.Nothing,
      poolId = Prelude.Nothing,
      tapeARN = Prelude.Nothing,
      tapeBarcode = Prelude.Nothing,
      tapeSizeInBytes = Prelude.Nothing,
      retentionStartDate = Prelude.Nothing,
      gatewayARN = Prelude.Nothing
    }

-- | The date that the tape entered the custom tape pool with tape retention
-- lock enabled.
tapeInfo_poolEntryDate :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.UTCTime)
tapeInfo_poolEntryDate = Lens.lens (\TapeInfo' {poolEntryDate} -> poolEntryDate) (\s@TapeInfo' {} a -> s {poolEntryDate = a} :: TapeInfo) Prelude.. Lens.mapping Prelude._Time

-- | The status of the tape.
tapeInfo_tapeStatus :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Text)
tapeInfo_tapeStatus = Lens.lens (\TapeInfo' {tapeStatus} -> tapeStatus) (\s@TapeInfo' {} a -> s {tapeStatus = a} :: TapeInfo)

-- | The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- Valid Values: @GLACIER@ | @DEEP_ARCHIVE@
tapeInfo_poolId :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Text)
tapeInfo_poolId = Lens.lens (\TapeInfo' {poolId} -> poolId) (\s@TapeInfo' {} a -> s {poolId = a} :: TapeInfo)

-- | The Amazon Resource Name (ARN) of a virtual tape.
tapeInfo_tapeARN :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Text)
tapeInfo_tapeARN = Lens.lens (\TapeInfo' {tapeARN} -> tapeARN) (\s@TapeInfo' {} a -> s {tapeARN = a} :: TapeInfo)

-- | The barcode that identifies a specific virtual tape.
tapeInfo_tapeBarcode :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Text)
tapeInfo_tapeBarcode = Lens.lens (\TapeInfo' {tapeBarcode} -> tapeBarcode) (\s@TapeInfo' {} a -> s {tapeBarcode = a} :: TapeInfo)

-- | The size, in bytes, of a virtual tape.
tapeInfo_tapeSizeInBytes :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Integer)
tapeInfo_tapeSizeInBytes = Lens.lens (\TapeInfo' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@TapeInfo' {} a -> s {tapeSizeInBytes = a} :: TapeInfo)

-- | The date that the tape became subject to tape retention lock.
tapeInfo_retentionStartDate :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.UTCTime)
tapeInfo_retentionStartDate = Lens.lens (\TapeInfo' {retentionStartDate} -> retentionStartDate) (\s@TapeInfo' {} a -> s {retentionStartDate = a} :: TapeInfo) Prelude.. Lens.mapping Prelude._Time

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and AWS Region.
tapeInfo_gatewayARN :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Text)
tapeInfo_gatewayARN = Lens.lens (\TapeInfo' {gatewayARN} -> gatewayARN) (\s@TapeInfo' {} a -> s {gatewayARN = a} :: TapeInfo)

instance Prelude.FromJSON TapeInfo where
  parseJSON =
    Prelude.withObject
      "TapeInfo"
      ( \x ->
          TapeInfo'
            Prelude.<$> (x Prelude..:? "PoolEntryDate")
            Prelude.<*> (x Prelude..:? "TapeStatus")
            Prelude.<*> (x Prelude..:? "PoolId")
            Prelude.<*> (x Prelude..:? "TapeARN")
            Prelude.<*> (x Prelude..:? "TapeBarcode")
            Prelude.<*> (x Prelude..:? "TapeSizeInBytes")
            Prelude.<*> (x Prelude..:? "RetentionStartDate")
            Prelude.<*> (x Prelude..:? "GatewayARN")
      )

instance Prelude.Hashable TapeInfo

instance Prelude.NFData TapeInfo
