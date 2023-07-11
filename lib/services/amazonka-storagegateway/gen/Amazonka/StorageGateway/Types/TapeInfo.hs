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
-- Module      : Amazonka.StorageGateway.Types.TapeInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.TapeInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude

-- | Describes a virtual tape.
--
-- /See:/ 'newTapeInfo' smart constructor.
data TapeInfo = TapeInfo'
  { -- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
    -- operation to return a list of gateways for your account and Amazon Web
    -- Services Region.
    gatewayARN :: Prelude.Maybe Prelude.Text,
    -- | The date that the tape entered the custom tape pool with tape retention
    -- lock enabled.
    poolEntryDate :: Prelude.Maybe Data.POSIX,
    -- | The ID of the pool that you want to add your tape to for archiving. The
    -- tape in this pool is archived in the S3 storage class that is associated
    -- with the pool. When you use your backup application to eject the tape,
    -- the tape is archived directly into the storage class (S3 Glacier or S3
    -- Glacier Deep Archive) that corresponds to the pool.
    poolId :: Prelude.Maybe Prelude.Text,
    -- | The date that the tape became subject to tape retention lock.
    retentionStartDate :: Prelude.Maybe Data.POSIX,
    -- | The Amazon Resource Name (ARN) of a virtual tape.
    tapeARN :: Prelude.Maybe Prelude.Text,
    -- | The barcode that identifies a specific virtual tape.
    tapeBarcode :: Prelude.Maybe Prelude.Text,
    -- | The size, in bytes, of a virtual tape.
    tapeSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The status of the tape.
    tapeStatus :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'TapeInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'gatewayARN', 'tapeInfo_gatewayARN' - The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
--
-- 'poolEntryDate', 'tapeInfo_poolEntryDate' - The date that the tape entered the custom tape pool with tape retention
-- lock enabled.
--
-- 'poolId', 'tapeInfo_poolId' - The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
--
-- 'retentionStartDate', 'tapeInfo_retentionStartDate' - The date that the tape became subject to tape retention lock.
--
-- 'tapeARN', 'tapeInfo_tapeARN' - The Amazon Resource Name (ARN) of a virtual tape.
--
-- 'tapeBarcode', 'tapeInfo_tapeBarcode' - The barcode that identifies a specific virtual tape.
--
-- 'tapeSizeInBytes', 'tapeInfo_tapeSizeInBytes' - The size, in bytes, of a virtual tape.
--
-- 'tapeStatus', 'tapeInfo_tapeStatus' - The status of the tape.
newTapeInfo ::
  TapeInfo
newTapeInfo =
  TapeInfo'
    { gatewayARN = Prelude.Nothing,
      poolEntryDate = Prelude.Nothing,
      poolId = Prelude.Nothing,
      retentionStartDate = Prelude.Nothing,
      tapeARN = Prelude.Nothing,
      tapeBarcode = Prelude.Nothing,
      tapeSizeInBytes = Prelude.Nothing,
      tapeStatus = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the gateway. Use the ListGateways
-- operation to return a list of gateways for your account and Amazon Web
-- Services Region.
tapeInfo_gatewayARN :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Text)
tapeInfo_gatewayARN = Lens.lens (\TapeInfo' {gatewayARN} -> gatewayARN) (\s@TapeInfo' {} a -> s {gatewayARN = a} :: TapeInfo)

-- | The date that the tape entered the custom tape pool with tape retention
-- lock enabled.
tapeInfo_poolEntryDate :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.UTCTime)
tapeInfo_poolEntryDate = Lens.lens (\TapeInfo' {poolEntryDate} -> poolEntryDate) (\s@TapeInfo' {} a -> s {poolEntryDate = a} :: TapeInfo) Prelude.. Lens.mapping Data._Time

-- | The ID of the pool that you want to add your tape to for archiving. The
-- tape in this pool is archived in the S3 storage class that is associated
-- with the pool. When you use your backup application to eject the tape,
-- the tape is archived directly into the storage class (S3 Glacier or S3
-- Glacier Deep Archive) that corresponds to the pool.
tapeInfo_poolId :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Text)
tapeInfo_poolId = Lens.lens (\TapeInfo' {poolId} -> poolId) (\s@TapeInfo' {} a -> s {poolId = a} :: TapeInfo)

-- | The date that the tape became subject to tape retention lock.
tapeInfo_retentionStartDate :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.UTCTime)
tapeInfo_retentionStartDate = Lens.lens (\TapeInfo' {retentionStartDate} -> retentionStartDate) (\s@TapeInfo' {} a -> s {retentionStartDate = a} :: TapeInfo) Prelude.. Lens.mapping Data._Time

-- | The Amazon Resource Name (ARN) of a virtual tape.
tapeInfo_tapeARN :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Text)
tapeInfo_tapeARN = Lens.lens (\TapeInfo' {tapeARN} -> tapeARN) (\s@TapeInfo' {} a -> s {tapeARN = a} :: TapeInfo)

-- | The barcode that identifies a specific virtual tape.
tapeInfo_tapeBarcode :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Text)
tapeInfo_tapeBarcode = Lens.lens (\TapeInfo' {tapeBarcode} -> tapeBarcode) (\s@TapeInfo' {} a -> s {tapeBarcode = a} :: TapeInfo)

-- | The size, in bytes, of a virtual tape.
tapeInfo_tapeSizeInBytes :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Integer)
tapeInfo_tapeSizeInBytes = Lens.lens (\TapeInfo' {tapeSizeInBytes} -> tapeSizeInBytes) (\s@TapeInfo' {} a -> s {tapeSizeInBytes = a} :: TapeInfo)

-- | The status of the tape.
tapeInfo_tapeStatus :: Lens.Lens' TapeInfo (Prelude.Maybe Prelude.Text)
tapeInfo_tapeStatus = Lens.lens (\TapeInfo' {tapeStatus} -> tapeStatus) (\s@TapeInfo' {} a -> s {tapeStatus = a} :: TapeInfo)

instance Data.FromJSON TapeInfo where
  parseJSON =
    Data.withObject
      "TapeInfo"
      ( \x ->
          TapeInfo'
            Prelude.<$> (x Data..:? "GatewayARN")
            Prelude.<*> (x Data..:? "PoolEntryDate")
            Prelude.<*> (x Data..:? "PoolId")
            Prelude.<*> (x Data..:? "RetentionStartDate")
            Prelude.<*> (x Data..:? "TapeARN")
            Prelude.<*> (x Data..:? "TapeBarcode")
            Prelude.<*> (x Data..:? "TapeSizeInBytes")
            Prelude.<*> (x Data..:? "TapeStatus")
      )

instance Prelude.Hashable TapeInfo where
  hashWithSalt _salt TapeInfo' {..} =
    _salt
      `Prelude.hashWithSalt` gatewayARN
      `Prelude.hashWithSalt` poolEntryDate
      `Prelude.hashWithSalt` poolId
      `Prelude.hashWithSalt` retentionStartDate
      `Prelude.hashWithSalt` tapeARN
      `Prelude.hashWithSalt` tapeBarcode
      `Prelude.hashWithSalt` tapeSizeInBytes
      `Prelude.hashWithSalt` tapeStatus

instance Prelude.NFData TapeInfo where
  rnf TapeInfo' {..} =
    Prelude.rnf gatewayARN
      `Prelude.seq` Prelude.rnf poolEntryDate
      `Prelude.seq` Prelude.rnf poolId
      `Prelude.seq` Prelude.rnf retentionStartDate
      `Prelude.seq` Prelude.rnf tapeARN
      `Prelude.seq` Prelude.rnf tapeBarcode
      `Prelude.seq` Prelude.rnf tapeSizeInBytes
      `Prelude.seq` Prelude.rnf tapeStatus
