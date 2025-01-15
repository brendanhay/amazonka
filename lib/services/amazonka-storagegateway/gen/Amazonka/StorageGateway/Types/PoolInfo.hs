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
-- Module      : Amazonka.StorageGateway.Types.PoolInfo
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.StorageGateway.Types.PoolInfo where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import Amazonka.StorageGateway.Types.PoolStatus
import Amazonka.StorageGateway.Types.RetentionLockType
import Amazonka.StorageGateway.Types.TapeStorageClass

-- | Describes a custom tape pool.
--
-- /See:/ 'newPoolInfo' smart constructor.
data PoolInfo = PoolInfo'
  { -- | The Amazon Resource Name (ARN) of the custom tape pool. Use the
    -- ListTapePools operation to return a list of custom tape pools for your
    -- account and Amazon Web Services Region.
    poolARN :: Prelude.Maybe Prelude.Text,
    -- | The name of the custom tape pool. @PoolName@ can use all ASCII
    -- characters, except \'\/\' and \'\\\'.
    poolName :: Prelude.Maybe Prelude.Text,
    -- | Status of the custom tape pool. Pool can be @ACTIVE@ or @DELETED@.
    poolStatus :: Prelude.Maybe PoolStatus,
    -- | Tape retention lock time is set in days. Tape retention lock can be
    -- enabled for up to 100 years (36,500 days).
    retentionLockTimeInDays :: Prelude.Maybe Prelude.Natural,
    -- | Tape retention lock type, which can be configured in two modes. When
    -- configured in governance mode, Amazon Web Services accounts with
    -- specific IAM permissions are authorized to remove the tape retention
    -- lock from archived virtual tapes. When configured in compliance mode,
    -- the tape retention lock cannot be removed by any user, including the
    -- root Amazon Web Services account.
    retentionLockType :: Prelude.Maybe RetentionLockType,
    -- | The storage class that is associated with the custom pool. When you use
    -- your backup application to eject the tape, the tape is archived directly
    -- into the storage class (S3 Glacier or S3 Glacier Deep Archive) that
    -- corresponds to the pool.
    storageClass :: Prelude.Maybe TapeStorageClass
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PoolInfo' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'poolARN', 'poolInfo_poolARN' - The Amazon Resource Name (ARN) of the custom tape pool. Use the
-- ListTapePools operation to return a list of custom tape pools for your
-- account and Amazon Web Services Region.
--
-- 'poolName', 'poolInfo_poolName' - The name of the custom tape pool. @PoolName@ can use all ASCII
-- characters, except \'\/\' and \'\\\'.
--
-- 'poolStatus', 'poolInfo_poolStatus' - Status of the custom tape pool. Pool can be @ACTIVE@ or @DELETED@.
--
-- 'retentionLockTimeInDays', 'poolInfo_retentionLockTimeInDays' - Tape retention lock time is set in days. Tape retention lock can be
-- enabled for up to 100 years (36,500 days).
--
-- 'retentionLockType', 'poolInfo_retentionLockType' - Tape retention lock type, which can be configured in two modes. When
-- configured in governance mode, Amazon Web Services accounts with
-- specific IAM permissions are authorized to remove the tape retention
-- lock from archived virtual tapes. When configured in compliance mode,
-- the tape retention lock cannot be removed by any user, including the
-- root Amazon Web Services account.
--
-- 'storageClass', 'poolInfo_storageClass' - The storage class that is associated with the custom pool. When you use
-- your backup application to eject the tape, the tape is archived directly
-- into the storage class (S3 Glacier or S3 Glacier Deep Archive) that
-- corresponds to the pool.
newPoolInfo ::
  PoolInfo
newPoolInfo =
  PoolInfo'
    { poolARN = Prelude.Nothing,
      poolName = Prelude.Nothing,
      poolStatus = Prelude.Nothing,
      retentionLockTimeInDays = Prelude.Nothing,
      retentionLockType = Prelude.Nothing,
      storageClass = Prelude.Nothing
    }

-- | The Amazon Resource Name (ARN) of the custom tape pool. Use the
-- ListTapePools operation to return a list of custom tape pools for your
-- account and Amazon Web Services Region.
poolInfo_poolARN :: Lens.Lens' PoolInfo (Prelude.Maybe Prelude.Text)
poolInfo_poolARN = Lens.lens (\PoolInfo' {poolARN} -> poolARN) (\s@PoolInfo' {} a -> s {poolARN = a} :: PoolInfo)

-- | The name of the custom tape pool. @PoolName@ can use all ASCII
-- characters, except \'\/\' and \'\\\'.
poolInfo_poolName :: Lens.Lens' PoolInfo (Prelude.Maybe Prelude.Text)
poolInfo_poolName = Lens.lens (\PoolInfo' {poolName} -> poolName) (\s@PoolInfo' {} a -> s {poolName = a} :: PoolInfo)

-- | Status of the custom tape pool. Pool can be @ACTIVE@ or @DELETED@.
poolInfo_poolStatus :: Lens.Lens' PoolInfo (Prelude.Maybe PoolStatus)
poolInfo_poolStatus = Lens.lens (\PoolInfo' {poolStatus} -> poolStatus) (\s@PoolInfo' {} a -> s {poolStatus = a} :: PoolInfo)

-- | Tape retention lock time is set in days. Tape retention lock can be
-- enabled for up to 100 years (36,500 days).
poolInfo_retentionLockTimeInDays :: Lens.Lens' PoolInfo (Prelude.Maybe Prelude.Natural)
poolInfo_retentionLockTimeInDays = Lens.lens (\PoolInfo' {retentionLockTimeInDays} -> retentionLockTimeInDays) (\s@PoolInfo' {} a -> s {retentionLockTimeInDays = a} :: PoolInfo)

-- | Tape retention lock type, which can be configured in two modes. When
-- configured in governance mode, Amazon Web Services accounts with
-- specific IAM permissions are authorized to remove the tape retention
-- lock from archived virtual tapes. When configured in compliance mode,
-- the tape retention lock cannot be removed by any user, including the
-- root Amazon Web Services account.
poolInfo_retentionLockType :: Lens.Lens' PoolInfo (Prelude.Maybe RetentionLockType)
poolInfo_retentionLockType = Lens.lens (\PoolInfo' {retentionLockType} -> retentionLockType) (\s@PoolInfo' {} a -> s {retentionLockType = a} :: PoolInfo)

-- | The storage class that is associated with the custom pool. When you use
-- your backup application to eject the tape, the tape is archived directly
-- into the storage class (S3 Glacier or S3 Glacier Deep Archive) that
-- corresponds to the pool.
poolInfo_storageClass :: Lens.Lens' PoolInfo (Prelude.Maybe TapeStorageClass)
poolInfo_storageClass = Lens.lens (\PoolInfo' {storageClass} -> storageClass) (\s@PoolInfo' {} a -> s {storageClass = a} :: PoolInfo)

instance Data.FromJSON PoolInfo where
  parseJSON =
    Data.withObject
      "PoolInfo"
      ( \x ->
          PoolInfo'
            Prelude.<$> (x Data..:? "PoolARN")
            Prelude.<*> (x Data..:? "PoolName")
            Prelude.<*> (x Data..:? "PoolStatus")
            Prelude.<*> (x Data..:? "RetentionLockTimeInDays")
            Prelude.<*> (x Data..:? "RetentionLockType")
            Prelude.<*> (x Data..:? "StorageClass")
      )

instance Prelude.Hashable PoolInfo where
  hashWithSalt _salt PoolInfo' {..} =
    _salt
      `Prelude.hashWithSalt` poolARN
      `Prelude.hashWithSalt` poolName
      `Prelude.hashWithSalt` poolStatus
      `Prelude.hashWithSalt` retentionLockTimeInDays
      `Prelude.hashWithSalt` retentionLockType
      `Prelude.hashWithSalt` storageClass

instance Prelude.NFData PoolInfo where
  rnf PoolInfo' {..} =
    Prelude.rnf poolARN `Prelude.seq`
      Prelude.rnf poolName `Prelude.seq`
        Prelude.rnf poolStatus `Prelude.seq`
          Prelude.rnf retentionLockTimeInDays `Prelude.seq`
            Prelude.rnf retentionLockType `Prelude.seq`
              Prelude.rnf storageClass
