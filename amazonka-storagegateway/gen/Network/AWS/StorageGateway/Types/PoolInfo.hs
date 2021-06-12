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
-- Module      : Network.AWS.StorageGateway.Types.PoolInfo
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.PoolInfo where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.StorageGateway.Types.PoolStatus
import Network.AWS.StorageGateway.Types.RetentionLockType
import Network.AWS.StorageGateway.Types.TapeStorageClass

-- | Describes a custom tape pool.
--
-- /See:/ 'newPoolInfo' smart constructor.
data PoolInfo = PoolInfo'
  { -- | The Amazon Resource Name (ARN) of the custom tape pool. Use the
    -- ListTapePools operation to return a list of custom tape pools for your
    -- account and AWS Region.
    poolARN :: Core.Maybe Core.Text,
    -- | The name of the custom tape pool. @PoolName@ can use all ASCII
    -- characters, except \'\/\' and \'\\\'.
    poolName :: Core.Maybe Core.Text,
    -- | The storage class that is associated with the custom pool. When you use
    -- your backup application to eject the tape, the tape is archived directly
    -- into the storage class (S3 Glacier or S3 Glacier Deep Archive) that
    -- corresponds to the pool.
    storageClass :: Core.Maybe TapeStorageClass,
    -- | Tape retention lock type, which can be configured in two modes. When
    -- configured in governance mode, AWS accounts with specific IAM
    -- permissions are authorized to remove the tape retention lock from
    -- archived virtual tapes. When configured in compliance mode, the tape
    -- retention lock cannot be removed by any user, including the root AWS
    -- account.
    retentionLockType :: Core.Maybe RetentionLockType,
    -- | Status of the custom tape pool. Pool can be @ACTIVE@ or @DELETED@.
    poolStatus :: Core.Maybe PoolStatus,
    -- | Tape retention lock time is set in days. Tape retention lock can be
    -- enabled for up to 100 years (36,500 days).
    retentionLockTimeInDays :: Core.Maybe Core.Natural
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- account and AWS Region.
--
-- 'poolName', 'poolInfo_poolName' - The name of the custom tape pool. @PoolName@ can use all ASCII
-- characters, except \'\/\' and \'\\\'.
--
-- 'storageClass', 'poolInfo_storageClass' - The storage class that is associated with the custom pool. When you use
-- your backup application to eject the tape, the tape is archived directly
-- into the storage class (S3 Glacier or S3 Glacier Deep Archive) that
-- corresponds to the pool.
--
-- 'retentionLockType', 'poolInfo_retentionLockType' - Tape retention lock type, which can be configured in two modes. When
-- configured in governance mode, AWS accounts with specific IAM
-- permissions are authorized to remove the tape retention lock from
-- archived virtual tapes. When configured in compliance mode, the tape
-- retention lock cannot be removed by any user, including the root AWS
-- account.
--
-- 'poolStatus', 'poolInfo_poolStatus' - Status of the custom tape pool. Pool can be @ACTIVE@ or @DELETED@.
--
-- 'retentionLockTimeInDays', 'poolInfo_retentionLockTimeInDays' - Tape retention lock time is set in days. Tape retention lock can be
-- enabled for up to 100 years (36,500 days).
newPoolInfo ::
  PoolInfo
newPoolInfo =
  PoolInfo'
    { poolARN = Core.Nothing,
      poolName = Core.Nothing,
      storageClass = Core.Nothing,
      retentionLockType = Core.Nothing,
      poolStatus = Core.Nothing,
      retentionLockTimeInDays = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the custom tape pool. Use the
-- ListTapePools operation to return a list of custom tape pools for your
-- account and AWS Region.
poolInfo_poolARN :: Lens.Lens' PoolInfo (Core.Maybe Core.Text)
poolInfo_poolARN = Lens.lens (\PoolInfo' {poolARN} -> poolARN) (\s@PoolInfo' {} a -> s {poolARN = a} :: PoolInfo)

-- | The name of the custom tape pool. @PoolName@ can use all ASCII
-- characters, except \'\/\' and \'\\\'.
poolInfo_poolName :: Lens.Lens' PoolInfo (Core.Maybe Core.Text)
poolInfo_poolName = Lens.lens (\PoolInfo' {poolName} -> poolName) (\s@PoolInfo' {} a -> s {poolName = a} :: PoolInfo)

-- | The storage class that is associated with the custom pool. When you use
-- your backup application to eject the tape, the tape is archived directly
-- into the storage class (S3 Glacier or S3 Glacier Deep Archive) that
-- corresponds to the pool.
poolInfo_storageClass :: Lens.Lens' PoolInfo (Core.Maybe TapeStorageClass)
poolInfo_storageClass = Lens.lens (\PoolInfo' {storageClass} -> storageClass) (\s@PoolInfo' {} a -> s {storageClass = a} :: PoolInfo)

-- | Tape retention lock type, which can be configured in two modes. When
-- configured in governance mode, AWS accounts with specific IAM
-- permissions are authorized to remove the tape retention lock from
-- archived virtual tapes. When configured in compliance mode, the tape
-- retention lock cannot be removed by any user, including the root AWS
-- account.
poolInfo_retentionLockType :: Lens.Lens' PoolInfo (Core.Maybe RetentionLockType)
poolInfo_retentionLockType = Lens.lens (\PoolInfo' {retentionLockType} -> retentionLockType) (\s@PoolInfo' {} a -> s {retentionLockType = a} :: PoolInfo)

-- | Status of the custom tape pool. Pool can be @ACTIVE@ or @DELETED@.
poolInfo_poolStatus :: Lens.Lens' PoolInfo (Core.Maybe PoolStatus)
poolInfo_poolStatus = Lens.lens (\PoolInfo' {poolStatus} -> poolStatus) (\s@PoolInfo' {} a -> s {poolStatus = a} :: PoolInfo)

-- | Tape retention lock time is set in days. Tape retention lock can be
-- enabled for up to 100 years (36,500 days).
poolInfo_retentionLockTimeInDays :: Lens.Lens' PoolInfo (Core.Maybe Core.Natural)
poolInfo_retentionLockTimeInDays = Lens.lens (\PoolInfo' {retentionLockTimeInDays} -> retentionLockTimeInDays) (\s@PoolInfo' {} a -> s {retentionLockTimeInDays = a} :: PoolInfo)

instance Core.FromJSON PoolInfo where
  parseJSON =
    Core.withObject
      "PoolInfo"
      ( \x ->
          PoolInfo'
            Core.<$> (x Core..:? "PoolARN")
            Core.<*> (x Core..:? "PoolName")
            Core.<*> (x Core..:? "StorageClass")
            Core.<*> (x Core..:? "RetentionLockType")
            Core.<*> (x Core..:? "PoolStatus")
            Core.<*> (x Core..:? "RetentionLockTimeInDays")
      )

instance Core.Hashable PoolInfo

instance Core.NFData PoolInfo
