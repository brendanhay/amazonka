{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.StorageGateway.Types.PoolInfo
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.StorageGateway.Types.PoolInfo
  ( PoolInfo (..),

    -- * Smart constructor
    mkPoolInfo,

    -- * Lenses
    piPoolARN,
    piPoolName,
    piPoolStatus,
    piRetentionLockTimeInDays,
    piRetentionLockType,
    piStorageClass,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.StorageGateway.Types.PoolARN as Types
import qualified Network.AWS.StorageGateway.Types.PoolName as Types
import qualified Network.AWS.StorageGateway.Types.PoolStatus as Types
import qualified Network.AWS.StorageGateway.Types.RetentionLockType as Types
import qualified Network.AWS.StorageGateway.Types.TapeStorageClass as Types

-- | Describes a custom tape pool.
--
-- /See:/ 'mkPoolInfo' smart constructor.
data PoolInfo = PoolInfo'
  { -- | The Amazon Resource Name (ARN) of the custom tape pool. Use the 'ListTapePools' operation to return a list of custom tape pools for your account and AWS Region.
    poolARN :: Core.Maybe Types.PoolARN,
    -- | The name of the custom tape pool. @PoolName@ can use all ASCII characters, except '/' and '\'.
    poolName :: Core.Maybe Types.PoolName,
    -- | Status of the custom tape pool. Pool can be @ACTIVE@ or @DELETED@ .
    poolStatus :: Core.Maybe Types.PoolStatus,
    -- | Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
    retentionLockTimeInDays :: Core.Maybe Core.Natural,
    -- | Tape retention lock type, which can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
    retentionLockType :: Core.Maybe Types.RetentionLockType,
    -- | The storage class that is associated with the custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
    storageClass :: Core.Maybe Types.TapeStorageClass
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'PoolInfo' value with any optional fields omitted.
mkPoolInfo ::
  PoolInfo
mkPoolInfo =
  PoolInfo'
    { poolARN = Core.Nothing,
      poolName = Core.Nothing,
      poolStatus = Core.Nothing,
      retentionLockTimeInDays = Core.Nothing,
      retentionLockType = Core.Nothing,
      storageClass = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the custom tape pool. Use the 'ListTapePools' operation to return a list of custom tape pools for your account and AWS Region.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piPoolARN :: Lens.Lens' PoolInfo (Core.Maybe Types.PoolARN)
piPoolARN = Lens.field @"poolARN"
{-# DEPRECATED piPoolARN "Use generic-lens or generic-optics with 'poolARN' instead." #-}

-- | The name of the custom tape pool. @PoolName@ can use all ASCII characters, except '/' and '\'.
--
-- /Note:/ Consider using 'poolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piPoolName :: Lens.Lens' PoolInfo (Core.Maybe Types.PoolName)
piPoolName = Lens.field @"poolName"
{-# DEPRECATED piPoolName "Use generic-lens or generic-optics with 'poolName' instead." #-}

-- | Status of the custom tape pool. Pool can be @ACTIVE@ or @DELETED@ .
--
-- /Note:/ Consider using 'poolStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piPoolStatus :: Lens.Lens' PoolInfo (Core.Maybe Types.PoolStatus)
piPoolStatus = Lens.field @"poolStatus"
{-# DEPRECATED piPoolStatus "Use generic-lens or generic-optics with 'poolStatus' instead." #-}

-- | Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
--
-- /Note:/ Consider using 'retentionLockTimeInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRetentionLockTimeInDays :: Lens.Lens' PoolInfo (Core.Maybe Core.Natural)
piRetentionLockTimeInDays = Lens.field @"retentionLockTimeInDays"
{-# DEPRECATED piRetentionLockTimeInDays "Use generic-lens or generic-optics with 'retentionLockTimeInDays' instead." #-}

-- | Tape retention lock type, which can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
--
-- /Note:/ Consider using 'retentionLockType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRetentionLockType :: Lens.Lens' PoolInfo (Core.Maybe Types.RetentionLockType)
piRetentionLockType = Lens.field @"retentionLockType"
{-# DEPRECATED piRetentionLockType "Use generic-lens or generic-optics with 'retentionLockType' instead." #-}

-- | The storage class that is associated with the custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piStorageClass :: Lens.Lens' PoolInfo (Core.Maybe Types.TapeStorageClass)
piStorageClass = Lens.field @"storageClass"
{-# DEPRECATED piStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

instance Core.FromJSON PoolInfo where
  parseJSON =
    Core.withObject "PoolInfo" Core.$
      \x ->
        PoolInfo'
          Core.<$> (x Core..:? "PoolARN")
          Core.<*> (x Core..:? "PoolName")
          Core.<*> (x Core..:? "PoolStatus")
          Core.<*> (x Core..:? "RetentionLockTimeInDays")
          Core.<*> (x Core..:? "RetentionLockType")
          Core.<*> (x Core..:? "StorageClass")
