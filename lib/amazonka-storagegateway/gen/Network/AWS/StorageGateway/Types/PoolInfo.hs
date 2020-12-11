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
    piRetentionLockType,
    piRetentionLockTimeInDays,
    piPoolName,
    piStorageClass,
    piPoolStatus,
    piPoolARN,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.StorageGateway.Types.PoolStatus
import Network.AWS.StorageGateway.Types.RetentionLockType
import Network.AWS.StorageGateway.Types.TapeStorageClass

-- | Describes a custom tape pool.
--
-- /See:/ 'mkPoolInfo' smart constructor.
data PoolInfo = PoolInfo'
  { retentionLockType ::
      Lude.Maybe RetentionLockType,
    retentionLockTimeInDays :: Lude.Maybe Lude.Natural,
    poolName :: Lude.Maybe Lude.Text,
    storageClass :: Lude.Maybe TapeStorageClass,
    poolStatus :: Lude.Maybe PoolStatus,
    poolARN :: Lude.Maybe Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PoolInfo' with the minimum fields required to make a request.
--
-- * 'poolARN' - The Amazon Resource Name (ARN) of the custom tape pool. Use the 'ListTapePools' operation to return a list of custom tape pools for your account and AWS Region.
-- * 'poolName' - The name of the custom tape pool. @PoolName@ can use all ASCII characters, except '/' and '\'.
-- * 'poolStatus' - Status of the custom tape pool. Pool can be @ACTIVE@ or @DELETED@ .
-- * 'retentionLockTimeInDays' - Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
-- * 'retentionLockType' - Tape retention lock type, which can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
-- * 'storageClass' - The storage class that is associated with the custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
mkPoolInfo ::
  PoolInfo
mkPoolInfo =
  PoolInfo'
    { retentionLockType = Lude.Nothing,
      retentionLockTimeInDays = Lude.Nothing,
      poolName = Lude.Nothing,
      storageClass = Lude.Nothing,
      poolStatus = Lude.Nothing,
      poolARN = Lude.Nothing
    }

-- | Tape retention lock type, which can be configured in two modes. When configured in governance mode, AWS accounts with specific IAM permissions are authorized to remove the tape retention lock from archived virtual tapes. When configured in compliance mode, the tape retention lock cannot be removed by any user, including the root AWS account.
--
-- /Note:/ Consider using 'retentionLockType' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRetentionLockType :: Lens.Lens' PoolInfo (Lude.Maybe RetentionLockType)
piRetentionLockType = Lens.lens (retentionLockType :: PoolInfo -> Lude.Maybe RetentionLockType) (\s a -> s {retentionLockType = a} :: PoolInfo)
{-# DEPRECATED piRetentionLockType "Use generic-lens or generic-optics with 'retentionLockType' instead." #-}

-- | Tape retention lock time is set in days. Tape retention lock can be enabled for up to 100 years (36,500 days).
--
-- /Note:/ Consider using 'retentionLockTimeInDays' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piRetentionLockTimeInDays :: Lens.Lens' PoolInfo (Lude.Maybe Lude.Natural)
piRetentionLockTimeInDays = Lens.lens (retentionLockTimeInDays :: PoolInfo -> Lude.Maybe Lude.Natural) (\s a -> s {retentionLockTimeInDays = a} :: PoolInfo)
{-# DEPRECATED piRetentionLockTimeInDays "Use generic-lens or generic-optics with 'retentionLockTimeInDays' instead." #-}

-- | The name of the custom tape pool. @PoolName@ can use all ASCII characters, except '/' and '\'.
--
-- /Note:/ Consider using 'poolName' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piPoolName :: Lens.Lens' PoolInfo (Lude.Maybe Lude.Text)
piPoolName = Lens.lens (poolName :: PoolInfo -> Lude.Maybe Lude.Text) (\s a -> s {poolName = a} :: PoolInfo)
{-# DEPRECATED piPoolName "Use generic-lens or generic-optics with 'poolName' instead." #-}

-- | The storage class that is associated with the custom pool. When you use your backup application to eject the tape, the tape is archived directly into the storage class (S3 Glacier or S3 Glacier Deep Archive) that corresponds to the pool.
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piStorageClass :: Lens.Lens' PoolInfo (Lude.Maybe TapeStorageClass)
piStorageClass = Lens.lens (storageClass :: PoolInfo -> Lude.Maybe TapeStorageClass) (\s a -> s {storageClass = a} :: PoolInfo)
{-# DEPRECATED piStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | Status of the custom tape pool. Pool can be @ACTIVE@ or @DELETED@ .
--
-- /Note:/ Consider using 'poolStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piPoolStatus :: Lens.Lens' PoolInfo (Lude.Maybe PoolStatus)
piPoolStatus = Lens.lens (poolStatus :: PoolInfo -> Lude.Maybe PoolStatus) (\s a -> s {poolStatus = a} :: PoolInfo)
{-# DEPRECATED piPoolStatus "Use generic-lens or generic-optics with 'poolStatus' instead." #-}

-- | The Amazon Resource Name (ARN) of the custom tape pool. Use the 'ListTapePools' operation to return a list of custom tape pools for your account and AWS Region.
--
-- /Note:/ Consider using 'poolARN' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
piPoolARN :: Lens.Lens' PoolInfo (Lude.Maybe Lude.Text)
piPoolARN = Lens.lens (poolARN :: PoolInfo -> Lude.Maybe Lude.Text) (\s a -> s {poolARN = a} :: PoolInfo)
{-# DEPRECATED piPoolARN "Use generic-lens or generic-optics with 'poolARN' instead." #-}

instance Lude.FromJSON PoolInfo where
  parseJSON =
    Lude.withObject
      "PoolInfo"
      ( \x ->
          PoolInfo'
            Lude.<$> (x Lude..:? "RetentionLockType")
            Lude.<*> (x Lude..:? "RetentionLockTimeInDays")
            Lude.<*> (x Lude..:? "PoolName")
            Lude.<*> (x Lude..:? "StorageClass")
            Lude.<*> (x Lude..:? "PoolStatus")
            Lude.<*> (x Lude..:? "PoolARN")
      )
