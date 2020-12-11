-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Destination
  ( Destination (..),

    -- * Smart constructor
    mkDestination,

    -- * Lenses
    dMetrics,
    dAccessControlTranslation,
    dAccount,
    dStorageClass,
    dEncryptionConfiguration,
    dReplicationTime,
    dBucket,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AccessControlTranslation
import Network.AWS.S3.Types.EncryptionConfiguration
import Network.AWS.S3.Types.Metrics
import Network.AWS.S3.Types.ReplicationTime
import Network.AWS.S3.Types.StorageClass

-- | Specifies information about where to publish analysis or configuration results for an Amazon S3 bucket and S3 Replication Time Control (S3 RTC).
--
-- /See:/ 'mkDestination' smart constructor.
data Destination = Destination'
  { metrics :: Lude.Maybe Metrics,
    accessControlTranslation :: Lude.Maybe AccessControlTranslation,
    account :: Lude.Maybe Lude.Text,
    storageClass :: Lude.Maybe StorageClass,
    encryptionConfiguration :: Lude.Maybe EncryptionConfiguration,
    replicationTime :: Lude.Maybe ReplicationTime,
    bucket :: BucketName
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- * 'accessControlTranslation' - Specify this only in a cross-account scenario (where source and destination bucket owners are not the same), and you want to change replica ownership to the AWS account that owns the destination bucket. If this is not specified in the replication configuration, the replicas are owned by same AWS account that owns the source object.
-- * 'account' - Destination bucket owner account ID. In a cross-account scenario, if you direct Amazon S3 to change replica ownership to the AWS account that owns the destination bucket by specifying the @AccessControlTranslation@ property, this is the account ID of the destination bucket owner. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner> in the /Amazon Simple Storage Service Developer Guide/ .
-- * 'bucket' - The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to store the results.
-- * 'encryptionConfiguration' - A container that provides information about encryption. If @SourceSelectionCriteria@ is specified, you must specify this element.
-- * 'metrics' - A container specifying replication metrics-related settings enabling replication metrics and events.
-- * 'replicationTime' - A container specifying S3 Replication Time Control (S3 RTC), including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
-- * 'storageClass' - The storage class to use when replicating objects, such as S3 Standard or reduced redundancy. By default, Amazon S3 uses the storage class of the source object to create the object replica.
--
-- For valid values, see the @StorageClass@ element of the <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication> action in the /Amazon Simple Storage Service API Reference/ .
mkDestination ::
  -- | 'bucket'
  BucketName ->
  Destination
mkDestination pBucket_ =
  Destination'
    { metrics = Lude.Nothing,
      accessControlTranslation = Lude.Nothing,
      account = Lude.Nothing,
      storageClass = Lude.Nothing,
      encryptionConfiguration = Lude.Nothing,
      replicationTime = Lude.Nothing,
      bucket = pBucket_
    }

-- | A container specifying replication metrics-related settings enabling replication metrics and events.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMetrics :: Lens.Lens' Destination (Lude.Maybe Metrics)
dMetrics = Lens.lens (metrics :: Destination -> Lude.Maybe Metrics) (\s a -> s {metrics = a} :: Destination)
{-# DEPRECATED dMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | Specify this only in a cross-account scenario (where source and destination bucket owners are not the same), and you want to change replica ownership to the AWS account that owns the destination bucket. If this is not specified in the replication configuration, the replicas are owned by same AWS account that owns the source object.
--
-- /Note:/ Consider using 'accessControlTranslation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccessControlTranslation :: Lens.Lens' Destination (Lude.Maybe AccessControlTranslation)
dAccessControlTranslation = Lens.lens (accessControlTranslation :: Destination -> Lude.Maybe AccessControlTranslation) (\s a -> s {accessControlTranslation = a} :: Destination)
{-# DEPRECATED dAccessControlTranslation "Use generic-lens or generic-optics with 'accessControlTranslation' instead." #-}

-- | Destination bucket owner account ID. In a cross-account scenario, if you direct Amazon S3 to change replica ownership to the AWS account that owns the destination bucket by specifying the @AccessControlTranslation@ property, this is the account ID of the destination bucket owner. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccount :: Lens.Lens' Destination (Lude.Maybe Lude.Text)
dAccount = Lens.lens (account :: Destination -> Lude.Maybe Lude.Text) (\s a -> s {account = a} :: Destination)
{-# DEPRECATED dAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | The storage class to use when replicating objects, such as S3 Standard or reduced redundancy. By default, Amazon S3 uses the storage class of the source object to create the object replica.
--
-- For valid values, see the @StorageClass@ element of the <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication> action in the /Amazon Simple Storage Service API Reference/ .
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStorageClass :: Lens.Lens' Destination (Lude.Maybe StorageClass)
dStorageClass = Lens.lens (storageClass :: Destination -> Lude.Maybe StorageClass) (\s a -> s {storageClass = a} :: Destination)
{-# DEPRECATED dStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

-- | A container that provides information about encryption. If @SourceSelectionCriteria@ is specified, you must specify this element.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEncryptionConfiguration :: Lens.Lens' Destination (Lude.Maybe EncryptionConfiguration)
dEncryptionConfiguration = Lens.lens (encryptionConfiguration :: Destination -> Lude.Maybe EncryptionConfiguration) (\s a -> s {encryptionConfiguration = a} :: Destination)
{-# DEPRECATED dEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | A container specifying S3 Replication Time Control (S3 RTC), including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
--
-- /Note:/ Consider using 'replicationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dReplicationTime :: Lens.Lens' Destination (Lude.Maybe ReplicationTime)
dReplicationTime = Lens.lens (replicationTime :: Destination -> Lude.Maybe ReplicationTime) (\s a -> s {replicationTime = a} :: Destination)
{-# DEPRECATED dReplicationTime "Use generic-lens or generic-optics with 'replicationTime' instead." #-}

-- | The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to store the results.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBucket :: Lens.Lens' Destination BucketName
dBucket = Lens.lens (bucket :: Destination -> BucketName) (\s a -> s {bucket = a} :: Destination)
{-# DEPRECATED dBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

instance Lude.FromXML Destination where
  parseXML x =
    Destination'
      Lude.<$> (x Lude..@? "Metrics")
      Lude.<*> (x Lude..@? "AccessControlTranslation")
      Lude.<*> (x Lude..@? "Account")
      Lude.<*> (x Lude..@? "StorageClass")
      Lude.<*> (x Lude..@? "EncryptionConfiguration")
      Lude.<*> (x Lude..@? "ReplicationTime")
      Lude.<*> (x Lude..@ "Bucket")

instance Lude.ToXML Destination where
  toXML Destination' {..} =
    Lude.mconcat
      [ "Metrics" Lude.@= metrics,
        "AccessControlTranslation" Lude.@= accessControlTranslation,
        "Account" Lude.@= account,
        "StorageClass" Lude.@= storageClass,
        "EncryptionConfiguration" Lude.@= encryptionConfiguration,
        "ReplicationTime" Lude.@= replicationTime,
        "Bucket" Lude.@= bucket
      ]
