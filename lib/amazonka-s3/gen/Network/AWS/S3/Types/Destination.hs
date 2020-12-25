{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

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
    dBucket,
    dAccessControlTranslation,
    dAccount,
    dEncryptionConfiguration,
    dMetrics,
    dReplicationTime,
    dStorageClass,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Core
import qualified Network.AWS.S3.Internal as Types
import qualified Network.AWS.S3.Types.AccessControlTranslation as Types
import qualified Network.AWS.S3.Types.Account as Types
import qualified Network.AWS.S3.Types.EncryptionConfiguration as Types
import qualified Network.AWS.S3.Types.Metrics as Types
import qualified Network.AWS.S3.Types.ReplicationTime as Types
import qualified Network.AWS.S3.Types.StorageClass as Types

-- | Specifies information about where to publish analysis or configuration results for an Amazon S3 bucket and S3 Replication Time Control (S3 RTC).
--
-- /See:/ 'mkDestination' smart constructor.
data Destination = Destination'
  { -- | The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to store the results.
    bucket :: Types.BucketName,
    -- | Specify this only in a cross-account scenario (where source and destination bucket owners are not the same), and you want to change replica ownership to the AWS account that owns the destination bucket. If this is not specified in the replication configuration, the replicas are owned by same AWS account that owns the source object.
    accessControlTranslation :: Core.Maybe Types.AccessControlTranslation,
    -- | Destination bucket owner account ID. In a cross-account scenario, if you direct Amazon S3 to change replica ownership to the AWS account that owns the destination bucket by specifying the @AccessControlTranslation@ property, this is the account ID of the destination bucket owner. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner> in the /Amazon Simple Storage Service Developer Guide/ .
    account :: Core.Maybe Types.Account,
    -- | A container that provides information about encryption. If @SourceSelectionCriteria@ is specified, you must specify this element.
    encryptionConfiguration :: Core.Maybe Types.EncryptionConfiguration,
    -- | A container specifying replication metrics-related settings enabling replication metrics and events.
    metrics :: Core.Maybe Types.Metrics,
    -- | A container specifying S3 Replication Time Control (S3 RTC), including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
    replicationTime :: Core.Maybe Types.ReplicationTime,
    -- | The storage class to use when replicating objects, such as S3 Standard or reduced redundancy. By default, Amazon S3 uses the storage class of the source object to create the object replica.
    --
    -- For valid values, see the @StorageClass@ element of the <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication> action in the /Amazon Simple Storage Service API Reference/ .
    storageClass :: Core.Maybe Types.StorageClass
  }
  deriving stock (Core.Eq, Core.Ord, Core.Read, Core.Show, Core.Generic)
  deriving anyclass (Core.Hashable, Core.NFData)

-- | Creates a 'Destination' value with any optional fields omitted.
mkDestination ::
  -- | 'bucket'
  Types.BucketName ->
  Destination
mkDestination bucket =
  Destination'
    { bucket,
      accessControlTranslation = Core.Nothing,
      account = Core.Nothing,
      encryptionConfiguration = Core.Nothing,
      metrics = Core.Nothing,
      replicationTime = Core.Nothing,
      storageClass = Core.Nothing
    }

-- | The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to store the results.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dBucket :: Lens.Lens' Destination Types.BucketName
dBucket = Lens.field @"bucket"
{-# DEPRECATED dBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Specify this only in a cross-account scenario (where source and destination bucket owners are not the same), and you want to change replica ownership to the AWS account that owns the destination bucket. If this is not specified in the replication configuration, the replicas are owned by same AWS account that owns the source object.
--
-- /Note:/ Consider using 'accessControlTranslation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccessControlTranslation :: Lens.Lens' Destination (Core.Maybe Types.AccessControlTranslation)
dAccessControlTranslation = Lens.field @"accessControlTranslation"
{-# DEPRECATED dAccessControlTranslation "Use generic-lens or generic-optics with 'accessControlTranslation' instead." #-}

-- | Destination bucket owner account ID. In a cross-account scenario, if you direct Amazon S3 to change replica ownership to the AWS account that owns the destination bucket by specifying the @AccessControlTranslation@ property, this is the account ID of the destination bucket owner. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- /Note:/ Consider using 'account' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dAccount :: Lens.Lens' Destination (Core.Maybe Types.Account)
dAccount = Lens.field @"account"
{-# DEPRECATED dAccount "Use generic-lens or generic-optics with 'account' instead." #-}

-- | A container that provides information about encryption. If @SourceSelectionCriteria@ is specified, you must specify this element.
--
-- /Note:/ Consider using 'encryptionConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dEncryptionConfiguration :: Lens.Lens' Destination (Core.Maybe Types.EncryptionConfiguration)
dEncryptionConfiguration = Lens.field @"encryptionConfiguration"
{-# DEPRECATED dEncryptionConfiguration "Use generic-lens or generic-optics with 'encryptionConfiguration' instead." #-}

-- | A container specifying replication metrics-related settings enabling replication metrics and events.
--
-- /Note:/ Consider using 'metrics' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dMetrics :: Lens.Lens' Destination (Core.Maybe Types.Metrics)
dMetrics = Lens.field @"metrics"
{-# DEPRECATED dMetrics "Use generic-lens or generic-optics with 'metrics' instead." #-}

-- | A container specifying S3 Replication Time Control (S3 RTC), including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
--
-- /Note:/ Consider using 'replicationTime' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dReplicationTime :: Lens.Lens' Destination (Core.Maybe Types.ReplicationTime)
dReplicationTime = Lens.field @"replicationTime"
{-# DEPRECATED dReplicationTime "Use generic-lens or generic-optics with 'replicationTime' instead." #-}

-- | The storage class to use when replicating objects, such as S3 Standard or reduced redundancy. By default, Amazon S3 uses the storage class of the source object to create the object replica.
--
-- For valid values, see the @StorageClass@ element of the <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication> action in the /Amazon Simple Storage Service API Reference/ .
--
-- /Note:/ Consider using 'storageClass' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dStorageClass :: Lens.Lens' Destination (Core.Maybe Types.StorageClass)
dStorageClass = Lens.field @"storageClass"
{-# DEPRECATED dStorageClass "Use generic-lens or generic-optics with 'storageClass' instead." #-}

instance Core.ToXML Destination where
  toXML Destination {..} =
    Core.toXMLNode "Bucket" bucket
      Core.<> Core.toXMLNode "AccessControlTranslation"
      Core.<$> accessControlTranslation
      Core.<> Core.toXMLNode "Account"
      Core.<$> account
      Core.<> Core.toXMLNode "EncryptionConfiguration"
      Core.<$> encryptionConfiguration
      Core.<> Core.toXMLNode "Metrics"
      Core.<$> metrics
      Core.<> Core.toXMLNode "ReplicationTime"
      Core.<$> replicationTime
      Core.<> Core.toXMLNode "StorageClass"
      Core.<$> storageClass

instance Core.FromXML Destination where
  parseXML x =
    Destination'
      Core.<$> (x Core..@ "Bucket")
      Core.<*> (x Core..@? "AccessControlTranslation")
      Core.<*> (x Core..@? "Account")
      Core.<*> (x Core..@? "EncryptionConfiguration")
      Core.<*> (x Core..@? "Metrics")
      Core.<*> (x Core..@? "ReplicationTime")
      Core.<*> (x Core..@? "StorageClass")
