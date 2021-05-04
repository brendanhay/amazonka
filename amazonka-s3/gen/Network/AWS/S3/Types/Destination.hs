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
-- Module      : Network.AWS.S3.Types.Destination
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Destination where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AccessControlTranslation
import Network.AWS.S3.Types.EncryptionConfiguration
import Network.AWS.S3.Types.Metrics
import Network.AWS.S3.Types.ReplicationTime
import Network.AWS.S3.Types.StorageClass

-- | Specifies information about where to publish analysis or configuration
-- results for an Amazon S3 bucket and S3 Replication Time Control (S3
-- RTC).
--
-- /See:/ 'newDestination' smart constructor.
data Destination = Destination'
  { -- | A container that provides information about encryption. If
    -- @SourceSelectionCriteria@ is specified, you must specify this element.
    encryptionConfiguration :: Prelude.Maybe EncryptionConfiguration,
    -- | A container specifying S3 Replication Time Control (S3 RTC), including
    -- whether S3 RTC is enabled and the time when all objects and operations
    -- on objects must be replicated. Must be specified together with a
    -- @Metrics@ block.
    replicationTime :: Prelude.Maybe ReplicationTime,
    -- | Specify this only in a cross-account scenario (where source and
    -- destination bucket owners are not the same), and you want to change
    -- replica ownership to the AWS account that owns the destination bucket.
    -- If this is not specified in the replication configuration, the replicas
    -- are owned by same AWS account that owns the source object.
    accessControlTranslation :: Prelude.Maybe AccessControlTranslation,
    -- | The storage class to use when replicating objects, such as S3 Standard
    -- or reduced redundancy. By default, Amazon S3 uses the storage class of
    -- the source object to create the object replica.
    --
    -- For valid values, see the @StorageClass@ element of the
    -- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication>
    -- action in the /Amazon Simple Storage Service API Reference/.
    storageClass :: Prelude.Maybe StorageClass,
    -- | A container specifying replication metrics-related settings enabling
    -- replication metrics and events.
    metrics :: Prelude.Maybe Metrics,
    -- | Destination bucket owner account ID. In a cross-account scenario, if you
    -- direct Amazon S3 to change replica ownership to the AWS account that
    -- owns the destination bucket by specifying the @AccessControlTranslation@
    -- property, this is the account ID of the destination bucket owner. For
    -- more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    account :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to
    -- store the results.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'Destination' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'encryptionConfiguration', 'destination_encryptionConfiguration' - A container that provides information about encryption. If
-- @SourceSelectionCriteria@ is specified, you must specify this element.
--
-- 'replicationTime', 'destination_replicationTime' - A container specifying S3 Replication Time Control (S3 RTC), including
-- whether S3 RTC is enabled and the time when all objects and operations
-- on objects must be replicated. Must be specified together with a
-- @Metrics@ block.
--
-- 'accessControlTranslation', 'destination_accessControlTranslation' - Specify this only in a cross-account scenario (where source and
-- destination bucket owners are not the same), and you want to change
-- replica ownership to the AWS account that owns the destination bucket.
-- If this is not specified in the replication configuration, the replicas
-- are owned by same AWS account that owns the source object.
--
-- 'storageClass', 'destination_storageClass' - The storage class to use when replicating objects, such as S3 Standard
-- or reduced redundancy. By default, Amazon S3 uses the storage class of
-- the source object to create the object replica.
--
-- For valid values, see the @StorageClass@ element of the
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication>
-- action in the /Amazon Simple Storage Service API Reference/.
--
-- 'metrics', 'destination_metrics' - A container specifying replication metrics-related settings enabling
-- replication metrics and events.
--
-- 'account', 'destination_account' - Destination bucket owner account ID. In a cross-account scenario, if you
-- direct Amazon S3 to change replica ownership to the AWS account that
-- owns the destination bucket by specifying the @AccessControlTranslation@
-- property, this is the account ID of the destination bucket owner. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- 'bucket', 'destination_bucket' - The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to
-- store the results.
newDestination ::
  -- | 'bucket'
  BucketName ->
  Destination
newDestination pBucket_ =
  Destination'
    { encryptionConfiguration =
        Prelude.Nothing,
      replicationTime = Prelude.Nothing,
      accessControlTranslation = Prelude.Nothing,
      storageClass = Prelude.Nothing,
      metrics = Prelude.Nothing,
      account = Prelude.Nothing,
      bucket = pBucket_
    }

-- | A container that provides information about encryption. If
-- @SourceSelectionCriteria@ is specified, you must specify this element.
destination_encryptionConfiguration :: Lens.Lens' Destination (Prelude.Maybe EncryptionConfiguration)
destination_encryptionConfiguration = Lens.lens (\Destination' {encryptionConfiguration} -> encryptionConfiguration) (\s@Destination' {} a -> s {encryptionConfiguration = a} :: Destination)

-- | A container specifying S3 Replication Time Control (S3 RTC), including
-- whether S3 RTC is enabled and the time when all objects and operations
-- on objects must be replicated. Must be specified together with a
-- @Metrics@ block.
destination_replicationTime :: Lens.Lens' Destination (Prelude.Maybe ReplicationTime)
destination_replicationTime = Lens.lens (\Destination' {replicationTime} -> replicationTime) (\s@Destination' {} a -> s {replicationTime = a} :: Destination)

-- | Specify this only in a cross-account scenario (where source and
-- destination bucket owners are not the same), and you want to change
-- replica ownership to the AWS account that owns the destination bucket.
-- If this is not specified in the replication configuration, the replicas
-- are owned by same AWS account that owns the source object.
destination_accessControlTranslation :: Lens.Lens' Destination (Prelude.Maybe AccessControlTranslation)
destination_accessControlTranslation = Lens.lens (\Destination' {accessControlTranslation} -> accessControlTranslation) (\s@Destination' {} a -> s {accessControlTranslation = a} :: Destination)

-- | The storage class to use when replicating objects, such as S3 Standard
-- or reduced redundancy. By default, Amazon S3 uses the storage class of
-- the source object to create the object replica.
--
-- For valid values, see the @StorageClass@ element of the
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication>
-- action in the /Amazon Simple Storage Service API Reference/.
destination_storageClass :: Lens.Lens' Destination (Prelude.Maybe StorageClass)
destination_storageClass = Lens.lens (\Destination' {storageClass} -> storageClass) (\s@Destination' {} a -> s {storageClass = a} :: Destination)

-- | A container specifying replication metrics-related settings enabling
-- replication metrics and events.
destination_metrics :: Lens.Lens' Destination (Prelude.Maybe Metrics)
destination_metrics = Lens.lens (\Destination' {metrics} -> metrics) (\s@Destination' {} a -> s {metrics = a} :: Destination)

-- | Destination bucket owner account ID. In a cross-account scenario, if you
-- direct Amazon S3 to change replica ownership to the AWS account that
-- owns the destination bucket by specifying the @AccessControlTranslation@
-- property, this is the account ID of the destination bucket owner. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner>
-- in the /Amazon Simple Storage Service Developer Guide/.
destination_account :: Lens.Lens' Destination (Prelude.Maybe Prelude.Text)
destination_account = Lens.lens (\Destination' {account} -> account) (\s@Destination' {} a -> s {account = a} :: Destination)

-- | The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to
-- store the results.
destination_bucket :: Lens.Lens' Destination BucketName
destination_bucket = Lens.lens (\Destination' {bucket} -> bucket) (\s@Destination' {} a -> s {bucket = a} :: Destination)

instance Prelude.FromXML Destination where
  parseXML x =
    Destination'
      Prelude.<$> (x Prelude..@? "EncryptionConfiguration")
      Prelude.<*> (x Prelude..@? "ReplicationTime")
      Prelude.<*> (x Prelude..@? "AccessControlTranslation")
      Prelude.<*> (x Prelude..@? "StorageClass")
      Prelude.<*> (x Prelude..@? "Metrics")
      Prelude.<*> (x Prelude..@? "Account")
      Prelude.<*> (x Prelude..@ "Bucket")

instance Prelude.Hashable Destination

instance Prelude.NFData Destination

instance Prelude.ToXML Destination where
  toXML Destination' {..} =
    Prelude.mconcat
      [ "EncryptionConfiguration"
          Prelude.@= encryptionConfiguration,
        "ReplicationTime" Prelude.@= replicationTime,
        "AccessControlTranslation"
          Prelude.@= accessControlTranslation,
        "StorageClass" Prelude.@= storageClass,
        "Metrics" Prelude.@= metrics,
        "Account" Prelude.@= account,
        "Bucket" Prelude.@= bucket
      ]
