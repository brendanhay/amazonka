{-# LANGUAGE DeriveDataTypeable #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.Types.Destination
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Network.AWS.S3.Types.Destination where

import Network.AWS.Lens
import Network.AWS.Prelude
import Network.AWS.S3.Internal
import Network.AWS.S3.Types.AccessControlTranslation
import Network.AWS.S3.Types.EncryptionConfiguration
import Network.AWS.S3.Types.Metrics
import Network.AWS.S3.Types.ReplicationTime
import Network.AWS.S3.Types.StorageClass

-- | Specifies information about where to publish analysis or configuration results for an Amazon S3 bucket and S3 Replication Time Control (S3 RTC).
--
--
--
-- /See:/ 'destination' smart constructor.
data Destination = Destination'
  { _dMetrics :: !(Maybe Metrics),
    _dAccessControlTranslation :: !(Maybe AccessControlTranslation),
    _dAccount :: !(Maybe Text),
    _dStorageClass :: !(Maybe StorageClass),
    _dEncryptionConfiguration :: !(Maybe EncryptionConfiguration),
    _dReplicationTime :: !(Maybe ReplicationTime),
    _dBucket :: !BucketName
  }
  deriving (Eq, Read, Show, Data, Typeable, Generic)

-- | Creates a value of 'Destination' with the minimum fields required to make a request.
--
-- Use one of the following lenses to modify other fields as desired:
--
-- * 'dMetrics' - A container specifying replication metrics-related settings enabling replication metrics and events.
--
-- * 'dAccessControlTranslation' - Specify this only in a cross-account scenario (where source and destination bucket owners are not the same), and you want to change replica ownership to the AWS account that owns the destination bucket. If this is not specified in the replication configuration, the replicas are owned by same AWS account that owns the source object.
--
-- * 'dAccount' - Destination bucket owner account ID. In a cross-account scenario, if you direct Amazon S3 to change replica ownership to the AWS account that owns the destination bucket by specifying the @AccessControlTranslation@ property, this is the account ID of the destination bucket owner. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner> in the /Amazon Simple Storage Service Developer Guide/ .
--
-- * 'dStorageClass' - The storage class to use when replicating objects, such as S3 Standard or reduced redundancy. By default, Amazon S3 uses the storage class of the source object to create the object replica.  For valid values, see the @StorageClass@ element of the <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication> action in the /Amazon Simple Storage Service API Reference/ .
--
-- * 'dEncryptionConfiguration' - A container that provides information about encryption. If @SourceSelectionCriteria@ is specified, you must specify this element.
--
-- * 'dReplicationTime' - A container specifying S3 Replication Time Control (S3 RTC), including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
--
-- * 'dBucket' - The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to store the results.
destination ::
  -- | 'dBucket'
  BucketName ->
  Destination
destination pBucket_ =
  Destination'
    { _dMetrics = Nothing,
      _dAccessControlTranslation = Nothing,
      _dAccount = Nothing,
      _dStorageClass = Nothing,
      _dEncryptionConfiguration = Nothing,
      _dReplicationTime = Nothing,
      _dBucket = pBucket_
    }

-- | A container specifying replication metrics-related settings enabling replication metrics and events.
dMetrics :: Lens' Destination (Maybe Metrics)
dMetrics = lens _dMetrics (\s a -> s {_dMetrics = a})

-- | Specify this only in a cross-account scenario (where source and destination bucket owners are not the same), and you want to change replica ownership to the AWS account that owns the destination bucket. If this is not specified in the replication configuration, the replicas are owned by same AWS account that owns the source object.
dAccessControlTranslation :: Lens' Destination (Maybe AccessControlTranslation)
dAccessControlTranslation = lens _dAccessControlTranslation (\s a -> s {_dAccessControlTranslation = a})

-- | Destination bucket owner account ID. In a cross-account scenario, if you direct Amazon S3 to change replica ownership to the AWS account that owns the destination bucket by specifying the @AccessControlTranslation@ property, this is the account ID of the destination bucket owner. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-change-owner.html Replication Additional Configuration: Changing the Replica Owner> in the /Amazon Simple Storage Service Developer Guide/ .
dAccount :: Lens' Destination (Maybe Text)
dAccount = lens _dAccount (\s a -> s {_dAccount = a})

-- | The storage class to use when replicating objects, such as S3 Standard or reduced redundancy. By default, Amazon S3 uses the storage class of the source object to create the object replica.  For valid values, see the @StorageClass@ element of the <https://docs.aws.amazon.com/AmazonS3/latest/API/RESTBucketPUTreplication.html PUT Bucket replication> action in the /Amazon Simple Storage Service API Reference/ .
dStorageClass :: Lens' Destination (Maybe StorageClass)
dStorageClass = lens _dStorageClass (\s a -> s {_dStorageClass = a})

-- | A container that provides information about encryption. If @SourceSelectionCriteria@ is specified, you must specify this element.
dEncryptionConfiguration :: Lens' Destination (Maybe EncryptionConfiguration)
dEncryptionConfiguration = lens _dEncryptionConfiguration (\s a -> s {_dEncryptionConfiguration = a})

-- | A container specifying S3 Replication Time Control (S3 RTC), including whether S3 RTC is enabled and the time when all objects and operations on objects must be replicated. Must be specified together with a @Metrics@ block.
dReplicationTime :: Lens' Destination (Maybe ReplicationTime)
dReplicationTime = lens _dReplicationTime (\s a -> s {_dReplicationTime = a})

-- | The Amazon Resource Name (ARN) of the bucket where you want Amazon S3 to store the results.
dBucket :: Lens' Destination BucketName
dBucket = lens _dBucket (\s a -> s {_dBucket = a})

instance FromXML Destination where
  parseXML x =
    Destination'
      <$> (x .@? "Metrics")
      <*> (x .@? "AccessControlTranslation")
      <*> (x .@? "Account")
      <*> (x .@? "StorageClass")
      <*> (x .@? "EncryptionConfiguration")
      <*> (x .@? "ReplicationTime")
      <*> (x .@ "Bucket")

instance Hashable Destination

instance NFData Destination

instance ToXML Destination where
  toXML Destination' {..} =
    mconcat
      [ "Metrics" @= _dMetrics,
        "AccessControlTranslation" @= _dAccessControlTranslation,
        "Account" @= _dAccount,
        "StorageClass" @= _dStorageClass,
        "EncryptionConfiguration" @= _dEncryptionConfiguration,
        "ReplicationTime" @= _dReplicationTime,
        "Bucket" @= _dBucket
      ]
