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
-- Module      : Amazonka.MacieV2.Types.BucketMetadata
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.BucketMetadata where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.AllowsUnencryptedObjectUploads
import Amazonka.MacieV2.Types.BucketMetadataErrorCode
import Amazonka.MacieV2.Types.BucketPublicAccess
import Amazonka.MacieV2.Types.BucketServerSideEncryption
import Amazonka.MacieV2.Types.JobDetails
import Amazonka.MacieV2.Types.KeyValuePair
import Amazonka.MacieV2.Types.ObjectCountByEncryptionType
import Amazonka.MacieV2.Types.ObjectLevelStatistics
import Amazonka.MacieV2.Types.ReplicationDetails
import Amazonka.MacieV2.Types.SharedAccess
import qualified Amazonka.Prelude as Prelude

-- | Provides statistical data and other information about an S3 bucket that
-- Amazon Macie monitors and analyzes for your account. If an error occurs
-- when Macie attempts to retrieve and process metadata from Amazon S3 for
-- the bucket and the bucket\'s objects, the value for the versioning
-- property is false and the value for most other properties is null. Key
-- exceptions are accountId, bucketArn, bucketCreatedAt, bucketName,
-- lastUpdated, and region. To identify the cause of the error, refer to
-- the errorCode and errorMessage values.
--
-- /See:/ 'newBucketMetadata' smart constructor.
data BucketMetadata = BucketMetadata'
  { -- | The unique identifier for the Amazon Web Services account that owns the
    -- bucket.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the bucket policy for the bucket requires server-side
    -- encryption of objects when objects are uploaded to the bucket. Possible
    -- values are:
    --
    -- -   FALSE - The bucket policy requires server-side encryption of new
    --     objects. PutObject requests must include a valid server-side
    --     encryption header.
    --
    -- -   TRUE - The bucket doesn\'t have a bucket policy or it has a bucket
    --     policy that doesn\'t require server-side encryption of new objects.
    --     If a bucket policy exists, it doesn\'t require PutObject requests to
    --     include a valid server-side encryption header.
    --
    -- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket policy
    --     requires server-side encryption of new objects.
    --
    -- Valid server-side encryption headers are: x-amz-server-side-encryption
    -- with a value of AES256 or aws:kms, and
    -- x-amz-server-side-encryption-customer-algorithm with a value of AES256.
    allowsUnencryptedObjectUploads :: Prelude.Maybe AllowsUnencryptedObjectUploads,
    -- | The Amazon Resource Name (ARN) of the bucket.
    bucketArn :: Prelude.Maybe Prelude.Text,
    -- | The date and time, in UTC and extended ISO 8601 format, when the bucket
    -- was created, or changes such as edits to the bucket\'s policy were most
    -- recently made to the bucket.
    bucketCreatedAt :: Prelude.Maybe Data.POSIX,
    -- | The name of the bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The total number of objects that Amazon Macie can analyze in the bucket.
    -- These objects use a supported storage class and have a file name
    -- extension for a supported file or storage format.
    classifiableObjectCount :: Prelude.Maybe Prelude.Integer,
    -- | The total storage size, in bytes, of the objects that Amazon Macie can
    -- analyze in the bucket. These objects use a supported storage class and
    -- have a file name extension for a supported file or storage format.
    --
    -- If versioning is enabled for the bucket, Macie calculates this value
    -- based on the size of the latest version of each applicable object in the
    -- bucket. This value doesn\'t reflect the storage size of all versions of
    -- each applicable object in the bucket.
    classifiableSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the error code for an error that prevented Amazon Macie from
    -- retrieving and processing information about the bucket and the bucket\'s
    -- objects. If this value is ACCESS_DENIED, Macie doesn\'t have permission
    -- to retrieve the information. For example, the bucket has a restrictive
    -- bucket policy and Amazon S3 denied the request. If this value is null,
    -- Macie was able to retrieve and process the information.
    errorCode :: Prelude.Maybe BucketMetadataErrorCode,
    -- | A brief description of the error (errorCode) that prevented Amazon Macie
    -- from retrieving and processing information about the bucket and the
    -- bucket\'s objects. This value is null if Macie was able to retrieve and
    -- process the information.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether any one-time or recurring classification jobs are
    -- configured to analyze data in the bucket, and, if so, the details of the
    -- job that ran most recently.
    jobDetails :: Prelude.Maybe JobDetails,
    -- | The date and time, in UTC and extended ISO 8601 format, when Amazon
    -- Macie most recently performed automated sensitive data discovery for the
    -- bucket. This value is null if automated sensitive data discovery is
    -- currently disabled for your account.
    lastAutomatedDiscoveryTime :: Prelude.Maybe Data.POSIX,
    -- | The date and time, in UTC and extended ISO 8601 format, when Amazon
    -- Macie most recently retrieved both bucket and object metadata from
    -- Amazon S3 for the bucket.
    lastUpdated :: Prelude.Maybe Data.POSIX,
    -- | The total number of objects in the bucket.
    objectCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of objects that are in the bucket, grouped by
    -- server-side encryption type. This includes a grouping that reports the
    -- total number of objects that aren\'t encrypted or use client-side
    -- encryption.
    objectCountByEncryptionType :: Prelude.Maybe ObjectCountByEncryptionType,
    -- | Specifies whether the bucket is publicly accessible due to the
    -- combination of permissions settings that apply to the bucket, and
    -- provides information about those settings.
    publicAccess :: Prelude.Maybe BucketPublicAccess,
    -- | The Amazon Web Services Region that hosts the bucket.
    region :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether the bucket is configured to replicate one or more
    -- objects to buckets for other Amazon Web Services accounts and, if so,
    -- which accounts.
    replicationDetails :: Prelude.Maybe ReplicationDetails,
    -- | The sensitivity score for the bucket, ranging from -1 (no analysis due
    -- to an error) to 100 (sensitive). This value is null if automated
    -- sensitive data discovery is currently disabled for your account.
    sensitivityScore :: Prelude.Maybe Prelude.Int,
    -- | Specifies whether the bucket encrypts new objects by default and, if so,
    -- the type of server-side encryption that\'s used.
    serverSideEncryption :: Prelude.Maybe BucketServerSideEncryption,
    -- | Specifies whether the bucket is shared with another Amazon Web Services
    -- account. Possible values are:
    --
    -- -   EXTERNAL - The bucket is shared with an Amazon Web Services account
    --     that isn\'t part of the same Amazon Macie organization.
    --
    -- -   INTERNAL - The bucket is shared with an Amazon Web Services account
    --     that\'s part of the same Amazon Macie organization.
    --
    -- -   NOT_SHARED - The bucket isn\'t shared with other Amazon Web Services
    --     accounts.
    --
    -- -   UNKNOWN - Amazon Macie wasn\'t able to evaluate the shared access
    --     settings for the bucket.
    sharedAccess :: Prelude.Maybe SharedAccess,
    -- | The total storage size, in bytes, of the bucket.
    --
    -- If versioning is enabled for the bucket, Amazon Macie calculates this
    -- value based on the size of the latest version of each object in the
    -- bucket. This value doesn\'t reflect the storage size of all versions of
    -- each object in the bucket.
    sizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The total storage size, in bytes, of the objects that are compressed
    -- (.gz, .gzip, .zip) files in the bucket.
    --
    -- If versioning is enabled for the bucket, Amazon Macie calculates this
    -- value based on the size of the latest version of each applicable object
    -- in the bucket. This value doesn\'t reflect the storage size of all
    -- versions of each applicable object in the bucket.
    sizeInBytesCompressed :: Prelude.Maybe Prelude.Integer,
    -- | An array that specifies the tags (keys and values) that are associated
    -- with the bucket.
    tags :: Prelude.Maybe [KeyValuePair],
    -- | The total number of objects that Amazon Macie can\'t analyze in the
    -- bucket. These objects don\'t use a supported storage class or don\'t
    -- have a file name extension for a supported file or storage format.
    unclassifiableObjectCount :: Prelude.Maybe ObjectLevelStatistics,
    -- | The total storage size, in bytes, of the objects that Amazon Macie
    -- can\'t analyze in the bucket. These objects don\'t use a supported
    -- storage class or don\'t have a file name extension for a supported file
    -- or storage format.
    unclassifiableObjectSizeInBytes :: Prelude.Maybe ObjectLevelStatistics,
    -- | Specifies whether versioning is enabled for the bucket.
    versioning :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'BucketMetadata' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'bucketMetadata_accountId' - The unique identifier for the Amazon Web Services account that owns the
-- bucket.
--
-- 'allowsUnencryptedObjectUploads', 'bucketMetadata_allowsUnencryptedObjectUploads' - Specifies whether the bucket policy for the bucket requires server-side
-- encryption of objects when objects are uploaded to the bucket. Possible
-- values are:
--
-- -   FALSE - The bucket policy requires server-side encryption of new
--     objects. PutObject requests must include a valid server-side
--     encryption header.
--
-- -   TRUE - The bucket doesn\'t have a bucket policy or it has a bucket
--     policy that doesn\'t require server-side encryption of new objects.
--     If a bucket policy exists, it doesn\'t require PutObject requests to
--     include a valid server-side encryption header.
--
-- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket policy
--     requires server-side encryption of new objects.
--
-- Valid server-side encryption headers are: x-amz-server-side-encryption
-- with a value of AES256 or aws:kms, and
-- x-amz-server-side-encryption-customer-algorithm with a value of AES256.
--
-- 'bucketArn', 'bucketMetadata_bucketArn' - The Amazon Resource Name (ARN) of the bucket.
--
-- 'bucketCreatedAt', 'bucketMetadata_bucketCreatedAt' - The date and time, in UTC and extended ISO 8601 format, when the bucket
-- was created, or changes such as edits to the bucket\'s policy were most
-- recently made to the bucket.
--
-- 'bucketName', 'bucketMetadata_bucketName' - The name of the bucket.
--
-- 'classifiableObjectCount', 'bucketMetadata_classifiableObjectCount' - The total number of objects that Amazon Macie can analyze in the bucket.
-- These objects use a supported storage class and have a file name
-- extension for a supported file or storage format.
--
-- 'classifiableSizeInBytes', 'bucketMetadata_classifiableSizeInBytes' - The total storage size, in bytes, of the objects that Amazon Macie can
-- analyze in the bucket. These objects use a supported storage class and
-- have a file name extension for a supported file or storage format.
--
-- If versioning is enabled for the bucket, Macie calculates this value
-- based on the size of the latest version of each applicable object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each applicable object in the bucket.
--
-- 'errorCode', 'bucketMetadata_errorCode' - Specifies the error code for an error that prevented Amazon Macie from
-- retrieving and processing information about the bucket and the bucket\'s
-- objects. If this value is ACCESS_DENIED, Macie doesn\'t have permission
-- to retrieve the information. For example, the bucket has a restrictive
-- bucket policy and Amazon S3 denied the request. If this value is null,
-- Macie was able to retrieve and process the information.
--
-- 'errorMessage', 'bucketMetadata_errorMessage' - A brief description of the error (errorCode) that prevented Amazon Macie
-- from retrieving and processing information about the bucket and the
-- bucket\'s objects. This value is null if Macie was able to retrieve and
-- process the information.
--
-- 'jobDetails', 'bucketMetadata_jobDetails' - Specifies whether any one-time or recurring classification jobs are
-- configured to analyze data in the bucket, and, if so, the details of the
-- job that ran most recently.
--
-- 'lastAutomatedDiscoveryTime', 'bucketMetadata_lastAutomatedDiscoveryTime' - The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently performed automated sensitive data discovery for the
-- bucket. This value is null if automated sensitive data discovery is
-- currently disabled for your account.
--
-- 'lastUpdated', 'bucketMetadata_lastUpdated' - The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently retrieved both bucket and object metadata from
-- Amazon S3 for the bucket.
--
-- 'objectCount', 'bucketMetadata_objectCount' - The total number of objects in the bucket.
--
-- 'objectCountByEncryptionType', 'bucketMetadata_objectCountByEncryptionType' - The total number of objects that are in the bucket, grouped by
-- server-side encryption type. This includes a grouping that reports the
-- total number of objects that aren\'t encrypted or use client-side
-- encryption.
--
-- 'publicAccess', 'bucketMetadata_publicAccess' - Specifies whether the bucket is publicly accessible due to the
-- combination of permissions settings that apply to the bucket, and
-- provides information about those settings.
--
-- 'region', 'bucketMetadata_region' - The Amazon Web Services Region that hosts the bucket.
--
-- 'replicationDetails', 'bucketMetadata_replicationDetails' - Specifies whether the bucket is configured to replicate one or more
-- objects to buckets for other Amazon Web Services accounts and, if so,
-- which accounts.
--
-- 'sensitivityScore', 'bucketMetadata_sensitivityScore' - The sensitivity score for the bucket, ranging from -1 (no analysis due
-- to an error) to 100 (sensitive). This value is null if automated
-- sensitive data discovery is currently disabled for your account.
--
-- 'serverSideEncryption', 'bucketMetadata_serverSideEncryption' - Specifies whether the bucket encrypts new objects by default and, if so,
-- the type of server-side encryption that\'s used.
--
-- 'sharedAccess', 'bucketMetadata_sharedAccess' - Specifies whether the bucket is shared with another Amazon Web Services
-- account. Possible values are:
--
-- -   EXTERNAL - The bucket is shared with an Amazon Web Services account
--     that isn\'t part of the same Amazon Macie organization.
--
-- -   INTERNAL - The bucket is shared with an Amazon Web Services account
--     that\'s part of the same Amazon Macie organization.
--
-- -   NOT_SHARED - The bucket isn\'t shared with other Amazon Web Services
--     accounts.
--
-- -   UNKNOWN - Amazon Macie wasn\'t able to evaluate the shared access
--     settings for the bucket.
--
-- 'sizeInBytes', 'bucketMetadata_sizeInBytes' - The total storage size, in bytes, of the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each object in the bucket.
--
-- 'sizeInBytesCompressed', 'bucketMetadata_sizeInBytesCompressed' - The total storage size, in bytes, of the objects that are compressed
-- (.gz, .gzip, .zip) files in the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each applicable object
-- in the bucket. This value doesn\'t reflect the storage size of all
-- versions of each applicable object in the bucket.
--
-- 'tags', 'bucketMetadata_tags' - An array that specifies the tags (keys and values) that are associated
-- with the bucket.
--
-- 'unclassifiableObjectCount', 'bucketMetadata_unclassifiableObjectCount' - The total number of objects that Amazon Macie can\'t analyze in the
-- bucket. These objects don\'t use a supported storage class or don\'t
-- have a file name extension for a supported file or storage format.
--
-- 'unclassifiableObjectSizeInBytes', 'bucketMetadata_unclassifiableObjectSizeInBytes' - The total storage size, in bytes, of the objects that Amazon Macie
-- can\'t analyze in the bucket. These objects don\'t use a supported
-- storage class or don\'t have a file name extension for a supported file
-- or storage format.
--
-- 'versioning', 'bucketMetadata_versioning' - Specifies whether versioning is enabled for the bucket.
newBucketMetadata ::
  BucketMetadata
newBucketMetadata =
  BucketMetadata'
    { accountId = Prelude.Nothing,
      allowsUnencryptedObjectUploads = Prelude.Nothing,
      bucketArn = Prelude.Nothing,
      bucketCreatedAt = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      classifiableObjectCount = Prelude.Nothing,
      classifiableSizeInBytes = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      jobDetails = Prelude.Nothing,
      lastAutomatedDiscoveryTime = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      objectCount = Prelude.Nothing,
      objectCountByEncryptionType = Prelude.Nothing,
      publicAccess = Prelude.Nothing,
      region = Prelude.Nothing,
      replicationDetails = Prelude.Nothing,
      sensitivityScore = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      sharedAccess = Prelude.Nothing,
      sizeInBytes = Prelude.Nothing,
      sizeInBytesCompressed = Prelude.Nothing,
      tags = Prelude.Nothing,
      unclassifiableObjectCount = Prelude.Nothing,
      unclassifiableObjectSizeInBytes = Prelude.Nothing,
      versioning = Prelude.Nothing
    }

-- | The unique identifier for the Amazon Web Services account that owns the
-- bucket.
bucketMetadata_accountId :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Text)
bucketMetadata_accountId = Lens.lens (\BucketMetadata' {accountId} -> accountId) (\s@BucketMetadata' {} a -> s {accountId = a} :: BucketMetadata)

-- | Specifies whether the bucket policy for the bucket requires server-side
-- encryption of objects when objects are uploaded to the bucket. Possible
-- values are:
--
-- -   FALSE - The bucket policy requires server-side encryption of new
--     objects. PutObject requests must include a valid server-side
--     encryption header.
--
-- -   TRUE - The bucket doesn\'t have a bucket policy or it has a bucket
--     policy that doesn\'t require server-side encryption of new objects.
--     If a bucket policy exists, it doesn\'t require PutObject requests to
--     include a valid server-side encryption header.
--
-- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket policy
--     requires server-side encryption of new objects.
--
-- Valid server-side encryption headers are: x-amz-server-side-encryption
-- with a value of AES256 or aws:kms, and
-- x-amz-server-side-encryption-customer-algorithm with a value of AES256.
bucketMetadata_allowsUnencryptedObjectUploads :: Lens.Lens' BucketMetadata (Prelude.Maybe AllowsUnencryptedObjectUploads)
bucketMetadata_allowsUnencryptedObjectUploads = Lens.lens (\BucketMetadata' {allowsUnencryptedObjectUploads} -> allowsUnencryptedObjectUploads) (\s@BucketMetadata' {} a -> s {allowsUnencryptedObjectUploads = a} :: BucketMetadata)

-- | The Amazon Resource Name (ARN) of the bucket.
bucketMetadata_bucketArn :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Text)
bucketMetadata_bucketArn = Lens.lens (\BucketMetadata' {bucketArn} -> bucketArn) (\s@BucketMetadata' {} a -> s {bucketArn = a} :: BucketMetadata)

-- | The date and time, in UTC and extended ISO 8601 format, when the bucket
-- was created, or changes such as edits to the bucket\'s policy were most
-- recently made to the bucket.
bucketMetadata_bucketCreatedAt :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.UTCTime)
bucketMetadata_bucketCreatedAt = Lens.lens (\BucketMetadata' {bucketCreatedAt} -> bucketCreatedAt) (\s@BucketMetadata' {} a -> s {bucketCreatedAt = a} :: BucketMetadata) Prelude.. Lens.mapping Data._Time

-- | The name of the bucket.
bucketMetadata_bucketName :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Text)
bucketMetadata_bucketName = Lens.lens (\BucketMetadata' {bucketName} -> bucketName) (\s@BucketMetadata' {} a -> s {bucketName = a} :: BucketMetadata)

-- | The total number of objects that Amazon Macie can analyze in the bucket.
-- These objects use a supported storage class and have a file name
-- extension for a supported file or storage format.
bucketMetadata_classifiableObjectCount :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Integer)
bucketMetadata_classifiableObjectCount = Lens.lens (\BucketMetadata' {classifiableObjectCount} -> classifiableObjectCount) (\s@BucketMetadata' {} a -> s {classifiableObjectCount = a} :: BucketMetadata)

-- | The total storage size, in bytes, of the objects that Amazon Macie can
-- analyze in the bucket. These objects use a supported storage class and
-- have a file name extension for a supported file or storage format.
--
-- If versioning is enabled for the bucket, Macie calculates this value
-- based on the size of the latest version of each applicable object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each applicable object in the bucket.
bucketMetadata_classifiableSizeInBytes :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Integer)
bucketMetadata_classifiableSizeInBytes = Lens.lens (\BucketMetadata' {classifiableSizeInBytes} -> classifiableSizeInBytes) (\s@BucketMetadata' {} a -> s {classifiableSizeInBytes = a} :: BucketMetadata)

-- | Specifies the error code for an error that prevented Amazon Macie from
-- retrieving and processing information about the bucket and the bucket\'s
-- objects. If this value is ACCESS_DENIED, Macie doesn\'t have permission
-- to retrieve the information. For example, the bucket has a restrictive
-- bucket policy and Amazon S3 denied the request. If this value is null,
-- Macie was able to retrieve and process the information.
bucketMetadata_errorCode :: Lens.Lens' BucketMetadata (Prelude.Maybe BucketMetadataErrorCode)
bucketMetadata_errorCode = Lens.lens (\BucketMetadata' {errorCode} -> errorCode) (\s@BucketMetadata' {} a -> s {errorCode = a} :: BucketMetadata)

-- | A brief description of the error (errorCode) that prevented Amazon Macie
-- from retrieving and processing information about the bucket and the
-- bucket\'s objects. This value is null if Macie was able to retrieve and
-- process the information.
bucketMetadata_errorMessage :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Text)
bucketMetadata_errorMessage = Lens.lens (\BucketMetadata' {errorMessage} -> errorMessage) (\s@BucketMetadata' {} a -> s {errorMessage = a} :: BucketMetadata)

-- | Specifies whether any one-time or recurring classification jobs are
-- configured to analyze data in the bucket, and, if so, the details of the
-- job that ran most recently.
bucketMetadata_jobDetails :: Lens.Lens' BucketMetadata (Prelude.Maybe JobDetails)
bucketMetadata_jobDetails = Lens.lens (\BucketMetadata' {jobDetails} -> jobDetails) (\s@BucketMetadata' {} a -> s {jobDetails = a} :: BucketMetadata)

-- | The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently performed automated sensitive data discovery for the
-- bucket. This value is null if automated sensitive data discovery is
-- currently disabled for your account.
bucketMetadata_lastAutomatedDiscoveryTime :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.UTCTime)
bucketMetadata_lastAutomatedDiscoveryTime = Lens.lens (\BucketMetadata' {lastAutomatedDiscoveryTime} -> lastAutomatedDiscoveryTime) (\s@BucketMetadata' {} a -> s {lastAutomatedDiscoveryTime = a} :: BucketMetadata) Prelude.. Lens.mapping Data._Time

-- | The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently retrieved both bucket and object metadata from
-- Amazon S3 for the bucket.
bucketMetadata_lastUpdated :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.UTCTime)
bucketMetadata_lastUpdated = Lens.lens (\BucketMetadata' {lastUpdated} -> lastUpdated) (\s@BucketMetadata' {} a -> s {lastUpdated = a} :: BucketMetadata) Prelude.. Lens.mapping Data._Time

-- | The total number of objects in the bucket.
bucketMetadata_objectCount :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Integer)
bucketMetadata_objectCount = Lens.lens (\BucketMetadata' {objectCount} -> objectCount) (\s@BucketMetadata' {} a -> s {objectCount = a} :: BucketMetadata)

-- | The total number of objects that are in the bucket, grouped by
-- server-side encryption type. This includes a grouping that reports the
-- total number of objects that aren\'t encrypted or use client-side
-- encryption.
bucketMetadata_objectCountByEncryptionType :: Lens.Lens' BucketMetadata (Prelude.Maybe ObjectCountByEncryptionType)
bucketMetadata_objectCountByEncryptionType = Lens.lens (\BucketMetadata' {objectCountByEncryptionType} -> objectCountByEncryptionType) (\s@BucketMetadata' {} a -> s {objectCountByEncryptionType = a} :: BucketMetadata)

-- | Specifies whether the bucket is publicly accessible due to the
-- combination of permissions settings that apply to the bucket, and
-- provides information about those settings.
bucketMetadata_publicAccess :: Lens.Lens' BucketMetadata (Prelude.Maybe BucketPublicAccess)
bucketMetadata_publicAccess = Lens.lens (\BucketMetadata' {publicAccess} -> publicAccess) (\s@BucketMetadata' {} a -> s {publicAccess = a} :: BucketMetadata)

-- | The Amazon Web Services Region that hosts the bucket.
bucketMetadata_region :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Text)
bucketMetadata_region = Lens.lens (\BucketMetadata' {region} -> region) (\s@BucketMetadata' {} a -> s {region = a} :: BucketMetadata)

-- | Specifies whether the bucket is configured to replicate one or more
-- objects to buckets for other Amazon Web Services accounts and, if so,
-- which accounts.
bucketMetadata_replicationDetails :: Lens.Lens' BucketMetadata (Prelude.Maybe ReplicationDetails)
bucketMetadata_replicationDetails = Lens.lens (\BucketMetadata' {replicationDetails} -> replicationDetails) (\s@BucketMetadata' {} a -> s {replicationDetails = a} :: BucketMetadata)

-- | The sensitivity score for the bucket, ranging from -1 (no analysis due
-- to an error) to 100 (sensitive). This value is null if automated
-- sensitive data discovery is currently disabled for your account.
bucketMetadata_sensitivityScore :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Int)
bucketMetadata_sensitivityScore = Lens.lens (\BucketMetadata' {sensitivityScore} -> sensitivityScore) (\s@BucketMetadata' {} a -> s {sensitivityScore = a} :: BucketMetadata)

-- | Specifies whether the bucket encrypts new objects by default and, if so,
-- the type of server-side encryption that\'s used.
bucketMetadata_serverSideEncryption :: Lens.Lens' BucketMetadata (Prelude.Maybe BucketServerSideEncryption)
bucketMetadata_serverSideEncryption = Lens.lens (\BucketMetadata' {serverSideEncryption} -> serverSideEncryption) (\s@BucketMetadata' {} a -> s {serverSideEncryption = a} :: BucketMetadata)

-- | Specifies whether the bucket is shared with another Amazon Web Services
-- account. Possible values are:
--
-- -   EXTERNAL - The bucket is shared with an Amazon Web Services account
--     that isn\'t part of the same Amazon Macie organization.
--
-- -   INTERNAL - The bucket is shared with an Amazon Web Services account
--     that\'s part of the same Amazon Macie organization.
--
-- -   NOT_SHARED - The bucket isn\'t shared with other Amazon Web Services
--     accounts.
--
-- -   UNKNOWN - Amazon Macie wasn\'t able to evaluate the shared access
--     settings for the bucket.
bucketMetadata_sharedAccess :: Lens.Lens' BucketMetadata (Prelude.Maybe SharedAccess)
bucketMetadata_sharedAccess = Lens.lens (\BucketMetadata' {sharedAccess} -> sharedAccess) (\s@BucketMetadata' {} a -> s {sharedAccess = a} :: BucketMetadata)

-- | The total storage size, in bytes, of the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each object in the bucket.
bucketMetadata_sizeInBytes :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Integer)
bucketMetadata_sizeInBytes = Lens.lens (\BucketMetadata' {sizeInBytes} -> sizeInBytes) (\s@BucketMetadata' {} a -> s {sizeInBytes = a} :: BucketMetadata)

-- | The total storage size, in bytes, of the objects that are compressed
-- (.gz, .gzip, .zip) files in the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each applicable object
-- in the bucket. This value doesn\'t reflect the storage size of all
-- versions of each applicable object in the bucket.
bucketMetadata_sizeInBytesCompressed :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Integer)
bucketMetadata_sizeInBytesCompressed = Lens.lens (\BucketMetadata' {sizeInBytesCompressed} -> sizeInBytesCompressed) (\s@BucketMetadata' {} a -> s {sizeInBytesCompressed = a} :: BucketMetadata)

-- | An array that specifies the tags (keys and values) that are associated
-- with the bucket.
bucketMetadata_tags :: Lens.Lens' BucketMetadata (Prelude.Maybe [KeyValuePair])
bucketMetadata_tags = Lens.lens (\BucketMetadata' {tags} -> tags) (\s@BucketMetadata' {} a -> s {tags = a} :: BucketMetadata) Prelude.. Lens.mapping Lens.coerced

-- | The total number of objects that Amazon Macie can\'t analyze in the
-- bucket. These objects don\'t use a supported storage class or don\'t
-- have a file name extension for a supported file or storage format.
bucketMetadata_unclassifiableObjectCount :: Lens.Lens' BucketMetadata (Prelude.Maybe ObjectLevelStatistics)
bucketMetadata_unclassifiableObjectCount = Lens.lens (\BucketMetadata' {unclassifiableObjectCount} -> unclassifiableObjectCount) (\s@BucketMetadata' {} a -> s {unclassifiableObjectCount = a} :: BucketMetadata)

-- | The total storage size, in bytes, of the objects that Amazon Macie
-- can\'t analyze in the bucket. These objects don\'t use a supported
-- storage class or don\'t have a file name extension for a supported file
-- or storage format.
bucketMetadata_unclassifiableObjectSizeInBytes :: Lens.Lens' BucketMetadata (Prelude.Maybe ObjectLevelStatistics)
bucketMetadata_unclassifiableObjectSizeInBytes = Lens.lens (\BucketMetadata' {unclassifiableObjectSizeInBytes} -> unclassifiableObjectSizeInBytes) (\s@BucketMetadata' {} a -> s {unclassifiableObjectSizeInBytes = a} :: BucketMetadata)

-- | Specifies whether versioning is enabled for the bucket.
bucketMetadata_versioning :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Bool)
bucketMetadata_versioning = Lens.lens (\BucketMetadata' {versioning} -> versioning) (\s@BucketMetadata' {} a -> s {versioning = a} :: BucketMetadata)

instance Data.FromJSON BucketMetadata where
  parseJSON =
    Data.withObject
      "BucketMetadata"
      ( \x ->
          BucketMetadata'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "allowsUnencryptedObjectUploads")
            Prelude.<*> (x Data..:? "bucketArn")
            Prelude.<*> (x Data..:? "bucketCreatedAt")
            Prelude.<*> (x Data..:? "bucketName")
            Prelude.<*> (x Data..:? "classifiableObjectCount")
            Prelude.<*> (x Data..:? "classifiableSizeInBytes")
            Prelude.<*> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "jobDetails")
            Prelude.<*> (x Data..:? "lastAutomatedDiscoveryTime")
            Prelude.<*> (x Data..:? "lastUpdated")
            Prelude.<*> (x Data..:? "objectCount")
            Prelude.<*> (x Data..:? "objectCountByEncryptionType")
            Prelude.<*> (x Data..:? "publicAccess")
            Prelude.<*> (x Data..:? "region")
            Prelude.<*> (x Data..:? "replicationDetails")
            Prelude.<*> (x Data..:? "sensitivityScore")
            Prelude.<*> (x Data..:? "serverSideEncryption")
            Prelude.<*> (x Data..:? "sharedAccess")
            Prelude.<*> (x Data..:? "sizeInBytes")
            Prelude.<*> (x Data..:? "sizeInBytesCompressed")
            Prelude.<*> (x Data..:? "tags" Data..!= Prelude.mempty)
            Prelude.<*> (x Data..:? "unclassifiableObjectCount")
            Prelude.<*> (x Data..:? "unclassifiableObjectSizeInBytes")
            Prelude.<*> (x Data..:? "versioning")
      )

instance Prelude.Hashable BucketMetadata where
  hashWithSalt _salt BucketMetadata' {..} =
    _salt `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` allowsUnencryptedObjectUploads
      `Prelude.hashWithSalt` bucketArn
      `Prelude.hashWithSalt` bucketCreatedAt
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` classifiableObjectCount
      `Prelude.hashWithSalt` classifiableSizeInBytes
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` jobDetails
      `Prelude.hashWithSalt` lastAutomatedDiscoveryTime
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` objectCount
      `Prelude.hashWithSalt` objectCountByEncryptionType
      `Prelude.hashWithSalt` publicAccess
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` replicationDetails
      `Prelude.hashWithSalt` sensitivityScore
      `Prelude.hashWithSalt` serverSideEncryption
      `Prelude.hashWithSalt` sharedAccess
      `Prelude.hashWithSalt` sizeInBytes
      `Prelude.hashWithSalt` sizeInBytesCompressed
      `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` unclassifiableObjectCount
      `Prelude.hashWithSalt` unclassifiableObjectSizeInBytes
      `Prelude.hashWithSalt` versioning

instance Prelude.NFData BucketMetadata where
  rnf BucketMetadata' {..} =
    Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf allowsUnencryptedObjectUploads
      `Prelude.seq` Prelude.rnf bucketArn
      `Prelude.seq` Prelude.rnf bucketCreatedAt
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf classifiableObjectCount
      `Prelude.seq` Prelude.rnf classifiableSizeInBytes
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf jobDetails
      `Prelude.seq` Prelude.rnf lastAutomatedDiscoveryTime
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf objectCount
      `Prelude.seq` Prelude.rnf objectCountByEncryptionType
      `Prelude.seq` Prelude.rnf publicAccess
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf replicationDetails
      `Prelude.seq` Prelude.rnf sensitivityScore
      `Prelude.seq` Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf sharedAccess
      `Prelude.seq` Prelude.rnf sizeInBytes
      `Prelude.seq` Prelude.rnf
        sizeInBytesCompressed
      `Prelude.seq` Prelude.rnf tags
      `Prelude.seq` Prelude.rnf
        unclassifiableObjectCount
      `Prelude.seq` Prelude.rnf
        unclassifiableObjectSizeInBytes
      `Prelude.seq` Prelude.rnf
        versioning
