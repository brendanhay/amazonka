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
-- when Macie attempts to retrieve and process information about the bucket
-- or the bucket\'s objects, the value for the versioning property is false
-- and the value for most other properties is null. Exceptions are
-- accountId, bucketArn, bucketCreatedAt, bucketName, lastUpdated, and
-- region. To identify the cause of the error, refer to the errorCode and
-- errorMessage values.
--
-- /See:/ 'newBucketMetadata' smart constructor.
data BucketMetadata = BucketMetadata'
  { -- | An array that specifies the tags (keys and values) that are associated
    -- with the bucket.
    tags :: Prelude.Maybe [KeyValuePair],
    -- | Specifies whether the bucket encrypts new objects by default and, if so,
    -- the type of server-side encryption that\'s used.
    serverSideEncryption :: Prelude.Maybe BucketServerSideEncryption,
    -- | The total number of objects that are in the bucket, grouped by
    -- server-side encryption type. This includes a grouping that reports the
    -- total number of objects that aren\'t encrypted or use client-side
    -- encryption.
    objectCountByEncryptionType :: Prelude.Maybe ObjectCountByEncryptionType,
    -- | The total storage size, in bytes, of the objects that Amazon Macie can
    -- analyze in the bucket. These objects use a supported storage class and
    -- have a file name extension for a supported file or storage format.
    --
    -- If versioning is enabled for the bucket, Macie calculates this value
    -- based on the size of the latest version of each applicable object in the
    -- bucket. This value doesn\'t reflect the storage size of all versions of
    -- each applicable object in the bucket.
    classifiableSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | A brief description of the error (errorCode) that prevented Amazon Macie
    -- from retrieving and processing information about the bucket and the
    -- bucket\'s objects. This value is null if Macie was able to retrieve and
    -- process the information.
    errorMessage :: Prelude.Maybe Prelude.Text,
    -- | Specifies whether any one-time or recurring classification jobs are
    -- configured to analyze data in the bucket, and, if so, the details of the
    -- job that ran most recently.
    jobDetails :: Prelude.Maybe JobDetails,
    -- | The total number of objects that Amazon Macie can\'t analyze in the
    -- bucket. These objects don\'t use a supported storage class or don\'t
    -- have a file name extension for a supported file or storage format.
    unclassifiableObjectCount :: Prelude.Maybe ObjectLevelStatistics,
    -- | The date and time, in UTC and extended ISO 8601 format, when the bucket
    -- was created.
    bucketCreatedAt :: Prelude.Maybe Core.POSIX,
    -- | Specifies whether the bucket is configured to replicate one or more
    -- objects to buckets for other Amazon Web Services accounts and, if so,
    -- which accounts.
    replicationDetails :: Prelude.Maybe ReplicationDetails,
    -- | Specifies whether the bucket policy for the bucket requires server-side
    -- encryption of objects when objects are uploaded to the bucket. Possible
    -- values are:
    --
    -- -   FALSE - The bucket policy requires server-side encryption of new
    --     objects. PutObject requests must include the
    --     x-amz-server-side-encryption header and the value for that header
    --     must be AES256 or aws:kms.
    --
    -- -   TRUE - The bucket doesn\'t have a bucket policy or it has a bucket
    --     policy that doesn\'t require server-side encryption of new objects.
    --     If a bucket policy exists, it doesn\'t require PutObject requests to
    --     include the x-amz-server-side-encryption header and it doesn\'t
    --     require the value for that header to be AES256 or aws:kms.
    --
    -- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket policy
    --     requires server-side encryption of new objects.
    allowsUnencryptedObjectUploads :: Prelude.Maybe AllowsUnencryptedObjectUploads,
    -- | Specifies whether the bucket is publicly accessible due to the
    -- combination of permissions settings that apply to the bucket, and
    -- provides information about those settings.
    publicAccess :: Prelude.Maybe BucketPublicAccess,
    -- | The total number of objects in the bucket.
    objectCount :: Prelude.Maybe Prelude.Integer,
    -- | Specifies whether versioning is enabled for the bucket.
    versioning :: Prelude.Maybe Prelude.Bool,
    -- | The date and time, in UTC and extended ISO 8601 format, when Amazon
    -- Macie most recently retrieved both bucket and object metadata from
    -- Amazon S3 for the bucket.
    lastUpdated :: Prelude.Maybe Core.POSIX,
    -- | The Amazon Web Services Region that hosts the bucket.
    region :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Amazon Web Services account that owns the
    -- bucket.
    accountId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon Resource Name (ARN) of the bucket.
    bucketArn :: Prelude.Maybe Prelude.Text,
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
    -- | The total number of objects that Amazon Macie can analyze in the bucket.
    -- These objects use a supported storage class and have a file name
    -- extension for a supported file or storage format.
    classifiableObjectCount :: Prelude.Maybe Prelude.Integer,
    -- | The total storage size, in bytes, of the bucket.
    --
    -- If versioning is enabled for the bucket, Amazon Macie calculates this
    -- value based on the size of the latest version of each object in the
    -- bucket. This value doesn\'t reflect the storage size of all versions of
    -- each object in the bucket.
    sizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | Specifies the error code for an error that prevented Amazon Macie from
    -- retrieving and processing information about the bucket and the bucket\'s
    -- objects. If this value is ACCESS_DENIED, Macie doesn\'t have permission
    -- to retrieve the information. For example, the bucket has a restrictive
    -- bucket policy and Amazon S3 denied the request. If this value is null,
    -- Macie was able to retrieve and process the information.
    errorCode :: Prelude.Maybe BucketMetadataErrorCode,
    -- | The total storage size, in bytes, of the objects that are compressed
    -- (.gz, .gzip, .zip) files in the bucket.
    --
    -- If versioning is enabled for the bucket, Amazon Macie calculates this
    -- value based on the size of the latest version of each applicable object
    -- in the bucket. This value doesn\'t reflect the storage size of all
    -- versions of each applicable object in the bucket.
    sizeInBytesCompressed :: Prelude.Maybe Prelude.Integer,
    -- | The total storage size, in bytes, of the objects that Amazon Macie
    -- can\'t analyze in the bucket. These objects don\'t use a supported
    -- storage class or don\'t have a file name extension for a supported file
    -- or storage format.
    unclassifiableObjectSizeInBytes :: Prelude.Maybe ObjectLevelStatistics
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
-- 'tags', 'bucketMetadata_tags' - An array that specifies the tags (keys and values) that are associated
-- with the bucket.
--
-- 'serverSideEncryption', 'bucketMetadata_serverSideEncryption' - Specifies whether the bucket encrypts new objects by default and, if so,
-- the type of server-side encryption that\'s used.
--
-- 'objectCountByEncryptionType', 'bucketMetadata_objectCountByEncryptionType' - The total number of objects that are in the bucket, grouped by
-- server-side encryption type. This includes a grouping that reports the
-- total number of objects that aren\'t encrypted or use client-side
-- encryption.
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
-- 'errorMessage', 'bucketMetadata_errorMessage' - A brief description of the error (errorCode) that prevented Amazon Macie
-- from retrieving and processing information about the bucket and the
-- bucket\'s objects. This value is null if Macie was able to retrieve and
-- process the information.
--
-- 'jobDetails', 'bucketMetadata_jobDetails' - Specifies whether any one-time or recurring classification jobs are
-- configured to analyze data in the bucket, and, if so, the details of the
-- job that ran most recently.
--
-- 'unclassifiableObjectCount', 'bucketMetadata_unclassifiableObjectCount' - The total number of objects that Amazon Macie can\'t analyze in the
-- bucket. These objects don\'t use a supported storage class or don\'t
-- have a file name extension for a supported file or storage format.
--
-- 'bucketCreatedAt', 'bucketMetadata_bucketCreatedAt' - The date and time, in UTC and extended ISO 8601 format, when the bucket
-- was created.
--
-- 'replicationDetails', 'bucketMetadata_replicationDetails' - Specifies whether the bucket is configured to replicate one or more
-- objects to buckets for other Amazon Web Services accounts and, if so,
-- which accounts.
--
-- 'allowsUnencryptedObjectUploads', 'bucketMetadata_allowsUnencryptedObjectUploads' - Specifies whether the bucket policy for the bucket requires server-side
-- encryption of objects when objects are uploaded to the bucket. Possible
-- values are:
--
-- -   FALSE - The bucket policy requires server-side encryption of new
--     objects. PutObject requests must include the
--     x-amz-server-side-encryption header and the value for that header
--     must be AES256 or aws:kms.
--
-- -   TRUE - The bucket doesn\'t have a bucket policy or it has a bucket
--     policy that doesn\'t require server-side encryption of new objects.
--     If a bucket policy exists, it doesn\'t require PutObject requests to
--     include the x-amz-server-side-encryption header and it doesn\'t
--     require the value for that header to be AES256 or aws:kms.
--
-- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket policy
--     requires server-side encryption of new objects.
--
-- 'publicAccess', 'bucketMetadata_publicAccess' - Specifies whether the bucket is publicly accessible due to the
-- combination of permissions settings that apply to the bucket, and
-- provides information about those settings.
--
-- 'objectCount', 'bucketMetadata_objectCount' - The total number of objects in the bucket.
--
-- 'versioning', 'bucketMetadata_versioning' - Specifies whether versioning is enabled for the bucket.
--
-- 'lastUpdated', 'bucketMetadata_lastUpdated' - The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently retrieved both bucket and object metadata from
-- Amazon S3 for the bucket.
--
-- 'region', 'bucketMetadata_region' - The Amazon Web Services Region that hosts the bucket.
--
-- 'bucketName', 'bucketMetadata_bucketName' - The name of the bucket.
--
-- 'accountId', 'bucketMetadata_accountId' - The unique identifier for the Amazon Web Services account that owns the
-- bucket.
--
-- 'bucketArn', 'bucketMetadata_bucketArn' - The Amazon Resource Name (ARN) of the bucket.
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
-- 'classifiableObjectCount', 'bucketMetadata_classifiableObjectCount' - The total number of objects that Amazon Macie can analyze in the bucket.
-- These objects use a supported storage class and have a file name
-- extension for a supported file or storage format.
--
-- 'sizeInBytes', 'bucketMetadata_sizeInBytes' - The total storage size, in bytes, of the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each object in the bucket.
--
-- 'errorCode', 'bucketMetadata_errorCode' - Specifies the error code for an error that prevented Amazon Macie from
-- retrieving and processing information about the bucket and the bucket\'s
-- objects. If this value is ACCESS_DENIED, Macie doesn\'t have permission
-- to retrieve the information. For example, the bucket has a restrictive
-- bucket policy and Amazon S3 denied the request. If this value is null,
-- Macie was able to retrieve and process the information.
--
-- 'sizeInBytesCompressed', 'bucketMetadata_sizeInBytesCompressed' - The total storage size, in bytes, of the objects that are compressed
-- (.gz, .gzip, .zip) files in the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each applicable object
-- in the bucket. This value doesn\'t reflect the storage size of all
-- versions of each applicable object in the bucket.
--
-- 'unclassifiableObjectSizeInBytes', 'bucketMetadata_unclassifiableObjectSizeInBytes' - The total storage size, in bytes, of the objects that Amazon Macie
-- can\'t analyze in the bucket. These objects don\'t use a supported
-- storage class or don\'t have a file name extension for a supported file
-- or storage format.
newBucketMetadata ::
  BucketMetadata
newBucketMetadata =
  BucketMetadata'
    { tags = Prelude.Nothing,
      serverSideEncryption = Prelude.Nothing,
      objectCountByEncryptionType = Prelude.Nothing,
      classifiableSizeInBytes = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      jobDetails = Prelude.Nothing,
      unclassifiableObjectCount = Prelude.Nothing,
      bucketCreatedAt = Prelude.Nothing,
      replicationDetails = Prelude.Nothing,
      allowsUnencryptedObjectUploads = Prelude.Nothing,
      publicAccess = Prelude.Nothing,
      objectCount = Prelude.Nothing,
      versioning = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      region = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      accountId = Prelude.Nothing,
      bucketArn = Prelude.Nothing,
      sharedAccess = Prelude.Nothing,
      classifiableObjectCount = Prelude.Nothing,
      sizeInBytes = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      sizeInBytesCompressed = Prelude.Nothing,
      unclassifiableObjectSizeInBytes = Prelude.Nothing
    }

-- | An array that specifies the tags (keys and values) that are associated
-- with the bucket.
bucketMetadata_tags :: Lens.Lens' BucketMetadata (Prelude.Maybe [KeyValuePair])
bucketMetadata_tags = Lens.lens (\BucketMetadata' {tags} -> tags) (\s@BucketMetadata' {} a -> s {tags = a} :: BucketMetadata) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether the bucket encrypts new objects by default and, if so,
-- the type of server-side encryption that\'s used.
bucketMetadata_serverSideEncryption :: Lens.Lens' BucketMetadata (Prelude.Maybe BucketServerSideEncryption)
bucketMetadata_serverSideEncryption = Lens.lens (\BucketMetadata' {serverSideEncryption} -> serverSideEncryption) (\s@BucketMetadata' {} a -> s {serverSideEncryption = a} :: BucketMetadata)

-- | The total number of objects that are in the bucket, grouped by
-- server-side encryption type. This includes a grouping that reports the
-- total number of objects that aren\'t encrypted or use client-side
-- encryption.
bucketMetadata_objectCountByEncryptionType :: Lens.Lens' BucketMetadata (Prelude.Maybe ObjectCountByEncryptionType)
bucketMetadata_objectCountByEncryptionType = Lens.lens (\BucketMetadata' {objectCountByEncryptionType} -> objectCountByEncryptionType) (\s@BucketMetadata' {} a -> s {objectCountByEncryptionType = a} :: BucketMetadata)

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

-- | The total number of objects that Amazon Macie can\'t analyze in the
-- bucket. These objects don\'t use a supported storage class or don\'t
-- have a file name extension for a supported file or storage format.
bucketMetadata_unclassifiableObjectCount :: Lens.Lens' BucketMetadata (Prelude.Maybe ObjectLevelStatistics)
bucketMetadata_unclassifiableObjectCount = Lens.lens (\BucketMetadata' {unclassifiableObjectCount} -> unclassifiableObjectCount) (\s@BucketMetadata' {} a -> s {unclassifiableObjectCount = a} :: BucketMetadata)

-- | The date and time, in UTC and extended ISO 8601 format, when the bucket
-- was created.
bucketMetadata_bucketCreatedAt :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.UTCTime)
bucketMetadata_bucketCreatedAt = Lens.lens (\BucketMetadata' {bucketCreatedAt} -> bucketCreatedAt) (\s@BucketMetadata' {} a -> s {bucketCreatedAt = a} :: BucketMetadata) Prelude.. Lens.mapping Core._Time

-- | Specifies whether the bucket is configured to replicate one or more
-- objects to buckets for other Amazon Web Services accounts and, if so,
-- which accounts.
bucketMetadata_replicationDetails :: Lens.Lens' BucketMetadata (Prelude.Maybe ReplicationDetails)
bucketMetadata_replicationDetails = Lens.lens (\BucketMetadata' {replicationDetails} -> replicationDetails) (\s@BucketMetadata' {} a -> s {replicationDetails = a} :: BucketMetadata)

-- | Specifies whether the bucket policy for the bucket requires server-side
-- encryption of objects when objects are uploaded to the bucket. Possible
-- values are:
--
-- -   FALSE - The bucket policy requires server-side encryption of new
--     objects. PutObject requests must include the
--     x-amz-server-side-encryption header and the value for that header
--     must be AES256 or aws:kms.
--
-- -   TRUE - The bucket doesn\'t have a bucket policy or it has a bucket
--     policy that doesn\'t require server-side encryption of new objects.
--     If a bucket policy exists, it doesn\'t require PutObject requests to
--     include the x-amz-server-side-encryption header and it doesn\'t
--     require the value for that header to be AES256 or aws:kms.
--
-- -   UNKNOWN - Amazon Macie can\'t determine whether the bucket policy
--     requires server-side encryption of new objects.
bucketMetadata_allowsUnencryptedObjectUploads :: Lens.Lens' BucketMetadata (Prelude.Maybe AllowsUnencryptedObjectUploads)
bucketMetadata_allowsUnencryptedObjectUploads = Lens.lens (\BucketMetadata' {allowsUnencryptedObjectUploads} -> allowsUnencryptedObjectUploads) (\s@BucketMetadata' {} a -> s {allowsUnencryptedObjectUploads = a} :: BucketMetadata)

-- | Specifies whether the bucket is publicly accessible due to the
-- combination of permissions settings that apply to the bucket, and
-- provides information about those settings.
bucketMetadata_publicAccess :: Lens.Lens' BucketMetadata (Prelude.Maybe BucketPublicAccess)
bucketMetadata_publicAccess = Lens.lens (\BucketMetadata' {publicAccess} -> publicAccess) (\s@BucketMetadata' {} a -> s {publicAccess = a} :: BucketMetadata)

-- | The total number of objects in the bucket.
bucketMetadata_objectCount :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Integer)
bucketMetadata_objectCount = Lens.lens (\BucketMetadata' {objectCount} -> objectCount) (\s@BucketMetadata' {} a -> s {objectCount = a} :: BucketMetadata)

-- | Specifies whether versioning is enabled for the bucket.
bucketMetadata_versioning :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Bool)
bucketMetadata_versioning = Lens.lens (\BucketMetadata' {versioning} -> versioning) (\s@BucketMetadata' {} a -> s {versioning = a} :: BucketMetadata)

-- | The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently retrieved both bucket and object metadata from
-- Amazon S3 for the bucket.
bucketMetadata_lastUpdated :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.UTCTime)
bucketMetadata_lastUpdated = Lens.lens (\BucketMetadata' {lastUpdated} -> lastUpdated) (\s@BucketMetadata' {} a -> s {lastUpdated = a} :: BucketMetadata) Prelude.. Lens.mapping Core._Time

-- | The Amazon Web Services Region that hosts the bucket.
bucketMetadata_region :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Text)
bucketMetadata_region = Lens.lens (\BucketMetadata' {region} -> region) (\s@BucketMetadata' {} a -> s {region = a} :: BucketMetadata)

-- | The name of the bucket.
bucketMetadata_bucketName :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Text)
bucketMetadata_bucketName = Lens.lens (\BucketMetadata' {bucketName} -> bucketName) (\s@BucketMetadata' {} a -> s {bucketName = a} :: BucketMetadata)

-- | The unique identifier for the Amazon Web Services account that owns the
-- bucket.
bucketMetadata_accountId :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Text)
bucketMetadata_accountId = Lens.lens (\BucketMetadata' {accountId} -> accountId) (\s@BucketMetadata' {} a -> s {accountId = a} :: BucketMetadata)

-- | The Amazon Resource Name (ARN) of the bucket.
bucketMetadata_bucketArn :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Text)
bucketMetadata_bucketArn = Lens.lens (\BucketMetadata' {bucketArn} -> bucketArn) (\s@BucketMetadata' {} a -> s {bucketArn = a} :: BucketMetadata)

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

-- | The total number of objects that Amazon Macie can analyze in the bucket.
-- These objects use a supported storage class and have a file name
-- extension for a supported file or storage format.
bucketMetadata_classifiableObjectCount :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Integer)
bucketMetadata_classifiableObjectCount = Lens.lens (\BucketMetadata' {classifiableObjectCount} -> classifiableObjectCount) (\s@BucketMetadata' {} a -> s {classifiableObjectCount = a} :: BucketMetadata)

-- | The total storage size, in bytes, of the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each object in the bucket.
bucketMetadata_sizeInBytes :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Integer)
bucketMetadata_sizeInBytes = Lens.lens (\BucketMetadata' {sizeInBytes} -> sizeInBytes) (\s@BucketMetadata' {} a -> s {sizeInBytes = a} :: BucketMetadata)

-- | Specifies the error code for an error that prevented Amazon Macie from
-- retrieving and processing information about the bucket and the bucket\'s
-- objects. If this value is ACCESS_DENIED, Macie doesn\'t have permission
-- to retrieve the information. For example, the bucket has a restrictive
-- bucket policy and Amazon S3 denied the request. If this value is null,
-- Macie was able to retrieve and process the information.
bucketMetadata_errorCode :: Lens.Lens' BucketMetadata (Prelude.Maybe BucketMetadataErrorCode)
bucketMetadata_errorCode = Lens.lens (\BucketMetadata' {errorCode} -> errorCode) (\s@BucketMetadata' {} a -> s {errorCode = a} :: BucketMetadata)

-- | The total storage size, in bytes, of the objects that are compressed
-- (.gz, .gzip, .zip) files in the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each applicable object
-- in the bucket. This value doesn\'t reflect the storage size of all
-- versions of each applicable object in the bucket.
bucketMetadata_sizeInBytesCompressed :: Lens.Lens' BucketMetadata (Prelude.Maybe Prelude.Integer)
bucketMetadata_sizeInBytesCompressed = Lens.lens (\BucketMetadata' {sizeInBytesCompressed} -> sizeInBytesCompressed) (\s@BucketMetadata' {} a -> s {sizeInBytesCompressed = a} :: BucketMetadata)

-- | The total storage size, in bytes, of the objects that Amazon Macie
-- can\'t analyze in the bucket. These objects don\'t use a supported
-- storage class or don\'t have a file name extension for a supported file
-- or storage format.
bucketMetadata_unclassifiableObjectSizeInBytes :: Lens.Lens' BucketMetadata (Prelude.Maybe ObjectLevelStatistics)
bucketMetadata_unclassifiableObjectSizeInBytes = Lens.lens (\BucketMetadata' {unclassifiableObjectSizeInBytes} -> unclassifiableObjectSizeInBytes) (\s@BucketMetadata' {} a -> s {unclassifiableObjectSizeInBytes = a} :: BucketMetadata)

instance Core.FromJSON BucketMetadata where
  parseJSON =
    Core.withObject
      "BucketMetadata"
      ( \x ->
          BucketMetadata'
            Prelude.<$> (x Core..:? "tags" Core..!= Prelude.mempty)
            Prelude.<*> (x Core..:? "serverSideEncryption")
            Prelude.<*> (x Core..:? "objectCountByEncryptionType")
            Prelude.<*> (x Core..:? "classifiableSizeInBytes")
            Prelude.<*> (x Core..:? "errorMessage")
            Prelude.<*> (x Core..:? "jobDetails")
            Prelude.<*> (x Core..:? "unclassifiableObjectCount")
            Prelude.<*> (x Core..:? "bucketCreatedAt")
            Prelude.<*> (x Core..:? "replicationDetails")
            Prelude.<*> (x Core..:? "allowsUnencryptedObjectUploads")
            Prelude.<*> (x Core..:? "publicAccess")
            Prelude.<*> (x Core..:? "objectCount")
            Prelude.<*> (x Core..:? "versioning")
            Prelude.<*> (x Core..:? "lastUpdated")
            Prelude.<*> (x Core..:? "region")
            Prelude.<*> (x Core..:? "bucketName")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "bucketArn")
            Prelude.<*> (x Core..:? "sharedAccess")
            Prelude.<*> (x Core..:? "classifiableObjectCount")
            Prelude.<*> (x Core..:? "sizeInBytes")
            Prelude.<*> (x Core..:? "errorCode")
            Prelude.<*> (x Core..:? "sizeInBytesCompressed")
            Prelude.<*> (x Core..:? "unclassifiableObjectSizeInBytes")
      )

instance Prelude.Hashable BucketMetadata where
  hashWithSalt _salt BucketMetadata' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` serverSideEncryption
      `Prelude.hashWithSalt` objectCountByEncryptionType
      `Prelude.hashWithSalt` classifiableSizeInBytes
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` jobDetails
      `Prelude.hashWithSalt` unclassifiableObjectCount
      `Prelude.hashWithSalt` bucketCreatedAt
      `Prelude.hashWithSalt` replicationDetails
      `Prelude.hashWithSalt` allowsUnencryptedObjectUploads
      `Prelude.hashWithSalt` publicAccess
      `Prelude.hashWithSalt` objectCount
      `Prelude.hashWithSalt` versioning
      `Prelude.hashWithSalt` lastUpdated
      `Prelude.hashWithSalt` region
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` bucketArn
      `Prelude.hashWithSalt` sharedAccess
      `Prelude.hashWithSalt` classifiableObjectCount
      `Prelude.hashWithSalt` sizeInBytes
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` sizeInBytesCompressed
      `Prelude.hashWithSalt` unclassifiableObjectSizeInBytes

instance Prelude.NFData BucketMetadata where
  rnf BucketMetadata' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf serverSideEncryption
      `Prelude.seq` Prelude.rnf objectCountByEncryptionType
      `Prelude.seq` Prelude.rnf classifiableSizeInBytes
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf jobDetails
      `Prelude.seq` Prelude.rnf unclassifiableObjectCount
      `Prelude.seq` Prelude.rnf bucketCreatedAt
      `Prelude.seq` Prelude.rnf replicationDetails
      `Prelude.seq` Prelude.rnf allowsUnencryptedObjectUploads
      `Prelude.seq` Prelude.rnf publicAccess
      `Prelude.seq` Prelude.rnf objectCount
      `Prelude.seq` Prelude.rnf versioning
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf region
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf bucketArn
      `Prelude.seq` Prelude.rnf sharedAccess
      `Prelude.seq` Prelude.rnf
        classifiableObjectCount
      `Prelude.seq` Prelude.rnf sizeInBytes
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf
        sizeInBytesCompressed
      `Prelude.seq` Prelude.rnf
        unclassifiableObjectSizeInBytes
