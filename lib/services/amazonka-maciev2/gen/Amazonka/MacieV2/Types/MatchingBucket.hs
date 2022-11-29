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
-- Module      : Amazonka.MacieV2.Types.MatchingBucket
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.MatchingBucket where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.MacieV2.Types.BucketMetadataErrorCode
import Amazonka.MacieV2.Types.JobDetails
import Amazonka.MacieV2.Types.ObjectCountByEncryptionType
import Amazonka.MacieV2.Types.ObjectLevelStatistics
import qualified Amazonka.Prelude as Prelude

-- | Provides statistical data and other information about an S3 bucket that
-- Amazon Macie monitors and analyzes for your account. If an error occurs
-- when Macie attempts to retrieve and process information about the bucket
-- or the bucket\'s objects, the value for most of these properties is
-- null. Exceptions are accountId and bucketName. To identify the cause of
-- the error, refer to the errorCode and errorMessage values.
--
-- /See:/ 'newMatchingBucket' smart constructor.
data MatchingBucket = MatchingBucket'
  { -- | The total number of objects that are in the bucket, grouped by
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
    -- configured to analyze objects in the bucket, and, if so, the details of
    -- the job that ran most recently.
    jobDetails :: Prelude.Maybe JobDetails,
    -- | The total number of objects that Amazon Macie can\'t analyze in the
    -- bucket. These objects don\'t use a supported storage class or don\'t
    -- have a file name extension for a supported file or storage format.
    unclassifiableObjectCount :: Prelude.Maybe ObjectLevelStatistics,
    -- | The total number of objects in the bucket.
    objectCount :: Prelude.Maybe Prelude.Integer,
    -- | The name of the bucket.
    bucketName :: Prelude.Maybe Prelude.Text,
    -- | The unique identifier for the Amazon Web Services account that owns the
    -- bucket.
    accountId :: Prelude.Maybe Prelude.Text,
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
-- Create a value of 'MatchingBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'objectCountByEncryptionType', 'matchingBucket_objectCountByEncryptionType' - The total number of objects that are in the bucket, grouped by
-- server-side encryption type. This includes a grouping that reports the
-- total number of objects that aren\'t encrypted or use client-side
-- encryption.
--
-- 'classifiableSizeInBytes', 'matchingBucket_classifiableSizeInBytes' - The total storage size, in bytes, of the objects that Amazon Macie can
-- analyze in the bucket. These objects use a supported storage class and
-- have a file name extension for a supported file or storage format.
--
-- If versioning is enabled for the bucket, Macie calculates this value
-- based on the size of the latest version of each applicable object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each applicable object in the bucket.
--
-- 'errorMessage', 'matchingBucket_errorMessage' - A brief description of the error (errorCode) that prevented Amazon Macie
-- from retrieving and processing information about the bucket and the
-- bucket\'s objects. This value is null if Macie was able to retrieve and
-- process the information.
--
-- 'jobDetails', 'matchingBucket_jobDetails' - Specifies whether any one-time or recurring classification jobs are
-- configured to analyze objects in the bucket, and, if so, the details of
-- the job that ran most recently.
--
-- 'unclassifiableObjectCount', 'matchingBucket_unclassifiableObjectCount' - The total number of objects that Amazon Macie can\'t analyze in the
-- bucket. These objects don\'t use a supported storage class or don\'t
-- have a file name extension for a supported file or storage format.
--
-- 'objectCount', 'matchingBucket_objectCount' - The total number of objects in the bucket.
--
-- 'bucketName', 'matchingBucket_bucketName' - The name of the bucket.
--
-- 'accountId', 'matchingBucket_accountId' - The unique identifier for the Amazon Web Services account that owns the
-- bucket.
--
-- 'classifiableObjectCount', 'matchingBucket_classifiableObjectCount' - The total number of objects that Amazon Macie can analyze in the bucket.
-- These objects use a supported storage class and have a file name
-- extension for a supported file or storage format.
--
-- 'sizeInBytes', 'matchingBucket_sizeInBytes' - The total storage size, in bytes, of the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each object in the bucket.
--
-- 'errorCode', 'matchingBucket_errorCode' - Specifies the error code for an error that prevented Amazon Macie from
-- retrieving and processing information about the bucket and the bucket\'s
-- objects. If this value is ACCESS_DENIED, Macie doesn\'t have permission
-- to retrieve the information. For example, the bucket has a restrictive
-- bucket policy and Amazon S3 denied the request. If this value is null,
-- Macie was able to retrieve and process the information.
--
-- 'sizeInBytesCompressed', 'matchingBucket_sizeInBytesCompressed' - The total storage size, in bytes, of the objects that are compressed
-- (.gz, .gzip, .zip) files in the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each applicable object
-- in the bucket. This value doesn\'t reflect the storage size of all
-- versions of each applicable object in the bucket.
--
-- 'unclassifiableObjectSizeInBytes', 'matchingBucket_unclassifiableObjectSizeInBytes' - The total storage size, in bytes, of the objects that Amazon Macie
-- can\'t analyze in the bucket. These objects don\'t use a supported
-- storage class or don\'t have a file name extension for a supported file
-- or storage format.
newMatchingBucket ::
  MatchingBucket
newMatchingBucket =
  MatchingBucket'
    { objectCountByEncryptionType =
        Prelude.Nothing,
      classifiableSizeInBytes = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      jobDetails = Prelude.Nothing,
      unclassifiableObjectCount = Prelude.Nothing,
      objectCount = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      accountId = Prelude.Nothing,
      classifiableObjectCount = Prelude.Nothing,
      sizeInBytes = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      sizeInBytesCompressed = Prelude.Nothing,
      unclassifiableObjectSizeInBytes = Prelude.Nothing
    }

-- | The total number of objects that are in the bucket, grouped by
-- server-side encryption type. This includes a grouping that reports the
-- total number of objects that aren\'t encrypted or use client-side
-- encryption.
matchingBucket_objectCountByEncryptionType :: Lens.Lens' MatchingBucket (Prelude.Maybe ObjectCountByEncryptionType)
matchingBucket_objectCountByEncryptionType = Lens.lens (\MatchingBucket' {objectCountByEncryptionType} -> objectCountByEncryptionType) (\s@MatchingBucket' {} a -> s {objectCountByEncryptionType = a} :: MatchingBucket)

-- | The total storage size, in bytes, of the objects that Amazon Macie can
-- analyze in the bucket. These objects use a supported storage class and
-- have a file name extension for a supported file or storage format.
--
-- If versioning is enabled for the bucket, Macie calculates this value
-- based on the size of the latest version of each applicable object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each applicable object in the bucket.
matchingBucket_classifiableSizeInBytes :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Integer)
matchingBucket_classifiableSizeInBytes = Lens.lens (\MatchingBucket' {classifiableSizeInBytes} -> classifiableSizeInBytes) (\s@MatchingBucket' {} a -> s {classifiableSizeInBytes = a} :: MatchingBucket)

-- | A brief description of the error (errorCode) that prevented Amazon Macie
-- from retrieving and processing information about the bucket and the
-- bucket\'s objects. This value is null if Macie was able to retrieve and
-- process the information.
matchingBucket_errorMessage :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Text)
matchingBucket_errorMessage = Lens.lens (\MatchingBucket' {errorMessage} -> errorMessage) (\s@MatchingBucket' {} a -> s {errorMessage = a} :: MatchingBucket)

-- | Specifies whether any one-time or recurring classification jobs are
-- configured to analyze objects in the bucket, and, if so, the details of
-- the job that ran most recently.
matchingBucket_jobDetails :: Lens.Lens' MatchingBucket (Prelude.Maybe JobDetails)
matchingBucket_jobDetails = Lens.lens (\MatchingBucket' {jobDetails} -> jobDetails) (\s@MatchingBucket' {} a -> s {jobDetails = a} :: MatchingBucket)

-- | The total number of objects that Amazon Macie can\'t analyze in the
-- bucket. These objects don\'t use a supported storage class or don\'t
-- have a file name extension for a supported file or storage format.
matchingBucket_unclassifiableObjectCount :: Lens.Lens' MatchingBucket (Prelude.Maybe ObjectLevelStatistics)
matchingBucket_unclassifiableObjectCount = Lens.lens (\MatchingBucket' {unclassifiableObjectCount} -> unclassifiableObjectCount) (\s@MatchingBucket' {} a -> s {unclassifiableObjectCount = a} :: MatchingBucket)

-- | The total number of objects in the bucket.
matchingBucket_objectCount :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Integer)
matchingBucket_objectCount = Lens.lens (\MatchingBucket' {objectCount} -> objectCount) (\s@MatchingBucket' {} a -> s {objectCount = a} :: MatchingBucket)

-- | The name of the bucket.
matchingBucket_bucketName :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Text)
matchingBucket_bucketName = Lens.lens (\MatchingBucket' {bucketName} -> bucketName) (\s@MatchingBucket' {} a -> s {bucketName = a} :: MatchingBucket)

-- | The unique identifier for the Amazon Web Services account that owns the
-- bucket.
matchingBucket_accountId :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Text)
matchingBucket_accountId = Lens.lens (\MatchingBucket' {accountId} -> accountId) (\s@MatchingBucket' {} a -> s {accountId = a} :: MatchingBucket)

-- | The total number of objects that Amazon Macie can analyze in the bucket.
-- These objects use a supported storage class and have a file name
-- extension for a supported file or storage format.
matchingBucket_classifiableObjectCount :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Integer)
matchingBucket_classifiableObjectCount = Lens.lens (\MatchingBucket' {classifiableObjectCount} -> classifiableObjectCount) (\s@MatchingBucket' {} a -> s {classifiableObjectCount = a} :: MatchingBucket)

-- | The total storage size, in bytes, of the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each object in the bucket.
matchingBucket_sizeInBytes :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Integer)
matchingBucket_sizeInBytes = Lens.lens (\MatchingBucket' {sizeInBytes} -> sizeInBytes) (\s@MatchingBucket' {} a -> s {sizeInBytes = a} :: MatchingBucket)

-- | Specifies the error code for an error that prevented Amazon Macie from
-- retrieving and processing information about the bucket and the bucket\'s
-- objects. If this value is ACCESS_DENIED, Macie doesn\'t have permission
-- to retrieve the information. For example, the bucket has a restrictive
-- bucket policy and Amazon S3 denied the request. If this value is null,
-- Macie was able to retrieve and process the information.
matchingBucket_errorCode :: Lens.Lens' MatchingBucket (Prelude.Maybe BucketMetadataErrorCode)
matchingBucket_errorCode = Lens.lens (\MatchingBucket' {errorCode} -> errorCode) (\s@MatchingBucket' {} a -> s {errorCode = a} :: MatchingBucket)

-- | The total storage size, in bytes, of the objects that are compressed
-- (.gz, .gzip, .zip) files in the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each applicable object
-- in the bucket. This value doesn\'t reflect the storage size of all
-- versions of each applicable object in the bucket.
matchingBucket_sizeInBytesCompressed :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Integer)
matchingBucket_sizeInBytesCompressed = Lens.lens (\MatchingBucket' {sizeInBytesCompressed} -> sizeInBytesCompressed) (\s@MatchingBucket' {} a -> s {sizeInBytesCompressed = a} :: MatchingBucket)

-- | The total storage size, in bytes, of the objects that Amazon Macie
-- can\'t analyze in the bucket. These objects don\'t use a supported
-- storage class or don\'t have a file name extension for a supported file
-- or storage format.
matchingBucket_unclassifiableObjectSizeInBytes :: Lens.Lens' MatchingBucket (Prelude.Maybe ObjectLevelStatistics)
matchingBucket_unclassifiableObjectSizeInBytes = Lens.lens (\MatchingBucket' {unclassifiableObjectSizeInBytes} -> unclassifiableObjectSizeInBytes) (\s@MatchingBucket' {} a -> s {unclassifiableObjectSizeInBytes = a} :: MatchingBucket)

instance Core.FromJSON MatchingBucket where
  parseJSON =
    Core.withObject
      "MatchingBucket"
      ( \x ->
          MatchingBucket'
            Prelude.<$> (x Core..:? "objectCountByEncryptionType")
            Prelude.<*> (x Core..:? "classifiableSizeInBytes")
            Prelude.<*> (x Core..:? "errorMessage")
            Prelude.<*> (x Core..:? "jobDetails")
            Prelude.<*> (x Core..:? "unclassifiableObjectCount")
            Prelude.<*> (x Core..:? "objectCount")
            Prelude.<*> (x Core..:? "bucketName")
            Prelude.<*> (x Core..:? "accountId")
            Prelude.<*> (x Core..:? "classifiableObjectCount")
            Prelude.<*> (x Core..:? "sizeInBytes")
            Prelude.<*> (x Core..:? "errorCode")
            Prelude.<*> (x Core..:? "sizeInBytesCompressed")
            Prelude.<*> (x Core..:? "unclassifiableObjectSizeInBytes")
      )

instance Prelude.Hashable MatchingBucket where
  hashWithSalt _salt MatchingBucket' {..} =
    _salt
      `Prelude.hashWithSalt` objectCountByEncryptionType
      `Prelude.hashWithSalt` classifiableSizeInBytes
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` jobDetails
      `Prelude.hashWithSalt` unclassifiableObjectCount
      `Prelude.hashWithSalt` objectCount
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` classifiableObjectCount
      `Prelude.hashWithSalt` sizeInBytes
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` sizeInBytesCompressed
      `Prelude.hashWithSalt` unclassifiableObjectSizeInBytes

instance Prelude.NFData MatchingBucket where
  rnf MatchingBucket' {..} =
    Prelude.rnf objectCountByEncryptionType
      `Prelude.seq` Prelude.rnf classifiableSizeInBytes
      `Prelude.seq` Prelude.rnf errorMessage
      `Prelude.seq` Prelude.rnf jobDetails
      `Prelude.seq` Prelude.rnf unclassifiableObjectCount
      `Prelude.seq` Prelude.rnf objectCount
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf accountId
      `Prelude.seq` Prelude.rnf classifiableObjectCount
      `Prelude.seq` Prelude.rnf sizeInBytes
      `Prelude.seq` Prelude.rnf errorCode
      `Prelude.seq` Prelude.rnf sizeInBytesCompressed
      `Prelude.seq` Prelude.rnf unclassifiableObjectSizeInBytes
