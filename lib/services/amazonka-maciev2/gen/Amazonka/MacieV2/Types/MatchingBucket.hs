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
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
module Amazonka.MacieV2.Types.MatchingBucket where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types.BucketMetadataErrorCode
import Amazonka.MacieV2.Types.JobDetails
import Amazonka.MacieV2.Types.ObjectCountByEncryptionType
import Amazonka.MacieV2.Types.ObjectLevelStatistics
import qualified Amazonka.Prelude as Prelude

-- | Provides statistical data and other information about an S3 bucket that
-- Amazon Macie monitors and analyzes for your account. If an error occurs
-- when Macie attempts to retrieve and process information about the bucket
-- or the bucket\'s objects, the value for most of these properties is
-- null. Key exceptions are accountId and bucketName. To identify the cause
-- of the error, refer to the errorCode and errorMessage values.
--
-- /See:/ 'newMatchingBucket' smart constructor.
data MatchingBucket = MatchingBucket'
  { -- | The unique identifier for the Amazon Web Services account that owns the
    -- bucket.
    accountId :: Prelude.Maybe Prelude.Text,
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
    -- configured to analyze objects in the bucket, and, if so, the details of
    -- the job that ran most recently.
    jobDetails :: Prelude.Maybe JobDetails,
    -- | The date and time, in UTC and extended ISO 8601 format, when Amazon
    -- Macie most recently performed automated sensitive data discovery for the
    -- bucket. This value is null if automated sensitive data discovery is
    -- currently disabled for your account.
    lastAutomatedDiscoveryTime :: Prelude.Maybe Data.ISO8601,
    -- | The total number of objects in the bucket.
    objectCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of objects in the bucket, grouped by server-side
    -- encryption type. This includes a grouping that reports the total number
    -- of objects that aren\'t encrypted or use client-side encryption.
    objectCountByEncryptionType :: Prelude.Maybe ObjectCountByEncryptionType,
    -- | The current sensitivity score for the bucket, ranging from -1 (no
    -- analysis due to an error) to 100 (sensitive). This value is null if
    -- automated sensitive data discovery is currently disabled for your
    -- account.
    sensitivityScore :: Prelude.Maybe Prelude.Int,
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
    -- | The total number of objects that Amazon Macie can\'t analyze in the
    -- bucket. These objects don\'t use a supported storage class or don\'t
    -- have a file name extension for a supported file or storage format.
    unclassifiableObjectCount :: Prelude.Maybe ObjectLevelStatistics,
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
-- 'accountId', 'matchingBucket_accountId' - The unique identifier for the Amazon Web Services account that owns the
-- bucket.
--
-- 'bucketName', 'matchingBucket_bucketName' - The name of the bucket.
--
-- 'classifiableObjectCount', 'matchingBucket_classifiableObjectCount' - The total number of objects that Amazon Macie can analyze in the bucket.
-- These objects use a supported storage class and have a file name
-- extension for a supported file or storage format.
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
-- 'errorCode', 'matchingBucket_errorCode' - Specifies the error code for an error that prevented Amazon Macie from
-- retrieving and processing information about the bucket and the bucket\'s
-- objects. If this value is ACCESS_DENIED, Macie doesn\'t have permission
-- to retrieve the information. For example, the bucket has a restrictive
-- bucket policy and Amazon S3 denied the request. If this value is null,
-- Macie was able to retrieve and process the information.
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
-- 'lastAutomatedDiscoveryTime', 'matchingBucket_lastAutomatedDiscoveryTime' - The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently performed automated sensitive data discovery for the
-- bucket. This value is null if automated sensitive data discovery is
-- currently disabled for your account.
--
-- 'objectCount', 'matchingBucket_objectCount' - The total number of objects in the bucket.
--
-- 'objectCountByEncryptionType', 'matchingBucket_objectCountByEncryptionType' - The total number of objects in the bucket, grouped by server-side
-- encryption type. This includes a grouping that reports the total number
-- of objects that aren\'t encrypted or use client-side encryption.
--
-- 'sensitivityScore', 'matchingBucket_sensitivityScore' - The current sensitivity score for the bucket, ranging from -1 (no
-- analysis due to an error) to 100 (sensitive). This value is null if
-- automated sensitive data discovery is currently disabled for your
-- account.
--
-- 'sizeInBytes', 'matchingBucket_sizeInBytes' - The total storage size, in bytes, of the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each object in the bucket.
--
-- 'sizeInBytesCompressed', 'matchingBucket_sizeInBytesCompressed' - The total storage size, in bytes, of the objects that are compressed
-- (.gz, .gzip, .zip) files in the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each applicable object
-- in the bucket. This value doesn\'t reflect the storage size of all
-- versions of each applicable object in the bucket.
--
-- 'unclassifiableObjectCount', 'matchingBucket_unclassifiableObjectCount' - The total number of objects that Amazon Macie can\'t analyze in the
-- bucket. These objects don\'t use a supported storage class or don\'t
-- have a file name extension for a supported file or storage format.
--
-- 'unclassifiableObjectSizeInBytes', 'matchingBucket_unclassifiableObjectSizeInBytes' - The total storage size, in bytes, of the objects that Amazon Macie
-- can\'t analyze in the bucket. These objects don\'t use a supported
-- storage class or don\'t have a file name extension for a supported file
-- or storage format.
newMatchingBucket ::
  MatchingBucket
newMatchingBucket =
  MatchingBucket'
    { accountId = Prelude.Nothing,
      bucketName = Prelude.Nothing,
      classifiableObjectCount = Prelude.Nothing,
      classifiableSizeInBytes = Prelude.Nothing,
      errorCode = Prelude.Nothing,
      errorMessage = Prelude.Nothing,
      jobDetails = Prelude.Nothing,
      lastAutomatedDiscoveryTime = Prelude.Nothing,
      objectCount = Prelude.Nothing,
      objectCountByEncryptionType = Prelude.Nothing,
      sensitivityScore = Prelude.Nothing,
      sizeInBytes = Prelude.Nothing,
      sizeInBytesCompressed = Prelude.Nothing,
      unclassifiableObjectCount = Prelude.Nothing,
      unclassifiableObjectSizeInBytes = Prelude.Nothing
    }

-- | The unique identifier for the Amazon Web Services account that owns the
-- bucket.
matchingBucket_accountId :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Text)
matchingBucket_accountId = Lens.lens (\MatchingBucket' {accountId} -> accountId) (\s@MatchingBucket' {} a -> s {accountId = a} :: MatchingBucket)

-- | The name of the bucket.
matchingBucket_bucketName :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Text)
matchingBucket_bucketName = Lens.lens (\MatchingBucket' {bucketName} -> bucketName) (\s@MatchingBucket' {} a -> s {bucketName = a} :: MatchingBucket)

-- | The total number of objects that Amazon Macie can analyze in the bucket.
-- These objects use a supported storage class and have a file name
-- extension for a supported file or storage format.
matchingBucket_classifiableObjectCount :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Integer)
matchingBucket_classifiableObjectCount = Lens.lens (\MatchingBucket' {classifiableObjectCount} -> classifiableObjectCount) (\s@MatchingBucket' {} a -> s {classifiableObjectCount = a} :: MatchingBucket)

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

-- | Specifies the error code for an error that prevented Amazon Macie from
-- retrieving and processing information about the bucket and the bucket\'s
-- objects. If this value is ACCESS_DENIED, Macie doesn\'t have permission
-- to retrieve the information. For example, the bucket has a restrictive
-- bucket policy and Amazon S3 denied the request. If this value is null,
-- Macie was able to retrieve and process the information.
matchingBucket_errorCode :: Lens.Lens' MatchingBucket (Prelude.Maybe BucketMetadataErrorCode)
matchingBucket_errorCode = Lens.lens (\MatchingBucket' {errorCode} -> errorCode) (\s@MatchingBucket' {} a -> s {errorCode = a} :: MatchingBucket)

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

-- | The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently performed automated sensitive data discovery for the
-- bucket. This value is null if automated sensitive data discovery is
-- currently disabled for your account.
matchingBucket_lastAutomatedDiscoveryTime :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.UTCTime)
matchingBucket_lastAutomatedDiscoveryTime = Lens.lens (\MatchingBucket' {lastAutomatedDiscoveryTime} -> lastAutomatedDiscoveryTime) (\s@MatchingBucket' {} a -> s {lastAutomatedDiscoveryTime = a} :: MatchingBucket) Prelude.. Lens.mapping Data._Time

-- | The total number of objects in the bucket.
matchingBucket_objectCount :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Integer)
matchingBucket_objectCount = Lens.lens (\MatchingBucket' {objectCount} -> objectCount) (\s@MatchingBucket' {} a -> s {objectCount = a} :: MatchingBucket)

-- | The total number of objects in the bucket, grouped by server-side
-- encryption type. This includes a grouping that reports the total number
-- of objects that aren\'t encrypted or use client-side encryption.
matchingBucket_objectCountByEncryptionType :: Lens.Lens' MatchingBucket (Prelude.Maybe ObjectCountByEncryptionType)
matchingBucket_objectCountByEncryptionType = Lens.lens (\MatchingBucket' {objectCountByEncryptionType} -> objectCountByEncryptionType) (\s@MatchingBucket' {} a -> s {objectCountByEncryptionType = a} :: MatchingBucket)

-- | The current sensitivity score for the bucket, ranging from -1 (no
-- analysis due to an error) to 100 (sensitive). This value is null if
-- automated sensitive data discovery is currently disabled for your
-- account.
matchingBucket_sensitivityScore :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Int)
matchingBucket_sensitivityScore = Lens.lens (\MatchingBucket' {sensitivityScore} -> sensitivityScore) (\s@MatchingBucket' {} a -> s {sensitivityScore = a} :: MatchingBucket)

-- | The total storage size, in bytes, of the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each object in the
-- bucket. This value doesn\'t reflect the storage size of all versions of
-- each object in the bucket.
matchingBucket_sizeInBytes :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Integer)
matchingBucket_sizeInBytes = Lens.lens (\MatchingBucket' {sizeInBytes} -> sizeInBytes) (\s@MatchingBucket' {} a -> s {sizeInBytes = a} :: MatchingBucket)

-- | The total storage size, in bytes, of the objects that are compressed
-- (.gz, .gzip, .zip) files in the bucket.
--
-- If versioning is enabled for the bucket, Amazon Macie calculates this
-- value based on the size of the latest version of each applicable object
-- in the bucket. This value doesn\'t reflect the storage size of all
-- versions of each applicable object in the bucket.
matchingBucket_sizeInBytesCompressed :: Lens.Lens' MatchingBucket (Prelude.Maybe Prelude.Integer)
matchingBucket_sizeInBytesCompressed = Lens.lens (\MatchingBucket' {sizeInBytesCompressed} -> sizeInBytesCompressed) (\s@MatchingBucket' {} a -> s {sizeInBytesCompressed = a} :: MatchingBucket)

-- | The total number of objects that Amazon Macie can\'t analyze in the
-- bucket. These objects don\'t use a supported storage class or don\'t
-- have a file name extension for a supported file or storage format.
matchingBucket_unclassifiableObjectCount :: Lens.Lens' MatchingBucket (Prelude.Maybe ObjectLevelStatistics)
matchingBucket_unclassifiableObjectCount = Lens.lens (\MatchingBucket' {unclassifiableObjectCount} -> unclassifiableObjectCount) (\s@MatchingBucket' {} a -> s {unclassifiableObjectCount = a} :: MatchingBucket)

-- | The total storage size, in bytes, of the objects that Amazon Macie
-- can\'t analyze in the bucket. These objects don\'t use a supported
-- storage class or don\'t have a file name extension for a supported file
-- or storage format.
matchingBucket_unclassifiableObjectSizeInBytes :: Lens.Lens' MatchingBucket (Prelude.Maybe ObjectLevelStatistics)
matchingBucket_unclassifiableObjectSizeInBytes = Lens.lens (\MatchingBucket' {unclassifiableObjectSizeInBytes} -> unclassifiableObjectSizeInBytes) (\s@MatchingBucket' {} a -> s {unclassifiableObjectSizeInBytes = a} :: MatchingBucket)

instance Data.FromJSON MatchingBucket where
  parseJSON =
    Data.withObject
      "MatchingBucket"
      ( \x ->
          MatchingBucket'
            Prelude.<$> (x Data..:? "accountId")
            Prelude.<*> (x Data..:? "bucketName")
            Prelude.<*> (x Data..:? "classifiableObjectCount")
            Prelude.<*> (x Data..:? "classifiableSizeInBytes")
            Prelude.<*> (x Data..:? "errorCode")
            Prelude.<*> (x Data..:? "errorMessage")
            Prelude.<*> (x Data..:? "jobDetails")
            Prelude.<*> (x Data..:? "lastAutomatedDiscoveryTime")
            Prelude.<*> (x Data..:? "objectCount")
            Prelude.<*> (x Data..:? "objectCountByEncryptionType")
            Prelude.<*> (x Data..:? "sensitivityScore")
            Prelude.<*> (x Data..:? "sizeInBytes")
            Prelude.<*> (x Data..:? "sizeInBytesCompressed")
            Prelude.<*> (x Data..:? "unclassifiableObjectCount")
            Prelude.<*> (x Data..:? "unclassifiableObjectSizeInBytes")
      )

instance Prelude.Hashable MatchingBucket where
  hashWithSalt _salt MatchingBucket' {..} =
    _salt
      `Prelude.hashWithSalt` accountId
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` classifiableObjectCount
      `Prelude.hashWithSalt` classifiableSizeInBytes
      `Prelude.hashWithSalt` errorCode
      `Prelude.hashWithSalt` errorMessage
      `Prelude.hashWithSalt` jobDetails
      `Prelude.hashWithSalt` lastAutomatedDiscoveryTime
      `Prelude.hashWithSalt` objectCount
      `Prelude.hashWithSalt` objectCountByEncryptionType
      `Prelude.hashWithSalt` sensitivityScore
      `Prelude.hashWithSalt` sizeInBytes
      `Prelude.hashWithSalt` sizeInBytesCompressed
      `Prelude.hashWithSalt` unclassifiableObjectCount
      `Prelude.hashWithSalt` unclassifiableObjectSizeInBytes

instance Prelude.NFData MatchingBucket where
  rnf MatchingBucket' {..} =
    Prelude.rnf accountId `Prelude.seq`
      Prelude.rnf bucketName `Prelude.seq`
        Prelude.rnf classifiableObjectCount `Prelude.seq`
          Prelude.rnf classifiableSizeInBytes `Prelude.seq`
            Prelude.rnf errorCode `Prelude.seq`
              Prelude.rnf errorMessage `Prelude.seq`
                Prelude.rnf jobDetails `Prelude.seq`
                  Prelude.rnf lastAutomatedDiscoveryTime `Prelude.seq`
                    Prelude.rnf objectCount `Prelude.seq`
                      Prelude.rnf objectCountByEncryptionType `Prelude.seq`
                        Prelude.rnf sensitivityScore `Prelude.seq`
                          Prelude.rnf sizeInBytes `Prelude.seq`
                            Prelude.rnf sizeInBytesCompressed `Prelude.seq`
                              Prelude.rnf unclassifiableObjectCount `Prelude.seq`
                                Prelude.rnf
                                  unclassifiableObjectSizeInBytes
