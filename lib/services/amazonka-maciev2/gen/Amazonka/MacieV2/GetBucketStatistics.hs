{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Amazonka.MacieV2.GetBucketStatistics
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves (queries) aggregated statistical data about all the S3 buckets
-- that Amazon Macie monitors and analyzes for an account.
module Amazonka.MacieV2.GetBucketStatistics
  ( -- * Creating a Request
    GetBucketStatistics (..),
    newGetBucketStatistics,

    -- * Request Lenses
    getBucketStatistics_accountId,

    -- * Destructuring the Response
    GetBucketStatisticsResponse (..),
    newGetBucketStatisticsResponse,

    -- * Response Lenses
    getBucketStatisticsResponse_bucketCount,
    getBucketStatisticsResponse_bucketCountByEffectivePermission,
    getBucketStatisticsResponse_bucketCountByEncryptionType,
    getBucketStatisticsResponse_bucketCountByObjectEncryptionRequirement,
    getBucketStatisticsResponse_bucketCountBySharedAccessType,
    getBucketStatisticsResponse_bucketStatisticsBySensitivity,
    getBucketStatisticsResponse_classifiableObjectCount,
    getBucketStatisticsResponse_classifiableSizeInBytes,
    getBucketStatisticsResponse_lastUpdated,
    getBucketStatisticsResponse_objectCount,
    getBucketStatisticsResponse_sizeInBytes,
    getBucketStatisticsResponse_sizeInBytesCompressed,
    getBucketStatisticsResponse_unclassifiableObjectCount,
    getBucketStatisticsResponse_unclassifiableObjectSizeInBytes,
    getBucketStatisticsResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.MacieV2.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBucketStatistics' smart constructor.
data GetBucketStatistics = GetBucketStatistics'
  { -- | The unique identifier for the Amazon Web Services account.
    accountId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketStatistics' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accountId', 'getBucketStatistics_accountId' - The unique identifier for the Amazon Web Services account.
newGetBucketStatistics ::
  GetBucketStatistics
newGetBucketStatistics =
  GetBucketStatistics' {accountId = Prelude.Nothing}

-- | The unique identifier for the Amazon Web Services account.
getBucketStatistics_accountId :: Lens.Lens' GetBucketStatistics (Prelude.Maybe Prelude.Text)
getBucketStatistics_accountId = Lens.lens (\GetBucketStatistics' {accountId} -> accountId) (\s@GetBucketStatistics' {} a -> s {accountId = a} :: GetBucketStatistics)

instance Core.AWSRequest GetBucketStatistics where
  type
    AWSResponse GetBucketStatistics =
      GetBucketStatisticsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBucketStatisticsResponse'
            Prelude.<$> (x Data..?> "bucketCount")
            Prelude.<*> (x Data..?> "bucketCountByEffectivePermission")
            Prelude.<*> (x Data..?> "bucketCountByEncryptionType")
            Prelude.<*> ( x
                            Data..?> "bucketCountByObjectEncryptionRequirement"
                        )
            Prelude.<*> (x Data..?> "bucketCountBySharedAccessType")
            Prelude.<*> (x Data..?> "bucketStatisticsBySensitivity")
            Prelude.<*> (x Data..?> "classifiableObjectCount")
            Prelude.<*> (x Data..?> "classifiableSizeInBytes")
            Prelude.<*> (x Data..?> "lastUpdated")
            Prelude.<*> (x Data..?> "objectCount")
            Prelude.<*> (x Data..?> "sizeInBytes")
            Prelude.<*> (x Data..?> "sizeInBytesCompressed")
            Prelude.<*> (x Data..?> "unclassifiableObjectCount")
            Prelude.<*> (x Data..?> "unclassifiableObjectSizeInBytes")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketStatistics where
  hashWithSalt _salt GetBucketStatistics' {..} =
    _salt `Prelude.hashWithSalt` accountId

instance Prelude.NFData GetBucketStatistics where
  rnf GetBucketStatistics' {..} = Prelude.rnf accountId

instance Data.ToHeaders GetBucketStatistics where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetBucketStatistics where
  toJSON GetBucketStatistics' {..} =
    Data.object
      ( Prelude.catMaybes
          [("accountId" Data..=) Prelude.<$> accountId]
      )

instance Data.ToPath GetBucketStatistics where
  toPath = Prelude.const "/datasources/s3/statistics"

instance Data.ToQuery GetBucketStatistics where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBucketStatisticsResponse' smart constructor.
data GetBucketStatisticsResponse = GetBucketStatisticsResponse'
  { -- | The total number of buckets.
    bucketCount :: Prelude.Maybe Prelude.Integer,
    -- | The total number of buckets that are publicly accessible due to a
    -- combination of permissions settings for each bucket.
    bucketCountByEffectivePermission :: Prelude.Maybe BucketCountByEffectivePermission,
    -- | The total number of buckets whose settings do or don\'t specify default
    -- server-side encryption behavior for objects that are added to the
    -- buckets.
    bucketCountByEncryptionType :: Prelude.Maybe BucketCountByEncryptionType,
    -- | The total number of buckets whose bucket policies do or don\'t require
    -- server-side encryption of objects when objects are added to the buckets.
    bucketCountByObjectEncryptionRequirement :: Prelude.Maybe BucketCountPolicyAllowsUnencryptedObjectUploads,
    -- | The total number of buckets that are or aren\'t shared with other Amazon
    -- Web Services accounts, Amazon CloudFront origin access identities
    -- (OAIs), or CloudFront origin access controls (OACs).
    bucketCountBySharedAccessType :: Prelude.Maybe BucketCountBySharedAccessType,
    -- | The aggregated sensitive data discovery statistics for the buckets. If
    -- automated sensitive data discovery is currently disabled for your
    -- account, the value for each statistic is 0.
    bucketStatisticsBySensitivity :: Prelude.Maybe BucketStatisticsBySensitivity,
    -- | The total number of objects that Amazon Macie can analyze in the
    -- buckets. These objects use a supported storage class and have a file
    -- name extension for a supported file or storage format.
    classifiableObjectCount :: Prelude.Maybe Prelude.Integer,
    -- | The total storage size, in bytes, of all the objects that Amazon Macie
    -- can analyze in the buckets. These objects use a supported storage class
    -- and have a file name extension for a supported file or storage format.
    --
    -- If versioning is enabled for any of the buckets, this value is based on
    -- the size of the latest version of each applicable object in the buckets.
    -- This value doesn\'t reflect the storage size of all versions of all
    -- applicable objects in the buckets.
    classifiableSizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The date and time, in UTC and extended ISO 8601 format, when Amazon
    -- Macie most recently retrieved bucket or object metadata from Amazon S3
    -- for the buckets.
    lastUpdated :: Prelude.Maybe Data.ISO8601,
    -- | The total number of objects in the buckets.
    objectCount :: Prelude.Maybe Prelude.Integer,
    -- | The total storage size, in bytes, of the buckets.
    --
    -- If versioning is enabled for any of the buckets, this value is based on
    -- the size of the latest version of each object in the buckets. This value
    -- doesn\'t reflect the storage size of all versions of the objects in the
    -- buckets.
    sizeInBytes :: Prelude.Maybe Prelude.Integer,
    -- | The total storage size, in bytes, of the objects that are compressed
    -- (.gz, .gzip, .zip) files in the buckets.
    --
    -- If versioning is enabled for any of the buckets, this value is based on
    -- the size of the latest version of each applicable object in the buckets.
    -- This value doesn\'t reflect the storage size of all versions of the
    -- applicable objects in the buckets.
    sizeInBytesCompressed :: Prelude.Maybe Prelude.Integer,
    -- | The total number of objects that Amazon Macie can\'t analyze in the
    -- buckets. These objects don\'t use a supported storage class or don\'t
    -- have a file name extension for a supported file or storage format.
    unclassifiableObjectCount :: Prelude.Maybe ObjectLevelStatistics,
    -- | The total storage size, in bytes, of the objects that Amazon Macie
    -- can\'t analyze in the buckets. These objects don\'t use a supported
    -- storage class or don\'t have a file name extension for a supported file
    -- or storage format.
    unclassifiableObjectSizeInBytes :: Prelude.Maybe ObjectLevelStatistics,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketStatisticsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketCount', 'getBucketStatisticsResponse_bucketCount' - The total number of buckets.
--
-- 'bucketCountByEffectivePermission', 'getBucketStatisticsResponse_bucketCountByEffectivePermission' - The total number of buckets that are publicly accessible due to a
-- combination of permissions settings for each bucket.
--
-- 'bucketCountByEncryptionType', 'getBucketStatisticsResponse_bucketCountByEncryptionType' - The total number of buckets whose settings do or don\'t specify default
-- server-side encryption behavior for objects that are added to the
-- buckets.
--
-- 'bucketCountByObjectEncryptionRequirement', 'getBucketStatisticsResponse_bucketCountByObjectEncryptionRequirement' - The total number of buckets whose bucket policies do or don\'t require
-- server-side encryption of objects when objects are added to the buckets.
--
-- 'bucketCountBySharedAccessType', 'getBucketStatisticsResponse_bucketCountBySharedAccessType' - The total number of buckets that are or aren\'t shared with other Amazon
-- Web Services accounts, Amazon CloudFront origin access identities
-- (OAIs), or CloudFront origin access controls (OACs).
--
-- 'bucketStatisticsBySensitivity', 'getBucketStatisticsResponse_bucketStatisticsBySensitivity' - The aggregated sensitive data discovery statistics for the buckets. If
-- automated sensitive data discovery is currently disabled for your
-- account, the value for each statistic is 0.
--
-- 'classifiableObjectCount', 'getBucketStatisticsResponse_classifiableObjectCount' - The total number of objects that Amazon Macie can analyze in the
-- buckets. These objects use a supported storage class and have a file
-- name extension for a supported file or storage format.
--
-- 'classifiableSizeInBytes', 'getBucketStatisticsResponse_classifiableSizeInBytes' - The total storage size, in bytes, of all the objects that Amazon Macie
-- can analyze in the buckets. These objects use a supported storage class
-- and have a file name extension for a supported file or storage format.
--
-- If versioning is enabled for any of the buckets, this value is based on
-- the size of the latest version of each applicable object in the buckets.
-- This value doesn\'t reflect the storage size of all versions of all
-- applicable objects in the buckets.
--
-- 'lastUpdated', 'getBucketStatisticsResponse_lastUpdated' - The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently retrieved bucket or object metadata from Amazon S3
-- for the buckets.
--
-- 'objectCount', 'getBucketStatisticsResponse_objectCount' - The total number of objects in the buckets.
--
-- 'sizeInBytes', 'getBucketStatisticsResponse_sizeInBytes' - The total storage size, in bytes, of the buckets.
--
-- If versioning is enabled for any of the buckets, this value is based on
-- the size of the latest version of each object in the buckets. This value
-- doesn\'t reflect the storage size of all versions of the objects in the
-- buckets.
--
-- 'sizeInBytesCompressed', 'getBucketStatisticsResponse_sizeInBytesCompressed' - The total storage size, in bytes, of the objects that are compressed
-- (.gz, .gzip, .zip) files in the buckets.
--
-- If versioning is enabled for any of the buckets, this value is based on
-- the size of the latest version of each applicable object in the buckets.
-- This value doesn\'t reflect the storage size of all versions of the
-- applicable objects in the buckets.
--
-- 'unclassifiableObjectCount', 'getBucketStatisticsResponse_unclassifiableObjectCount' - The total number of objects that Amazon Macie can\'t analyze in the
-- buckets. These objects don\'t use a supported storage class or don\'t
-- have a file name extension for a supported file or storage format.
--
-- 'unclassifiableObjectSizeInBytes', 'getBucketStatisticsResponse_unclassifiableObjectSizeInBytes' - The total storage size, in bytes, of the objects that Amazon Macie
-- can\'t analyze in the buckets. These objects don\'t use a supported
-- storage class or don\'t have a file name extension for a supported file
-- or storage format.
--
-- 'httpStatus', 'getBucketStatisticsResponse_httpStatus' - The response's http status code.
newGetBucketStatisticsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketStatisticsResponse
newGetBucketStatisticsResponse pHttpStatus_ =
  GetBucketStatisticsResponse'
    { bucketCount =
        Prelude.Nothing,
      bucketCountByEffectivePermission =
        Prelude.Nothing,
      bucketCountByEncryptionType = Prelude.Nothing,
      bucketCountByObjectEncryptionRequirement =
        Prelude.Nothing,
      bucketCountBySharedAccessType =
        Prelude.Nothing,
      bucketStatisticsBySensitivity =
        Prelude.Nothing,
      classifiableObjectCount = Prelude.Nothing,
      classifiableSizeInBytes = Prelude.Nothing,
      lastUpdated = Prelude.Nothing,
      objectCount = Prelude.Nothing,
      sizeInBytes = Prelude.Nothing,
      sizeInBytesCompressed = Prelude.Nothing,
      unclassifiableObjectCount = Prelude.Nothing,
      unclassifiableObjectSizeInBytes =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The total number of buckets.
getBucketStatisticsResponse_bucketCount :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe Prelude.Integer)
getBucketStatisticsResponse_bucketCount = Lens.lens (\GetBucketStatisticsResponse' {bucketCount} -> bucketCount) (\s@GetBucketStatisticsResponse' {} a -> s {bucketCount = a} :: GetBucketStatisticsResponse)

-- | The total number of buckets that are publicly accessible due to a
-- combination of permissions settings for each bucket.
getBucketStatisticsResponse_bucketCountByEffectivePermission :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe BucketCountByEffectivePermission)
getBucketStatisticsResponse_bucketCountByEffectivePermission = Lens.lens (\GetBucketStatisticsResponse' {bucketCountByEffectivePermission} -> bucketCountByEffectivePermission) (\s@GetBucketStatisticsResponse' {} a -> s {bucketCountByEffectivePermission = a} :: GetBucketStatisticsResponse)

-- | The total number of buckets whose settings do or don\'t specify default
-- server-side encryption behavior for objects that are added to the
-- buckets.
getBucketStatisticsResponse_bucketCountByEncryptionType :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe BucketCountByEncryptionType)
getBucketStatisticsResponse_bucketCountByEncryptionType = Lens.lens (\GetBucketStatisticsResponse' {bucketCountByEncryptionType} -> bucketCountByEncryptionType) (\s@GetBucketStatisticsResponse' {} a -> s {bucketCountByEncryptionType = a} :: GetBucketStatisticsResponse)

-- | The total number of buckets whose bucket policies do or don\'t require
-- server-side encryption of objects when objects are added to the buckets.
getBucketStatisticsResponse_bucketCountByObjectEncryptionRequirement :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe BucketCountPolicyAllowsUnencryptedObjectUploads)
getBucketStatisticsResponse_bucketCountByObjectEncryptionRequirement = Lens.lens (\GetBucketStatisticsResponse' {bucketCountByObjectEncryptionRequirement} -> bucketCountByObjectEncryptionRequirement) (\s@GetBucketStatisticsResponse' {} a -> s {bucketCountByObjectEncryptionRequirement = a} :: GetBucketStatisticsResponse)

-- | The total number of buckets that are or aren\'t shared with other Amazon
-- Web Services accounts, Amazon CloudFront origin access identities
-- (OAIs), or CloudFront origin access controls (OACs).
getBucketStatisticsResponse_bucketCountBySharedAccessType :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe BucketCountBySharedAccessType)
getBucketStatisticsResponse_bucketCountBySharedAccessType = Lens.lens (\GetBucketStatisticsResponse' {bucketCountBySharedAccessType} -> bucketCountBySharedAccessType) (\s@GetBucketStatisticsResponse' {} a -> s {bucketCountBySharedAccessType = a} :: GetBucketStatisticsResponse)

-- | The aggregated sensitive data discovery statistics for the buckets. If
-- automated sensitive data discovery is currently disabled for your
-- account, the value for each statistic is 0.
getBucketStatisticsResponse_bucketStatisticsBySensitivity :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe BucketStatisticsBySensitivity)
getBucketStatisticsResponse_bucketStatisticsBySensitivity = Lens.lens (\GetBucketStatisticsResponse' {bucketStatisticsBySensitivity} -> bucketStatisticsBySensitivity) (\s@GetBucketStatisticsResponse' {} a -> s {bucketStatisticsBySensitivity = a} :: GetBucketStatisticsResponse)

-- | The total number of objects that Amazon Macie can analyze in the
-- buckets. These objects use a supported storage class and have a file
-- name extension for a supported file or storage format.
getBucketStatisticsResponse_classifiableObjectCount :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe Prelude.Integer)
getBucketStatisticsResponse_classifiableObjectCount = Lens.lens (\GetBucketStatisticsResponse' {classifiableObjectCount} -> classifiableObjectCount) (\s@GetBucketStatisticsResponse' {} a -> s {classifiableObjectCount = a} :: GetBucketStatisticsResponse)

-- | The total storage size, in bytes, of all the objects that Amazon Macie
-- can analyze in the buckets. These objects use a supported storage class
-- and have a file name extension for a supported file or storage format.
--
-- If versioning is enabled for any of the buckets, this value is based on
-- the size of the latest version of each applicable object in the buckets.
-- This value doesn\'t reflect the storage size of all versions of all
-- applicable objects in the buckets.
getBucketStatisticsResponse_classifiableSizeInBytes :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe Prelude.Integer)
getBucketStatisticsResponse_classifiableSizeInBytes = Lens.lens (\GetBucketStatisticsResponse' {classifiableSizeInBytes} -> classifiableSizeInBytes) (\s@GetBucketStatisticsResponse' {} a -> s {classifiableSizeInBytes = a} :: GetBucketStatisticsResponse)

-- | The date and time, in UTC and extended ISO 8601 format, when Amazon
-- Macie most recently retrieved bucket or object metadata from Amazon S3
-- for the buckets.
getBucketStatisticsResponse_lastUpdated :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe Prelude.UTCTime)
getBucketStatisticsResponse_lastUpdated = Lens.lens (\GetBucketStatisticsResponse' {lastUpdated} -> lastUpdated) (\s@GetBucketStatisticsResponse' {} a -> s {lastUpdated = a} :: GetBucketStatisticsResponse) Prelude.. Lens.mapping Data._Time

-- | The total number of objects in the buckets.
getBucketStatisticsResponse_objectCount :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe Prelude.Integer)
getBucketStatisticsResponse_objectCount = Lens.lens (\GetBucketStatisticsResponse' {objectCount} -> objectCount) (\s@GetBucketStatisticsResponse' {} a -> s {objectCount = a} :: GetBucketStatisticsResponse)

-- | The total storage size, in bytes, of the buckets.
--
-- If versioning is enabled for any of the buckets, this value is based on
-- the size of the latest version of each object in the buckets. This value
-- doesn\'t reflect the storage size of all versions of the objects in the
-- buckets.
getBucketStatisticsResponse_sizeInBytes :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe Prelude.Integer)
getBucketStatisticsResponse_sizeInBytes = Lens.lens (\GetBucketStatisticsResponse' {sizeInBytes} -> sizeInBytes) (\s@GetBucketStatisticsResponse' {} a -> s {sizeInBytes = a} :: GetBucketStatisticsResponse)

-- | The total storage size, in bytes, of the objects that are compressed
-- (.gz, .gzip, .zip) files in the buckets.
--
-- If versioning is enabled for any of the buckets, this value is based on
-- the size of the latest version of each applicable object in the buckets.
-- This value doesn\'t reflect the storage size of all versions of the
-- applicable objects in the buckets.
getBucketStatisticsResponse_sizeInBytesCompressed :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe Prelude.Integer)
getBucketStatisticsResponse_sizeInBytesCompressed = Lens.lens (\GetBucketStatisticsResponse' {sizeInBytesCompressed} -> sizeInBytesCompressed) (\s@GetBucketStatisticsResponse' {} a -> s {sizeInBytesCompressed = a} :: GetBucketStatisticsResponse)

-- | The total number of objects that Amazon Macie can\'t analyze in the
-- buckets. These objects don\'t use a supported storage class or don\'t
-- have a file name extension for a supported file or storage format.
getBucketStatisticsResponse_unclassifiableObjectCount :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe ObjectLevelStatistics)
getBucketStatisticsResponse_unclassifiableObjectCount = Lens.lens (\GetBucketStatisticsResponse' {unclassifiableObjectCount} -> unclassifiableObjectCount) (\s@GetBucketStatisticsResponse' {} a -> s {unclassifiableObjectCount = a} :: GetBucketStatisticsResponse)

-- | The total storage size, in bytes, of the objects that Amazon Macie
-- can\'t analyze in the buckets. These objects don\'t use a supported
-- storage class or don\'t have a file name extension for a supported file
-- or storage format.
getBucketStatisticsResponse_unclassifiableObjectSizeInBytes :: Lens.Lens' GetBucketStatisticsResponse (Prelude.Maybe ObjectLevelStatistics)
getBucketStatisticsResponse_unclassifiableObjectSizeInBytes = Lens.lens (\GetBucketStatisticsResponse' {unclassifiableObjectSizeInBytes} -> unclassifiableObjectSizeInBytes) (\s@GetBucketStatisticsResponse' {} a -> s {unclassifiableObjectSizeInBytes = a} :: GetBucketStatisticsResponse)

-- | The response's http status code.
getBucketStatisticsResponse_httpStatus :: Lens.Lens' GetBucketStatisticsResponse Prelude.Int
getBucketStatisticsResponse_httpStatus = Lens.lens (\GetBucketStatisticsResponse' {httpStatus} -> httpStatus) (\s@GetBucketStatisticsResponse' {} a -> s {httpStatus = a} :: GetBucketStatisticsResponse)

instance Prelude.NFData GetBucketStatisticsResponse where
  rnf GetBucketStatisticsResponse' {..} =
    Prelude.rnf bucketCount
      `Prelude.seq` Prelude.rnf bucketCountByEffectivePermission
      `Prelude.seq` Prelude.rnf bucketCountByEncryptionType
      `Prelude.seq` Prelude.rnf bucketCountByObjectEncryptionRequirement
      `Prelude.seq` Prelude.rnf bucketCountBySharedAccessType
      `Prelude.seq` Prelude.rnf bucketStatisticsBySensitivity
      `Prelude.seq` Prelude.rnf classifiableObjectCount
      `Prelude.seq` Prelude.rnf classifiableSizeInBytes
      `Prelude.seq` Prelude.rnf lastUpdated
      `Prelude.seq` Prelude.rnf objectCount
      `Prelude.seq` Prelude.rnf sizeInBytes
      `Prelude.seq` Prelude.rnf sizeInBytesCompressed
      `Prelude.seq` Prelude.rnf unclassifiableObjectCount
      `Prelude.seq` Prelude.rnf
        unclassifiableObjectSizeInBytes
      `Prelude.seq` Prelude.rnf httpStatus
