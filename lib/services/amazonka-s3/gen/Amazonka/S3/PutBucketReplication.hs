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
-- Module      : Amazonka.S3.PutBucketReplication
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication configuration or replaces an existing one. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication>
-- in the /Amazon S3 User Guide/.
--
-- Specify the replication configuration in the request body. In the
-- replication configuration, you provide the name of the destination
-- bucket or buckets where you want Amazon S3 to replicate objects, the IAM
-- role that Amazon S3 can assume to replicate objects on your behalf, and
-- other relevant information.
--
-- A replication configuration must include at least one rule, and can
-- contain a maximum of 1,000. Each rule identifies a subset of objects to
-- replicate by filtering the objects in the source bucket. To choose
-- additional subsets of objects to replicate, add a rule for each subset.
--
-- To specify a subset of the objects in the source bucket to apply a
-- replication rule to, add the Filter element as a child of the Rule
-- element. You can filter objects based on an object key prefix, one or
-- more object tags, or both. When you add the Filter element in the
-- configuration, you must also add the following elements:
-- @DeleteMarkerReplication@, @Status@, and @Priority@.
--
-- If you are using an earlier version of the replication configuration,
-- Amazon S3 handles replication of delete markers differently. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-add-config.html#replication-backward-compat-considerations Backward Compatibility>.
--
-- For information about enabling versioning on a bucket, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/Versioning.html Using Versioning>.
--
-- __Handling Replication of Encrypted Objects__
--
-- By default, Amazon S3 doesn\'t replicate objects that are stored at rest
-- using server-side encryption with KMS keys. To replicate Amazon Web
-- Services KMS-encrypted objects, add the following:
-- @SourceSelectionCriteria@, @SseKmsEncryptedObjects@, @Status@,
-- @EncryptionConfiguration@, and @ReplicaKmsKeyID@. For information about
-- replication configuration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-config-for-kms-objects.html Replicating Objects Created with SSE Using KMS keys>.
--
-- For information on @PutBucketReplication@ errors, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ReplicationErrorCodeList List of replication-related error codes>
--
-- __Permissions__
--
-- To create a @PutBucketReplication@ request, you must have
-- @s3:PutReplicationConfiguration@ permissions for the bucket.
--
-- By default, a resource owner, in this case the Amazon Web Services
-- account that created the bucket, can perform this operation. The
-- resource owner can also grant others permissions to perform the
-- operation. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- To perform this operation, the user or role performing the action must
-- have the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html iam:PassRole>
-- permission.
--
-- The following operations are related to @PutBucketReplication@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketReplication.html GetBucketReplication>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketReplication.html DeleteBucketReplication>
module Amazonka.S3.PutBucketReplication
  ( -- * Creating a Request
    PutBucketReplication (..),
    newPutBucketReplication,

    -- * Request Lenses
    putBucketReplication_checksumAlgorithm,
    putBucketReplication_contentMD5,
    putBucketReplication_expectedBucketOwner,
    putBucketReplication_token,
    putBucketReplication_bucket,
    putBucketReplication_replicationConfiguration,

    -- * Destructuring the Response
    PutBucketReplicationResponse (..),
    newPutBucketReplicationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketReplication' smart constructor.
data PutBucketReplication = PutBucketReplication'
  { -- | Indicates the algorithm used to create the checksum for the object when
    -- using the SDK. This header will not provide any additional functionality
    -- if not using the SDK. When sending this header, there must be a
    -- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
    -- Otherwise, Amazon S3 fails the request with the HTTP status code
    -- @400 Bad Request@. For more information, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
    -- in the /Amazon S3 User Guide/.
    --
    -- If you provide an individual checksum, Amazon S3 ignores any provided
    -- @ChecksumAlgorithm@ parameter.
    checksumAlgorithm :: Prelude.Maybe ChecksumAlgorithm,
    -- | The base64-encoded 128-bit MD5 digest of the data. You must use this
    -- header as a message integrity check to verify that the request body was
    -- not corrupted in transit. For more information, see
    -- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | A token to allow Object Lock to be enabled for an existing bucket.
    token :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket
    bucket :: BucketName,
    replicationConfiguration :: ReplicationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putBucketReplication_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
-- using the SDK. This header will not provide any additional functionality
-- if not using the SDK. When sending this header, there must be a
-- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
-- Otherwise, Amazon S3 fails the request with the HTTP status code
-- @400 Bad Request@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- If you provide an individual checksum, Amazon S3 ignores any provided
-- @ChecksumAlgorithm@ parameter.
--
-- 'contentMD5', 'putBucketReplication_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketReplication_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'token', 'putBucketReplication_token' - A token to allow Object Lock to be enabled for an existing bucket.
--
-- 'bucket', 'putBucketReplication_bucket' - The name of the bucket
--
-- 'replicationConfiguration', 'putBucketReplication_replicationConfiguration' - Undocumented member.
newPutBucketReplication ::
  -- | 'bucket'
  BucketName ->
  -- | 'replicationConfiguration'
  ReplicationConfiguration ->
  PutBucketReplication
newPutBucketReplication
  pBucket_
  pReplicationConfiguration_ =
    PutBucketReplication'
      { checksumAlgorithm =
          Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        expectedBucketOwner = Prelude.Nothing,
        token = Prelude.Nothing,
        bucket = pBucket_,
        replicationConfiguration =
          pReplicationConfiguration_
      }

-- | Indicates the algorithm used to create the checksum for the object when
-- using the SDK. This header will not provide any additional functionality
-- if not using the SDK. When sending this header, there must be a
-- corresponding @x-amz-checksum@ or @x-amz-trailer@ header sent.
-- Otherwise, Amazon S3 fails the request with the HTTP status code
-- @400 Bad Request@. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/checking-object-integrity.html Checking object integrity>
-- in the /Amazon S3 User Guide/.
--
-- If you provide an individual checksum, Amazon S3 ignores any provided
-- @ChecksumAlgorithm@ parameter.
putBucketReplication_checksumAlgorithm :: Lens.Lens' PutBucketReplication (Prelude.Maybe ChecksumAlgorithm)
putBucketReplication_checksumAlgorithm = Lens.lens (\PutBucketReplication' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutBucketReplication' {} a -> s {checksumAlgorithm = a} :: PutBucketReplication)

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketReplication_contentMD5 :: Lens.Lens' PutBucketReplication (Prelude.Maybe Prelude.Text)
putBucketReplication_contentMD5 = Lens.lens (\PutBucketReplication' {contentMD5} -> contentMD5) (\s@PutBucketReplication' {} a -> s {contentMD5 = a} :: PutBucketReplication)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketReplication_expectedBucketOwner :: Lens.Lens' PutBucketReplication (Prelude.Maybe Prelude.Text)
putBucketReplication_expectedBucketOwner = Lens.lens (\PutBucketReplication' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketReplication' {} a -> s {expectedBucketOwner = a} :: PutBucketReplication)

-- | A token to allow Object Lock to be enabled for an existing bucket.
putBucketReplication_token :: Lens.Lens' PutBucketReplication (Prelude.Maybe Prelude.Text)
putBucketReplication_token = Lens.lens (\PutBucketReplication' {token} -> token) (\s@PutBucketReplication' {} a -> s {token = a} :: PutBucketReplication)

-- | The name of the bucket
putBucketReplication_bucket :: Lens.Lens' PutBucketReplication BucketName
putBucketReplication_bucket = Lens.lens (\PutBucketReplication' {bucket} -> bucket) (\s@PutBucketReplication' {} a -> s {bucket = a} :: PutBucketReplication)

-- | Undocumented member.
putBucketReplication_replicationConfiguration :: Lens.Lens' PutBucketReplication ReplicationConfiguration
putBucketReplication_replicationConfiguration = Lens.lens (\PutBucketReplication' {replicationConfiguration} -> replicationConfiguration) (\s@PutBucketReplication' {} a -> s {replicationConfiguration = a} :: PutBucketReplication)

instance Core.AWSRequest PutBucketReplication where
  type
    AWSResponse PutBucketReplication =
      PutBucketReplicationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull PutBucketReplicationResponse'

instance Prelude.Hashable PutBucketReplication where
  hashWithSalt _salt PutBucketReplication' {..} =
    _salt `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` token
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` replicationConfiguration

instance Prelude.NFData PutBucketReplication where
  rnf PutBucketReplication' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf token
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf replicationConfiguration

instance Data.ToElement PutBucketReplication where
  toElement PutBucketReplication' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}ReplicationConfiguration"
      replicationConfiguration

instance Data.ToHeaders PutBucketReplication where
  toHeaders PutBucketReplication' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-bucket-object-lock-token" Data.=# token
      ]

instance Data.ToPath PutBucketReplication where
  toPath PutBucketReplication' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutBucketReplication where
  toQuery =
    Prelude.const (Prelude.mconcat ["replication"])

-- | /See:/ 'newPutBucketReplicationResponse' smart constructor.
data PutBucketReplicationResponse = PutBucketReplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketReplicationResponse ::
  PutBucketReplicationResponse
newPutBucketReplicationResponse =
  PutBucketReplicationResponse'

instance Prelude.NFData PutBucketReplicationResponse where
  rnf _ = ()
