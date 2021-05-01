{-# LANGUAGE DeriveDataTypeable #-}
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
-- Module      : Network.AWS.S3.PutBucketReplication
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication configuration or replaces an existing one. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication>
-- in the /Amazon S3 Developer Guide/.
--
-- To perform this operation, the user or role performing the operation
-- must have the
-- <https://docs.aws.amazon.com/IAM/latest/UserGuide/id_roles_use_passrole.html iam:PassRole>
-- permission.
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
-- By default, a resource owner, in this case the AWS account that created
-- the bucket, can perform this operation. The resource owner can also
-- grant others permissions to perform the operation. For more information
-- about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- __Handling Replication of Encrypted Objects__
--
-- By default, Amazon S3 doesn\'t replicate objects that are stored at rest
-- using server-side encryption with CMKs stored in AWS KMS. To replicate
-- AWS KMS-encrypted objects, add the following: @SourceSelectionCriteria@,
-- @SseKmsEncryptedObjects@, @Status@, @EncryptionConfiguration@, and
-- @ReplicaKmsKeyID@. For information about replication configuration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-config-for-kms-objects.html Replicating Objects Created with SSE Using CMKs stored in AWS KMS>.
--
-- For information on @PutBucketReplication@ errors, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ReplicationErrorCodeList List of replication-related error codes>
--
-- The following operations are related to @PutBucketReplication@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketReplication.html GetBucketReplication>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketReplication.html DeleteBucketReplication>
module Network.AWS.S3.PutBucketReplication
  ( -- * Creating a Request
    PutBucketReplication (..),
    newPutBucketReplication,

    -- * Request Lenses
    putBucketReplication_expectedBucketOwner,
    putBucketReplication_contentMD5,
    putBucketReplication_token,
    putBucketReplication_bucket,
    putBucketReplication_replicationConfiguration,

    -- * Destructuring the Response
    PutBucketReplicationResponse (..),
    newPutBucketReplicationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutBucketReplication' smart constructor.
data PutBucketReplication = PutBucketReplication'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded 128-bit MD5 digest of the data. You must use this
    -- header as a message integrity check to verify that the request body was
    -- not corrupted in transit. For more information, see
    -- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS
    -- SDKs, this field is calculated automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | A token to allow Object Lock to be enabled for an existing bucket.
    token :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket
    bucket :: BucketName,
    replicationConfiguration :: ReplicationConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketReplication' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketReplication_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentMD5', 'putBucketReplication_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
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
      { expectedBucketOwner =
          Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        token = Prelude.Nothing,
        bucket = pBucket_,
        replicationConfiguration =
          pReplicationConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketReplication_expectedBucketOwner :: Lens.Lens' PutBucketReplication (Prelude.Maybe Prelude.Text)
putBucketReplication_expectedBucketOwner = Lens.lens (\PutBucketReplication' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketReplication' {} a -> s {expectedBucketOwner = a} :: PutBucketReplication)

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putBucketReplication_contentMD5 :: Lens.Lens' PutBucketReplication (Prelude.Maybe Prelude.Text)
putBucketReplication_contentMD5 = Lens.lens (\PutBucketReplication' {contentMD5} -> contentMD5) (\s@PutBucketReplication' {} a -> s {contentMD5 = a} :: PutBucketReplication)

-- | A token to allow Object Lock to be enabled for an existing bucket.
putBucketReplication_token :: Lens.Lens' PutBucketReplication (Prelude.Maybe Prelude.Text)
putBucketReplication_token = Lens.lens (\PutBucketReplication' {token} -> token) (\s@PutBucketReplication' {} a -> s {token = a} :: PutBucketReplication)

-- | The name of the bucket
putBucketReplication_bucket :: Lens.Lens' PutBucketReplication BucketName
putBucketReplication_bucket = Lens.lens (\PutBucketReplication' {bucket} -> bucket) (\s@PutBucketReplication' {} a -> s {bucket = a} :: PutBucketReplication)

-- | Undocumented member.
putBucketReplication_replicationConfiguration :: Lens.Lens' PutBucketReplication ReplicationConfiguration
putBucketReplication_replicationConfiguration = Lens.lens (\PutBucketReplication' {replicationConfiguration} -> replicationConfiguration) (\s@PutBucketReplication' {} a -> s {replicationConfiguration = a} :: PutBucketReplication)

instance Prelude.AWSRequest PutBucketReplication where
  type
    Rs PutBucketReplication =
      PutBucketReplicationResponse
  request = Request.putXML defaultService
  response =
    Response.receiveNull PutBucketReplicationResponse'

instance Prelude.Hashable PutBucketReplication

instance Prelude.NFData PutBucketReplication

instance Prelude.ToElement PutBucketReplication where
  toElement PutBucketReplication' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}ReplicationConfiguration"
      replicationConfiguration

instance Prelude.ToHeaders PutBucketReplication where
  toHeaders PutBucketReplication' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "Content-MD5" Prelude.=# contentMD5,
        "x-amz-bucket-object-lock-token" Prelude.=# token
      ]

instance Prelude.ToPath PutBucketReplication where
  toPath PutBucketReplication' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery PutBucketReplication where
  toQuery =
    Prelude.const (Prelude.mconcat ["replication"])

-- | /See:/ 'newPutBucketReplicationResponse' smart constructor.
data PutBucketReplicationResponse = PutBucketReplicationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketReplicationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketReplicationResponse ::
  PutBucketReplicationResponse
newPutBucketReplicationResponse =
  PutBucketReplicationResponse'

instance Prelude.NFData PutBucketReplicationResponse
