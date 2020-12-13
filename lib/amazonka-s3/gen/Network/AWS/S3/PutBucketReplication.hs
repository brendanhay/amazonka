{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketReplication
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a replication configuration or replaces an existing one. For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication.html Replication> in the /Amazon S3 Developer Guide/ .
--
-- Specify the replication configuration in the request body. In the replication configuration, you provide the name of the destination bucket where you want Amazon S3 to replicate objects, the IAM role that Amazon S3 can assume to replicate objects on your behalf, and other relevant information.
-- A replication configuration must include at least one rule, and can contain a maximum of 1,000. Each rule identifies a subset of objects to replicate by filtering the objects in the source bucket. To choose additional subsets of objects to replicate, add a rule for each subset. All rules must specify the same destination bucket.
-- To specify a subset of the objects in the source bucket to apply a replication rule to, add the Filter element as a child of the Rule element. You can filter objects based on an object key prefix, one or more object tags, or both. When you add the Filter element in the configuration, you must also add the following elements: @DeleteMarkerReplication@ , @Status@ , and @Priority@ .
-- For information about enabling versioning on a bucket, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/Versioning.html Using Versioning> .
-- By default, a resource owner, in this case the AWS account that created the bucket, can perform this operation. The resource owner can also grant others permissions to perform the operation. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- __Handling Replication of Encrypted Objects__
-- By default, Amazon S3 doesn't replicate objects that are stored at rest using server-side encryption with CMKs stored in AWS KMS. To replicate AWS KMS-encrypted objects, add the following: @SourceSelectionCriteria@ , @SseKmsEncryptedObjects@ , @Status@ , @EncryptionConfiguration@ , and @ReplicaKmsKeyID@ . For information about replication configuration, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/replication-config-for-kms-objects.html Replicating Objects Created with SSE Using CMKs stored in AWS KMS> .
-- For information on @PutBucketReplication@ errors, see <https://docs.aws.amazon.com/AmazonS3/latest/API/ErrorResponses.html#ReplicationErrorCodeList List of replication-related error codes>
-- The following operations are related to @PutBucketReplication@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketReplication.html GetBucketReplication>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketReplication.html DeleteBucketReplication>
module Network.AWS.S3.PutBucketReplication
  ( -- * Creating a request
    PutBucketReplication (..),
    mkPutBucketReplication,

    -- ** Request lenses
    pbrReplicationConfiguration,
    pbrToken,
    pbrBucket,
    pbrContentMD5,
    pbrExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketReplicationResponse (..),
    mkPutBucketReplicationResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketReplication' smart constructor.
data PutBucketReplication = PutBucketReplication'
  { replicationConfiguration :: ReplicationConfiguration,
    -- | A token to allow Object Lock to be enabled for an existing bucket.
    token :: Lude.Maybe Lude.Text,
    -- | The name of the bucket
    bucket :: BucketName,
    -- | The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketReplication' with the minimum fields required to make a request.
--
-- * 'replicationConfiguration' -
-- * 'token' - A token to allow Object Lock to be enabled for an existing bucket.
-- * 'bucket' - The name of the bucket
-- * 'contentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketReplication ::
  -- | 'replicationConfiguration'
  ReplicationConfiguration ->
  -- | 'bucket'
  BucketName ->
  PutBucketReplication
mkPutBucketReplication pReplicationConfiguration_ pBucket_ =
  PutBucketReplication'
    { replicationConfiguration =
        pReplicationConfiguration_,
      token = Lude.Nothing,
      bucket = pBucket_,
      contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'replicationConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrReplicationConfiguration :: Lens.Lens' PutBucketReplication ReplicationConfiguration
pbrReplicationConfiguration = Lens.lens (replicationConfiguration :: PutBucketReplication -> ReplicationConfiguration) (\s a -> s {replicationConfiguration = a} :: PutBucketReplication)
{-# DEPRECATED pbrReplicationConfiguration "Use generic-lens or generic-optics with 'replicationConfiguration' instead." #-}

-- | A token to allow Object Lock to be enabled for an existing bucket.
--
-- /Note:/ Consider using 'token' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrToken :: Lens.Lens' PutBucketReplication (Lude.Maybe Lude.Text)
pbrToken = Lens.lens (token :: PutBucketReplication -> Lude.Maybe Lude.Text) (\s a -> s {token = a} :: PutBucketReplication)
{-# DEPRECATED pbrToken "Use generic-lens or generic-optics with 'token' instead." #-}

-- | The name of the bucket
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrBucket :: Lens.Lens' PutBucketReplication BucketName
pbrBucket = Lens.lens (bucket :: PutBucketReplication -> BucketName) (\s a -> s {bucket = a} :: PutBucketReplication)
{-# DEPRECATED pbrBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrContentMD5 :: Lens.Lens' PutBucketReplication (Lude.Maybe Lude.Text)
pbrContentMD5 = Lens.lens (contentMD5 :: PutBucketReplication -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutBucketReplication)
{-# DEPRECATED pbrContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbrExpectedBucketOwner :: Lens.Lens' PutBucketReplication (Lude.Maybe Lude.Text)
pbrExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketReplication -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketReplication)
{-# DEPRECATED pbrExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutBucketReplication where
  type Rs PutBucketReplication = PutBucketReplicationResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketReplicationResponse'

instance Lude.ToElement PutBucketReplication where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}ReplicationConfiguration"
      Lude.. replicationConfiguration

instance Lude.ToHeaders PutBucketReplication where
  toHeaders PutBucketReplication' {..} =
    Lude.mconcat
      [ "x-amz-bucket-object-lock-token" Lude.=# token,
        "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutBucketReplication where
  toPath PutBucketReplication' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketReplication where
  toQuery = Lude.const (Lude.mconcat ["replication"])

-- | /See:/ 'mkPutBucketReplicationResponse' smart constructor.
data PutBucketReplicationResponse = PutBucketReplicationResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketReplicationResponse' with the minimum fields required to make a request.
mkPutBucketReplicationResponse ::
  PutBucketReplicationResponse
mkPutBucketReplicationResponse = PutBucketReplicationResponse'
