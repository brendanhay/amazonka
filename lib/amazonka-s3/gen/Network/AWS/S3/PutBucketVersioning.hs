{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketVersioning
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the versioning state of an existing bucket. To set the versioning state, you must be the bucket owner.
--
-- You can set the versioning state with one of the following values:
-- __Enabled__ —Enables versioning for the objects in the bucket. All objects added to the bucket receive a unique version ID.
-- __Suspended__ —Disables versioning for the objects in the bucket. All objects added to the bucket receive the version ID null.
-- If the versioning state has never been set on a bucket, it has no versioning state; a <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning> request does not return a versioning state value.
-- If the bucket owner enables MFA Delete in the bucket versioning configuration, the bucket owner must include the @x-amz-mfa request@ header and the @Status@ and the @MfaDelete@ request elements in a request to set the versioning state of the bucket.
-- /Important:/ If you have an object expiration lifecycle policy in your non-versioned bucket and you want to maintain the same permanent delete behavior when you enable versioning, you must add a noncurrent expiration policy. The noncurrent expiration lifecycle policy will manage the deletes of the noncurrent object versions in the version-enabled bucket. (A version-enabled bucket maintains one current and zero or more noncurrent object versions.) For more information, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html#lifecycle-and-other-bucket-config Lifecycle and Versioning> .
-- __Related Resources__
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketVersioning.html GetBucketVersioning>
module Network.AWS.S3.PutBucketVersioning
  ( -- * Creating a request
    PutBucketVersioning (..),
    mkPutBucketVersioning,

    -- ** Request lenses
    pbvVersioningConfiguration,
    pbvMFA,
    pbvBucket,
    pbvContentMD5,
    pbvExpectedBucketOwner,

    -- * Destructuring the response
    PutBucketVersioningResponse (..),
    mkPutBucketVersioningResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketVersioning' smart constructor.
data PutBucketVersioning = PutBucketVersioning'
  { -- | Container for setting the versioning state.
    versioningConfiguration :: VersioningConfiguration,
    -- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
    mfa :: Lude.Maybe Lude.Text,
    -- | The bucket name.
    bucket :: BucketName,
    -- | >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
    contentMD5 :: Lude.Maybe Lude.Text,
    -- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
    expectedBucketOwner :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketVersioning' with the minimum fields required to make a request.
--
-- * 'versioningConfiguration' - Container for setting the versioning state.
-- * 'mfa' - The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
-- * 'bucket' - The bucket name.
-- * 'contentMD5' - >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
mkPutBucketVersioning ::
  -- | 'versioningConfiguration'
  VersioningConfiguration ->
  -- | 'bucket'
  BucketName ->
  PutBucketVersioning
mkPutBucketVersioning pVersioningConfiguration_ pBucket_ =
  PutBucketVersioning'
    { versioningConfiguration =
        pVersioningConfiguration_,
      mfa = Lude.Nothing,
      bucket = pBucket_,
      contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing
    }

-- | Container for setting the versioning state.
--
-- /Note:/ Consider using 'versioningConfiguration' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbvVersioningConfiguration :: Lens.Lens' PutBucketVersioning VersioningConfiguration
pbvVersioningConfiguration = Lens.lens (versioningConfiguration :: PutBucketVersioning -> VersioningConfiguration) (\s a -> s {versioningConfiguration = a} :: PutBucketVersioning)
{-# DEPRECATED pbvVersioningConfiguration "Use generic-lens or generic-optics with 'versioningConfiguration' instead." #-}

-- | The concatenation of the authentication device's serial number, a space, and the value that is displayed on your authentication device.
--
-- /Note:/ Consider using 'mfa' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbvMFA :: Lens.Lens' PutBucketVersioning (Lude.Maybe Lude.Text)
pbvMFA = Lens.lens (mfa :: PutBucketVersioning -> Lude.Maybe Lude.Text) (\s a -> s {mfa = a} :: PutBucketVersioning)
{-# DEPRECATED pbvMFA "Use generic-lens or generic-optics with 'mfa' instead." #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbvBucket :: Lens.Lens' PutBucketVersioning BucketName
pbvBucket = Lens.lens (bucket :: PutBucketVersioning -> BucketName) (\s a -> s {bucket = a} :: PutBucketVersioning)
{-# DEPRECATED pbvBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | >The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbvContentMD5 :: Lens.Lens' PutBucketVersioning (Lude.Maybe Lude.Text)
pbvContentMD5 = Lens.lens (contentMD5 :: PutBucketVersioning -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutBucketVersioning)
{-# DEPRECATED pbvContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbvExpectedBucketOwner :: Lens.Lens' PutBucketVersioning (Lude.Maybe Lude.Text)
pbvExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketVersioning -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketVersioning)
{-# DEPRECATED pbvExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

instance Lude.AWSRequest PutBucketVersioning where
  type Rs PutBucketVersioning = PutBucketVersioningResponse
  request = Req.putXML s3Service
  response = Res.receiveNull PutBucketVersioningResponse'

instance Lude.ToElement PutBucketVersioning where
  toElement =
    Lude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}VersioningConfiguration"
      Lude.. versioningConfiguration

instance Lude.ToHeaders PutBucketVersioning where
  toHeaders PutBucketVersioning' {..} =
    Lude.mconcat
      [ "x-amz-mfa" Lude.=# mfa,
        "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutBucketVersioning where
  toPath PutBucketVersioning' {..} =
    Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketVersioning where
  toQuery = Lude.const (Lude.mconcat ["versioning"])

-- | /See:/ 'mkPutBucketVersioningResponse' smart constructor.
data PutBucketVersioningResponse = PutBucketVersioningResponse'
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketVersioningResponse' with the minimum fields required to make a request.
mkPutBucketVersioningResponse ::
  PutBucketVersioningResponse
mkPutBucketVersioningResponse = PutBucketVersioningResponse'
