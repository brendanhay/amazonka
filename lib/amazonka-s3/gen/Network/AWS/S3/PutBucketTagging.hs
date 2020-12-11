{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.S3.PutBucketTagging
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the tags for a bucket.
--
-- Use tags to organize your AWS bill to reflect your own cost structure. To do this, sign up to get your AWS account bill with tag key values included. Then, to see the cost of combined resources, organize your billing information according to resources with the same tag key values. For example, you can tag several resources with a specific application name, and then organize your billing information to see the total cost of that application across several services. For more information, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Cost Allocation and Tagging> .
-- To use this operation, you must have permissions to perform the @s3:PutBucketTagging@ action. The bucket owner has this permission by default and can grant this permission to others. For more information about permissions, see <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations> and <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources> .
-- @PutBucketTagging@ has the following special errors:
--
--     * Error code: @InvalidTagError@
--
--     * Description: The tag provided was not a valid tag. This error can occur if the tag did not pass input validation. For information about tag restrictions, see <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions> and <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/aws-tag-restrictions.html AWS-Generated Cost Allocation Tag Restrictions> .
--
--
--
--
--     * Error code: @MalformedXMLError@
--
--     * Description: The XML provided does not match the schema.
--
--
--
--
--     * Error code: @OperationAbortedError @
--
--     * Description: A conflicting conditional operation is currently in progress against this resource. Please try again.
--
--
--
--
--     * Error code: @InternalError@
--
--     * Description: The service was unable to apply the provided tag to the bucket.
--
--
--
--
-- The following operations are related to @PutBucketTagging@ :
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketTagging.html GetBucketTagging>
--
--
--     * <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketTagging.html DeleteBucketTagging>
module Network.AWS.S3.PutBucketTagging
  ( -- * Creating a request
    PutBucketTagging (..),
    mkPutBucketTagging,

    -- ** Request lenses
    pbtContentMD5,
    pbtExpectedBucketOwner,
    pbtBucket,
    pbtTagging,

    -- * Destructuring the response
    PutBucketTaggingResponse (..),
    mkPutBucketTaggingResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res
import Network.AWS.S3.Types

-- | /See:/ 'mkPutBucketTagging' smart constructor.
data PutBucketTagging = PutBucketTagging'
  { contentMD5 ::
      Lude.Maybe Lude.Text,
    expectedBucketOwner :: Lude.Maybe Lude.Text,
    bucket :: BucketName,
    tagging :: Tagging
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketTagging' with the minimum fields required to make a request.
--
-- * 'bucket' - The bucket name.
-- * 'contentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
-- * 'expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
-- * 'tagging' - Container for the @TagSet@ and @Tag@ elements.
mkPutBucketTagging ::
  -- | 'bucket'
  BucketName ->
  -- | 'tagging'
  Tagging ->
  PutBucketTagging
mkPutBucketTagging pBucket_ pTagging_ =
  PutBucketTagging'
    { contentMD5 = Lude.Nothing,
      expectedBucketOwner = Lude.Nothing,
      bucket = pBucket_,
      tagging = pTagging_
    }

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this header as a message integrity check to verify that the request body was not corrupted in transit. For more information, see <http://www.ietf.org/rfc/rfc1864.txt RFC 1864> .
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS SDKs, this field is calculated automatically.
--
-- /Note:/ Consider using 'contentMD5' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbtContentMD5 :: Lens.Lens' PutBucketTagging (Lude.Maybe Lude.Text)
pbtContentMD5 = Lens.lens (contentMD5 :: PutBucketTagging -> Lude.Maybe Lude.Text) (\s a -> s {contentMD5 = a} :: PutBucketTagging)
{-# DEPRECATED pbtContentMD5 "Use generic-lens or generic-optics with 'contentMD5' instead." #-}

-- | The account id of the expected bucket owner. If the bucket is owned by a different account, the request will fail with an HTTP @403 (Access Denied)@ error.
--
-- /Note:/ Consider using 'expectedBucketOwner' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbtExpectedBucketOwner :: Lens.Lens' PutBucketTagging (Lude.Maybe Lude.Text)
pbtExpectedBucketOwner = Lens.lens (expectedBucketOwner :: PutBucketTagging -> Lude.Maybe Lude.Text) (\s a -> s {expectedBucketOwner = a} :: PutBucketTagging)
{-# DEPRECATED pbtExpectedBucketOwner "Use generic-lens or generic-optics with 'expectedBucketOwner' instead." #-}

-- | The bucket name.
--
-- /Note:/ Consider using 'bucket' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbtBucket :: Lens.Lens' PutBucketTagging BucketName
pbtBucket = Lens.lens (bucket :: PutBucketTagging -> BucketName) (\s a -> s {bucket = a} :: PutBucketTagging)
{-# DEPRECATED pbtBucket "Use generic-lens or generic-optics with 'bucket' instead." #-}

-- | Container for the @TagSet@ and @Tag@ elements.
--
-- /Note:/ Consider using 'tagging' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
pbtTagging :: Lens.Lens' PutBucketTagging Tagging
pbtTagging = Lens.lens (tagging :: PutBucketTagging -> Tagging) (\s a -> s {tagging = a} :: PutBucketTagging)
{-# DEPRECATED pbtTagging "Use generic-lens or generic-optics with 'tagging' instead." #-}

instance Lude.AWSRequest PutBucketTagging where
  type Rs PutBucketTagging = PutBucketTaggingResponse
  request = contentMD5Header Lude.. Req.putXML s3Service
  response = Res.receiveNull PutBucketTaggingResponse'

instance Lude.ToElement PutBucketTagging where
  toElement =
    Lude.mkElement "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
      Lude.. tagging

instance Lude.ToHeaders PutBucketTagging where
  toHeaders PutBucketTagging' {..} =
    Lude.mconcat
      [ "Content-MD5" Lude.=# contentMD5,
        "x-amz-expected-bucket-owner" Lude.=# expectedBucketOwner
      ]

instance Lude.ToPath PutBucketTagging where
  toPath PutBucketTagging' {..} = Lude.mconcat ["/", Lude.toBS bucket]

instance Lude.ToQuery PutBucketTagging where
  toQuery = Lude.const (Lude.mconcat ["tagging"])

-- | /See:/ 'mkPutBucketTaggingResponse' smart constructor.
data PutBucketTaggingResponse = PutBucketTaggingResponse'
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'PutBucketTaggingResponse' with the minimum fields required to make a request.
mkPutBucketTaggingResponse ::
  PutBucketTaggingResponse
mkPutBucketTaggingResponse = PutBucketTaggingResponse'
