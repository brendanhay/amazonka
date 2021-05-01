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
-- Module      : Network.AWS.S3.PutBucketTagging
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the tags for a bucket.
--
-- Use tags to organize your AWS bill to reflect your own cost structure.
-- To do this, sign up to get your AWS account bill with tag key values
-- included. Then, to see the cost of combined resources, organize your
-- billing information according to resources with the same tag key values.
-- For example, you can tag several resources with a specific application
-- name, and then organize your billing information to see the total cost
-- of that application across several services. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Cost Allocation and Tagging>.
--
-- Within a bucket, if you add a tag that has the same key as an existing
-- tag, the new value overwrites the old value. For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/CostAllocTagging.html Using Cost Allocation in Amazon S3 Bucket Tags>.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutBucketTagging@ action. The bucket owner has this permission by
-- default and can grant this permission to others. For more information
-- about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- @PutBucketTagging@ has the following special errors:
--
-- -   Error code: @InvalidTagError@
--
--     -   Description: The tag provided was not a valid tag. This error
--         can occur if the tag did not pass input validation. For
--         information about tag restrictions, see
--         <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html User-Defined Tag Restrictions>
--         and
--         <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/aws-tag-restrictions.html AWS-Generated Cost Allocation Tag Restrictions>.
--
-- -   Error code: @MalformedXMLError@
--
--     -   Description: The XML provided does not match the schema.
--
-- -   Error code: @OperationAbortedError @
--
--     -   Description: A conflicting conditional operation is currently in
--         progress against this resource. Please try again.
--
-- -   Error code: @InternalError@
--
--     -   Description: The service was unable to apply the provided tag to
--         the bucket.
--
-- The following operations are related to @PutBucketTagging@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketTagging.html GetBucketTagging>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketTagging.html DeleteBucketTagging>
module Network.AWS.S3.PutBucketTagging
  ( -- * Creating a Request
    PutBucketTagging (..),
    newPutBucketTagging,

    -- * Request Lenses
    putBucketTagging_expectedBucketOwner,
    putBucketTagging_contentMD5,
    putBucketTagging_bucket,
    putBucketTagging_tagging,

    -- * Destructuring the Response
    PutBucketTaggingResponse (..),
    newPutBucketTaggingResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutBucketTagging' smart constructor.
data PutBucketTagging = PutBucketTagging'
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
    -- | The bucket name.
    bucket :: BucketName,
    -- | Container for the @TagSet@ and @Tag@ elements.
    tagging :: Tagging
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketTagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketTagging_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentMD5', 'putBucketTagging_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
--
-- 'bucket', 'putBucketTagging_bucket' - The bucket name.
--
-- 'tagging', 'putBucketTagging_tagging' - Container for the @TagSet@ and @Tag@ elements.
newPutBucketTagging ::
  -- | 'bucket'
  BucketName ->
  -- | 'tagging'
  Tagging ->
  PutBucketTagging
newPutBucketTagging pBucket_ pTagging_ =
  PutBucketTagging'
    { expectedBucketOwner =
        Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      bucket = pBucket_,
      tagging = pTagging_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketTagging_expectedBucketOwner :: Lens.Lens' PutBucketTagging (Prelude.Maybe Prelude.Text)
putBucketTagging_expectedBucketOwner = Lens.lens (\PutBucketTagging' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketTagging' {} a -> s {expectedBucketOwner = a} :: PutBucketTagging)

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putBucketTagging_contentMD5 :: Lens.Lens' PutBucketTagging (Prelude.Maybe Prelude.Text)
putBucketTagging_contentMD5 = Lens.lens (\PutBucketTagging' {contentMD5} -> contentMD5) (\s@PutBucketTagging' {} a -> s {contentMD5 = a} :: PutBucketTagging)

-- | The bucket name.
putBucketTagging_bucket :: Lens.Lens' PutBucketTagging BucketName
putBucketTagging_bucket = Lens.lens (\PutBucketTagging' {bucket} -> bucket) (\s@PutBucketTagging' {} a -> s {bucket = a} :: PutBucketTagging)

-- | Container for the @TagSet@ and @Tag@ elements.
putBucketTagging_tagging :: Lens.Lens' PutBucketTagging Tagging
putBucketTagging_tagging = Lens.lens (\PutBucketTagging' {tagging} -> tagging) (\s@PutBucketTagging' {} a -> s {tagging = a} :: PutBucketTagging)

instance Prelude.AWSRequest PutBucketTagging where
  type Rs PutBucketTagging = PutBucketTaggingResponse
  request =
    Request.contentMD5Header
      Prelude.. Request.putXML defaultService
  response =
    Response.receiveNull PutBucketTaggingResponse'

instance Prelude.Hashable PutBucketTagging

instance Prelude.NFData PutBucketTagging

instance Prelude.ToElement PutBucketTagging where
  toElement PutBucketTagging' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
      tagging

instance Prelude.ToHeaders PutBucketTagging where
  toHeaders PutBucketTagging' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "Content-MD5" Prelude.=# contentMD5
      ]

instance Prelude.ToPath PutBucketTagging where
  toPath PutBucketTagging' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery PutBucketTagging where
  toQuery = Prelude.const (Prelude.mconcat ["tagging"])

-- | /See:/ 'newPutBucketTaggingResponse' smart constructor.
data PutBucketTaggingResponse = PutBucketTaggingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketTaggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketTaggingResponse ::
  PutBucketTaggingResponse
newPutBucketTaggingResponse =
  PutBucketTaggingResponse'

instance Prelude.NFData PutBucketTaggingResponse
