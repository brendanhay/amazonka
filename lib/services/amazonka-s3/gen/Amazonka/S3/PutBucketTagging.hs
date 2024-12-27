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
-- Module      : Amazonka.S3.PutBucketTagging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the tags for a bucket.
--
-- Use tags to organize your Amazon Web Services bill to reflect your own
-- cost structure. To do this, sign up to get your Amazon Web Services
-- account bill with tag key values included. Then, to see the cost of
-- combined resources, organize your billing information according to
-- resources with the same tag key values. For example, you can tag several
-- resources with a specific application name, and then organize your
-- billing information to see the total cost of that application across
-- several services. For more information, see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/cost-alloc-tags.html Cost Allocation and Tagging>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/CostAllocTagging.html Using Cost Allocation in Amazon S3 Bucket Tags>.
--
-- When this operation sets the tags for a bucket, it will overwrite any
-- current tags the bucket already has. You cannot use this operation to
-- add tags to an existing list of tags.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutBucketTagging@ action. The bucket owner has this permission by
-- default and can grant this permission to others. For more information
-- about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
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
--         <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/aws-tag-restrictions.html Amazon Web Services-Generated Cost Allocation Tag Restrictions>.
--
-- -   Error code: @MalformedXMLError@
--
--     -   Description: The XML provided does not match the schema.
--
-- -   Error code: @OperationAbortedError @
--
--     -   Description: A conflicting conditional action is currently in
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
module Amazonka.S3.PutBucketTagging
  ( -- * Creating a Request
    PutBucketTagging (..),
    newPutBucketTagging,

    -- * Request Lenses
    putBucketTagging_checksumAlgorithm,
    putBucketTagging_contentMD5,
    putBucketTagging_expectedBucketOwner,
    putBucketTagging_bucket,
    putBucketTagging_tagging,

    -- * Destructuring the Response
    PutBucketTaggingResponse (..),
    newPutBucketTaggingResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketTagging' smart constructor.
data PutBucketTagging = PutBucketTagging'
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
    -- | The bucket name.
    bucket :: BucketName,
    -- | Container for the @TagSet@ and @Tag@ elements.
    tagging :: Tagging
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketTagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putBucketTagging_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'contentMD5', 'putBucketTagging_contentMD5' - The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketTagging_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
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
    { checksumAlgorithm =
        Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_,
      tagging = pTagging_
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
putBucketTagging_checksumAlgorithm :: Lens.Lens' PutBucketTagging (Prelude.Maybe ChecksumAlgorithm)
putBucketTagging_checksumAlgorithm = Lens.lens (\PutBucketTagging' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutBucketTagging' {} a -> s {checksumAlgorithm = a} :: PutBucketTagging)

-- | The base64-encoded 128-bit MD5 digest of the data. You must use this
-- header as a message integrity check to verify that the request body was
-- not corrupted in transit. For more information, see
-- <http://www.ietf.org/rfc/rfc1864.txt RFC 1864>.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketTagging_contentMD5 :: Lens.Lens' PutBucketTagging (Prelude.Maybe Prelude.Text)
putBucketTagging_contentMD5 = Lens.lens (\PutBucketTagging' {contentMD5} -> contentMD5) (\s@PutBucketTagging' {} a -> s {contentMD5 = a} :: PutBucketTagging)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketTagging_expectedBucketOwner :: Lens.Lens' PutBucketTagging (Prelude.Maybe Prelude.Text)
putBucketTagging_expectedBucketOwner = Lens.lens (\PutBucketTagging' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketTagging' {} a -> s {expectedBucketOwner = a} :: PutBucketTagging)

-- | The bucket name.
putBucketTagging_bucket :: Lens.Lens' PutBucketTagging BucketName
putBucketTagging_bucket = Lens.lens (\PutBucketTagging' {bucket} -> bucket) (\s@PutBucketTagging' {} a -> s {bucket = a} :: PutBucketTagging)

-- | Container for the @TagSet@ and @Tag@ elements.
putBucketTagging_tagging :: Lens.Lens' PutBucketTagging Tagging
putBucketTagging_tagging = Lens.lens (\PutBucketTagging' {tagging} -> tagging) (\s@PutBucketTagging' {} a -> s {tagging = a} :: PutBucketTagging)

instance Core.AWSRequest PutBucketTagging where
  type
    AWSResponse PutBucketTagging =
      PutBucketTaggingResponse
  request overrides =
    Request.contentMD5Header
      Prelude.. Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull PutBucketTaggingResponse'

instance Prelude.Hashable PutBucketTagging where
  hashWithSalt _salt PutBucketTagging' {..} =
    _salt
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` tagging

instance Prelude.NFData PutBucketTagging where
  rnf PutBucketTagging' {..} =
    Prelude.rnf checksumAlgorithm `Prelude.seq`
      Prelude.rnf contentMD5 `Prelude.seq`
        Prelude.rnf expectedBucketOwner `Prelude.seq`
          Prelude.rnf bucket `Prelude.seq`
            Prelude.rnf tagging

instance Data.ToElement PutBucketTagging where
  toElement PutBucketTagging' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
      tagging

instance Data.ToHeaders PutBucketTagging where
  toHeaders PutBucketTagging' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath PutBucketTagging where
  toPath PutBucketTagging' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutBucketTagging where
  toQuery = Prelude.const (Prelude.mconcat ["tagging"])

-- | /See:/ 'newPutBucketTaggingResponse' smart constructor.
data PutBucketTaggingResponse = PutBucketTaggingResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketTaggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketTaggingResponse ::
  PutBucketTaggingResponse
newPutBucketTaggingResponse =
  PutBucketTaggingResponse'

instance Prelude.NFData PutBucketTaggingResponse where
  rnf _ = ()
