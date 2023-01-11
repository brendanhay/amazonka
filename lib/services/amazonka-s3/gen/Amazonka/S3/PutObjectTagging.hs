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
-- Module      : Amazonka.S3.PutObjectTagging
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the supplied tag-set to an object that already exists in a bucket.
--
-- A tag is a key-value pair. You can associate tags with an object by
-- sending a PUT request against the tagging subresource that is associated
-- with the object. You can retrieve tags by sending a GET request. For
-- more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging>.
--
-- For tagging-related restrictions related to characters and encodings,
-- see
-- <https://docs.aws.amazon.com/awsaccountbilling/latest/aboutv2/allocation-tag-restrictions.html Tag Restrictions>.
-- Note that Amazon S3 limits the maximum number of tags to 10 tags per
-- object.
--
-- To use this operation, you must have permission to perform the
-- @s3:PutObjectTagging@ action. By default, the bucket owner has this
-- permission and can grant this permission to others.
--
-- To put tags of any other version, use the @versionId@ query parameter.
-- You also need permission for the @s3:PutObjectVersionTagging@ action.
--
-- For information about the Amazon S3 object tagging feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging>.
--
-- __Special Errors__
--
-- -   -   /Code: InvalidTagError/
--
--     -   /Cause: The tag provided was not a valid tag. This error can
--         occur if the tag did not pass input validation. For more
--         information, see
--         <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-tagging.html Object Tagging>./
--
-- -   -   /Code: MalformedXMLError/
--
--     -   /Cause: The XML provided does not match the schema./
--
-- -   -   /Code: OperationAbortedError/
--
--     -   /Cause: A conflicting conditional action is currently in
--         progress against this resource. Please try again./
--
-- -   -   /Code: InternalError/
--
--     -   /Cause: The service was unable to apply the provided tag to the
--         object./
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetObjectTagging.html GetObjectTagging>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteObjectTagging.html DeleteObjectTagging>
module Amazonka.S3.PutObjectTagging
  ( -- * Creating a Request
    PutObjectTagging (..),
    newPutObjectTagging,

    -- * Request Lenses
    putObjectTagging_checksumAlgorithm,
    putObjectTagging_contentMD5,
    putObjectTagging_expectedBucketOwner,
    putObjectTagging_requestPayer,
    putObjectTagging_versionId,
    putObjectTagging_bucket,
    putObjectTagging_key,
    putObjectTagging_tagging,

    -- * Destructuring the Response
    PutObjectTaggingResponse (..),
    newPutObjectTaggingResponse,

    -- * Response Lenses
    putObjectTaggingResponse_versionId,
    putObjectTaggingResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutObjectTagging' smart constructor.
data PutObjectTagging = PutObjectTagging'
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
    -- | The MD5 hash for the request body.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The versionId of the object that the tag-set will be added to.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The bucket name containing the object.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    --
    -- When using this action with Amazon S3 on Outposts, you must direct
    -- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
    -- takes the form
    -- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
    -- When using this action with S3 on Outposts through the Amazon Web
    -- Services SDKs, you provide the Outposts bucket ARN in place of the
    -- bucket name. For more information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | Name of the object key.
    key :: ObjectKey,
    -- | Container for the @TagSet@ and @Tag@ elements
    tagging :: Tagging
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObjectTagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putObjectTagging_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'contentMD5', 'putObjectTagging_contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putObjectTagging_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'requestPayer', 'putObjectTagging_requestPayer' - Undocumented member.
--
-- 'versionId', 'putObjectTagging_versionId' - The versionId of the object that the tag-set will be added to.
--
-- 'bucket', 'putObjectTagging_bucket' - The bucket name containing the object.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
--
-- 'key', 'putObjectTagging_key' - Name of the object key.
--
-- 'tagging', 'putObjectTagging_tagging' - Container for the @TagSet@ and @Tag@ elements
newPutObjectTagging ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  -- | 'tagging'
  Tagging ->
  PutObjectTagging
newPutObjectTagging pBucket_ pKey_ pTagging_ =
  PutObjectTagging'
    { checksumAlgorithm =
        Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_,
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
putObjectTagging_checksumAlgorithm :: Lens.Lens' PutObjectTagging (Prelude.Maybe ChecksumAlgorithm)
putObjectTagging_checksumAlgorithm = Lens.lens (\PutObjectTagging' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutObjectTagging' {} a -> s {checksumAlgorithm = a} :: PutObjectTagging)

-- | The MD5 hash for the request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putObjectTagging_contentMD5 :: Lens.Lens' PutObjectTagging (Prelude.Maybe Prelude.Text)
putObjectTagging_contentMD5 = Lens.lens (\PutObjectTagging' {contentMD5} -> contentMD5) (\s@PutObjectTagging' {} a -> s {contentMD5 = a} :: PutObjectTagging)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putObjectTagging_expectedBucketOwner :: Lens.Lens' PutObjectTagging (Prelude.Maybe Prelude.Text)
putObjectTagging_expectedBucketOwner = Lens.lens (\PutObjectTagging' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObjectTagging' {} a -> s {expectedBucketOwner = a} :: PutObjectTagging)

-- | Undocumented member.
putObjectTagging_requestPayer :: Lens.Lens' PutObjectTagging (Prelude.Maybe RequestPayer)
putObjectTagging_requestPayer = Lens.lens (\PutObjectTagging' {requestPayer} -> requestPayer) (\s@PutObjectTagging' {} a -> s {requestPayer = a} :: PutObjectTagging)

-- | The versionId of the object that the tag-set will be added to.
putObjectTagging_versionId :: Lens.Lens' PutObjectTagging (Prelude.Maybe ObjectVersionId)
putObjectTagging_versionId = Lens.lens (\PutObjectTagging' {versionId} -> versionId) (\s@PutObjectTagging' {} a -> s {versionId = a} :: PutObjectTagging)

-- | The bucket name containing the object.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
--
-- When using this action with Amazon S3 on Outposts, you must direct
-- requests to the S3 on Outposts hostname. The S3 on Outposts hostname
-- takes the form
-- @ AccessPointName-AccountId.outpostID.s3-outposts.Region.amazonaws.com@.
-- When using this action with S3 on Outposts through the Amazon Web
-- Services SDKs, you provide the Outposts bucket ARN in place of the
-- bucket name. For more information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/S3onOutposts.html Using Amazon S3 on Outposts>
-- in the /Amazon S3 User Guide/.
putObjectTagging_bucket :: Lens.Lens' PutObjectTagging BucketName
putObjectTagging_bucket = Lens.lens (\PutObjectTagging' {bucket} -> bucket) (\s@PutObjectTagging' {} a -> s {bucket = a} :: PutObjectTagging)

-- | Name of the object key.
putObjectTagging_key :: Lens.Lens' PutObjectTagging ObjectKey
putObjectTagging_key = Lens.lens (\PutObjectTagging' {key} -> key) (\s@PutObjectTagging' {} a -> s {key = a} :: PutObjectTagging)

-- | Container for the @TagSet@ and @Tag@ elements
putObjectTagging_tagging :: Lens.Lens' PutObjectTagging Tagging
putObjectTagging_tagging = Lens.lens (\PutObjectTagging' {tagging} -> tagging) (\s@PutObjectTagging' {} a -> s {tagging = a} :: PutObjectTagging)

instance Core.AWSRequest PutObjectTagging where
  type
    AWSResponse PutObjectTagging =
      PutObjectTaggingResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectTaggingResponse'
            Prelude.<$> (h Data..#? "x-amz-version-id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutObjectTagging where
  hashWithSalt _salt PutObjectTagging' {..} =
    _salt `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key
      `Prelude.hashWithSalt` tagging

instance Prelude.NFData PutObjectTagging where
  rnf PutObjectTagging' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key
      `Prelude.seq` Prelude.rnf tagging

instance Data.ToElement PutObjectTagging where
  toElement PutObjectTagging' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
      tagging

instance Data.ToHeaders PutObjectTagging where
  toHeaders PutObjectTagging' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath PutObjectTagging where
  toPath PutObjectTagging' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery PutObjectTagging where
  toQuery PutObjectTagging' {..} =
    Prelude.mconcat
      ["versionId" Data.=: versionId, "tagging"]

-- | /See:/ 'newPutObjectTaggingResponse' smart constructor.
data PutObjectTaggingResponse = PutObjectTaggingResponse'
  { -- | The versionId of the object the tag-set was added to.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObjectTaggingResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'versionId', 'putObjectTaggingResponse_versionId' - The versionId of the object the tag-set was added to.
--
-- 'httpStatus', 'putObjectTaggingResponse_httpStatus' - The response's http status code.
newPutObjectTaggingResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutObjectTaggingResponse
newPutObjectTaggingResponse pHttpStatus_ =
  PutObjectTaggingResponse'
    { versionId =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The versionId of the object the tag-set was added to.
putObjectTaggingResponse_versionId :: Lens.Lens' PutObjectTaggingResponse (Prelude.Maybe ObjectVersionId)
putObjectTaggingResponse_versionId = Lens.lens (\PutObjectTaggingResponse' {versionId} -> versionId) (\s@PutObjectTaggingResponse' {} a -> s {versionId = a} :: PutObjectTaggingResponse)

-- | The response's http status code.
putObjectTaggingResponse_httpStatus :: Lens.Lens' PutObjectTaggingResponse Prelude.Int
putObjectTaggingResponse_httpStatus = Lens.lens (\PutObjectTaggingResponse' {httpStatus} -> httpStatus) (\s@PutObjectTaggingResponse' {} a -> s {httpStatus = a} :: PutObjectTaggingResponse)

instance Prelude.NFData PutObjectTaggingResponse where
  rnf PutObjectTaggingResponse' {..} =
    Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf httpStatus
