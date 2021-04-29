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
-- Module      : Network.AWS.S3.PutObjectTagging
-- Copyright   : (c) 2013-2021 Brendan Hay
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
--     -   /Cause: A conflicting conditional operation is currently in
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
module Network.AWS.S3.PutObjectTagging
  ( -- * Creating a Request
    PutObjectTagging (..),
    newPutObjectTagging,

    -- * Request Lenses
    putObjectTagging_expectedBucketOwner,
    putObjectTagging_contentMD5,
    putObjectTagging_versionId,
    putObjectTagging_requestPayer,
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

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutObjectTagging' smart constructor.
data PutObjectTagging = PutObjectTagging'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The MD5 hash for the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS
    -- SDKs, this field is calculated automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The versionId of the object that the tag-set will be added to.
    versionId :: Prelude.Maybe ObjectVersionId,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The bucket name containing the object.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    --
    -- When using this API with Amazon S3 on Outposts, you must direct requests
    -- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
    -- form
    -- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
    -- When using this operation using S3 on Outposts through the AWS SDKs, you
    -- provide the Outposts bucket ARN in place of the bucket name. For more
    -- information about S3 on Outposts ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    bucket :: BucketName,
    -- | Name of the object key.
    key :: ObjectKey,
    -- | Container for the @TagSet@ and @Tag@ elements
    tagging :: Tagging
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutObjectTagging' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putObjectTagging_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentMD5', 'putObjectTagging_contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
--
-- 'versionId', 'putObjectTagging_versionId' - The versionId of the object that the tag-set will be added to.
--
-- 'requestPayer', 'putObjectTagging_requestPayer' - Undocumented member.
--
-- 'bucket', 'putObjectTagging_bucket' - The bucket name containing the object.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
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
    { expectedBucketOwner =
        Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      versionId = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_,
      tagging = pTagging_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putObjectTagging_expectedBucketOwner :: Lens.Lens' PutObjectTagging (Prelude.Maybe Prelude.Text)
putObjectTagging_expectedBucketOwner = Lens.lens (\PutObjectTagging' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObjectTagging' {} a -> s {expectedBucketOwner = a} :: PutObjectTagging)

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putObjectTagging_contentMD5 :: Lens.Lens' PutObjectTagging (Prelude.Maybe Prelude.Text)
putObjectTagging_contentMD5 = Lens.lens (\PutObjectTagging' {contentMD5} -> contentMD5) (\s@PutObjectTagging' {} a -> s {contentMD5 = a} :: PutObjectTagging)

-- | The versionId of the object that the tag-set will be added to.
putObjectTagging_versionId :: Lens.Lens' PutObjectTagging (Prelude.Maybe ObjectVersionId)
putObjectTagging_versionId = Lens.lens (\PutObjectTagging' {versionId} -> versionId) (\s@PutObjectTagging' {} a -> s {versionId = a} :: PutObjectTagging)

-- | Undocumented member.
putObjectTagging_requestPayer :: Lens.Lens' PutObjectTagging (Prelude.Maybe RequestPayer)
putObjectTagging_requestPayer = Lens.lens (\PutObjectTagging' {requestPayer} -> requestPayer) (\s@PutObjectTagging' {} a -> s {requestPayer = a} :: PutObjectTagging)

-- | The bucket name containing the object.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- When using this API with Amazon S3 on Outposts, you must direct requests
-- to the S3 on Outposts hostname. The S3 on Outposts hostname takes the
-- form
-- /AccessPointName/-/AccountId/./outpostID/.s3-outposts./Region/.amazonaws.com.
-- When using this operation using S3 on Outposts through the AWS SDKs, you
-- provide the Outposts bucket ARN in place of the bucket name. For more
-- information about S3 on Outposts ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/S3onOutposts.html Using S3 on Outposts>
-- in the /Amazon Simple Storage Service Developer Guide/.
putObjectTagging_bucket :: Lens.Lens' PutObjectTagging BucketName
putObjectTagging_bucket = Lens.lens (\PutObjectTagging' {bucket} -> bucket) (\s@PutObjectTagging' {} a -> s {bucket = a} :: PutObjectTagging)

-- | Name of the object key.
putObjectTagging_key :: Lens.Lens' PutObjectTagging ObjectKey
putObjectTagging_key = Lens.lens (\PutObjectTagging' {key} -> key) (\s@PutObjectTagging' {} a -> s {key = a} :: PutObjectTagging)

-- | Container for the @TagSet@ and @Tag@ elements
putObjectTagging_tagging :: Lens.Lens' PutObjectTagging Tagging
putObjectTagging_tagging = Lens.lens (\PutObjectTagging' {tagging} -> tagging) (\s@PutObjectTagging' {} a -> s {tagging = a} :: PutObjectTagging)

instance Prelude.AWSRequest PutObjectTagging where
  type Rs PutObjectTagging = PutObjectTaggingResponse
  request = Request.putXML defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectTaggingResponse'
            Prelude.<$> (h Prelude..#? "x-amz-version-id")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutObjectTagging

instance Prelude.NFData PutObjectTagging

instance Prelude.ToElement PutObjectTagging where
  toElement PutObjectTagging' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}Tagging"
      tagging

instance Prelude.ToHeaders PutObjectTagging where
  toHeaders PutObjectTagging' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "Content-MD5" Prelude.=# contentMD5,
        "x-amz-request-payer" Prelude.=# requestPayer
      ]

instance Prelude.ToPath PutObjectTagging where
  toPath PutObjectTagging' {..} =
    Prelude.mconcat
      ["/", Prelude.toBS bucket, "/", Prelude.toBS key]

instance Prelude.ToQuery PutObjectTagging where
  toQuery PutObjectTagging' {..} =
    Prelude.mconcat
      ["versionId" Prelude.=: versionId, "tagging"]

-- | /See:/ 'newPutObjectTaggingResponse' smart constructor.
data PutObjectTaggingResponse = PutObjectTaggingResponse'
  { -- | The versionId of the object the tag-set was added to.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

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

instance Prelude.NFData PutObjectTaggingResponse
