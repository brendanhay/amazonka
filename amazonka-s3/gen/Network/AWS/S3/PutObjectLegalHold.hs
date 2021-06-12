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
-- Module      : Network.AWS.S3.PutObjectLegalHold
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a Legal Hold configuration to the specified object.
--
-- This action is not supported by Amazon S3 on Outposts.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>
module Network.AWS.S3.PutObjectLegalHold
  ( -- * Creating a Request
    PutObjectLegalHold (..),
    newPutObjectLegalHold,

    -- * Request Lenses
    putObjectLegalHold_expectedBucketOwner,
    putObjectLegalHold_contentMD5,
    putObjectLegalHold_versionId,
    putObjectLegalHold_legalHold,
    putObjectLegalHold_requestPayer,
    putObjectLegalHold_bucket,
    putObjectLegalHold_key,

    -- * Destructuring the Response
    PutObjectLegalHoldResponse (..),
    newPutObjectLegalHoldResponse,

    -- * Response Lenses
    putObjectLegalHoldResponse_requestCharged,
    putObjectLegalHoldResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutObjectLegalHold' smart constructor.
data PutObjectLegalHold = PutObjectLegalHold'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Core.Maybe Core.Text,
    -- | The MD5 hash for the request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS
    -- SDKs, this field is calculated automatically.
    contentMD5 :: Core.Maybe Core.Text,
    -- | The version ID of the object that you want to place a Legal Hold on.
    versionId :: Core.Maybe ObjectVersionId,
    -- | Container element for the Legal Hold configuration you want to apply to
    -- the specified object.
    legalHold :: Core.Maybe ObjectLockLegalHold,
    requestPayer :: Core.Maybe RequestPayer,
    -- | The bucket name containing the object that you want to place a Legal
    -- Hold on.
    --
    -- When using this API with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this operation with an access point through the AWS SDKs, you
    -- provide the access point ARN in place of the bucket name. For more
    -- information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    bucket :: BucketName,
    -- | The key name for the object that you want to place a Legal Hold on.
    key :: ObjectKey
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutObjectLegalHold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putObjectLegalHold_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentMD5', 'putObjectLegalHold_contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
--
-- 'versionId', 'putObjectLegalHold_versionId' - The version ID of the object that you want to place a Legal Hold on.
--
-- 'legalHold', 'putObjectLegalHold_legalHold' - Container element for the Legal Hold configuration you want to apply to
-- the specified object.
--
-- 'requestPayer', 'putObjectLegalHold_requestPayer' - Undocumented member.
--
-- 'bucket', 'putObjectLegalHold_bucket' - The bucket name containing the object that you want to place a Legal
-- Hold on.
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
-- 'key', 'putObjectLegalHold_key' - The key name for the object that you want to place a Legal Hold on.
newPutObjectLegalHold ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  PutObjectLegalHold
newPutObjectLegalHold pBucket_ pKey_ =
  PutObjectLegalHold'
    { expectedBucketOwner =
        Core.Nothing,
      contentMD5 = Core.Nothing,
      versionId = Core.Nothing,
      legalHold = Core.Nothing,
      requestPayer = Core.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putObjectLegalHold_expectedBucketOwner :: Lens.Lens' PutObjectLegalHold (Core.Maybe Core.Text)
putObjectLegalHold_expectedBucketOwner = Lens.lens (\PutObjectLegalHold' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObjectLegalHold' {} a -> s {expectedBucketOwner = a} :: PutObjectLegalHold)

-- | The MD5 hash for the request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putObjectLegalHold_contentMD5 :: Lens.Lens' PutObjectLegalHold (Core.Maybe Core.Text)
putObjectLegalHold_contentMD5 = Lens.lens (\PutObjectLegalHold' {contentMD5} -> contentMD5) (\s@PutObjectLegalHold' {} a -> s {contentMD5 = a} :: PutObjectLegalHold)

-- | The version ID of the object that you want to place a Legal Hold on.
putObjectLegalHold_versionId :: Lens.Lens' PutObjectLegalHold (Core.Maybe ObjectVersionId)
putObjectLegalHold_versionId = Lens.lens (\PutObjectLegalHold' {versionId} -> versionId) (\s@PutObjectLegalHold' {} a -> s {versionId = a} :: PutObjectLegalHold)

-- | Container element for the Legal Hold configuration you want to apply to
-- the specified object.
putObjectLegalHold_legalHold :: Lens.Lens' PutObjectLegalHold (Core.Maybe ObjectLockLegalHold)
putObjectLegalHold_legalHold = Lens.lens (\PutObjectLegalHold' {legalHold} -> legalHold) (\s@PutObjectLegalHold' {} a -> s {legalHold = a} :: PutObjectLegalHold)

-- | Undocumented member.
putObjectLegalHold_requestPayer :: Lens.Lens' PutObjectLegalHold (Core.Maybe RequestPayer)
putObjectLegalHold_requestPayer = Lens.lens (\PutObjectLegalHold' {requestPayer} -> requestPayer) (\s@PutObjectLegalHold' {} a -> s {requestPayer = a} :: PutObjectLegalHold)

-- | The bucket name containing the object that you want to place a Legal
-- Hold on.
--
-- When using this API with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this operation with an access point through the AWS SDKs, you
-- provide the access point ARN in place of the bucket name. For more
-- information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-access-points.html Using Access Points>
-- in the /Amazon Simple Storage Service Developer Guide/.
putObjectLegalHold_bucket :: Lens.Lens' PutObjectLegalHold BucketName
putObjectLegalHold_bucket = Lens.lens (\PutObjectLegalHold' {bucket} -> bucket) (\s@PutObjectLegalHold' {} a -> s {bucket = a} :: PutObjectLegalHold)

-- | The key name for the object that you want to place a Legal Hold on.
putObjectLegalHold_key :: Lens.Lens' PutObjectLegalHold ObjectKey
putObjectLegalHold_key = Lens.lens (\PutObjectLegalHold' {key} -> key) (\s@PutObjectLegalHold' {} a -> s {key = a} :: PutObjectLegalHold)

instance Core.AWSRequest PutObjectLegalHold where
  type
    AWSResponse PutObjectLegalHold =
      PutObjectLegalHoldResponse
  request = Request.putXML defaultService
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectLegalHoldResponse'
            Core.<$> (h Core..#? "x-amz-request-charged")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable PutObjectLegalHold

instance Core.NFData PutObjectLegalHold

instance Core.ToElement PutObjectLegalHold where
  toElement PutObjectLegalHold' {..} =
    Core.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}LegalHold"
      legalHold

instance Core.ToHeaders PutObjectLegalHold where
  toHeaders PutObjectLegalHold' {..} =
    Core.mconcat
      [ "x-amz-expected-bucket-owner"
          Core.=# expectedBucketOwner,
        "Content-MD5" Core.=# contentMD5,
        "x-amz-request-payer" Core.=# requestPayer
      ]

instance Core.ToPath PutObjectLegalHold where
  toPath PutObjectLegalHold' {..} =
    Core.mconcat
      ["/", Core.toBS bucket, "/", Core.toBS key]

instance Core.ToQuery PutObjectLegalHold where
  toQuery PutObjectLegalHold' {..} =
    Core.mconcat
      ["versionId" Core.=: versionId, "legal-hold"]

-- | /See:/ 'newPutObjectLegalHoldResponse' smart constructor.
data PutObjectLegalHoldResponse = PutObjectLegalHoldResponse'
  { requestCharged :: Core.Maybe RequestCharged,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'PutObjectLegalHoldResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'putObjectLegalHoldResponse_requestCharged' - Undocumented member.
--
-- 'httpStatus', 'putObjectLegalHoldResponse_httpStatus' - The response's http status code.
newPutObjectLegalHoldResponse ::
  -- | 'httpStatus'
  Core.Int ->
  PutObjectLegalHoldResponse
newPutObjectLegalHoldResponse pHttpStatus_ =
  PutObjectLegalHoldResponse'
    { requestCharged =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putObjectLegalHoldResponse_requestCharged :: Lens.Lens' PutObjectLegalHoldResponse (Core.Maybe RequestCharged)
putObjectLegalHoldResponse_requestCharged = Lens.lens (\PutObjectLegalHoldResponse' {requestCharged} -> requestCharged) (\s@PutObjectLegalHoldResponse' {} a -> s {requestCharged = a} :: PutObjectLegalHoldResponse)

-- | The response's http status code.
putObjectLegalHoldResponse_httpStatus :: Lens.Lens' PutObjectLegalHoldResponse Core.Int
putObjectLegalHoldResponse_httpStatus = Lens.lens (\PutObjectLegalHoldResponse' {httpStatus} -> httpStatus) (\s@PutObjectLegalHoldResponse' {} a -> s {httpStatus = a} :: PutObjectLegalHoldResponse)

instance Core.NFData PutObjectLegalHoldResponse
