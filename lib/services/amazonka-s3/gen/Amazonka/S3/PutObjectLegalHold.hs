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
-- Module      : Amazonka.S3.PutObjectLegalHold
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies a legal hold configuration to the specified object. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>.
--
-- This action is not supported by Amazon S3 on Outposts.
module Amazonka.S3.PutObjectLegalHold
  ( -- * Creating a Request
    PutObjectLegalHold (..),
    newPutObjectLegalHold,

    -- * Request Lenses
    putObjectLegalHold_checksumAlgorithm,
    putObjectLegalHold_contentMD5,
    putObjectLegalHold_expectedBucketOwner,
    putObjectLegalHold_legalHold,
    putObjectLegalHold_requestPayer,
    putObjectLegalHold_versionId,
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutObjectLegalHold' smart constructor.
data PutObjectLegalHold = PutObjectLegalHold'
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
    -- | Container element for the legal hold configuration you want to apply to
    -- the specified object.
    legalHold :: Prelude.Maybe ObjectLockLegalHold,
    requestPayer :: Prelude.Maybe RequestPayer,
    -- | The version ID of the object that you want to place a legal hold on.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The bucket name containing the object that you want to place a legal
    -- hold on.
    --
    -- When using this action with an access point, you must direct requests to
    -- the access point hostname. The access point hostname takes the form
    -- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
    -- When using this action with an access point through the Amazon Web
    -- Services SDKs, you provide the access point ARN in place of the bucket
    -- name. For more information about access point ARNs, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    -- | The key name for the object that you want to place a legal hold on.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObjectLegalHold' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putObjectLegalHold_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'contentMD5', 'putObjectLegalHold_contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putObjectLegalHold_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'legalHold', 'putObjectLegalHold_legalHold' - Container element for the legal hold configuration you want to apply to
-- the specified object.
--
-- 'requestPayer', 'putObjectLegalHold_requestPayer' - Undocumented member.
--
-- 'versionId', 'putObjectLegalHold_versionId' - The version ID of the object that you want to place a legal hold on.
--
-- 'bucket', 'putObjectLegalHold_bucket' - The bucket name containing the object that you want to place a legal
-- hold on.
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
-- 'key', 'putObjectLegalHold_key' - The key name for the object that you want to place a legal hold on.
newPutObjectLegalHold ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  PutObjectLegalHold
newPutObjectLegalHold pBucket_ pKey_ =
  PutObjectLegalHold'
    { checksumAlgorithm =
        Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      legalHold = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
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
putObjectLegalHold_checksumAlgorithm :: Lens.Lens' PutObjectLegalHold (Prelude.Maybe ChecksumAlgorithm)
putObjectLegalHold_checksumAlgorithm = Lens.lens (\PutObjectLegalHold' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutObjectLegalHold' {} a -> s {checksumAlgorithm = a} :: PutObjectLegalHold)

-- | The MD5 hash for the request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putObjectLegalHold_contentMD5 :: Lens.Lens' PutObjectLegalHold (Prelude.Maybe Prelude.Text)
putObjectLegalHold_contentMD5 = Lens.lens (\PutObjectLegalHold' {contentMD5} -> contentMD5) (\s@PutObjectLegalHold' {} a -> s {contentMD5 = a} :: PutObjectLegalHold)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putObjectLegalHold_expectedBucketOwner :: Lens.Lens' PutObjectLegalHold (Prelude.Maybe Prelude.Text)
putObjectLegalHold_expectedBucketOwner = Lens.lens (\PutObjectLegalHold' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObjectLegalHold' {} a -> s {expectedBucketOwner = a} :: PutObjectLegalHold)

-- | Container element for the legal hold configuration you want to apply to
-- the specified object.
putObjectLegalHold_legalHold :: Lens.Lens' PutObjectLegalHold (Prelude.Maybe ObjectLockLegalHold)
putObjectLegalHold_legalHold = Lens.lens (\PutObjectLegalHold' {legalHold} -> legalHold) (\s@PutObjectLegalHold' {} a -> s {legalHold = a} :: PutObjectLegalHold)

-- | Undocumented member.
putObjectLegalHold_requestPayer :: Lens.Lens' PutObjectLegalHold (Prelude.Maybe RequestPayer)
putObjectLegalHold_requestPayer = Lens.lens (\PutObjectLegalHold' {requestPayer} -> requestPayer) (\s@PutObjectLegalHold' {} a -> s {requestPayer = a} :: PutObjectLegalHold)

-- | The version ID of the object that you want to place a legal hold on.
putObjectLegalHold_versionId :: Lens.Lens' PutObjectLegalHold (Prelude.Maybe ObjectVersionId)
putObjectLegalHold_versionId = Lens.lens (\PutObjectLegalHold' {versionId} -> versionId) (\s@PutObjectLegalHold' {} a -> s {versionId = a} :: PutObjectLegalHold)

-- | The bucket name containing the object that you want to place a legal
-- hold on.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
putObjectLegalHold_bucket :: Lens.Lens' PutObjectLegalHold BucketName
putObjectLegalHold_bucket = Lens.lens (\PutObjectLegalHold' {bucket} -> bucket) (\s@PutObjectLegalHold' {} a -> s {bucket = a} :: PutObjectLegalHold)

-- | The key name for the object that you want to place a legal hold on.
putObjectLegalHold_key :: Lens.Lens' PutObjectLegalHold ObjectKey
putObjectLegalHold_key = Lens.lens (\PutObjectLegalHold' {key} -> key) (\s@PutObjectLegalHold' {} a -> s {key = a} :: PutObjectLegalHold)

instance Core.AWSRequest PutObjectLegalHold where
  type
    AWSResponse PutObjectLegalHold =
      PutObjectLegalHoldResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectLegalHoldResponse'
            Prelude.<$> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutObjectLegalHold where
  hashWithSalt _salt PutObjectLegalHold' {..} =
    _salt
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` legalHold
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData PutObjectLegalHold where
  rnf PutObjectLegalHold' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf legalHold
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToElement PutObjectLegalHold where
  toElement PutObjectLegalHold' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}LegalHold"
      legalHold

instance Data.ToHeaders PutObjectLegalHold where
  toHeaders PutObjectLegalHold' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath PutObjectLegalHold where
  toPath PutObjectLegalHold' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery PutObjectLegalHold where
  toQuery PutObjectLegalHold' {..} =
    Prelude.mconcat
      ["versionId" Data.=: versionId, "legal-hold"]

-- | /See:/ 'newPutObjectLegalHoldResponse' smart constructor.
data PutObjectLegalHoldResponse = PutObjectLegalHoldResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  PutObjectLegalHoldResponse
newPutObjectLegalHoldResponse pHttpStatus_ =
  PutObjectLegalHoldResponse'
    { requestCharged =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putObjectLegalHoldResponse_requestCharged :: Lens.Lens' PutObjectLegalHoldResponse (Prelude.Maybe RequestCharged)
putObjectLegalHoldResponse_requestCharged = Lens.lens (\PutObjectLegalHoldResponse' {requestCharged} -> requestCharged) (\s@PutObjectLegalHoldResponse' {} a -> s {requestCharged = a} :: PutObjectLegalHoldResponse)

-- | The response's http status code.
putObjectLegalHoldResponse_httpStatus :: Lens.Lens' PutObjectLegalHoldResponse Prelude.Int
putObjectLegalHoldResponse_httpStatus = Lens.lens (\PutObjectLegalHoldResponse' {httpStatus} -> httpStatus) (\s@PutObjectLegalHoldResponse' {} a -> s {httpStatus = a} :: PutObjectLegalHoldResponse)

instance Prelude.NFData PutObjectLegalHoldResponse where
  rnf PutObjectLegalHoldResponse' {..} =
    Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf httpStatus
