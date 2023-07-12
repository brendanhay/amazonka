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
-- Module      : Amazonka.S3.PutObjectRetention
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Places an Object Retention configuration on an object. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lock.html Locking Objects>.
-- Users or accounts require the @s3:PutObjectRetention@ permission in
-- order to place an Object Retention configuration on objects. Bypassing a
-- Governance Retention configuration requires the
-- @s3:BypassGovernanceRetention@ permission.
--
-- This action is not supported by Amazon S3 on Outposts.
module Amazonka.S3.PutObjectRetention
  ( -- * Creating a Request
    PutObjectRetention (..),
    newPutObjectRetention,

    -- * Request Lenses
    putObjectRetention_bypassGovernanceRetention,
    putObjectRetention_checksumAlgorithm,
    putObjectRetention_contentMD5,
    putObjectRetention_expectedBucketOwner,
    putObjectRetention_requestPayer,
    putObjectRetention_retention,
    putObjectRetention_versionId,
    putObjectRetention_bucket,
    putObjectRetention_key,

    -- * Destructuring the Response
    PutObjectRetentionResponse (..),
    newPutObjectRetentionResponse,

    -- * Response Lenses
    putObjectRetentionResponse_requestCharged,
    putObjectRetentionResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutObjectRetention' smart constructor.
data PutObjectRetention = PutObjectRetention'
  { -- | Indicates whether this action should bypass Governance-mode
    -- restrictions.
    bypassGovernanceRetention :: Prelude.Maybe Prelude.Bool,
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
    -- | The container element for the Object Retention configuration.
    retention :: Prelude.Maybe ObjectLockRetention,
    -- | The version ID for the object that you want to apply this Object
    -- Retention configuration to.
    versionId :: Prelude.Maybe ObjectVersionId,
    -- | The bucket name that contains the object you want to apply this Object
    -- Retention configuration to.
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
    -- | The key name for the object that you want to apply this Object Retention
    -- configuration to.
    key :: ObjectKey
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObjectRetention' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bypassGovernanceRetention', 'putObjectRetention_bypassGovernanceRetention' - Indicates whether this action should bypass Governance-mode
-- restrictions.
--
-- 'checksumAlgorithm', 'putObjectRetention_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'contentMD5', 'putObjectRetention_contentMD5' - The MD5 hash for the request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putObjectRetention_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'requestPayer', 'putObjectRetention_requestPayer' - Undocumented member.
--
-- 'retention', 'putObjectRetention_retention' - The container element for the Object Retention configuration.
--
-- 'versionId', 'putObjectRetention_versionId' - The version ID for the object that you want to apply this Object
-- Retention configuration to.
--
-- 'bucket', 'putObjectRetention_bucket' - The bucket name that contains the object you want to apply this Object
-- Retention configuration to.
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
-- 'key', 'putObjectRetention_key' - The key name for the object that you want to apply this Object Retention
-- configuration to.
newPutObjectRetention ::
  -- | 'bucket'
  BucketName ->
  -- | 'key'
  ObjectKey ->
  PutObjectRetention
newPutObjectRetention pBucket_ pKey_ =
  PutObjectRetention'
    { bypassGovernanceRetention =
        Prelude.Nothing,
      checksumAlgorithm = Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      requestPayer = Prelude.Nothing,
      retention = Prelude.Nothing,
      versionId = Prelude.Nothing,
      bucket = pBucket_,
      key = pKey_
    }

-- | Indicates whether this action should bypass Governance-mode
-- restrictions.
putObjectRetention_bypassGovernanceRetention :: Lens.Lens' PutObjectRetention (Prelude.Maybe Prelude.Bool)
putObjectRetention_bypassGovernanceRetention = Lens.lens (\PutObjectRetention' {bypassGovernanceRetention} -> bypassGovernanceRetention) (\s@PutObjectRetention' {} a -> s {bypassGovernanceRetention = a} :: PutObjectRetention)

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
putObjectRetention_checksumAlgorithm :: Lens.Lens' PutObjectRetention (Prelude.Maybe ChecksumAlgorithm)
putObjectRetention_checksumAlgorithm = Lens.lens (\PutObjectRetention' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutObjectRetention' {} a -> s {checksumAlgorithm = a} :: PutObjectRetention)

-- | The MD5 hash for the request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putObjectRetention_contentMD5 :: Lens.Lens' PutObjectRetention (Prelude.Maybe Prelude.Text)
putObjectRetention_contentMD5 = Lens.lens (\PutObjectRetention' {contentMD5} -> contentMD5) (\s@PutObjectRetention' {} a -> s {contentMD5 = a} :: PutObjectRetention)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putObjectRetention_expectedBucketOwner :: Lens.Lens' PutObjectRetention (Prelude.Maybe Prelude.Text)
putObjectRetention_expectedBucketOwner = Lens.lens (\PutObjectRetention' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutObjectRetention' {} a -> s {expectedBucketOwner = a} :: PutObjectRetention)

-- | Undocumented member.
putObjectRetention_requestPayer :: Lens.Lens' PutObjectRetention (Prelude.Maybe RequestPayer)
putObjectRetention_requestPayer = Lens.lens (\PutObjectRetention' {requestPayer} -> requestPayer) (\s@PutObjectRetention' {} a -> s {requestPayer = a} :: PutObjectRetention)

-- | The container element for the Object Retention configuration.
putObjectRetention_retention :: Lens.Lens' PutObjectRetention (Prelude.Maybe ObjectLockRetention)
putObjectRetention_retention = Lens.lens (\PutObjectRetention' {retention} -> retention) (\s@PutObjectRetention' {} a -> s {retention = a} :: PutObjectRetention)

-- | The version ID for the object that you want to apply this Object
-- Retention configuration to.
putObjectRetention_versionId :: Lens.Lens' PutObjectRetention (Prelude.Maybe ObjectVersionId)
putObjectRetention_versionId = Lens.lens (\PutObjectRetention' {versionId} -> versionId) (\s@PutObjectRetention' {} a -> s {versionId = a} :: PutObjectRetention)

-- | The bucket name that contains the object you want to apply this Object
-- Retention configuration to.
--
-- When using this action with an access point, you must direct requests to
-- the access point hostname. The access point hostname takes the form
-- /AccessPointName/-/AccountId/.s3-accesspoint./Region/.amazonaws.com.
-- When using this action with an access point through the Amazon Web
-- Services SDKs, you provide the access point ARN in place of the bucket
-- name. For more information about access point ARNs, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-access-points.html Using access points>
-- in the /Amazon S3 User Guide/.
putObjectRetention_bucket :: Lens.Lens' PutObjectRetention BucketName
putObjectRetention_bucket = Lens.lens (\PutObjectRetention' {bucket} -> bucket) (\s@PutObjectRetention' {} a -> s {bucket = a} :: PutObjectRetention)

-- | The key name for the object that you want to apply this Object Retention
-- configuration to.
putObjectRetention_key :: Lens.Lens' PutObjectRetention ObjectKey
putObjectRetention_key = Lens.lens (\PutObjectRetention' {key} -> key) (\s@PutObjectRetention' {} a -> s {key = a} :: PutObjectRetention)

instance Core.AWSRequest PutObjectRetention where
  type
    AWSResponse PutObjectRetention =
      PutObjectRetentionResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveEmpty
      ( \s h x ->
          PutObjectRetentionResponse'
            Prelude.<$> (h Data..#? "x-amz-request-charged")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable PutObjectRetention where
  hashWithSalt _salt PutObjectRetention' {..} =
    _salt
      `Prelude.hashWithSalt` bypassGovernanceRetention
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` requestPayer
      `Prelude.hashWithSalt` retention
      `Prelude.hashWithSalt` versionId
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` key

instance Prelude.NFData PutObjectRetention where
  rnf PutObjectRetention' {..} =
    Prelude.rnf bypassGovernanceRetention
      `Prelude.seq` Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf requestPayer
      `Prelude.seq` Prelude.rnf retention
      `Prelude.seq` Prelude.rnf versionId
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf key

instance Data.ToElement PutObjectRetention where
  toElement PutObjectRetention' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}Retention"
      retention

instance Data.ToHeaders PutObjectRetention where
  toHeaders PutObjectRetention' {..} =
    Prelude.mconcat
      [ "x-amz-bypass-governance-retention"
          Data.=# bypassGovernanceRetention,
        "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner,
        "x-amz-request-payer" Data.=# requestPayer
      ]

instance Data.ToPath PutObjectRetention where
  toPath PutObjectRetention' {..} =
    Prelude.mconcat
      ["/", Data.toBS bucket, "/", Data.toBS key]

instance Data.ToQuery PutObjectRetention where
  toQuery PutObjectRetention' {..} =
    Prelude.mconcat
      ["versionId" Data.=: versionId, "retention"]

-- | /See:/ 'newPutObjectRetentionResponse' smart constructor.
data PutObjectRetentionResponse = PutObjectRetentionResponse'
  { requestCharged :: Prelude.Maybe RequestCharged,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutObjectRetentionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'requestCharged', 'putObjectRetentionResponse_requestCharged' - Undocumented member.
--
-- 'httpStatus', 'putObjectRetentionResponse_httpStatus' - The response's http status code.
newPutObjectRetentionResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  PutObjectRetentionResponse
newPutObjectRetentionResponse pHttpStatus_ =
  PutObjectRetentionResponse'
    { requestCharged =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Undocumented member.
putObjectRetentionResponse_requestCharged :: Lens.Lens' PutObjectRetentionResponse (Prelude.Maybe RequestCharged)
putObjectRetentionResponse_requestCharged = Lens.lens (\PutObjectRetentionResponse' {requestCharged} -> requestCharged) (\s@PutObjectRetentionResponse' {} a -> s {requestCharged = a} :: PutObjectRetentionResponse)

-- | The response's http status code.
putObjectRetentionResponse_httpStatus :: Lens.Lens' PutObjectRetentionResponse Prelude.Int
putObjectRetentionResponse_httpStatus = Lens.lens (\PutObjectRetentionResponse' {httpStatus} -> httpStatus) (\s@PutObjectRetentionResponse' {} a -> s {httpStatus = a} :: PutObjectRetentionResponse)

instance Prelude.NFData PutObjectRetentionResponse where
  rnf PutObjectRetentionResponse' {..} =
    Prelude.rnf requestCharged
      `Prelude.seq` Prelude.rnf httpStatus
