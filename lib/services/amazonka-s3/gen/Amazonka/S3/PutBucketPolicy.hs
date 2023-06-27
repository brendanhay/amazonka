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
-- Module      : Amazonka.S3.PutBucketPolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Applies an Amazon S3 bucket policy to an Amazon S3 bucket. If you are
-- using an identity other than the root user of the Amazon Web Services
-- account that owns the bucket, the calling identity must have the
-- @PutBucketPolicy@ permissions on the specified bucket and belong to the
-- bucket owner\'s account in order to use this operation.
--
-- If you don\'t have @PutBucketPolicy@ permissions, Amazon S3 returns a
-- @403 Access Denied@ error. If you have the correct permissions, but
-- you\'re not using an identity that belongs to the bucket owner\'s
-- account, Amazon S3 returns a @405 Method Not Allowed@ error.
--
-- To ensure that bucket owners don\'t inadvertently lock themselves out of
-- their own buckets, the root principal in a bucket owner\'s Amazon Web
-- Services account can perform the @GetBucketPolicy@, @PutBucketPolicy@,
-- and @DeleteBucketPolicy@ API actions, even if their bucket policy
-- explicitly denies the root principal\'s access. Bucket owner root
-- principals can only be blocked from performing these API actions by VPC
-- endpoint policies and Amazon Web Services Organizations policies.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/example-bucket-policies.html Bucket policy examples>.
--
-- The following operations are related to @PutBucketPolicy@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucket.html DeleteBucket>
module Amazonka.S3.PutBucketPolicy
  ( -- * Creating a Request
    PutBucketPolicy (..),
    newPutBucketPolicy,

    -- * Request Lenses
    putBucketPolicy_checksumAlgorithm,
    putBucketPolicy_confirmRemoveSelfBucketAccess,
    putBucketPolicy_contentMD5,
    putBucketPolicy_expectedBucketOwner,
    putBucketPolicy_bucket,
    putBucketPolicy_policy,

    -- * Destructuring the Response
    PutBucketPolicyResponse (..),
    newPutBucketPolicyResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketPolicy' smart constructor.
data PutBucketPolicy = PutBucketPolicy'
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
    -- | Set this parameter to true to confirm that you want to remove your
    -- permissions to change this bucket policy in the future.
    confirmRemoveSelfBucketAccess :: Prelude.Maybe Prelude.Bool,
    -- | The MD5 hash of the request body.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket.
    bucket :: BucketName,
    -- | The bucket policy as a JSON document.
    policy :: Prelude.ByteString
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketPolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putBucketPolicy_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'confirmRemoveSelfBucketAccess', 'putBucketPolicy_confirmRemoveSelfBucketAccess' - Set this parameter to true to confirm that you want to remove your
-- permissions to change this bucket policy in the future.
--
-- 'contentMD5', 'putBucketPolicy_contentMD5' - The MD5 hash of the request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketPolicy_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'putBucketPolicy_bucket' - The name of the bucket.
--
-- 'policy', 'putBucketPolicy_policy' - The bucket policy as a JSON document.
newPutBucketPolicy ::
  -- | 'bucket'
  BucketName ->
  -- | 'policy'
  Prelude.ByteString ->
  PutBucketPolicy
newPutBucketPolicy pBucket_ pPolicy_ =
  PutBucketPolicy'
    { checksumAlgorithm =
        Prelude.Nothing,
      confirmRemoveSelfBucketAccess = Prelude.Nothing,
      contentMD5 = Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      bucket = pBucket_,
      policy = pPolicy_
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
putBucketPolicy_checksumAlgorithm :: Lens.Lens' PutBucketPolicy (Prelude.Maybe ChecksumAlgorithm)
putBucketPolicy_checksumAlgorithm = Lens.lens (\PutBucketPolicy' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutBucketPolicy' {} a -> s {checksumAlgorithm = a} :: PutBucketPolicy)

-- | Set this parameter to true to confirm that you want to remove your
-- permissions to change this bucket policy in the future.
putBucketPolicy_confirmRemoveSelfBucketAccess :: Lens.Lens' PutBucketPolicy (Prelude.Maybe Prelude.Bool)
putBucketPolicy_confirmRemoveSelfBucketAccess = Lens.lens (\PutBucketPolicy' {confirmRemoveSelfBucketAccess} -> confirmRemoveSelfBucketAccess) (\s@PutBucketPolicy' {} a -> s {confirmRemoveSelfBucketAccess = a} :: PutBucketPolicy)

-- | The MD5 hash of the request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketPolicy_contentMD5 :: Lens.Lens' PutBucketPolicy (Prelude.Maybe Prelude.Text)
putBucketPolicy_contentMD5 = Lens.lens (\PutBucketPolicy' {contentMD5} -> contentMD5) (\s@PutBucketPolicy' {} a -> s {contentMD5 = a} :: PutBucketPolicy)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketPolicy_expectedBucketOwner :: Lens.Lens' PutBucketPolicy (Prelude.Maybe Prelude.Text)
putBucketPolicy_expectedBucketOwner = Lens.lens (\PutBucketPolicy' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketPolicy' {} a -> s {expectedBucketOwner = a} :: PutBucketPolicy)

-- | The name of the bucket.
putBucketPolicy_bucket :: Lens.Lens' PutBucketPolicy BucketName
putBucketPolicy_bucket = Lens.lens (\PutBucketPolicy' {bucket} -> bucket) (\s@PutBucketPolicy' {} a -> s {bucket = a} :: PutBucketPolicy)

-- | The bucket policy as a JSON document.
putBucketPolicy_policy :: Lens.Lens' PutBucketPolicy Prelude.ByteString
putBucketPolicy_policy = Lens.lens (\PutBucketPolicy' {policy} -> policy) (\s@PutBucketPolicy' {} a -> s {policy = a} :: PutBucketPolicy)

instance Core.AWSRequest PutBucketPolicy where
  type
    AWSResponse PutBucketPolicy =
      PutBucketPolicyResponse
  request overrides =
    Request.contentMD5Header
      Prelude.. Request.s3vhost
      Prelude.. Request.putBody (overrides defaultService)
  response =
    Response.receiveNull PutBucketPolicyResponse'

instance Prelude.Hashable PutBucketPolicy where
  hashWithSalt _salt PutBucketPolicy' {..} =
    _salt
      `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` confirmRemoveSelfBucketAccess
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` policy

instance Prelude.NFData PutBucketPolicy where
  rnf PutBucketPolicy' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf confirmRemoveSelfBucketAccess
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf policy

instance Data.ToBody PutBucketPolicy where
  toBody PutBucketPolicy' {..} = Data.toBody policy

instance Data.ToHeaders PutBucketPolicy where
  toHeaders PutBucketPolicy' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "x-amz-confirm-remove-self-bucket-access"
          Data.=# confirmRemoveSelfBucketAccess,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath PutBucketPolicy where
  toPath PutBucketPolicy' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutBucketPolicy where
  toQuery = Prelude.const (Prelude.mconcat ["policy"])

-- | /See:/ 'newPutBucketPolicyResponse' smart constructor.
data PutBucketPolicyResponse = PutBucketPolicyResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketPolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketPolicyResponse ::
  PutBucketPolicyResponse
newPutBucketPolicyResponse = PutBucketPolicyResponse'

instance Prelude.NFData PutBucketPolicyResponse where
  rnf _ = ()
