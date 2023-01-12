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
-- Module      : Amazonka.S3.PutBucketEncryption
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This action uses the @encryption@ subresource to configure default
-- encryption and Amazon S3 Bucket Key for an existing bucket.
--
-- Default encryption for a bucket can use server-side encryption with
-- Amazon S3-managed keys (SSE-S3) or customer managed keys (SSE-KMS). If
-- you specify default encryption using SSE-KMS, you can also configure
-- Amazon S3 Bucket Key. When the default encryption is SSE-KMS, if you
-- upload an object to the bucket and do not specify the KMS key to use for
-- encryption, Amazon S3 uses the default Amazon Web Services managed KMS
-- key for your account. For information about default encryption, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 default bucket encryption>
-- in the /Amazon S3 User Guide/. For more information about S3 Bucket
-- Keys, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys>
-- in the /Amazon S3 User Guide/.
--
-- This action requires Amazon Web Services Signature Version 4. For more
-- information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/sig-v4-authenticating-requests.html Authenticating Requests (Amazon Web Services Signature Version 4)>.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutEncryptionConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>
-- in the Amazon S3 User Guide.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketEncryption.html GetBucketEncryption>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html DeleteBucketEncryption>
module Amazonka.S3.PutBucketEncryption
  ( -- * Creating a Request
    PutBucketEncryption (..),
    newPutBucketEncryption,

    -- * Request Lenses
    putBucketEncryption_checksumAlgorithm,
    putBucketEncryption_contentMD5,
    putBucketEncryption_expectedBucketOwner,
    putBucketEncryption_bucket,
    putBucketEncryption_serverSideEncryptionConfiguration,

    -- * Destructuring the Response
    PutBucketEncryptionResponse (..),
    newPutBucketEncryptionResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketEncryption' smart constructor.
data PutBucketEncryption = PutBucketEncryption'
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
    -- | The base64-encoded 128-bit MD5 digest of the server-side encryption
    -- configuration.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | Specifies default encryption for a bucket using server-side encryption
    -- with Amazon S3-managed keys (SSE-S3) or customer managed keys (SSE-KMS).
    -- For information about the Amazon S3 default encryption feature, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption>
    -- in the /Amazon S3 User Guide/.
    bucket :: BucketName,
    serverSideEncryptionConfiguration :: ServerSideEncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putBucketEncryption_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'contentMD5', 'putBucketEncryption_contentMD5' - The base64-encoded 128-bit MD5 digest of the server-side encryption
-- configuration.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putBucketEncryption_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'putBucketEncryption_bucket' - Specifies default encryption for a bucket using server-side encryption
-- with Amazon S3-managed keys (SSE-S3) or customer managed keys (SSE-KMS).
-- For information about the Amazon S3 default encryption feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption>
-- in the /Amazon S3 User Guide/.
--
-- 'serverSideEncryptionConfiguration', 'putBucketEncryption_serverSideEncryptionConfiguration' - Undocumented member.
newPutBucketEncryption ::
  -- | 'bucket'
  BucketName ->
  -- | 'serverSideEncryptionConfiguration'
  ServerSideEncryptionConfiguration ->
  PutBucketEncryption
newPutBucketEncryption
  pBucket_
  pServerSideEncryptionConfiguration_ =
    PutBucketEncryption'
      { checksumAlgorithm =
          Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        expectedBucketOwner = Prelude.Nothing,
        bucket = pBucket_,
        serverSideEncryptionConfiguration =
          pServerSideEncryptionConfiguration_
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
putBucketEncryption_checksumAlgorithm :: Lens.Lens' PutBucketEncryption (Prelude.Maybe ChecksumAlgorithm)
putBucketEncryption_checksumAlgorithm = Lens.lens (\PutBucketEncryption' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutBucketEncryption' {} a -> s {checksumAlgorithm = a} :: PutBucketEncryption)

-- | The base64-encoded 128-bit MD5 digest of the server-side encryption
-- configuration.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putBucketEncryption_contentMD5 :: Lens.Lens' PutBucketEncryption (Prelude.Maybe Prelude.Text)
putBucketEncryption_contentMD5 = Lens.lens (\PutBucketEncryption' {contentMD5} -> contentMD5) (\s@PutBucketEncryption' {} a -> s {contentMD5 = a} :: PutBucketEncryption)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketEncryption_expectedBucketOwner :: Lens.Lens' PutBucketEncryption (Prelude.Maybe Prelude.Text)
putBucketEncryption_expectedBucketOwner = Lens.lens (\PutBucketEncryption' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketEncryption' {} a -> s {expectedBucketOwner = a} :: PutBucketEncryption)

-- | Specifies default encryption for a bucket using server-side encryption
-- with Amazon S3-managed keys (SSE-S3) or customer managed keys (SSE-KMS).
-- For information about the Amazon S3 default encryption feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption>
-- in the /Amazon S3 User Guide/.
putBucketEncryption_bucket :: Lens.Lens' PutBucketEncryption BucketName
putBucketEncryption_bucket = Lens.lens (\PutBucketEncryption' {bucket} -> bucket) (\s@PutBucketEncryption' {} a -> s {bucket = a} :: PutBucketEncryption)

-- | Undocumented member.
putBucketEncryption_serverSideEncryptionConfiguration :: Lens.Lens' PutBucketEncryption ServerSideEncryptionConfiguration
putBucketEncryption_serverSideEncryptionConfiguration = Lens.lens (\PutBucketEncryption' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@PutBucketEncryption' {} a -> s {serverSideEncryptionConfiguration = a} :: PutBucketEncryption)

instance Core.AWSRequest PutBucketEncryption where
  type
    AWSResponse PutBucketEncryption =
      PutBucketEncryptionResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull PutBucketEncryptionResponse'

instance Prelude.Hashable PutBucketEncryption where
  hashWithSalt _salt PutBucketEncryption' {..} =
    _salt `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` serverSideEncryptionConfiguration

instance Prelude.NFData PutBucketEncryption where
  rnf PutBucketEncryption' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf serverSideEncryptionConfiguration

instance Data.ToElement PutBucketEncryption where
  toElement PutBucketEncryption' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}ServerSideEncryptionConfiguration"
      serverSideEncryptionConfiguration

instance Data.ToHeaders PutBucketEncryption where
  toHeaders PutBucketEncryption' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath PutBucketEncryption where
  toPath PutBucketEncryption' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutBucketEncryption where
  toQuery =
    Prelude.const (Prelude.mconcat ["encryption"])

-- | /See:/ 'newPutBucketEncryptionResponse' smart constructor.
data PutBucketEncryptionResponse = PutBucketEncryptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketEncryptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketEncryptionResponse ::
  PutBucketEncryptionResponse
newPutBucketEncryptionResponse =
  PutBucketEncryptionResponse'

instance Prelude.NFData PutBucketEncryptionResponse where
  rnf _ = ()
