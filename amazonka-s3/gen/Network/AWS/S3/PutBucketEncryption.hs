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
-- Module      : Network.AWS.S3.PutBucketEncryption
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- This operation uses the @encryption@ subresource to configure default
-- encryption and Amazon S3 Bucket Key for an existing bucket.
--
-- Default encryption for a bucket can use server-side encryption with
-- Amazon S3-managed keys (SSE-S3) or AWS KMS customer master keys
-- (SSE-KMS). If you specify default encryption using SSE-KMS, you can also
-- configure Amazon S3 Bucket Key. For information about default
-- encryption, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 default bucket encryption>
-- in the /Amazon Simple Storage Service Developer Guide/. For more
-- information about S3 Bucket Keys, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-key.html Amazon S3 Bucket Keys>
-- in the /Amazon Simple Storage Service Developer Guide/.
--
-- This operation requires AWS Signature Version 4. For more information,
-- see
-- <sig-v4-authenticating-requests.html Authenticating Requests (AWS Signature Version 4)>.
--
-- To use this operation, you must have permissions to perform the
-- @s3:PutEncryptionConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>
-- in the Amazon Simple Storage Service Developer Guide.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketEncryption.html GetBucketEncryption>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketEncryption.html DeleteBucketEncryption>
module Network.AWS.S3.PutBucketEncryption
  ( -- * Creating a Request
    PutBucketEncryption (..),
    newPutBucketEncryption,

    -- * Request Lenses
    putBucketEncryption_expectedBucketOwner,
    putBucketEncryption_contentMD5,
    putBucketEncryption_bucket,
    putBucketEncryption_serverSideEncryptionConfiguration,

    -- * Destructuring the Response
    PutBucketEncryptionResponse (..),
    newPutBucketEncryptionResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutBucketEncryption' smart constructor.
data PutBucketEncryption = PutBucketEncryption'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The base64-encoded 128-bit MD5 digest of the server-side encryption
    -- configuration.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS
    -- SDKs, this field is calculated automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | Specifies default encryption for a bucket using server-side encryption
    -- with Amazon S3-managed keys (SSE-S3) or customer master keys stored in
    -- AWS KMS (SSE-KMS). For information about the Amazon S3 default
    -- encryption feature, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption>
    -- in the /Amazon Simple Storage Service Developer Guide/.
    bucket :: BucketName,
    serverSideEncryptionConfiguration :: ServerSideEncryptionConfiguration
  }
  deriving (Prelude.Eq, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketEncryption' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putBucketEncryption_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentMD5', 'putBucketEncryption_contentMD5' - The base64-encoded 128-bit MD5 digest of the server-side encryption
-- configuration.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
--
-- 'bucket', 'putBucketEncryption_bucket' - Specifies default encryption for a bucket using server-side encryption
-- with Amazon S3-managed keys (SSE-S3) or customer master keys stored in
-- AWS KMS (SSE-KMS). For information about the Amazon S3 default
-- encryption feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption>
-- in the /Amazon Simple Storage Service Developer Guide/.
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
      { expectedBucketOwner =
          Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        bucket = pBucket_,
        serverSideEncryptionConfiguration =
          pServerSideEncryptionConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putBucketEncryption_expectedBucketOwner :: Lens.Lens' PutBucketEncryption (Prelude.Maybe Prelude.Text)
putBucketEncryption_expectedBucketOwner = Lens.lens (\PutBucketEncryption' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketEncryption' {} a -> s {expectedBucketOwner = a} :: PutBucketEncryption)

-- | The base64-encoded 128-bit MD5 digest of the server-side encryption
-- configuration.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putBucketEncryption_contentMD5 :: Lens.Lens' PutBucketEncryption (Prelude.Maybe Prelude.Text)
putBucketEncryption_contentMD5 = Lens.lens (\PutBucketEncryption' {contentMD5} -> contentMD5) (\s@PutBucketEncryption' {} a -> s {contentMD5 = a} :: PutBucketEncryption)

-- | Specifies default encryption for a bucket using server-side encryption
-- with Amazon S3-managed keys (SSE-S3) or customer master keys stored in
-- AWS KMS (SSE-KMS). For information about the Amazon S3 default
-- encryption feature, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/bucket-encryption.html Amazon S3 Default Bucket Encryption>
-- in the /Amazon Simple Storage Service Developer Guide/.
putBucketEncryption_bucket :: Lens.Lens' PutBucketEncryption BucketName
putBucketEncryption_bucket = Lens.lens (\PutBucketEncryption' {bucket} -> bucket) (\s@PutBucketEncryption' {} a -> s {bucket = a} :: PutBucketEncryption)

-- | Undocumented member.
putBucketEncryption_serverSideEncryptionConfiguration :: Lens.Lens' PutBucketEncryption ServerSideEncryptionConfiguration
putBucketEncryption_serverSideEncryptionConfiguration = Lens.lens (\PutBucketEncryption' {serverSideEncryptionConfiguration} -> serverSideEncryptionConfiguration) (\s@PutBucketEncryption' {} a -> s {serverSideEncryptionConfiguration = a} :: PutBucketEncryption)

instance Prelude.AWSRequest PutBucketEncryption where
  type
    Rs PutBucketEncryption =
      PutBucketEncryptionResponse
  request = Request.putXML defaultService
  response =
    Response.receiveNull PutBucketEncryptionResponse'

instance Prelude.Hashable PutBucketEncryption

instance Prelude.NFData PutBucketEncryption

instance Prelude.ToElement PutBucketEncryption where
  toElement PutBucketEncryption' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}ServerSideEncryptionConfiguration"
      serverSideEncryptionConfiguration

instance Prelude.ToHeaders PutBucketEncryption where
  toHeaders PutBucketEncryption' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "Content-MD5" Prelude.=# contentMD5
      ]

instance Prelude.ToPath PutBucketEncryption where
  toPath PutBucketEncryption' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery PutBucketEncryption where
  toQuery =
    Prelude.const (Prelude.mconcat ["encryption"])

-- | /See:/ 'newPutBucketEncryptionResponse' smart constructor.
data PutBucketEncryptionResponse = PutBucketEncryptionResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutBucketEncryptionResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketEncryptionResponse ::
  PutBucketEncryptionResponse
newPutBucketEncryptionResponse =
  PutBucketEncryptionResponse'

instance Prelude.NFData PutBucketEncryptionResponse
