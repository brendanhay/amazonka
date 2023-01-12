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
-- Module      : Amazonka.S3.PutPublicAccessBlock
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates or modifies the @PublicAccessBlock@ configuration for an Amazon
-- S3 bucket. To use this operation, you must have the
-- @s3:PutBucketPublicAccessBlock@ permission. For more information about
-- Amazon S3 permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/using-with-s3-actions.html Specifying Permissions in a Policy>.
--
-- When Amazon S3 evaluates the @PublicAccessBlock@ configuration for a
-- bucket or an object, it checks the @PublicAccessBlock@ configuration for
-- both the bucket (or the bucket that contains the object) and the bucket
-- owner\'s account. If the @PublicAccessBlock@ configurations are
-- different between the bucket and the account, Amazon S3 uses the most
-- restrictive combination of the bucket-level and account-level settings.
--
-- For more information about when Amazon S3 considers a bucket or an
-- object public, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of \"Public\">.
--
-- __Related Resources__
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetPublicAccessBlock.html GetPublicAccessBlock>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeletePublicAccessBlock.html DeletePublicAccessBlock>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketPolicyStatus.html GetBucketPolicyStatus>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html Using Amazon S3 Block Public Access>
module Amazonka.S3.PutPublicAccessBlock
  ( -- * Creating a Request
    PutPublicAccessBlock (..),
    newPutPublicAccessBlock,

    -- * Request Lenses
    putPublicAccessBlock_checksumAlgorithm,
    putPublicAccessBlock_contentMD5,
    putPublicAccessBlock_expectedBucketOwner,
    putPublicAccessBlock_bucket,
    putPublicAccessBlock_publicAccessBlockConfiguration,

    -- * Destructuring the Response
    PutPublicAccessBlockResponse (..),
    newPutPublicAccessBlockResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutPublicAccessBlock' smart constructor.
data PutPublicAccessBlock = PutPublicAccessBlock'
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
    -- | The MD5 hash of the @PutPublicAccessBlock@ request body.
    --
    -- For requests made using the Amazon Web Services Command Line Interface
    -- (CLI) or Amazon Web Services SDKs, this field is calculated
    -- automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration
    -- you want to set.
    bucket :: BucketName,
    -- | The @PublicAccessBlock@ configuration that you want to apply to this
    -- Amazon S3 bucket. You can enable the configuration options in any
    -- combination. For more information about when Amazon S3 considers a
    -- bucket or object public, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of \"Public\">
    -- in the /Amazon S3 User Guide/.
    publicAccessBlockConfiguration :: PublicAccessBlockConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPublicAccessBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putPublicAccessBlock_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'contentMD5', 'putPublicAccessBlock_contentMD5' - The MD5 hash of the @PutPublicAccessBlock@ request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
--
-- 'expectedBucketOwner', 'putPublicAccessBlock_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'putPublicAccessBlock_bucket' - The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration
-- you want to set.
--
-- 'publicAccessBlockConfiguration', 'putPublicAccessBlock_publicAccessBlockConfiguration' - The @PublicAccessBlock@ configuration that you want to apply to this
-- Amazon S3 bucket. You can enable the configuration options in any
-- combination. For more information about when Amazon S3 considers a
-- bucket or object public, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of \"Public\">
-- in the /Amazon S3 User Guide/.
newPutPublicAccessBlock ::
  -- | 'bucket'
  BucketName ->
  -- | 'publicAccessBlockConfiguration'
  PublicAccessBlockConfiguration ->
  PutPublicAccessBlock
newPutPublicAccessBlock
  pBucket_
  pPublicAccessBlockConfiguration_ =
    PutPublicAccessBlock'
      { checksumAlgorithm =
          Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        expectedBucketOwner = Prelude.Nothing,
        bucket = pBucket_,
        publicAccessBlockConfiguration =
          pPublicAccessBlockConfiguration_
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
putPublicAccessBlock_checksumAlgorithm :: Lens.Lens' PutPublicAccessBlock (Prelude.Maybe ChecksumAlgorithm)
putPublicAccessBlock_checksumAlgorithm = Lens.lens (\PutPublicAccessBlock' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutPublicAccessBlock' {} a -> s {checksumAlgorithm = a} :: PutPublicAccessBlock)

-- | The MD5 hash of the @PutPublicAccessBlock@ request body.
--
-- For requests made using the Amazon Web Services Command Line Interface
-- (CLI) or Amazon Web Services SDKs, this field is calculated
-- automatically.
putPublicAccessBlock_contentMD5 :: Lens.Lens' PutPublicAccessBlock (Prelude.Maybe Prelude.Text)
putPublicAccessBlock_contentMD5 = Lens.lens (\PutPublicAccessBlock' {contentMD5} -> contentMD5) (\s@PutPublicAccessBlock' {} a -> s {contentMD5 = a} :: PutPublicAccessBlock)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putPublicAccessBlock_expectedBucketOwner :: Lens.Lens' PutPublicAccessBlock (Prelude.Maybe Prelude.Text)
putPublicAccessBlock_expectedBucketOwner = Lens.lens (\PutPublicAccessBlock' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutPublicAccessBlock' {} a -> s {expectedBucketOwner = a} :: PutPublicAccessBlock)

-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration
-- you want to set.
putPublicAccessBlock_bucket :: Lens.Lens' PutPublicAccessBlock BucketName
putPublicAccessBlock_bucket = Lens.lens (\PutPublicAccessBlock' {bucket} -> bucket) (\s@PutPublicAccessBlock' {} a -> s {bucket = a} :: PutPublicAccessBlock)

-- | The @PublicAccessBlock@ configuration that you want to apply to this
-- Amazon S3 bucket. You can enable the configuration options in any
-- combination. For more information about when Amazon S3 considers a
-- bucket or object public, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of \"Public\">
-- in the /Amazon S3 User Guide/.
putPublicAccessBlock_publicAccessBlockConfiguration :: Lens.Lens' PutPublicAccessBlock PublicAccessBlockConfiguration
putPublicAccessBlock_publicAccessBlockConfiguration = Lens.lens (\PutPublicAccessBlock' {publicAccessBlockConfiguration} -> publicAccessBlockConfiguration) (\s@PutPublicAccessBlock' {} a -> s {publicAccessBlockConfiguration = a} :: PutPublicAccessBlock)

instance Core.AWSRequest PutPublicAccessBlock where
  type
    AWSResponse PutPublicAccessBlock =
      PutPublicAccessBlockResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull PutPublicAccessBlockResponse'

instance Prelude.Hashable PutPublicAccessBlock where
  hashWithSalt _salt PutPublicAccessBlock' {..} =
    _salt `Prelude.hashWithSalt` checksumAlgorithm
      `Prelude.hashWithSalt` contentMD5
      `Prelude.hashWithSalt` expectedBucketOwner
      `Prelude.hashWithSalt` bucket
      `Prelude.hashWithSalt` publicAccessBlockConfiguration

instance Prelude.NFData PutPublicAccessBlock where
  rnf PutPublicAccessBlock' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf contentMD5
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf publicAccessBlockConfiguration

instance Data.ToElement PutPublicAccessBlock where
  toElement PutPublicAccessBlock' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}PublicAccessBlockConfiguration"
      publicAccessBlockConfiguration

instance Data.ToHeaders PutPublicAccessBlock where
  toHeaders PutPublicAccessBlock' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "Content-MD5" Data.=# contentMD5,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath PutPublicAccessBlock where
  toPath PutPublicAccessBlock' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutPublicAccessBlock where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["publicAccessBlock"])

-- | /See:/ 'newPutPublicAccessBlockResponse' smart constructor.
data PutPublicAccessBlockResponse = PutPublicAccessBlockResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutPublicAccessBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutPublicAccessBlockResponse ::
  PutPublicAccessBlockResponse
newPutPublicAccessBlockResponse =
  PutPublicAccessBlockResponse'

instance Prelude.NFData PutPublicAccessBlockResponse where
  rnf _ = ()
