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
-- Module      : Network.AWS.S3.PutPublicAccessBlock
-- Copyright   : (c) 2013-2021 Brendan Hay
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
module Network.AWS.S3.PutPublicAccessBlock
  ( -- * Creating a Request
    PutPublicAccessBlock (..),
    newPutPublicAccessBlock,

    -- * Request Lenses
    putPublicAccessBlock_expectedBucketOwner,
    putPublicAccessBlock_contentMD5,
    putPublicAccessBlock_bucket,
    putPublicAccessBlock_publicAccessBlockConfiguration,

    -- * Destructuring the Response
    PutPublicAccessBlockResponse (..),
    newPutPublicAccessBlockResponse,
  )
where

import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response
import Network.AWS.S3.Types

-- | /See:/ 'newPutPublicAccessBlock' smart constructor.
data PutPublicAccessBlock = PutPublicAccessBlock'
  { -- | The account id of the expected bucket owner. If the bucket is owned by a
    -- different account, the request will fail with an HTTP
    -- @403 (Access Denied)@ error.
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The MD5 hash of the @PutPublicAccessBlock@ request body.
    --
    -- For requests made using the AWS Command Line Interface (CLI) or AWS
    -- SDKs, this field is calculated automatically.
    contentMD5 :: Prelude.Maybe Prelude.Text,
    -- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration
    -- you want to set.
    bucket :: BucketName,
    -- | The @PublicAccessBlock@ configuration that you want to apply to this
    -- Amazon S3 bucket. You can enable the configuration options in any
    -- combination. For more information about when Amazon S3 considers a
    -- bucket or object public, see
    -- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of \"Public\">
    -- in the /Amazon Simple Storage Service Developer Guide/.
    publicAccessBlockConfiguration :: PublicAccessBlockConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutPublicAccessBlock' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'expectedBucketOwner', 'putPublicAccessBlock_expectedBucketOwner' - The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
--
-- 'contentMD5', 'putPublicAccessBlock_contentMD5' - The MD5 hash of the @PutPublicAccessBlock@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
--
-- 'bucket', 'putPublicAccessBlock_bucket' - The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration
-- you want to set.
--
-- 'publicAccessBlockConfiguration', 'putPublicAccessBlock_publicAccessBlockConfiguration' - The @PublicAccessBlock@ configuration that you want to apply to this
-- Amazon S3 bucket. You can enable the configuration options in any
-- combination. For more information about when Amazon S3 considers a
-- bucket or object public, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of \"Public\">
-- in the /Amazon Simple Storage Service Developer Guide/.
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
      { expectedBucketOwner =
          Prelude.Nothing,
        contentMD5 = Prelude.Nothing,
        bucket = pBucket_,
        publicAccessBlockConfiguration =
          pPublicAccessBlockConfiguration_
      }

-- | The account id of the expected bucket owner. If the bucket is owned by a
-- different account, the request will fail with an HTTP
-- @403 (Access Denied)@ error.
putPublicAccessBlock_expectedBucketOwner :: Lens.Lens' PutPublicAccessBlock (Prelude.Maybe Prelude.Text)
putPublicAccessBlock_expectedBucketOwner = Lens.lens (\PutPublicAccessBlock' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutPublicAccessBlock' {} a -> s {expectedBucketOwner = a} :: PutPublicAccessBlock)

-- | The MD5 hash of the @PutPublicAccessBlock@ request body.
--
-- For requests made using the AWS Command Line Interface (CLI) or AWS
-- SDKs, this field is calculated automatically.
putPublicAccessBlock_contentMD5 :: Lens.Lens' PutPublicAccessBlock (Prelude.Maybe Prelude.Text)
putPublicAccessBlock_contentMD5 = Lens.lens (\PutPublicAccessBlock' {contentMD5} -> contentMD5) (\s@PutPublicAccessBlock' {} a -> s {contentMD5 = a} :: PutPublicAccessBlock)

-- | The name of the Amazon S3 bucket whose @PublicAccessBlock@ configuration
-- you want to set.
putPublicAccessBlock_bucket :: Lens.Lens' PutPublicAccessBlock BucketName
putPublicAccessBlock_bucket = Lens.lens (\PutPublicAccessBlock' {bucket} -> bucket) (\s@PutPublicAccessBlock' {} a -> s {bucket = a} :: PutPublicAccessBlock)

-- | The @PublicAccessBlock@ configuration that you want to apply to this
-- Amazon S3 bucket. You can enable the configuration options in any
-- combination. For more information about when Amazon S3 considers a
-- bucket or object public, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/access-control-block-public-access.html#access-control-block-public-access-policy-status The Meaning of \"Public\">
-- in the /Amazon Simple Storage Service Developer Guide/.
putPublicAccessBlock_publicAccessBlockConfiguration :: Lens.Lens' PutPublicAccessBlock PublicAccessBlockConfiguration
putPublicAccessBlock_publicAccessBlockConfiguration = Lens.lens (\PutPublicAccessBlock' {publicAccessBlockConfiguration} -> publicAccessBlockConfiguration) (\s@PutPublicAccessBlock' {} a -> s {publicAccessBlockConfiguration = a} :: PutPublicAccessBlock)

instance Prelude.AWSRequest PutPublicAccessBlock where
  type
    Rs PutPublicAccessBlock =
      PutPublicAccessBlockResponse
  request = Request.putXML defaultService
  response =
    Response.receiveNull PutPublicAccessBlockResponse'

instance Prelude.Hashable PutPublicAccessBlock

instance Prelude.NFData PutPublicAccessBlock

instance Prelude.ToElement PutPublicAccessBlock where
  toElement PutPublicAccessBlock' {..} =
    Prelude.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}PublicAccessBlockConfiguration"
      publicAccessBlockConfiguration

instance Prelude.ToHeaders PutPublicAccessBlock where
  toHeaders PutPublicAccessBlock' {..} =
    Prelude.mconcat
      [ "x-amz-expected-bucket-owner"
          Prelude.=# expectedBucketOwner,
        "Content-MD5" Prelude.=# contentMD5
      ]

instance Prelude.ToPath PutPublicAccessBlock where
  toPath PutPublicAccessBlock' {..} =
    Prelude.mconcat ["/", Prelude.toBS bucket]

instance Prelude.ToQuery PutPublicAccessBlock where
  toQuery =
    Prelude.const
      (Prelude.mconcat ["publicAccessBlock"])

-- | /See:/ 'newPutPublicAccessBlockResponse' smart constructor.
data PutPublicAccessBlockResponse = PutPublicAccessBlockResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Data, Prelude.Typeable, Prelude.Generic)

-- |
-- Create a value of 'PutPublicAccessBlockResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutPublicAccessBlockResponse ::
  PutPublicAccessBlockResponse
newPutPublicAccessBlockResponse =
  PutPublicAccessBlockResponse'

instance Prelude.NFData PutPublicAccessBlockResponse
