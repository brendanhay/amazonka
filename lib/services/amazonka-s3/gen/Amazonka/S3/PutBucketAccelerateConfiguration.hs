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
-- Module      : Amazonka.S3.PutBucketAccelerateConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the accelerate configuration of an existing bucket. Amazon S3
-- Transfer Acceleration is a bucket-level feature that enables you to
-- perform faster data transfers to Amazon S3.
--
-- To use this operation, you must have permission to perform the
-- @s3:PutAccelerateConfiguration@ action. The bucket owner has this
-- permission by default. The bucket owner can grant this permission to
-- others. For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/using-with-s3-actions.html#using-with-s3-actions-related-to-bucket-subresources Permissions Related to Bucket Subresource Operations>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- The Transfer Acceleration state of a bucket can be set to one of the
-- following two values:
--
-- -   Enabled – Enables accelerated data transfers to the bucket.
--
-- -   Suspended – Disables accelerated data transfers to the bucket.
--
-- The
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAccelerateConfiguration.html GetBucketAccelerateConfiguration>
-- action returns the transfer acceleration state of a bucket.
--
-- After setting the Transfer Acceleration state of a bucket to Enabled, it
-- might take up to thirty minutes before the data transfer rates to the
-- bucket increase.
--
-- The name of the bucket used for Transfer Acceleration must be
-- DNS-compliant and must not contain periods (\".\").
--
-- For more information about transfer acceleration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/transfer-acceleration.html Transfer Acceleration>.
--
-- The following operations are related to
-- @PutBucketAccelerateConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketAccelerateConfiguration.html GetBucketAccelerateConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_CreateBucket.html CreateBucket>
module Amazonka.S3.PutBucketAccelerateConfiguration
  ( -- * Creating a Request
    PutBucketAccelerateConfiguration (..),
    newPutBucketAccelerateConfiguration,

    -- * Request Lenses
    putBucketAccelerateConfiguration_checksumAlgorithm,
    putBucketAccelerateConfiguration_expectedBucketOwner,
    putBucketAccelerateConfiguration_bucket,
    putBucketAccelerateConfiguration_accelerateConfiguration,

    -- * Destructuring the Response
    PutBucketAccelerateConfigurationResponse (..),
    newPutBucketAccelerateConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketAccelerateConfiguration' smart constructor.
data PutBucketAccelerateConfiguration = PutBucketAccelerateConfiguration'
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
    -- | The account ID of the expected bucket owner. If the bucket is owned by a
    -- different account, the request fails with the HTTP status code
    -- @403 Forbidden@ (access denied).
    expectedBucketOwner :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket for which the accelerate configuration is set.
    bucket :: BucketName,
    -- | Container for setting the transfer acceleration state.
    accelerateConfiguration :: AccelerateConfiguration
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketAccelerateConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putBucketAccelerateConfiguration_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'expectedBucketOwner', 'putBucketAccelerateConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'bucket', 'putBucketAccelerateConfiguration_bucket' - The name of the bucket for which the accelerate configuration is set.
--
-- 'accelerateConfiguration', 'putBucketAccelerateConfiguration_accelerateConfiguration' - Container for setting the transfer acceleration state.
newPutBucketAccelerateConfiguration ::
  -- | 'bucket'
  BucketName ->
  -- | 'accelerateConfiguration'
  AccelerateConfiguration ->
  PutBucketAccelerateConfiguration
newPutBucketAccelerateConfiguration
  pBucket_
  pAccelerateConfiguration_ =
    PutBucketAccelerateConfiguration'
      { checksumAlgorithm =
          Prelude.Nothing,
        expectedBucketOwner = Prelude.Nothing,
        bucket = pBucket_,
        accelerateConfiguration =
          pAccelerateConfiguration_
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
putBucketAccelerateConfiguration_checksumAlgorithm :: Lens.Lens' PutBucketAccelerateConfiguration (Prelude.Maybe ChecksumAlgorithm)
putBucketAccelerateConfiguration_checksumAlgorithm = Lens.lens (\PutBucketAccelerateConfiguration' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutBucketAccelerateConfiguration' {} a -> s {checksumAlgorithm = a} :: PutBucketAccelerateConfiguration)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketAccelerateConfiguration_expectedBucketOwner :: Lens.Lens' PutBucketAccelerateConfiguration (Prelude.Maybe Prelude.Text)
putBucketAccelerateConfiguration_expectedBucketOwner = Lens.lens (\PutBucketAccelerateConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketAccelerateConfiguration' {} a -> s {expectedBucketOwner = a} :: PutBucketAccelerateConfiguration)

-- | The name of the bucket for which the accelerate configuration is set.
putBucketAccelerateConfiguration_bucket :: Lens.Lens' PutBucketAccelerateConfiguration BucketName
putBucketAccelerateConfiguration_bucket = Lens.lens (\PutBucketAccelerateConfiguration' {bucket} -> bucket) (\s@PutBucketAccelerateConfiguration' {} a -> s {bucket = a} :: PutBucketAccelerateConfiguration)

-- | Container for setting the transfer acceleration state.
putBucketAccelerateConfiguration_accelerateConfiguration :: Lens.Lens' PutBucketAccelerateConfiguration AccelerateConfiguration
putBucketAccelerateConfiguration_accelerateConfiguration = Lens.lens (\PutBucketAccelerateConfiguration' {accelerateConfiguration} -> accelerateConfiguration) (\s@PutBucketAccelerateConfiguration' {} a -> s {accelerateConfiguration = a} :: PutBucketAccelerateConfiguration)

instance
  Core.AWSRequest
    PutBucketAccelerateConfiguration
  where
  type
    AWSResponse PutBucketAccelerateConfiguration =
      PutBucketAccelerateConfigurationResponse
  request overrides =
    Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull
      PutBucketAccelerateConfigurationResponse'

instance
  Prelude.Hashable
    PutBucketAccelerateConfiguration
  where
  hashWithSalt
    _salt
    PutBucketAccelerateConfiguration' {..} =
      _salt `Prelude.hashWithSalt` checksumAlgorithm
        `Prelude.hashWithSalt` expectedBucketOwner
        `Prelude.hashWithSalt` bucket
        `Prelude.hashWithSalt` accelerateConfiguration

instance
  Prelude.NFData
    PutBucketAccelerateConfiguration
  where
  rnf PutBucketAccelerateConfiguration' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf accelerateConfiguration

instance
  Data.ToElement
    PutBucketAccelerateConfiguration
  where
  toElement PutBucketAccelerateConfiguration' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}AccelerateConfiguration"
      accelerateConfiguration

instance
  Data.ToHeaders
    PutBucketAccelerateConfiguration
  where
  toHeaders PutBucketAccelerateConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath PutBucketAccelerateConfiguration where
  toPath PutBucketAccelerateConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance
  Data.ToQuery
    PutBucketAccelerateConfiguration
  where
  toQuery =
    Prelude.const (Prelude.mconcat ["accelerate"])

-- | /See:/ 'newPutBucketAccelerateConfigurationResponse' smart constructor.
data PutBucketAccelerateConfigurationResponse = PutBucketAccelerateConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketAccelerateConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketAccelerateConfigurationResponse ::
  PutBucketAccelerateConfigurationResponse
newPutBucketAccelerateConfigurationResponse =
  PutBucketAccelerateConfigurationResponse'

instance
  Prelude.NFData
    PutBucketAccelerateConfigurationResponse
  where
  rnf _ = ()
