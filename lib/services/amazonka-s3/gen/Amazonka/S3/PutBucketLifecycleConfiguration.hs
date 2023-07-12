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
-- Module      : Amazonka.S3.PutBucketLifecycleConfiguration
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a new lifecycle configuration for the bucket or replaces an
-- existing lifecycle configuration. Keep in mind that this will overwrite
-- an existing lifecycle configuration, so if you want to retain any
-- configuration details, they must be included in the new lifecycle
-- configuration. For information about lifecycle configuration, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/object-lifecycle-mgmt.html Managing your storage lifecycle>.
--
-- Bucket lifecycle configuration now supports specifying a lifecycle rule
-- using an object key name prefix, one or more object tags, or a
-- combination of both. Accordingly, this section describes the latest API.
-- The previous version of the API supported filtering based only on an
-- object key name prefix, which is supported for backward compatibility.
-- For the related API description, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/API/API_PutBucketLifecycle.html PutBucketLifecycle>.
--
-- __Rules__
--
-- You specify the lifecycle configuration in your request body. The
-- lifecycle configuration is specified as XML consisting of one or more
-- rules. An Amazon S3 Lifecycle configuration can have up to 1,000 rules.
-- This limit is not adjustable. Each rule consists of the following:
--
-- -   Filter identifying a subset of objects to which the rule applies.
--     The filter can be based on a key name prefix, object tags, or a
--     combination of both.
--
-- -   Status whether the rule is in effect.
--
-- -   One or more lifecycle transition and expiration actions that you
--     want Amazon S3 to perform on the objects identified by the filter.
--     If the state of your bucket is versioning-enabled or
--     versioning-suspended, you can have many versions of the same object
--     (one current version and zero or more noncurrent versions). Amazon
--     S3 provides predefined actions that you can specify for current and
--     noncurrent object versions.
--
-- For more information, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/object-lifecycle-mgmt.html Object Lifecycle Management>
-- and
-- <https://docs.aws.amazon.com/AmazonS3/latest/dev/intro-lifecycle-rules.html Lifecycle Configuration Elements>.
--
-- __Permissions__
--
-- By default, all Amazon S3 resources are private, including buckets,
-- objects, and related subresources (for example, lifecycle configuration
-- and website configuration). Only the resource owner (that is, the Amazon
-- Web Services account that created it) can access the resource. The
-- resource owner can optionally grant access permissions to others by
-- writing an access policy. For this operation, a user must get the
-- @s3:PutLifecycleConfiguration@ permission.
--
-- You can also explicitly deny permissions. Explicit deny also supersedes
-- any other permissions. If you want to block users or accounts from
-- removing or deleting objects from your bucket, you must deny them
-- permissions for the following actions:
--
-- -   @s3:DeleteObject@
--
-- -   @s3:DeleteObjectVersion@
--
-- -   @s3:PutLifecycleConfiguration@
--
-- For more information about permissions, see
-- <https://docs.aws.amazon.com/AmazonS3/latest/userguide/s3-access-control.html Managing Access Permissions to Your Amazon S3 Resources>.
--
-- The following are related to @PutBucketLifecycleConfiguration@:
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/dev/lifecycle-configuration-examples.html Examples of Lifecycle Configuration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_GetBucketLifecycleConfiguration.html GetBucketLifecycleConfiguration>
--
-- -   <https://docs.aws.amazon.com/AmazonS3/latest/API/API_DeleteBucketLifecycle.html DeleteBucketLifecycle>
module Amazonka.S3.PutBucketLifecycleConfiguration
  ( -- * Creating a Request
    PutBucketLifecycleConfiguration (..),
    newPutBucketLifecycleConfiguration,

    -- * Request Lenses
    putBucketLifecycleConfiguration_checksumAlgorithm,
    putBucketLifecycleConfiguration_expectedBucketOwner,
    putBucketLifecycleConfiguration_lifecycleConfiguration,
    putBucketLifecycleConfiguration_bucket,

    -- * Destructuring the Response
    PutBucketLifecycleConfigurationResponse (..),
    newPutBucketLifecycleConfigurationResponse,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.S3.Types

-- | /See:/ 'newPutBucketLifecycleConfiguration' smart constructor.
data PutBucketLifecycleConfiguration = PutBucketLifecycleConfiguration'
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
    -- | Container for lifecycle rules. You can add as many as 1,000 rules.
    lifecycleConfiguration :: Prelude.Maybe BucketLifecycleConfiguration,
    -- | The name of the bucket for which to set the configuration.
    bucket :: BucketName
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketLifecycleConfiguration' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'checksumAlgorithm', 'putBucketLifecycleConfiguration_checksumAlgorithm' - Indicates the algorithm used to create the checksum for the object when
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
-- 'expectedBucketOwner', 'putBucketLifecycleConfiguration_expectedBucketOwner' - The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
--
-- 'lifecycleConfiguration', 'putBucketLifecycleConfiguration_lifecycleConfiguration' - Container for lifecycle rules. You can add as many as 1,000 rules.
--
-- 'bucket', 'putBucketLifecycleConfiguration_bucket' - The name of the bucket for which to set the configuration.
newPutBucketLifecycleConfiguration ::
  -- | 'bucket'
  BucketName ->
  PutBucketLifecycleConfiguration
newPutBucketLifecycleConfiguration pBucket_ =
  PutBucketLifecycleConfiguration'
    { checksumAlgorithm =
        Prelude.Nothing,
      expectedBucketOwner = Prelude.Nothing,
      lifecycleConfiguration = Prelude.Nothing,
      bucket = pBucket_
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
putBucketLifecycleConfiguration_checksumAlgorithm :: Lens.Lens' PutBucketLifecycleConfiguration (Prelude.Maybe ChecksumAlgorithm)
putBucketLifecycleConfiguration_checksumAlgorithm = Lens.lens (\PutBucketLifecycleConfiguration' {checksumAlgorithm} -> checksumAlgorithm) (\s@PutBucketLifecycleConfiguration' {} a -> s {checksumAlgorithm = a} :: PutBucketLifecycleConfiguration)

-- | The account ID of the expected bucket owner. If the bucket is owned by a
-- different account, the request fails with the HTTP status code
-- @403 Forbidden@ (access denied).
putBucketLifecycleConfiguration_expectedBucketOwner :: Lens.Lens' PutBucketLifecycleConfiguration (Prelude.Maybe Prelude.Text)
putBucketLifecycleConfiguration_expectedBucketOwner = Lens.lens (\PutBucketLifecycleConfiguration' {expectedBucketOwner} -> expectedBucketOwner) (\s@PutBucketLifecycleConfiguration' {} a -> s {expectedBucketOwner = a} :: PutBucketLifecycleConfiguration)

-- | Container for lifecycle rules. You can add as many as 1,000 rules.
putBucketLifecycleConfiguration_lifecycleConfiguration :: Lens.Lens' PutBucketLifecycleConfiguration (Prelude.Maybe BucketLifecycleConfiguration)
putBucketLifecycleConfiguration_lifecycleConfiguration = Lens.lens (\PutBucketLifecycleConfiguration' {lifecycleConfiguration} -> lifecycleConfiguration) (\s@PutBucketLifecycleConfiguration' {} a -> s {lifecycleConfiguration = a} :: PutBucketLifecycleConfiguration)

-- | The name of the bucket for which to set the configuration.
putBucketLifecycleConfiguration_bucket :: Lens.Lens' PutBucketLifecycleConfiguration BucketName
putBucketLifecycleConfiguration_bucket = Lens.lens (\PutBucketLifecycleConfiguration' {bucket} -> bucket) (\s@PutBucketLifecycleConfiguration' {} a -> s {bucket = a} :: PutBucketLifecycleConfiguration)

instance
  Core.AWSRequest
    PutBucketLifecycleConfiguration
  where
  type
    AWSResponse PutBucketLifecycleConfiguration =
      PutBucketLifecycleConfigurationResponse
  request overrides =
    Request.contentMD5Header
      Prelude.. Request.s3vhost
      Prelude.. Request.putXML (overrides defaultService)
  response =
    Response.receiveNull
      PutBucketLifecycleConfigurationResponse'

instance
  Prelude.Hashable
    PutBucketLifecycleConfiguration
  where
  hashWithSalt
    _salt
    PutBucketLifecycleConfiguration' {..} =
      _salt
        `Prelude.hashWithSalt` checksumAlgorithm
        `Prelude.hashWithSalt` expectedBucketOwner
        `Prelude.hashWithSalt` lifecycleConfiguration
        `Prelude.hashWithSalt` bucket

instance
  Prelude.NFData
    PutBucketLifecycleConfiguration
  where
  rnf PutBucketLifecycleConfiguration' {..} =
    Prelude.rnf checksumAlgorithm
      `Prelude.seq` Prelude.rnf expectedBucketOwner
      `Prelude.seq` Prelude.rnf lifecycleConfiguration
      `Prelude.seq` Prelude.rnf bucket

instance
  Data.ToElement
    PutBucketLifecycleConfiguration
  where
  toElement PutBucketLifecycleConfiguration' {..} =
    Data.mkElement
      "{http://s3.amazonaws.com/doc/2006-03-01/}LifecycleConfiguration"
      lifecycleConfiguration

instance
  Data.ToHeaders
    PutBucketLifecycleConfiguration
  where
  toHeaders PutBucketLifecycleConfiguration' {..} =
    Prelude.mconcat
      [ "x-amz-sdk-checksum-algorithm"
          Data.=# checksumAlgorithm,
        "x-amz-expected-bucket-owner"
          Data.=# expectedBucketOwner
      ]

instance Data.ToPath PutBucketLifecycleConfiguration where
  toPath PutBucketLifecycleConfiguration' {..} =
    Prelude.mconcat ["/", Data.toBS bucket]

instance Data.ToQuery PutBucketLifecycleConfiguration where
  toQuery =
    Prelude.const (Prelude.mconcat ["lifecycle"])

-- | /See:/ 'newPutBucketLifecycleConfigurationResponse' smart constructor.
data PutBucketLifecycleConfigurationResponse = PutBucketLifecycleConfigurationResponse'
  {
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'PutBucketLifecycleConfigurationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
newPutBucketLifecycleConfigurationResponse ::
  PutBucketLifecycleConfigurationResponse
newPutBucketLifecycleConfigurationResponse =
  PutBucketLifecycleConfigurationResponse'

instance
  Prelude.NFData
    PutBucketLifecycleConfigurationResponse
  where
  rnf _ = ()
