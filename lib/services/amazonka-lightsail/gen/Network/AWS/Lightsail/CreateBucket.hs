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
-- Module      : Network.AWS.Lightsail.CreateBucket
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an Amazon Lightsail bucket.
--
-- A bucket is a cloud storage resource available in the Lightsail object
-- storage service. Use buckets to store objects such as data and its
-- descriptive metadata. For more information about buckets, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/buckets-in-amazon-lightsail Buckets in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
module Network.AWS.Lightsail.CreateBucket
  ( -- * Creating a Request
    CreateBucket (..),
    newCreateBucket,

    -- * Request Lenses
    createBucket_enableObjectVersioning,
    createBucket_tags,
    createBucket_bucketName,
    createBucket_bundleId,

    -- * Destructuring the Response
    CreateBucketResponse (..),
    newCreateBucketResponse,

    -- * Response Lenses
    createBucketResponse_bucket,
    createBucketResponse_operations,
    createBucketResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import Network.AWS.Lightsail.Types
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateBucket' smart constructor.
data CreateBucket = CreateBucket'
  { -- | A Boolean value that indicates whether to enable versioning of objects
    -- in the bucket.
    --
    -- For more information about versioning, see
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-managing-bucket-object-versioning Enabling and suspending object versioning in a bucket in Amazon Lightsail>
    -- in the /Amazon Lightsail Developer Guide/.
    enableObjectVersioning :: Prelude.Maybe Prelude.Bool,
    -- | The tag keys and optional values to add to the bucket during creation.
    --
    -- Use the TagResource action to tag the bucket after it\'s created.
    tags :: Prelude.Maybe [Tag],
    -- | The name for the bucket.
    --
    -- For more information about bucket names, see
    -- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/bucket-naming-rules-in-amazon-lightsail Bucket naming rules in Amazon Lightsail>
    -- in the /Amazon Lightsail Developer Guide/.
    bucketName :: Prelude.Text,
    -- | The ID of the bundle to use for the bucket.
    --
    -- A bucket bundle specifies the monthly cost, storage space, and data
    -- transfer quota for a bucket.
    --
    -- Use the GetBucketBundles action to get a list of bundle IDs that you can
    -- specify.
    --
    -- Use the UpdateBucketBundle action to change the bundle after the bucket
    -- is created.
    bundleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'enableObjectVersioning', 'createBucket_enableObjectVersioning' - A Boolean value that indicates whether to enable versioning of objects
-- in the bucket.
--
-- For more information about versioning, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-managing-bucket-object-versioning Enabling and suspending object versioning in a bucket in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- 'tags', 'createBucket_tags' - The tag keys and optional values to add to the bucket during creation.
--
-- Use the TagResource action to tag the bucket after it\'s created.
--
-- 'bucketName', 'createBucket_bucketName' - The name for the bucket.
--
-- For more information about bucket names, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/bucket-naming-rules-in-amazon-lightsail Bucket naming rules in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
--
-- 'bundleId', 'createBucket_bundleId' - The ID of the bundle to use for the bucket.
--
-- A bucket bundle specifies the monthly cost, storage space, and data
-- transfer quota for a bucket.
--
-- Use the GetBucketBundles action to get a list of bundle IDs that you can
-- specify.
--
-- Use the UpdateBucketBundle action to change the bundle after the bucket
-- is created.
newCreateBucket ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'bundleId'
  Prelude.Text ->
  CreateBucket
newCreateBucket pBucketName_ pBundleId_ =
  CreateBucket'
    { enableObjectVersioning =
        Prelude.Nothing,
      tags = Prelude.Nothing,
      bucketName = pBucketName_,
      bundleId = pBundleId_
    }

-- | A Boolean value that indicates whether to enable versioning of objects
-- in the bucket.
--
-- For more information about versioning, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/amazon-lightsail-managing-bucket-object-versioning Enabling and suspending object versioning in a bucket in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
createBucket_enableObjectVersioning :: Lens.Lens' CreateBucket (Prelude.Maybe Prelude.Bool)
createBucket_enableObjectVersioning = Lens.lens (\CreateBucket' {enableObjectVersioning} -> enableObjectVersioning) (\s@CreateBucket' {} a -> s {enableObjectVersioning = a} :: CreateBucket)

-- | The tag keys and optional values to add to the bucket during creation.
--
-- Use the TagResource action to tag the bucket after it\'s created.
createBucket_tags :: Lens.Lens' CreateBucket (Prelude.Maybe [Tag])
createBucket_tags = Lens.lens (\CreateBucket' {tags} -> tags) (\s@CreateBucket' {} a -> s {tags = a} :: CreateBucket) Prelude.. Lens.mapping Lens.coerced

-- | The name for the bucket.
--
-- For more information about bucket names, see
-- <https://lightsail.aws.amazon.com/ls/docs/en_us/articles/bucket-naming-rules-in-amazon-lightsail Bucket naming rules in Amazon Lightsail>
-- in the /Amazon Lightsail Developer Guide/.
createBucket_bucketName :: Lens.Lens' CreateBucket Prelude.Text
createBucket_bucketName = Lens.lens (\CreateBucket' {bucketName} -> bucketName) (\s@CreateBucket' {} a -> s {bucketName = a} :: CreateBucket)

-- | The ID of the bundle to use for the bucket.
--
-- A bucket bundle specifies the monthly cost, storage space, and data
-- transfer quota for a bucket.
--
-- Use the GetBucketBundles action to get a list of bundle IDs that you can
-- specify.
--
-- Use the UpdateBucketBundle action to change the bundle after the bucket
-- is created.
createBucket_bundleId :: Lens.Lens' CreateBucket Prelude.Text
createBucket_bundleId = Lens.lens (\CreateBucket' {bundleId} -> bundleId) (\s@CreateBucket' {} a -> s {bundleId = a} :: CreateBucket)

instance Core.AWSRequest CreateBucket where
  type AWSResponse CreateBucket = CreateBucketResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateBucketResponse'
            Prelude.<$> (x Core..?> "bucket")
            Prelude.<*> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateBucket

instance Prelude.NFData CreateBucket

instance Core.ToHeaders CreateBucket where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.CreateBucket" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON CreateBucket where
  toJSON CreateBucket' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("enableObjectVersioning" Core..=)
              Prelude.<$> enableObjectVersioning,
            ("tags" Core..=) Prelude.<$> tags,
            Prelude.Just ("bucketName" Core..= bucketName),
            Prelude.Just ("bundleId" Core..= bundleId)
          ]
      )

instance Core.ToPath CreateBucket where
  toPath = Prelude.const "/"

instance Core.ToQuery CreateBucket where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateBucketResponse' smart constructor.
data CreateBucketResponse = CreateBucketResponse'
  { -- | An object that describes the bucket that is created.
    bucket :: Prelude.Maybe Bucket,
    -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'createBucketResponse_bucket' - An object that describes the bucket that is created.
--
-- 'operations', 'createBucketResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'createBucketResponse_httpStatus' - The response's http status code.
newCreateBucketResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateBucketResponse
newCreateBucketResponse pHttpStatus_ =
  CreateBucketResponse'
    { bucket = Prelude.Nothing,
      operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the bucket that is created.
createBucketResponse_bucket :: Lens.Lens' CreateBucketResponse (Prelude.Maybe Bucket)
createBucketResponse_bucket = Lens.lens (\CreateBucketResponse' {bucket} -> bucket) (\s@CreateBucketResponse' {} a -> s {bucket = a} :: CreateBucketResponse)

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
createBucketResponse_operations :: Lens.Lens' CreateBucketResponse (Prelude.Maybe [Operation])
createBucketResponse_operations = Lens.lens (\CreateBucketResponse' {operations} -> operations) (\s@CreateBucketResponse' {} a -> s {operations = a} :: CreateBucketResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
createBucketResponse_httpStatus :: Lens.Lens' CreateBucketResponse Prelude.Int
createBucketResponse_httpStatus = Lens.lens (\CreateBucketResponse' {httpStatus} -> httpStatus) (\s@CreateBucketResponse' {} a -> s {httpStatus = a} :: CreateBucketResponse)

instance Prelude.NFData CreateBucketResponse
