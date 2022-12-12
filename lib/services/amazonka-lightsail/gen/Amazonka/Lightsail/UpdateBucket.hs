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
-- Module      : Amazonka.Lightsail.UpdateBucket
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates an existing Amazon Lightsail bucket.
--
-- Use this action to update the configuration of an existing bucket, such
-- as versioning, public accessibility, and the Amazon Web Services
-- accounts that can access the bucket.
module Amazonka.Lightsail.UpdateBucket
  ( -- * Creating a Request
    UpdateBucket (..),
    newUpdateBucket,

    -- * Request Lenses
    updateBucket_accessLogConfig,
    updateBucket_accessRules,
    updateBucket_readonlyAccessAccounts,
    updateBucket_versioning,
    updateBucket_bucketName,

    -- * Destructuring the Response
    UpdateBucketResponse (..),
    newUpdateBucketResponse,

    -- * Response Lenses
    updateBucketResponse_bucket,
    updateBucketResponse_operations,
    updateBucketResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBucket' smart constructor.
data UpdateBucket = UpdateBucket'
  { -- | An object that describes the access log configuration for the bucket.
    accessLogConfig :: Prelude.Maybe BucketAccessLogConfig,
    -- | An object that sets the public accessibility of objects in the specified
    -- bucket.
    accessRules :: Prelude.Maybe AccessRules,
    -- | An array of strings to specify the Amazon Web Services account IDs that
    -- can access the bucket.
    --
    -- You can give a maximum of 10 Amazon Web Services accounts access to a
    -- bucket.
    readonlyAccessAccounts :: Prelude.Maybe [Prelude.Text],
    -- | Specifies whether to enable or suspend versioning of objects in the
    -- bucket.
    --
    -- The following options can be specified:
    --
    -- -   @Enabled@ - Enables versioning of objects in the specified bucket.
    --
    -- -   @Suspended@ - Suspends versioning of objects in the specified
    --     bucket. Existing object versions are retained.
    versioning :: Prelude.Maybe Prelude.Text,
    -- | The name of the bucket to update.
    bucketName :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'accessLogConfig', 'updateBucket_accessLogConfig' - An object that describes the access log configuration for the bucket.
--
-- 'accessRules', 'updateBucket_accessRules' - An object that sets the public accessibility of objects in the specified
-- bucket.
--
-- 'readonlyAccessAccounts', 'updateBucket_readonlyAccessAccounts' - An array of strings to specify the Amazon Web Services account IDs that
-- can access the bucket.
--
-- You can give a maximum of 10 Amazon Web Services accounts access to a
-- bucket.
--
-- 'versioning', 'updateBucket_versioning' - Specifies whether to enable or suspend versioning of objects in the
-- bucket.
--
-- The following options can be specified:
--
-- -   @Enabled@ - Enables versioning of objects in the specified bucket.
--
-- -   @Suspended@ - Suspends versioning of objects in the specified
--     bucket. Existing object versions are retained.
--
-- 'bucketName', 'updateBucket_bucketName' - The name of the bucket to update.
newUpdateBucket ::
  -- | 'bucketName'
  Prelude.Text ->
  UpdateBucket
newUpdateBucket pBucketName_ =
  UpdateBucket'
    { accessLogConfig = Prelude.Nothing,
      accessRules = Prelude.Nothing,
      readonlyAccessAccounts = Prelude.Nothing,
      versioning = Prelude.Nothing,
      bucketName = pBucketName_
    }

-- | An object that describes the access log configuration for the bucket.
updateBucket_accessLogConfig :: Lens.Lens' UpdateBucket (Prelude.Maybe BucketAccessLogConfig)
updateBucket_accessLogConfig = Lens.lens (\UpdateBucket' {accessLogConfig} -> accessLogConfig) (\s@UpdateBucket' {} a -> s {accessLogConfig = a} :: UpdateBucket)

-- | An object that sets the public accessibility of objects in the specified
-- bucket.
updateBucket_accessRules :: Lens.Lens' UpdateBucket (Prelude.Maybe AccessRules)
updateBucket_accessRules = Lens.lens (\UpdateBucket' {accessRules} -> accessRules) (\s@UpdateBucket' {} a -> s {accessRules = a} :: UpdateBucket)

-- | An array of strings to specify the Amazon Web Services account IDs that
-- can access the bucket.
--
-- You can give a maximum of 10 Amazon Web Services accounts access to a
-- bucket.
updateBucket_readonlyAccessAccounts :: Lens.Lens' UpdateBucket (Prelude.Maybe [Prelude.Text])
updateBucket_readonlyAccessAccounts = Lens.lens (\UpdateBucket' {readonlyAccessAccounts} -> readonlyAccessAccounts) (\s@UpdateBucket' {} a -> s {readonlyAccessAccounts = a} :: UpdateBucket) Prelude.. Lens.mapping Lens.coerced

-- | Specifies whether to enable or suspend versioning of objects in the
-- bucket.
--
-- The following options can be specified:
--
-- -   @Enabled@ - Enables versioning of objects in the specified bucket.
--
-- -   @Suspended@ - Suspends versioning of objects in the specified
--     bucket. Existing object versions are retained.
updateBucket_versioning :: Lens.Lens' UpdateBucket (Prelude.Maybe Prelude.Text)
updateBucket_versioning = Lens.lens (\UpdateBucket' {versioning} -> versioning) (\s@UpdateBucket' {} a -> s {versioning = a} :: UpdateBucket)

-- | The name of the bucket to update.
updateBucket_bucketName :: Lens.Lens' UpdateBucket Prelude.Text
updateBucket_bucketName = Lens.lens (\UpdateBucket' {bucketName} -> bucketName) (\s@UpdateBucket' {} a -> s {bucketName = a} :: UpdateBucket)

instance Core.AWSRequest UpdateBucket where
  type AWSResponse UpdateBucket = UpdateBucketResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBucketResponse'
            Prelude.<$> (x Data..?> "bucket")
            Prelude.<*> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBucket where
  hashWithSalt _salt UpdateBucket' {..} =
    _salt `Prelude.hashWithSalt` accessLogConfig
      `Prelude.hashWithSalt` accessRules
      `Prelude.hashWithSalt` readonlyAccessAccounts
      `Prelude.hashWithSalt` versioning
      `Prelude.hashWithSalt` bucketName

instance Prelude.NFData UpdateBucket where
  rnf UpdateBucket' {..} =
    Prelude.rnf accessLogConfig
      `Prelude.seq` Prelude.rnf accessRules
      `Prelude.seq` Prelude.rnf readonlyAccessAccounts
      `Prelude.seq` Prelude.rnf versioning
      `Prelude.seq` Prelude.rnf bucketName

instance Data.ToHeaders UpdateBucket where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.UpdateBucket" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBucket where
  toJSON UpdateBucket' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("accessLogConfig" Data..=)
              Prelude.<$> accessLogConfig,
            ("accessRules" Data..=) Prelude.<$> accessRules,
            ("readonlyAccessAccounts" Data..=)
              Prelude.<$> readonlyAccessAccounts,
            ("versioning" Data..=) Prelude.<$> versioning,
            Prelude.Just ("bucketName" Data..= bucketName)
          ]
      )

instance Data.ToPath UpdateBucket where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateBucket where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBucketResponse' smart constructor.
data UpdateBucketResponse = UpdateBucketResponse'
  { -- | An object that describes the bucket that is updated.
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
-- Create a value of 'UpdateBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucket', 'updateBucketResponse_bucket' - An object that describes the bucket that is updated.
--
-- 'operations', 'updateBucketResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'updateBucketResponse_httpStatus' - The response's http status code.
newUpdateBucketResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBucketResponse
newUpdateBucketResponse pHttpStatus_ =
  UpdateBucketResponse'
    { bucket = Prelude.Nothing,
      operations = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes the bucket that is updated.
updateBucketResponse_bucket :: Lens.Lens' UpdateBucketResponse (Prelude.Maybe Bucket)
updateBucketResponse_bucket = Lens.lens (\UpdateBucketResponse' {bucket} -> bucket) (\s@UpdateBucketResponse' {} a -> s {bucket = a} :: UpdateBucketResponse)

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
updateBucketResponse_operations :: Lens.Lens' UpdateBucketResponse (Prelude.Maybe [Operation])
updateBucketResponse_operations = Lens.lens (\UpdateBucketResponse' {operations} -> operations) (\s@UpdateBucketResponse' {} a -> s {operations = a} :: UpdateBucketResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateBucketResponse_httpStatus :: Lens.Lens' UpdateBucketResponse Prelude.Int
updateBucketResponse_httpStatus = Lens.lens (\UpdateBucketResponse' {httpStatus} -> httpStatus) (\s@UpdateBucketResponse' {} a -> s {httpStatus = a} :: UpdateBucketResponse)

instance Prelude.NFData UpdateBucketResponse where
  rnf UpdateBucketResponse' {..} =
    Prelude.rnf bucket
      `Prelude.seq` Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
