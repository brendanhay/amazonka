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
-- Module      : Amazonka.Lightsail.UpdateBucketBundle
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the bundle, or storage plan, of an existing Amazon Lightsail
-- bucket.
--
-- A bucket bundle specifies the monthly cost, storage space, and data
-- transfer quota for a bucket. You can update a bucket\'s bundle only one
-- time within a monthly Amazon Web Services billing cycle. To determine if
-- you can update a bucket\'s bundle, use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetBuckets.html GetBuckets>
-- action. The @ableToUpdateBundle@ parameter in the response will indicate
-- whether you can currently update a bucket\'s bundle.
--
-- Update a bucket\'s bundle if it\'s consistently going over its storage
-- space or data transfer quota, or if a bucket\'s usage is consistently in
-- the lower range of its storage space or data transfer quota. Due to the
-- unpredictable usage fluctuations that a bucket might experience, we
-- strongly recommend that you update a bucket\'s bundle only as a
-- long-term strategy, instead of as a short-term, monthly cost-cutting
-- measure. Choose a bucket bundle that will provide the bucket with ample
-- storage space and data transfer for a long time to come.
module Amazonka.Lightsail.UpdateBucketBundle
  ( -- * Creating a Request
    UpdateBucketBundle (..),
    newUpdateBucketBundle,

    -- * Request Lenses
    updateBucketBundle_bucketName,
    updateBucketBundle_bundleId,

    -- * Destructuring the Response
    UpdateBucketBundleResponse (..),
    newUpdateBucketBundleResponse,

    -- * Response Lenses
    updateBucketBundleResponse_operations,
    updateBucketBundleResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newUpdateBucketBundle' smart constructor.
data UpdateBucketBundle = UpdateBucketBundle'
  { -- | The name of the bucket for which to update the bundle.
    bucketName :: Prelude.Text,
    -- | The ID of the new bundle to apply to the bucket.
    --
    -- Use the
    -- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetBucketBundles.html GetBucketBundles>
    -- action to get a list of bundle IDs that you can specify.
    bundleId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBucketBundle' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bucketName', 'updateBucketBundle_bucketName' - The name of the bucket for which to update the bundle.
--
-- 'bundleId', 'updateBucketBundle_bundleId' - The ID of the new bundle to apply to the bucket.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetBucketBundles.html GetBucketBundles>
-- action to get a list of bundle IDs that you can specify.
newUpdateBucketBundle ::
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'bundleId'
  Prelude.Text ->
  UpdateBucketBundle
newUpdateBucketBundle pBucketName_ pBundleId_ =
  UpdateBucketBundle'
    { bucketName = pBucketName_,
      bundleId = pBundleId_
    }

-- | The name of the bucket for which to update the bundle.
updateBucketBundle_bucketName :: Lens.Lens' UpdateBucketBundle Prelude.Text
updateBucketBundle_bucketName = Lens.lens (\UpdateBucketBundle' {bucketName} -> bucketName) (\s@UpdateBucketBundle' {} a -> s {bucketName = a} :: UpdateBucketBundle)

-- | The ID of the new bundle to apply to the bucket.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_GetBucketBundles.html GetBucketBundles>
-- action to get a list of bundle IDs that you can specify.
updateBucketBundle_bundleId :: Lens.Lens' UpdateBucketBundle Prelude.Text
updateBucketBundle_bundleId = Lens.lens (\UpdateBucketBundle' {bundleId} -> bundleId) (\s@UpdateBucketBundle' {} a -> s {bundleId = a} :: UpdateBucketBundle)

instance Core.AWSRequest UpdateBucketBundle where
  type
    AWSResponse UpdateBucketBundle =
      UpdateBucketBundleResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateBucketBundleResponse'
            Prelude.<$> (x Data..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateBucketBundle where
  hashWithSalt _salt UpdateBucketBundle' {..} =
    _salt `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` bundleId

instance Prelude.NFData UpdateBucketBundle where
  rnf UpdateBucketBundle' {..} =
    Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf bundleId

instance Data.ToHeaders UpdateBucketBundle where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Lightsail_20161128.UpdateBucketBundle" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateBucketBundle where
  toJSON UpdateBucketBundle' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just ("bucketName" Data..= bucketName),
            Prelude.Just ("bundleId" Data..= bundleId)
          ]
      )

instance Data.ToPath UpdateBucketBundle where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateBucketBundle where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateBucketBundleResponse' smart constructor.
data UpdateBucketBundleResponse = UpdateBucketBundleResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateBucketBundleResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'updateBucketBundleResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'updateBucketBundleResponse_httpStatus' - The response's http status code.
newUpdateBucketBundleResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateBucketBundleResponse
newUpdateBucketBundleResponse pHttpStatus_ =
  UpdateBucketBundleResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
updateBucketBundleResponse_operations :: Lens.Lens' UpdateBucketBundleResponse (Prelude.Maybe [Operation])
updateBucketBundleResponse_operations = Lens.lens (\UpdateBucketBundleResponse' {operations} -> operations) (\s@UpdateBucketBundleResponse' {} a -> s {operations = a} :: UpdateBucketBundleResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
updateBucketBundleResponse_httpStatus :: Lens.Lens' UpdateBucketBundleResponse Prelude.Int
updateBucketBundleResponse_httpStatus = Lens.lens (\UpdateBucketBundleResponse' {httpStatus} -> httpStatus) (\s@UpdateBucketBundleResponse' {} a -> s {httpStatus = a} :: UpdateBucketBundleResponse)

instance Prelude.NFData UpdateBucketBundleResponse where
  rnf UpdateBucketBundleResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
