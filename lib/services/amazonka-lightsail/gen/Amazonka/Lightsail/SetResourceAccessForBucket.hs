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
-- Module      : Amazonka.Lightsail.SetResourceAccessForBucket
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Sets the Amazon Lightsail resources that can access the specified
-- Lightsail bucket.
--
-- Lightsail buckets currently support setting access for Lightsail
-- instances in the same Amazon Web Services Region.
module Amazonka.Lightsail.SetResourceAccessForBucket
  ( -- * Creating a Request
    SetResourceAccessForBucket (..),
    newSetResourceAccessForBucket,

    -- * Request Lenses
    setResourceAccessForBucket_resourceName,
    setResourceAccessForBucket_bucketName,
    setResourceAccessForBucket_access,

    -- * Destructuring the Response
    SetResourceAccessForBucketResponse (..),
    newSetResourceAccessForBucketResponse,

    -- * Response Lenses
    setResourceAccessForBucketResponse_operations,
    setResourceAccessForBucketResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newSetResourceAccessForBucket' smart constructor.
data SetResourceAccessForBucket = SetResourceAccessForBucket'
  { -- | The name of the Lightsail instance for which to set bucket access. The
    -- instance must be in a running or stopped state.
    resourceName :: Prelude.Text,
    -- | The name of the bucket for which to set access to another Lightsail
    -- resource.
    bucketName :: Prelude.Text,
    -- | The access setting.
    --
    -- The following access settings are available:
    --
    -- -   @allow@ - Allows access to the bucket and its objects.
    --
    -- -   @deny@ - Denies access to the bucket and its objects. Use this
    --     setting to remove access for a resource previously set to @allow@.
    access :: ResourceBucketAccess
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetResourceAccessForBucket' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceName', 'setResourceAccessForBucket_resourceName' - The name of the Lightsail instance for which to set bucket access. The
-- instance must be in a running or stopped state.
--
-- 'bucketName', 'setResourceAccessForBucket_bucketName' - The name of the bucket for which to set access to another Lightsail
-- resource.
--
-- 'access', 'setResourceAccessForBucket_access' - The access setting.
--
-- The following access settings are available:
--
-- -   @allow@ - Allows access to the bucket and its objects.
--
-- -   @deny@ - Denies access to the bucket and its objects. Use this
--     setting to remove access for a resource previously set to @allow@.
newSetResourceAccessForBucket ::
  -- | 'resourceName'
  Prelude.Text ->
  -- | 'bucketName'
  Prelude.Text ->
  -- | 'access'
  ResourceBucketAccess ->
  SetResourceAccessForBucket
newSetResourceAccessForBucket
  pResourceName_
  pBucketName_
  pAccess_ =
    SetResourceAccessForBucket'
      { resourceName =
          pResourceName_,
        bucketName = pBucketName_,
        access = pAccess_
      }

-- | The name of the Lightsail instance for which to set bucket access. The
-- instance must be in a running or stopped state.
setResourceAccessForBucket_resourceName :: Lens.Lens' SetResourceAccessForBucket Prelude.Text
setResourceAccessForBucket_resourceName = Lens.lens (\SetResourceAccessForBucket' {resourceName} -> resourceName) (\s@SetResourceAccessForBucket' {} a -> s {resourceName = a} :: SetResourceAccessForBucket)

-- | The name of the bucket for which to set access to another Lightsail
-- resource.
setResourceAccessForBucket_bucketName :: Lens.Lens' SetResourceAccessForBucket Prelude.Text
setResourceAccessForBucket_bucketName = Lens.lens (\SetResourceAccessForBucket' {bucketName} -> bucketName) (\s@SetResourceAccessForBucket' {} a -> s {bucketName = a} :: SetResourceAccessForBucket)

-- | The access setting.
--
-- The following access settings are available:
--
-- -   @allow@ - Allows access to the bucket and its objects.
--
-- -   @deny@ - Denies access to the bucket and its objects. Use this
--     setting to remove access for a resource previously set to @allow@.
setResourceAccessForBucket_access :: Lens.Lens' SetResourceAccessForBucket ResourceBucketAccess
setResourceAccessForBucket_access = Lens.lens (\SetResourceAccessForBucket' {access} -> access) (\s@SetResourceAccessForBucket' {} a -> s {access = a} :: SetResourceAccessForBucket)

instance Core.AWSRequest SetResourceAccessForBucket where
  type
    AWSResponse SetResourceAccessForBucket =
      SetResourceAccessForBucketResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          SetResourceAccessForBucketResponse'
            Prelude.<$> (x Core..?> "operations" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable SetResourceAccessForBucket where
  hashWithSalt _salt SetResourceAccessForBucket' {..} =
    _salt `Prelude.hashWithSalt` resourceName
      `Prelude.hashWithSalt` bucketName
      `Prelude.hashWithSalt` access

instance Prelude.NFData SetResourceAccessForBucket where
  rnf SetResourceAccessForBucket' {..} =
    Prelude.rnf resourceName
      `Prelude.seq` Prelude.rnf bucketName
      `Prelude.seq` Prelude.rnf access

instance Core.ToHeaders SetResourceAccessForBucket where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.SetResourceAccessForBucket" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON SetResourceAccessForBucket where
  toJSON SetResourceAccessForBucket' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just ("resourceName" Core..= resourceName),
            Prelude.Just ("bucketName" Core..= bucketName),
            Prelude.Just ("access" Core..= access)
          ]
      )

instance Core.ToPath SetResourceAccessForBucket where
  toPath = Prelude.const "/"

instance Core.ToQuery SetResourceAccessForBucket where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newSetResourceAccessForBucketResponse' smart constructor.
data SetResourceAccessForBucketResponse = SetResourceAccessForBucketResponse'
  { -- | An array of objects that describe the result of the action, such as the
    -- status of the request, the timestamp of the request, and the resources
    -- affected by the request.
    operations :: Prelude.Maybe [Operation],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'SetResourceAccessForBucketResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'operations', 'setResourceAccessForBucketResponse_operations' - An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
--
-- 'httpStatus', 'setResourceAccessForBucketResponse_httpStatus' - The response's http status code.
newSetResourceAccessForBucketResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  SetResourceAccessForBucketResponse
newSetResourceAccessForBucketResponse pHttpStatus_ =
  SetResourceAccessForBucketResponse'
    { operations =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An array of objects that describe the result of the action, such as the
-- status of the request, the timestamp of the request, and the resources
-- affected by the request.
setResourceAccessForBucketResponse_operations :: Lens.Lens' SetResourceAccessForBucketResponse (Prelude.Maybe [Operation])
setResourceAccessForBucketResponse_operations = Lens.lens (\SetResourceAccessForBucketResponse' {operations} -> operations) (\s@SetResourceAccessForBucketResponse' {} a -> s {operations = a} :: SetResourceAccessForBucketResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
setResourceAccessForBucketResponse_httpStatus :: Lens.Lens' SetResourceAccessForBucketResponse Prelude.Int
setResourceAccessForBucketResponse_httpStatus = Lens.lens (\SetResourceAccessForBucketResponse' {httpStatus} -> httpStatus) (\s@SetResourceAccessForBucketResponse' {} a -> s {httpStatus = a} :: SetResourceAccessForBucketResponse)

instance
  Prelude.NFData
    SetResourceAccessForBucketResponse
  where
  rnf SetResourceAccessForBucketResponse' {..} =
    Prelude.rnf operations
      `Prelude.seq` Prelude.rnf httpStatus
