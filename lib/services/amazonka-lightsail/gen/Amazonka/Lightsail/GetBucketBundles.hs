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
-- Module      : Amazonka.Lightsail.GetBucketBundles
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns the bundles that you can apply to a Amazon Lightsail bucket.
--
-- The bucket bundle specifies the monthly cost, storage quota, and data
-- transfer quota for a bucket.
--
-- Use the
-- <https://docs.aws.amazon.com/lightsail/2016-11-28/api-reference/API_UpdateBucketBundle.html UpdateBucketBundle>
-- action to update the bundle for a bucket.
module Amazonka.Lightsail.GetBucketBundles
  ( -- * Creating a Request
    GetBucketBundles (..),
    newGetBucketBundles,

    -- * Request Lenses
    getBucketBundles_includeInactive,

    -- * Destructuring the Response
    GetBucketBundlesResponse (..),
    newGetBucketBundlesResponse,

    -- * Response Lenses
    getBucketBundlesResponse_bundles,
    getBucketBundlesResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.Lightsail.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetBucketBundles' smart constructor.
data GetBucketBundles = GetBucketBundles'
  { -- | A Boolean value that indicates whether to include inactive (unavailable)
    -- bundles in the response of your request.
    includeInactive :: Prelude.Maybe Prelude.Bool
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketBundles' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'includeInactive', 'getBucketBundles_includeInactive' - A Boolean value that indicates whether to include inactive (unavailable)
-- bundles in the response of your request.
newGetBucketBundles ::
  GetBucketBundles
newGetBucketBundles =
  GetBucketBundles'
    { includeInactive =
        Prelude.Nothing
    }

-- | A Boolean value that indicates whether to include inactive (unavailable)
-- bundles in the response of your request.
getBucketBundles_includeInactive :: Lens.Lens' GetBucketBundles (Prelude.Maybe Prelude.Bool)
getBucketBundles_includeInactive = Lens.lens (\GetBucketBundles' {includeInactive} -> includeInactive) (\s@GetBucketBundles' {} a -> s {includeInactive = a} :: GetBucketBundles)

instance Core.AWSRequest GetBucketBundles where
  type
    AWSResponse GetBucketBundles =
      GetBucketBundlesResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetBucketBundlesResponse'
            Prelude.<$> (x Core..?> "bundles" Core..!@ Prelude.mempty)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetBucketBundles where
  hashWithSalt _salt GetBucketBundles' {..} =
    _salt `Prelude.hashWithSalt` includeInactive

instance Prelude.NFData GetBucketBundles where
  rnf GetBucketBundles' {..} =
    Prelude.rnf includeInactive

instance Core.ToHeaders GetBucketBundles where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Lightsail_20161128.GetBucketBundles" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetBucketBundles where
  toJSON GetBucketBundles' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("includeInactive" Core..=)
              Prelude.<$> includeInactive
          ]
      )

instance Core.ToPath GetBucketBundles where
  toPath = Prelude.const "/"

instance Core.ToQuery GetBucketBundles where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetBucketBundlesResponse' smart constructor.
data GetBucketBundlesResponse = GetBucketBundlesResponse'
  { -- | An object that describes bucket bundles.
    bundles :: Prelude.Maybe [BucketBundle],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetBucketBundlesResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'bundles', 'getBucketBundlesResponse_bundles' - An object that describes bucket bundles.
--
-- 'httpStatus', 'getBucketBundlesResponse_httpStatus' - The response's http status code.
newGetBucketBundlesResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetBucketBundlesResponse
newGetBucketBundlesResponse pHttpStatus_ =
  GetBucketBundlesResponse'
    { bundles =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | An object that describes bucket bundles.
getBucketBundlesResponse_bundles :: Lens.Lens' GetBucketBundlesResponse (Prelude.Maybe [BucketBundle])
getBucketBundlesResponse_bundles = Lens.lens (\GetBucketBundlesResponse' {bundles} -> bundles) (\s@GetBucketBundlesResponse' {} a -> s {bundles = a} :: GetBucketBundlesResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
getBucketBundlesResponse_httpStatus :: Lens.Lens' GetBucketBundlesResponse Prelude.Int
getBucketBundlesResponse_httpStatus = Lens.lens (\GetBucketBundlesResponse' {httpStatus} -> httpStatus) (\s@GetBucketBundlesResponse' {} a -> s {httpStatus = a} :: GetBucketBundlesResponse)

instance Prelude.NFData GetBucketBundlesResponse where
  rnf GetBucketBundlesResponse' {..} =
    Prelude.rnf bundles
      `Prelude.seq` Prelude.rnf httpStatus
