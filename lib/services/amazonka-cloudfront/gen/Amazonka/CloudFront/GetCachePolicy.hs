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
-- Module      : Amazonka.CloudFront.GetCachePolicy
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a cache policy, including the following metadata:
--
-- -   The policy\'s identifier.
--
-- -   The date and time when the policy was last modified.
--
-- To get a cache policy, you must provide the policy\'s identifier. If the
-- cache policy is attached to a distribution\'s cache behavior, you can
-- get the policy\'s identifier using @ListDistributions@ or
-- @GetDistribution@. If the cache policy is not attached to a cache
-- behavior, you can get the identifier using @ListCachePolicies@.
module Amazonka.CloudFront.GetCachePolicy
  ( -- * Creating a Request
    GetCachePolicy (..),
    newGetCachePolicy,

    -- * Request Lenses
    getCachePolicy_id,

    -- * Destructuring the Response
    GetCachePolicyResponse (..),
    newGetCachePolicyResponse,

    -- * Response Lenses
    getCachePolicyResponse_cachePolicy,
    getCachePolicyResponse_eTag,
    getCachePolicyResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCachePolicy' smart constructor.
data GetCachePolicy = GetCachePolicy'
  { -- | The unique identifier for the cache policy. If the cache policy is
    -- attached to a distribution\'s cache behavior, you can get the policy\'s
    -- identifier using @ListDistributions@ or @GetDistribution@. If the cache
    -- policy is not attached to a cache behavior, you can get the identifier
    -- using @ListCachePolicies@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCachePolicy' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getCachePolicy_id' - The unique identifier for the cache policy. If the cache policy is
-- attached to a distribution\'s cache behavior, you can get the policy\'s
-- identifier using @ListDistributions@ or @GetDistribution@. If the cache
-- policy is not attached to a cache behavior, you can get the identifier
-- using @ListCachePolicies@.
newGetCachePolicy ::
  -- | 'id'
  Prelude.Text ->
  GetCachePolicy
newGetCachePolicy pId_ = GetCachePolicy' {id = pId_}

-- | The unique identifier for the cache policy. If the cache policy is
-- attached to a distribution\'s cache behavior, you can get the policy\'s
-- identifier using @ListDistributions@ or @GetDistribution@. If the cache
-- policy is not attached to a cache behavior, you can get the identifier
-- using @ListCachePolicies@.
getCachePolicy_id :: Lens.Lens' GetCachePolicy Prelude.Text
getCachePolicy_id = Lens.lens (\GetCachePolicy' {id} -> id) (\s@GetCachePolicy' {} a -> s {id = a} :: GetCachePolicy)

instance Core.AWSRequest GetCachePolicy where
  type
    AWSResponse GetCachePolicy =
      GetCachePolicyResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetCachePolicyResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCachePolicy where
  hashWithSalt _salt GetCachePolicy' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetCachePolicy where
  rnf GetCachePolicy' {..} = Prelude.rnf id

instance Data.ToHeaders GetCachePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetCachePolicy where
  toPath GetCachePolicy' {..} =
    Prelude.mconcat
      ["/2020-05-31/cache-policy/", Data.toBS id]

instance Data.ToQuery GetCachePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCachePolicyResponse' smart constructor.
data GetCachePolicyResponse = GetCachePolicyResponse'
  { -- | The cache policy.
    cachePolicy :: Prelude.Maybe CachePolicy,
    -- | The current version of the cache policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCachePolicyResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachePolicy', 'getCachePolicyResponse_cachePolicy' - The cache policy.
--
-- 'eTag', 'getCachePolicyResponse_eTag' - The current version of the cache policy.
--
-- 'httpStatus', 'getCachePolicyResponse_httpStatus' - The response's http status code.
newGetCachePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCachePolicyResponse
newGetCachePolicyResponse pHttpStatus_ =
  GetCachePolicyResponse'
    { cachePolicy =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The cache policy.
getCachePolicyResponse_cachePolicy :: Lens.Lens' GetCachePolicyResponse (Prelude.Maybe CachePolicy)
getCachePolicyResponse_cachePolicy = Lens.lens (\GetCachePolicyResponse' {cachePolicy} -> cachePolicy) (\s@GetCachePolicyResponse' {} a -> s {cachePolicy = a} :: GetCachePolicyResponse)

-- | The current version of the cache policy.
getCachePolicyResponse_eTag :: Lens.Lens' GetCachePolicyResponse (Prelude.Maybe Prelude.Text)
getCachePolicyResponse_eTag = Lens.lens (\GetCachePolicyResponse' {eTag} -> eTag) (\s@GetCachePolicyResponse' {} a -> s {eTag = a} :: GetCachePolicyResponse)

-- | The response's http status code.
getCachePolicyResponse_httpStatus :: Lens.Lens' GetCachePolicyResponse Prelude.Int
getCachePolicyResponse_httpStatus = Lens.lens (\GetCachePolicyResponse' {httpStatus} -> httpStatus) (\s@GetCachePolicyResponse' {} a -> s {httpStatus = a} :: GetCachePolicyResponse)

instance Prelude.NFData GetCachePolicyResponse where
  rnf GetCachePolicyResponse' {..} =
    Prelude.rnf cachePolicy
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
