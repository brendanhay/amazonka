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
-- Module      : Network.AWS.CloudFront.GetCachePolicy
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a cache policy, including the following metadata:
--
-- -   The policy’s identifier.
--
-- -   The date and time when the policy was last modified.
--
-- To get a cache policy, you must provide the policy’s identifier. If the
-- cache policy is attached to a distribution’s cache behavior, you can get
-- the policy’s identifier using @ListDistributions@ or @GetDistribution@.
-- If the cache policy is not attached to a cache behavior, you can get the
-- identifier using @ListCachePolicies@.
module Network.AWS.CloudFront.GetCachePolicy
  ( -- * Creating a Request
    GetCachePolicy (..),
    newGetCachePolicy,

    -- * Request Lenses
    getCachePolicy_id,

    -- * Destructuring the Response
    GetCachePolicyResponse (..),
    newGetCachePolicyResponse,

    -- * Response Lenses
    getCachePolicyResponse_eTag,
    getCachePolicyResponse_cachePolicy,
    getCachePolicyResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCachePolicy' smart constructor.
data GetCachePolicy = GetCachePolicy'
  { -- | The unique identifier for the cache policy. If the cache policy is
    -- attached to a distribution’s cache behavior, you can get the policy’s
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
-- attached to a distribution’s cache behavior, you can get the policy’s
-- identifier using @ListDistributions@ or @GetDistribution@. If the cache
-- policy is not attached to a cache behavior, you can get the identifier
-- using @ListCachePolicies@.
newGetCachePolicy ::
  -- | 'id'
  Prelude.Text ->
  GetCachePolicy
newGetCachePolicy pId_ = GetCachePolicy' {id = pId_}

-- | The unique identifier for the cache policy. If the cache policy is
-- attached to a distribution’s cache behavior, you can get the policy’s
-- identifier using @ListDistributions@ or @GetDistribution@. If the cache
-- policy is not attached to a cache behavior, you can get the identifier
-- using @ListCachePolicies@.
getCachePolicy_id :: Lens.Lens' GetCachePolicy Prelude.Text
getCachePolicy_id = Lens.lens (\GetCachePolicy' {id} -> id) (\s@GetCachePolicy' {} a -> s {id = a} :: GetCachePolicy)

instance Core.AWSRequest GetCachePolicy where
  type
    AWSResponse GetCachePolicy =
      GetCachePolicyResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetCachePolicyResponse'
            Prelude.<$> (h Core..#? "ETag")
            Prelude.<*> (Core.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCachePolicy

instance Prelude.NFData GetCachePolicy

instance Core.ToHeaders GetCachePolicy where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath GetCachePolicy where
  toPath GetCachePolicy' {..} =
    Prelude.mconcat
      ["/2020-05-31/cache-policy/", Core.toBS id]

instance Core.ToQuery GetCachePolicy where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCachePolicyResponse' smart constructor.
data GetCachePolicyResponse = GetCachePolicyResponse'
  { -- | The current version of the cache policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The cache policy.
    cachePolicy :: Prelude.Maybe CachePolicy,
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
-- 'eTag', 'getCachePolicyResponse_eTag' - The current version of the cache policy.
--
-- 'cachePolicy', 'getCachePolicyResponse_cachePolicy' - The cache policy.
--
-- 'httpStatus', 'getCachePolicyResponse_httpStatus' - The response's http status code.
newGetCachePolicyResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCachePolicyResponse
newGetCachePolicyResponse pHttpStatus_ =
  GetCachePolicyResponse'
    { eTag = Prelude.Nothing,
      cachePolicy = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the cache policy.
getCachePolicyResponse_eTag :: Lens.Lens' GetCachePolicyResponse (Prelude.Maybe Prelude.Text)
getCachePolicyResponse_eTag = Lens.lens (\GetCachePolicyResponse' {eTag} -> eTag) (\s@GetCachePolicyResponse' {} a -> s {eTag = a} :: GetCachePolicyResponse)

-- | The cache policy.
getCachePolicyResponse_cachePolicy :: Lens.Lens' GetCachePolicyResponse (Prelude.Maybe CachePolicy)
getCachePolicyResponse_cachePolicy = Lens.lens (\GetCachePolicyResponse' {cachePolicy} -> cachePolicy) (\s@GetCachePolicyResponse' {} a -> s {cachePolicy = a} :: GetCachePolicyResponse)

-- | The response's http status code.
getCachePolicyResponse_httpStatus :: Lens.Lens' GetCachePolicyResponse Prelude.Int
getCachePolicyResponse_httpStatus = Lens.lens (\GetCachePolicyResponse' {httpStatus} -> httpStatus) (\s@GetCachePolicyResponse' {} a -> s {httpStatus = a} :: GetCachePolicyResponse)

instance Prelude.NFData GetCachePolicyResponse
