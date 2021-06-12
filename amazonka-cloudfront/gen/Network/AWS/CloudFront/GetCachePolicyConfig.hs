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
-- Module      : Network.AWS.CloudFront.GetCachePolicyConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a cache policy configuration.
--
-- To get a cache policy configuration, you must provide the policy’s
-- identifier. If the cache policy is attached to a distribution’s cache
-- behavior, you can get the policy’s identifier using @ListDistributions@
-- or @GetDistribution@. If the cache policy is not attached to a cache
-- behavior, you can get the identifier using @ListCachePolicies@.
module Network.AWS.CloudFront.GetCachePolicyConfig
  ( -- * Creating a Request
    GetCachePolicyConfig (..),
    newGetCachePolicyConfig,

    -- * Request Lenses
    getCachePolicyConfig_id,

    -- * Destructuring the Response
    GetCachePolicyConfigResponse (..),
    newGetCachePolicyConfigResponse,

    -- * Response Lenses
    getCachePolicyConfigResponse_eTag,
    getCachePolicyConfigResponse_cachePolicyConfig,
    getCachePolicyConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetCachePolicyConfig' smart constructor.
data GetCachePolicyConfig = GetCachePolicyConfig'
  { -- | The unique identifier for the cache policy. If the cache policy is
    -- attached to a distribution’s cache behavior, you can get the policy’s
    -- identifier using @ListDistributions@ or @GetDistribution@. If the cache
    -- policy is not attached to a cache behavior, you can get the identifier
    -- using @ListCachePolicies@.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCachePolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getCachePolicyConfig_id' - The unique identifier for the cache policy. If the cache policy is
-- attached to a distribution’s cache behavior, you can get the policy’s
-- identifier using @ListDistributions@ or @GetDistribution@. If the cache
-- policy is not attached to a cache behavior, you can get the identifier
-- using @ListCachePolicies@.
newGetCachePolicyConfig ::
  -- | 'id'
  Core.Text ->
  GetCachePolicyConfig
newGetCachePolicyConfig pId_ =
  GetCachePolicyConfig' {id = pId_}

-- | The unique identifier for the cache policy. If the cache policy is
-- attached to a distribution’s cache behavior, you can get the policy’s
-- identifier using @ListDistributions@ or @GetDistribution@. If the cache
-- policy is not attached to a cache behavior, you can get the identifier
-- using @ListCachePolicies@.
getCachePolicyConfig_id :: Lens.Lens' GetCachePolicyConfig Core.Text
getCachePolicyConfig_id = Lens.lens (\GetCachePolicyConfig' {id} -> id) (\s@GetCachePolicyConfig' {} a -> s {id = a} :: GetCachePolicyConfig)

instance Core.AWSRequest GetCachePolicyConfig where
  type
    AWSResponse GetCachePolicyConfig =
      GetCachePolicyConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetCachePolicyConfigResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetCachePolicyConfig

instance Core.NFData GetCachePolicyConfig

instance Core.ToHeaders GetCachePolicyConfig where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetCachePolicyConfig where
  toPath GetCachePolicyConfig' {..} =
    Core.mconcat
      [ "/2020-05-31/cache-policy/",
        Core.toBS id,
        "/config"
      ]

instance Core.ToQuery GetCachePolicyConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetCachePolicyConfigResponse' smart constructor.
data GetCachePolicyConfigResponse = GetCachePolicyConfigResponse'
  { -- | The current version of the cache policy.
    eTag :: Core.Maybe Core.Text,
    -- | The cache policy configuration.
    cachePolicyConfig :: Core.Maybe CachePolicyConfig,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetCachePolicyConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getCachePolicyConfigResponse_eTag' - The current version of the cache policy.
--
-- 'cachePolicyConfig', 'getCachePolicyConfigResponse_cachePolicyConfig' - The cache policy configuration.
--
-- 'httpStatus', 'getCachePolicyConfigResponse_httpStatus' - The response's http status code.
newGetCachePolicyConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetCachePolicyConfigResponse
newGetCachePolicyConfigResponse pHttpStatus_ =
  GetCachePolicyConfigResponse'
    { eTag = Core.Nothing,
      cachePolicyConfig = Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the cache policy.
getCachePolicyConfigResponse_eTag :: Lens.Lens' GetCachePolicyConfigResponse (Core.Maybe Core.Text)
getCachePolicyConfigResponse_eTag = Lens.lens (\GetCachePolicyConfigResponse' {eTag} -> eTag) (\s@GetCachePolicyConfigResponse' {} a -> s {eTag = a} :: GetCachePolicyConfigResponse)

-- | The cache policy configuration.
getCachePolicyConfigResponse_cachePolicyConfig :: Lens.Lens' GetCachePolicyConfigResponse (Core.Maybe CachePolicyConfig)
getCachePolicyConfigResponse_cachePolicyConfig = Lens.lens (\GetCachePolicyConfigResponse' {cachePolicyConfig} -> cachePolicyConfig) (\s@GetCachePolicyConfigResponse' {} a -> s {cachePolicyConfig = a} :: GetCachePolicyConfigResponse)

-- | The response's http status code.
getCachePolicyConfigResponse_httpStatus :: Lens.Lens' GetCachePolicyConfigResponse Core.Int
getCachePolicyConfigResponse_httpStatus = Lens.lens (\GetCachePolicyConfigResponse' {httpStatus} -> httpStatus) (\s@GetCachePolicyConfigResponse' {} a -> s {httpStatus = a} :: GetCachePolicyConfigResponse)

instance Core.NFData GetCachePolicyConfigResponse
