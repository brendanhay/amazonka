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
-- Module      : Amazonka.CloudFront.GetCachePolicyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a cache policy configuration.
--
-- To get a cache policy configuration, you must provide the policy\'s
-- identifier. If the cache policy is attached to a distribution\'s cache
-- behavior, you can get the policy\'s identifier using @ListDistributions@
-- or @GetDistribution@. If the cache policy is not attached to a cache
-- behavior, you can get the identifier using @ListCachePolicies@.
module Amazonka.CloudFront.GetCachePolicyConfig
  ( -- * Creating a Request
    GetCachePolicyConfig (..),
    newGetCachePolicyConfig,

    -- * Request Lenses
    getCachePolicyConfig_id,

    -- * Destructuring the Response
    GetCachePolicyConfigResponse (..),
    newGetCachePolicyConfigResponse,

    -- * Response Lenses
    getCachePolicyConfigResponse_cachePolicyConfig,
    getCachePolicyConfigResponse_eTag,
    getCachePolicyConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetCachePolicyConfig' smart constructor.
data GetCachePolicyConfig = GetCachePolicyConfig'
  { -- | The unique identifier for the cache policy. If the cache policy is
    -- attached to a distribution\'s cache behavior, you can get the policy\'s
    -- identifier using @ListDistributions@ or @GetDistribution@. If the cache
    -- policy is not attached to a cache behavior, you can get the identifier
    -- using @ListCachePolicies@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCachePolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getCachePolicyConfig_id' - The unique identifier for the cache policy. If the cache policy is
-- attached to a distribution\'s cache behavior, you can get the policy\'s
-- identifier using @ListDistributions@ or @GetDistribution@. If the cache
-- policy is not attached to a cache behavior, you can get the identifier
-- using @ListCachePolicies@.
newGetCachePolicyConfig ::
  -- | 'id'
  Prelude.Text ->
  GetCachePolicyConfig
newGetCachePolicyConfig pId_ =
  GetCachePolicyConfig' {id = pId_}

-- | The unique identifier for the cache policy. If the cache policy is
-- attached to a distribution\'s cache behavior, you can get the policy\'s
-- identifier using @ListDistributions@ or @GetDistribution@. If the cache
-- policy is not attached to a cache behavior, you can get the identifier
-- using @ListCachePolicies@.
getCachePolicyConfig_id :: Lens.Lens' GetCachePolicyConfig Prelude.Text
getCachePolicyConfig_id = Lens.lens (\GetCachePolicyConfig' {id} -> id) (\s@GetCachePolicyConfig' {} a -> s {id = a} :: GetCachePolicyConfig)

instance Core.AWSRequest GetCachePolicyConfig where
  type
    AWSResponse GetCachePolicyConfig =
      GetCachePolicyConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetCachePolicyConfigResponse'
            Prelude.<$> (Data.parseXML x)
            Prelude.<*> (h Data..#? "ETag")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetCachePolicyConfig where
  hashWithSalt _salt GetCachePolicyConfig' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetCachePolicyConfig where
  rnf GetCachePolicyConfig' {..} = Prelude.rnf id

instance Data.ToHeaders GetCachePolicyConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetCachePolicyConfig where
  toPath GetCachePolicyConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/cache-policy/",
        Data.toBS id,
        "/config"
      ]

instance Data.ToQuery GetCachePolicyConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetCachePolicyConfigResponse' smart constructor.
data GetCachePolicyConfigResponse = GetCachePolicyConfigResponse'
  { -- | The cache policy configuration.
    cachePolicyConfig :: Prelude.Maybe CachePolicyConfig,
    -- | The current version of the cache policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetCachePolicyConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'cachePolicyConfig', 'getCachePolicyConfigResponse_cachePolicyConfig' - The cache policy configuration.
--
-- 'eTag', 'getCachePolicyConfigResponse_eTag' - The current version of the cache policy.
--
-- 'httpStatus', 'getCachePolicyConfigResponse_httpStatus' - The response's http status code.
newGetCachePolicyConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetCachePolicyConfigResponse
newGetCachePolicyConfigResponse pHttpStatus_ =
  GetCachePolicyConfigResponse'
    { cachePolicyConfig =
        Prelude.Nothing,
      eTag = Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The cache policy configuration.
getCachePolicyConfigResponse_cachePolicyConfig :: Lens.Lens' GetCachePolicyConfigResponse (Prelude.Maybe CachePolicyConfig)
getCachePolicyConfigResponse_cachePolicyConfig = Lens.lens (\GetCachePolicyConfigResponse' {cachePolicyConfig} -> cachePolicyConfig) (\s@GetCachePolicyConfigResponse' {} a -> s {cachePolicyConfig = a} :: GetCachePolicyConfigResponse)

-- | The current version of the cache policy.
getCachePolicyConfigResponse_eTag :: Lens.Lens' GetCachePolicyConfigResponse (Prelude.Maybe Prelude.Text)
getCachePolicyConfigResponse_eTag = Lens.lens (\GetCachePolicyConfigResponse' {eTag} -> eTag) (\s@GetCachePolicyConfigResponse' {} a -> s {eTag = a} :: GetCachePolicyConfigResponse)

-- | The response's http status code.
getCachePolicyConfigResponse_httpStatus :: Lens.Lens' GetCachePolicyConfigResponse Prelude.Int
getCachePolicyConfigResponse_httpStatus = Lens.lens (\GetCachePolicyConfigResponse' {httpStatus} -> httpStatus) (\s@GetCachePolicyConfigResponse' {} a -> s {httpStatus = a} :: GetCachePolicyConfigResponse)

instance Prelude.NFData GetCachePolicyConfigResponse where
  rnf GetCachePolicyConfigResponse' {..} =
    Prelude.rnf cachePolicyConfig
      `Prelude.seq` Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf httpStatus
