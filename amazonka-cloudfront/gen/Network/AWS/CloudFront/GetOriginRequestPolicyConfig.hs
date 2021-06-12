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
-- Module      : Network.AWS.CloudFront.GetOriginRequestPolicyConfig
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an origin request policy configuration.
--
-- To get an origin request policy configuration, you must provide the
-- policy’s identifier. If the origin request policy is attached to a
-- distribution’s cache behavior, you can get the policy’s identifier using
-- @ListDistributions@ or @GetDistribution@. If the origin request policy
-- is not attached to a cache behavior, you can get the identifier using
-- @ListOriginRequestPolicies@.
module Network.AWS.CloudFront.GetOriginRequestPolicyConfig
  ( -- * Creating a Request
    GetOriginRequestPolicyConfig (..),
    newGetOriginRequestPolicyConfig,

    -- * Request Lenses
    getOriginRequestPolicyConfig_id,

    -- * Destructuring the Response
    GetOriginRequestPolicyConfigResponse (..),
    newGetOriginRequestPolicyConfigResponse,

    -- * Response Lenses
    getOriginRequestPolicyConfigResponse_eTag,
    getOriginRequestPolicyConfigResponse_originRequestPolicyConfig,
    getOriginRequestPolicyConfigResponse_httpStatus,
  )
where

import Network.AWS.CloudFront.Types
import qualified Network.AWS.Core as Core
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newGetOriginRequestPolicyConfig' smart constructor.
data GetOriginRequestPolicyConfig = GetOriginRequestPolicyConfig'
  { -- | The unique identifier for the origin request policy. If the origin
    -- request policy is attached to a distribution’s cache behavior, you can
    -- get the policy’s identifier using @ListDistributions@ or
    -- @GetDistribution@. If the origin request policy is not attached to a
    -- cache behavior, you can get the identifier using
    -- @ListOriginRequestPolicies@.
    id :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOriginRequestPolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getOriginRequestPolicyConfig_id' - The unique identifier for the origin request policy. If the origin
-- request policy is attached to a distribution’s cache behavior, you can
-- get the policy’s identifier using @ListDistributions@ or
-- @GetDistribution@. If the origin request policy is not attached to a
-- cache behavior, you can get the identifier using
-- @ListOriginRequestPolicies@.
newGetOriginRequestPolicyConfig ::
  -- | 'id'
  Core.Text ->
  GetOriginRequestPolicyConfig
newGetOriginRequestPolicyConfig pId_ =
  GetOriginRequestPolicyConfig' {id = pId_}

-- | The unique identifier for the origin request policy. If the origin
-- request policy is attached to a distribution’s cache behavior, you can
-- get the policy’s identifier using @ListDistributions@ or
-- @GetDistribution@. If the origin request policy is not attached to a
-- cache behavior, you can get the identifier using
-- @ListOriginRequestPolicies@.
getOriginRequestPolicyConfig_id :: Lens.Lens' GetOriginRequestPolicyConfig Core.Text
getOriginRequestPolicyConfig_id = Lens.lens (\GetOriginRequestPolicyConfig' {id} -> id) (\s@GetOriginRequestPolicyConfig' {} a -> s {id = a} :: GetOriginRequestPolicyConfig)

instance Core.AWSRequest GetOriginRequestPolicyConfig where
  type
    AWSResponse GetOriginRequestPolicyConfig =
      GetOriginRequestPolicyConfigResponse
  request = Request.get defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          GetOriginRequestPolicyConfigResponse'
            Core.<$> (h Core..#? "ETag")
            Core.<*> (Core.parseXML x)
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable GetOriginRequestPolicyConfig

instance Core.NFData GetOriginRequestPolicyConfig

instance Core.ToHeaders GetOriginRequestPolicyConfig where
  toHeaders = Core.const Core.mempty

instance Core.ToPath GetOriginRequestPolicyConfig where
  toPath GetOriginRequestPolicyConfig' {..} =
    Core.mconcat
      [ "/2020-05-31/origin-request-policy/",
        Core.toBS id,
        "/config"
      ]

instance Core.ToQuery GetOriginRequestPolicyConfig where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newGetOriginRequestPolicyConfigResponse' smart constructor.
data GetOriginRequestPolicyConfigResponse = GetOriginRequestPolicyConfigResponse'
  { -- | The current version of the origin request policy.
    eTag :: Core.Maybe Core.Text,
    -- | The origin request policy configuration.
    originRequestPolicyConfig :: Core.Maybe OriginRequestPolicyConfig,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'GetOriginRequestPolicyConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getOriginRequestPolicyConfigResponse_eTag' - The current version of the origin request policy.
--
-- 'originRequestPolicyConfig', 'getOriginRequestPolicyConfigResponse_originRequestPolicyConfig' - The origin request policy configuration.
--
-- 'httpStatus', 'getOriginRequestPolicyConfigResponse_httpStatus' - The response's http status code.
newGetOriginRequestPolicyConfigResponse ::
  -- | 'httpStatus'
  Core.Int ->
  GetOriginRequestPolicyConfigResponse
newGetOriginRequestPolicyConfigResponse pHttpStatus_ =
  GetOriginRequestPolicyConfigResponse'
    { eTag =
        Core.Nothing,
      originRequestPolicyConfig =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the origin request policy.
getOriginRequestPolicyConfigResponse_eTag :: Lens.Lens' GetOriginRequestPolicyConfigResponse (Core.Maybe Core.Text)
getOriginRequestPolicyConfigResponse_eTag = Lens.lens (\GetOriginRequestPolicyConfigResponse' {eTag} -> eTag) (\s@GetOriginRequestPolicyConfigResponse' {} a -> s {eTag = a} :: GetOriginRequestPolicyConfigResponse)

-- | The origin request policy configuration.
getOriginRequestPolicyConfigResponse_originRequestPolicyConfig :: Lens.Lens' GetOriginRequestPolicyConfigResponse (Core.Maybe OriginRequestPolicyConfig)
getOriginRequestPolicyConfigResponse_originRequestPolicyConfig = Lens.lens (\GetOriginRequestPolicyConfigResponse' {originRequestPolicyConfig} -> originRequestPolicyConfig) (\s@GetOriginRequestPolicyConfigResponse' {} a -> s {originRequestPolicyConfig = a} :: GetOriginRequestPolicyConfigResponse)

-- | The response's http status code.
getOriginRequestPolicyConfigResponse_httpStatus :: Lens.Lens' GetOriginRequestPolicyConfigResponse Core.Int
getOriginRequestPolicyConfigResponse_httpStatus = Lens.lens (\GetOriginRequestPolicyConfigResponse' {httpStatus} -> httpStatus) (\s@GetOriginRequestPolicyConfigResponse' {} a -> s {httpStatus = a} :: GetOriginRequestPolicyConfigResponse)

instance
  Core.NFData
    GetOriginRequestPolicyConfigResponse
