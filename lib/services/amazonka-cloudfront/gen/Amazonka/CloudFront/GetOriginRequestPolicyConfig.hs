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
-- Module      : Amazonka.CloudFront.GetOriginRequestPolicyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets an origin request policy configuration.
--
-- To get an origin request policy configuration, you must provide the
-- policy\'s identifier. If the origin request policy is attached to a
-- distribution\'s cache behavior, you can get the policy\'s identifier
-- using @ListDistributions@ or @GetDistribution@. If the origin request
-- policy is not attached to a cache behavior, you can get the identifier
-- using @ListOriginRequestPolicies@.
module Amazonka.CloudFront.GetOriginRequestPolicyConfig
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

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetOriginRequestPolicyConfig' smart constructor.
data GetOriginRequestPolicyConfig = GetOriginRequestPolicyConfig'
  { -- | The unique identifier for the origin request policy. If the origin
    -- request policy is attached to a distribution\'s cache behavior, you can
    -- get the policy\'s identifier using @ListDistributions@ or
    -- @GetDistribution@. If the origin request policy is not attached to a
    -- cache behavior, you can get the identifier using
    -- @ListOriginRequestPolicies@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetOriginRequestPolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getOriginRequestPolicyConfig_id' - The unique identifier for the origin request policy. If the origin
-- request policy is attached to a distribution\'s cache behavior, you can
-- get the policy\'s identifier using @ListDistributions@ or
-- @GetDistribution@. If the origin request policy is not attached to a
-- cache behavior, you can get the identifier using
-- @ListOriginRequestPolicies@.
newGetOriginRequestPolicyConfig ::
  -- | 'id'
  Prelude.Text ->
  GetOriginRequestPolicyConfig
newGetOriginRequestPolicyConfig pId_ =
  GetOriginRequestPolicyConfig' {id = pId_}

-- | The unique identifier for the origin request policy. If the origin
-- request policy is attached to a distribution\'s cache behavior, you can
-- get the policy\'s identifier using @ListDistributions@ or
-- @GetDistribution@. If the origin request policy is not attached to a
-- cache behavior, you can get the identifier using
-- @ListOriginRequestPolicies@.
getOriginRequestPolicyConfig_id :: Lens.Lens' GetOriginRequestPolicyConfig Prelude.Text
getOriginRequestPolicyConfig_id = Lens.lens (\GetOriginRequestPolicyConfig' {id} -> id) (\s@GetOriginRequestPolicyConfig' {} a -> s {id = a} :: GetOriginRequestPolicyConfig)

instance Core.AWSRequest GetOriginRequestPolicyConfig where
  type
    AWSResponse GetOriginRequestPolicyConfig =
      GetOriginRequestPolicyConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetOriginRequestPolicyConfigResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetOriginRequestPolicyConfig
  where
  hashWithSalt _salt GetOriginRequestPolicyConfig' {..} =
    _salt `Prelude.hashWithSalt` id

instance Prelude.NFData GetOriginRequestPolicyConfig where
  rnf GetOriginRequestPolicyConfig' {..} =
    Prelude.rnf id

instance Data.ToHeaders GetOriginRequestPolicyConfig where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetOriginRequestPolicyConfig where
  toPath GetOriginRequestPolicyConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/origin-request-policy/",
        Data.toBS id,
        "/config"
      ]

instance Data.ToQuery GetOriginRequestPolicyConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetOriginRequestPolicyConfigResponse' smart constructor.
data GetOriginRequestPolicyConfigResponse = GetOriginRequestPolicyConfigResponse'
  { -- | The current version of the origin request policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | The origin request policy configuration.
    originRequestPolicyConfig :: Prelude.Maybe OriginRequestPolicyConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  GetOriginRequestPolicyConfigResponse
newGetOriginRequestPolicyConfigResponse pHttpStatus_ =
  GetOriginRequestPolicyConfigResponse'
    { eTag =
        Prelude.Nothing,
      originRequestPolicyConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The current version of the origin request policy.
getOriginRequestPolicyConfigResponse_eTag :: Lens.Lens' GetOriginRequestPolicyConfigResponse (Prelude.Maybe Prelude.Text)
getOriginRequestPolicyConfigResponse_eTag = Lens.lens (\GetOriginRequestPolicyConfigResponse' {eTag} -> eTag) (\s@GetOriginRequestPolicyConfigResponse' {} a -> s {eTag = a} :: GetOriginRequestPolicyConfigResponse)

-- | The origin request policy configuration.
getOriginRequestPolicyConfigResponse_originRequestPolicyConfig :: Lens.Lens' GetOriginRequestPolicyConfigResponse (Prelude.Maybe OriginRequestPolicyConfig)
getOriginRequestPolicyConfigResponse_originRequestPolicyConfig = Lens.lens (\GetOriginRequestPolicyConfigResponse' {originRequestPolicyConfig} -> originRequestPolicyConfig) (\s@GetOriginRequestPolicyConfigResponse' {} a -> s {originRequestPolicyConfig = a} :: GetOriginRequestPolicyConfigResponse)

-- | The response's http status code.
getOriginRequestPolicyConfigResponse_httpStatus :: Lens.Lens' GetOriginRequestPolicyConfigResponse Prelude.Int
getOriginRequestPolicyConfigResponse_httpStatus = Lens.lens (\GetOriginRequestPolicyConfigResponse' {httpStatus} -> httpStatus) (\s@GetOriginRequestPolicyConfigResponse' {} a -> s {httpStatus = a} :: GetOriginRequestPolicyConfigResponse)

instance
  Prelude.NFData
    GetOriginRequestPolicyConfigResponse
  where
  rnf GetOriginRequestPolicyConfigResponse' {..} =
    Prelude.rnf eTag `Prelude.seq`
      Prelude.rnf originRequestPolicyConfig `Prelude.seq`
        Prelude.rnf httpStatus
