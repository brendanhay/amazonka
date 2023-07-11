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
-- Module      : Amazonka.CloudFront.GetResponseHeadersPolicyConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets a response headers policy configuration.
--
-- To get a response headers policy configuration, you must provide the
-- policy\'s identifier. If the response headers policy is attached to a
-- distribution\'s cache behavior, you can get the policy\'s identifier
-- using @ListDistributions@ or @GetDistribution@. If the response headers
-- policy is not attached to a cache behavior, you can get the identifier
-- using @ListResponseHeadersPolicies@.
module Amazonka.CloudFront.GetResponseHeadersPolicyConfig
  ( -- * Creating a Request
    GetResponseHeadersPolicyConfig (..),
    newGetResponseHeadersPolicyConfig,

    -- * Request Lenses
    getResponseHeadersPolicyConfig_id,

    -- * Destructuring the Response
    GetResponseHeadersPolicyConfigResponse (..),
    newGetResponseHeadersPolicyConfigResponse,

    -- * Response Lenses
    getResponseHeadersPolicyConfigResponse_eTag,
    getResponseHeadersPolicyConfigResponse_responseHeadersPolicyConfig,
    getResponseHeadersPolicyConfigResponse_httpStatus,
  )
where

import Amazonka.CloudFront.Types
import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetResponseHeadersPolicyConfig' smart constructor.
data GetResponseHeadersPolicyConfig = GetResponseHeadersPolicyConfig'
  { -- | The identifier for the response headers policy.
    --
    -- If the response headers policy is attached to a distribution\'s cache
    -- behavior, you can get the policy\'s identifier using @ListDistributions@
    -- or @GetDistribution@. If the response headers policy is not attached to
    -- a cache behavior, you can get the identifier using
    -- @ListResponseHeadersPolicies@.
    id :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResponseHeadersPolicyConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'id', 'getResponseHeadersPolicyConfig_id' - The identifier for the response headers policy.
--
-- If the response headers policy is attached to a distribution\'s cache
-- behavior, you can get the policy\'s identifier using @ListDistributions@
-- or @GetDistribution@. If the response headers policy is not attached to
-- a cache behavior, you can get the identifier using
-- @ListResponseHeadersPolicies@.
newGetResponseHeadersPolicyConfig ::
  -- | 'id'
  Prelude.Text ->
  GetResponseHeadersPolicyConfig
newGetResponseHeadersPolicyConfig pId_ =
  GetResponseHeadersPolicyConfig' {id = pId_}

-- | The identifier for the response headers policy.
--
-- If the response headers policy is attached to a distribution\'s cache
-- behavior, you can get the policy\'s identifier using @ListDistributions@
-- or @GetDistribution@. If the response headers policy is not attached to
-- a cache behavior, you can get the identifier using
-- @ListResponseHeadersPolicies@.
getResponseHeadersPolicyConfig_id :: Lens.Lens' GetResponseHeadersPolicyConfig Prelude.Text
getResponseHeadersPolicyConfig_id = Lens.lens (\GetResponseHeadersPolicyConfig' {id} -> id) (\s@GetResponseHeadersPolicyConfig' {} a -> s {id = a} :: GetResponseHeadersPolicyConfig)

instance
  Core.AWSRequest
    GetResponseHeadersPolicyConfig
  where
  type
    AWSResponse GetResponseHeadersPolicyConfig =
      GetResponseHeadersPolicyConfigResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveXML
      ( \s h x ->
          GetResponseHeadersPolicyConfigResponse'
            Prelude.<$> (h Data..#? "ETag")
            Prelude.<*> (Data.parseXML x)
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    GetResponseHeadersPolicyConfig
  where
  hashWithSalt
    _salt
    GetResponseHeadersPolicyConfig' {..} =
      _salt `Prelude.hashWithSalt` id

instance
  Prelude.NFData
    GetResponseHeadersPolicyConfig
  where
  rnf GetResponseHeadersPolicyConfig' {..} =
    Prelude.rnf id

instance
  Data.ToHeaders
    GetResponseHeadersPolicyConfig
  where
  toHeaders = Prelude.const Prelude.mempty

instance Data.ToPath GetResponseHeadersPolicyConfig where
  toPath GetResponseHeadersPolicyConfig' {..} =
    Prelude.mconcat
      [ "/2020-05-31/response-headers-policy/",
        Data.toBS id,
        "/config"
      ]

instance Data.ToQuery GetResponseHeadersPolicyConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResponseHeadersPolicyConfigResponse' smart constructor.
data GetResponseHeadersPolicyConfigResponse = GetResponseHeadersPolicyConfigResponse'
  { -- | The version identifier for the current version of the response headers
    -- policy.
    eTag :: Prelude.Maybe Prelude.Text,
    -- | Contains a response headers policy.
    responseHeadersPolicyConfig :: Prelude.Maybe ResponseHeadersPolicyConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResponseHeadersPolicyConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'eTag', 'getResponseHeadersPolicyConfigResponse_eTag' - The version identifier for the current version of the response headers
-- policy.
--
-- 'responseHeadersPolicyConfig', 'getResponseHeadersPolicyConfigResponse_responseHeadersPolicyConfig' - Contains a response headers policy.
--
-- 'httpStatus', 'getResponseHeadersPolicyConfigResponse_httpStatus' - The response's http status code.
newGetResponseHeadersPolicyConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResponseHeadersPolicyConfigResponse
newGetResponseHeadersPolicyConfigResponse
  pHttpStatus_ =
    GetResponseHeadersPolicyConfigResponse'
      { eTag =
          Prelude.Nothing,
        responseHeadersPolicyConfig =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The version identifier for the current version of the response headers
-- policy.
getResponseHeadersPolicyConfigResponse_eTag :: Lens.Lens' GetResponseHeadersPolicyConfigResponse (Prelude.Maybe Prelude.Text)
getResponseHeadersPolicyConfigResponse_eTag = Lens.lens (\GetResponseHeadersPolicyConfigResponse' {eTag} -> eTag) (\s@GetResponseHeadersPolicyConfigResponse' {} a -> s {eTag = a} :: GetResponseHeadersPolicyConfigResponse)

-- | Contains a response headers policy.
getResponseHeadersPolicyConfigResponse_responseHeadersPolicyConfig :: Lens.Lens' GetResponseHeadersPolicyConfigResponse (Prelude.Maybe ResponseHeadersPolicyConfig)
getResponseHeadersPolicyConfigResponse_responseHeadersPolicyConfig = Lens.lens (\GetResponseHeadersPolicyConfigResponse' {responseHeadersPolicyConfig} -> responseHeadersPolicyConfig) (\s@GetResponseHeadersPolicyConfigResponse' {} a -> s {responseHeadersPolicyConfig = a} :: GetResponseHeadersPolicyConfigResponse)

-- | The response's http status code.
getResponseHeadersPolicyConfigResponse_httpStatus :: Lens.Lens' GetResponseHeadersPolicyConfigResponse Prelude.Int
getResponseHeadersPolicyConfigResponse_httpStatus = Lens.lens (\GetResponseHeadersPolicyConfigResponse' {httpStatus} -> httpStatus) (\s@GetResponseHeadersPolicyConfigResponse' {} a -> s {httpStatus = a} :: GetResponseHeadersPolicyConfigResponse)

instance
  Prelude.NFData
    GetResponseHeadersPolicyConfigResponse
  where
  rnf GetResponseHeadersPolicyConfigResponse' {..} =
    Prelude.rnf eTag
      `Prelude.seq` Prelude.rnf responseHeadersPolicyConfig
      `Prelude.seq` Prelude.rnf httpStatus
