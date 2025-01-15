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
-- Module      : Amazonka.Route53Resolver.GetResolverDnssecConfig
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets DNSSEC validation information for a specified resource.
module Amazonka.Route53Resolver.GetResolverDnssecConfig
  ( -- * Creating a Request
    GetResolverDnssecConfig (..),
    newGetResolverDnssecConfig,

    -- * Request Lenses
    getResolverDnssecConfig_resourceId,

    -- * Destructuring the Response
    GetResolverDnssecConfigResponse (..),
    newGetResolverDnssecConfigResponse,

    -- * Response Lenses
    getResolverDnssecConfigResponse_resolverDNSSECConfig,
    getResolverDnssecConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetResolverDnssecConfig' smart constructor.
data GetResolverDnssecConfig = GetResolverDnssecConfig'
  { -- | The ID of the virtual private cloud (VPC) for the DNSSEC validation
    -- status.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverDnssecConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'getResolverDnssecConfig_resourceId' - The ID of the virtual private cloud (VPC) for the DNSSEC validation
-- status.
newGetResolverDnssecConfig ::
  -- | 'resourceId'
  Prelude.Text ->
  GetResolverDnssecConfig
newGetResolverDnssecConfig pResourceId_ =
  GetResolverDnssecConfig' {resourceId = pResourceId_}

-- | The ID of the virtual private cloud (VPC) for the DNSSEC validation
-- status.
getResolverDnssecConfig_resourceId :: Lens.Lens' GetResolverDnssecConfig Prelude.Text
getResolverDnssecConfig_resourceId = Lens.lens (\GetResolverDnssecConfig' {resourceId} -> resourceId) (\s@GetResolverDnssecConfig' {} a -> s {resourceId = a} :: GetResolverDnssecConfig)

instance Core.AWSRequest GetResolverDnssecConfig where
  type
    AWSResponse GetResolverDnssecConfig =
      GetResolverDnssecConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverDnssecConfigResponse'
            Prelude.<$> (x Data..?> "ResolverDNSSECConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResolverDnssecConfig where
  hashWithSalt _salt GetResolverDnssecConfig' {..} =
    _salt `Prelude.hashWithSalt` resourceId

instance Prelude.NFData GetResolverDnssecConfig where
  rnf GetResolverDnssecConfig' {..} =
    Prelude.rnf resourceId

instance Data.ToHeaders GetResolverDnssecConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.GetResolverDnssecConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResolverDnssecConfig where
  toJSON GetResolverDnssecConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceId" Data..= resourceId)]
      )

instance Data.ToPath GetResolverDnssecConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResolverDnssecConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResolverDnssecConfigResponse' smart constructor.
data GetResolverDnssecConfigResponse = GetResolverDnssecConfigResponse'
  { -- | The information about a configuration for DNSSEC validation.
    resolverDNSSECConfig :: Prelude.Maybe ResolverDnssecConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverDnssecConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverDNSSECConfig', 'getResolverDnssecConfigResponse_resolverDNSSECConfig' - The information about a configuration for DNSSEC validation.
--
-- 'httpStatus', 'getResolverDnssecConfigResponse_httpStatus' - The response's http status code.
newGetResolverDnssecConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResolverDnssecConfigResponse
newGetResolverDnssecConfigResponse pHttpStatus_ =
  GetResolverDnssecConfigResponse'
    { resolverDNSSECConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The information about a configuration for DNSSEC validation.
getResolverDnssecConfigResponse_resolverDNSSECConfig :: Lens.Lens' GetResolverDnssecConfigResponse (Prelude.Maybe ResolverDnssecConfig)
getResolverDnssecConfigResponse_resolverDNSSECConfig = Lens.lens (\GetResolverDnssecConfigResponse' {resolverDNSSECConfig} -> resolverDNSSECConfig) (\s@GetResolverDnssecConfigResponse' {} a -> s {resolverDNSSECConfig = a} :: GetResolverDnssecConfigResponse)

-- | The response's http status code.
getResolverDnssecConfigResponse_httpStatus :: Lens.Lens' GetResolverDnssecConfigResponse Prelude.Int
getResolverDnssecConfigResponse_httpStatus = Lens.lens (\GetResolverDnssecConfigResponse' {httpStatus} -> httpStatus) (\s@GetResolverDnssecConfigResponse' {} a -> s {httpStatus = a} :: GetResolverDnssecConfigResponse)

instance
  Prelude.NFData
    GetResolverDnssecConfigResponse
  where
  rnf GetResolverDnssecConfigResponse' {..} =
    Prelude.rnf resolverDNSSECConfig `Prelude.seq`
      Prelude.rnf httpStatus
