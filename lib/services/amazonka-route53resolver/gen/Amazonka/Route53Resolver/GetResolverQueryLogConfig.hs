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
-- Module      : Amazonka.Route53Resolver.GetResolverQueryLogConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified Resolver query logging configuration,
-- such as the number of VPCs that the configuration is logging queries for
-- and the location that logs are sent to.
module Amazonka.Route53Resolver.GetResolverQueryLogConfig
  ( -- * Creating a Request
    GetResolverQueryLogConfig (..),
    newGetResolverQueryLogConfig,

    -- * Request Lenses
    getResolverQueryLogConfig_resolverQueryLogConfigId,

    -- * Destructuring the Response
    GetResolverQueryLogConfigResponse (..),
    newGetResolverQueryLogConfigResponse,

    -- * Response Lenses
    getResolverQueryLogConfigResponse_resolverQueryLogConfig,
    getResolverQueryLogConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetResolverQueryLogConfig' smart constructor.
data GetResolverQueryLogConfig = GetResolverQueryLogConfig'
  { -- | The ID of the Resolver query logging configuration that you want to get
    -- information about.
    resolverQueryLogConfigId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverQueryLogConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfigId', 'getResolverQueryLogConfig_resolverQueryLogConfigId' - The ID of the Resolver query logging configuration that you want to get
-- information about.
newGetResolverQueryLogConfig ::
  -- | 'resolverQueryLogConfigId'
  Prelude.Text ->
  GetResolverQueryLogConfig
newGetResolverQueryLogConfig
  pResolverQueryLogConfigId_ =
    GetResolverQueryLogConfig'
      { resolverQueryLogConfigId =
          pResolverQueryLogConfigId_
      }

-- | The ID of the Resolver query logging configuration that you want to get
-- information about.
getResolverQueryLogConfig_resolverQueryLogConfigId :: Lens.Lens' GetResolverQueryLogConfig Prelude.Text
getResolverQueryLogConfig_resolverQueryLogConfigId = Lens.lens (\GetResolverQueryLogConfig' {resolverQueryLogConfigId} -> resolverQueryLogConfigId) (\s@GetResolverQueryLogConfig' {} a -> s {resolverQueryLogConfigId = a} :: GetResolverQueryLogConfig)

instance Core.AWSRequest GetResolverQueryLogConfig where
  type
    AWSResponse GetResolverQueryLogConfig =
      GetResolverQueryLogConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverQueryLogConfigResponse'
            Prelude.<$> (x Core..?> "ResolverQueryLogConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResolverQueryLogConfig where
  hashWithSalt _salt GetResolverQueryLogConfig' {..} =
    _salt
      `Prelude.hashWithSalt` resolverQueryLogConfigId

instance Prelude.NFData GetResolverQueryLogConfig where
  rnf GetResolverQueryLogConfig' {..} =
    Prelude.rnf resolverQueryLogConfigId

instance Core.ToHeaders GetResolverQueryLogConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Resolver.GetResolverQueryLogConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResolverQueryLogConfig where
  toJSON GetResolverQueryLogConfig' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ( "ResolverQueryLogConfigId"
                  Core..= resolverQueryLogConfigId
              )
          ]
      )

instance Core.ToPath GetResolverQueryLogConfig where
  toPath = Prelude.const "/"

instance Core.ToQuery GetResolverQueryLogConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResolverQueryLogConfigResponse' smart constructor.
data GetResolverQueryLogConfigResponse = GetResolverQueryLogConfigResponse'
  { -- | Information about the Resolver query logging configuration that you
    -- specified in a @GetQueryLogConfig@ request.
    resolverQueryLogConfig :: Prelude.Maybe ResolverQueryLogConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverQueryLogConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverQueryLogConfig', 'getResolverQueryLogConfigResponse_resolverQueryLogConfig' - Information about the Resolver query logging configuration that you
-- specified in a @GetQueryLogConfig@ request.
--
-- 'httpStatus', 'getResolverQueryLogConfigResponse_httpStatus' - The response's http status code.
newGetResolverQueryLogConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResolverQueryLogConfigResponse
newGetResolverQueryLogConfigResponse pHttpStatus_ =
  GetResolverQueryLogConfigResponse'
    { resolverQueryLogConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Resolver query logging configuration that you
-- specified in a @GetQueryLogConfig@ request.
getResolverQueryLogConfigResponse_resolverQueryLogConfig :: Lens.Lens' GetResolverQueryLogConfigResponse (Prelude.Maybe ResolverQueryLogConfig)
getResolverQueryLogConfigResponse_resolverQueryLogConfig = Lens.lens (\GetResolverQueryLogConfigResponse' {resolverQueryLogConfig} -> resolverQueryLogConfig) (\s@GetResolverQueryLogConfigResponse' {} a -> s {resolverQueryLogConfig = a} :: GetResolverQueryLogConfigResponse)

-- | The response's http status code.
getResolverQueryLogConfigResponse_httpStatus :: Lens.Lens' GetResolverQueryLogConfigResponse Prelude.Int
getResolverQueryLogConfigResponse_httpStatus = Lens.lens (\GetResolverQueryLogConfigResponse' {httpStatus} -> httpStatus) (\s@GetResolverQueryLogConfigResponse' {} a -> s {httpStatus = a} :: GetResolverQueryLogConfigResponse)

instance
  Prelude.NFData
    GetResolverQueryLogConfigResponse
  where
  rnf GetResolverQueryLogConfigResponse' {..} =
    Prelude.rnf resolverQueryLogConfig
      `Prelude.seq` Prelude.rnf httpStatus
