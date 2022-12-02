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
-- Module      : Amazonka.Route53Resolver.GetResolverConfig
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves the behavior configuration of Route 53 Resolver behavior for a
-- single VPC from Amazon Virtual Private Cloud.
module Amazonka.Route53Resolver.GetResolverConfig
  ( -- * Creating a Request
    GetResolverConfig (..),
    newGetResolverConfig,

    -- * Request Lenses
    getResolverConfig_resourceId,

    -- * Destructuring the Response
    GetResolverConfigResponse (..),
    newGetResolverConfigResponse,

    -- * Response Lenses
    getResolverConfigResponse_resolverConfig,
    getResolverConfigResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetResolverConfig' smart constructor.
data GetResolverConfig = GetResolverConfig'
  { -- | Resource ID of the Amazon VPC that you want to get information about.
    resourceId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverConfig' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resourceId', 'getResolverConfig_resourceId' - Resource ID of the Amazon VPC that you want to get information about.
newGetResolverConfig ::
  -- | 'resourceId'
  Prelude.Text ->
  GetResolverConfig
newGetResolverConfig pResourceId_ =
  GetResolverConfig' {resourceId = pResourceId_}

-- | Resource ID of the Amazon VPC that you want to get information about.
getResolverConfig_resourceId :: Lens.Lens' GetResolverConfig Prelude.Text
getResolverConfig_resourceId = Lens.lens (\GetResolverConfig' {resourceId} -> resourceId) (\s@GetResolverConfig' {} a -> s {resourceId = a} :: GetResolverConfig)

instance Core.AWSRequest GetResolverConfig where
  type
    AWSResponse GetResolverConfig =
      GetResolverConfigResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverConfigResponse'
            Prelude.<$> (x Data..?> "ResolverConfig")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResolverConfig where
  hashWithSalt _salt GetResolverConfig' {..} =
    _salt `Prelude.hashWithSalt` resourceId

instance Prelude.NFData GetResolverConfig where
  rnf GetResolverConfig' {..} = Prelude.rnf resourceId

instance Data.ToHeaders GetResolverConfig where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.GetResolverConfig" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON GetResolverConfig where
  toJSON GetResolverConfig' {..} =
    Data.object
      ( Prelude.catMaybes
          [Prelude.Just ("ResourceId" Data..= resourceId)]
      )

instance Data.ToPath GetResolverConfig where
  toPath = Prelude.const "/"

instance Data.ToQuery GetResolverConfig where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResolverConfigResponse' smart constructor.
data GetResolverConfigResponse = GetResolverConfigResponse'
  { -- | Information about the behavior configuration of Route 53 Resolver
    -- behavior for the VPC you specified in the @GetResolverConfig@ request.
    resolverConfig :: Prelude.Maybe ResolverConfig,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverConfigResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverConfig', 'getResolverConfigResponse_resolverConfig' - Information about the behavior configuration of Route 53 Resolver
-- behavior for the VPC you specified in the @GetResolverConfig@ request.
--
-- 'httpStatus', 'getResolverConfigResponse_httpStatus' - The response's http status code.
newGetResolverConfigResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResolverConfigResponse
newGetResolverConfigResponse pHttpStatus_ =
  GetResolverConfigResponse'
    { resolverConfig =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the behavior configuration of Route 53 Resolver
-- behavior for the VPC you specified in the @GetResolverConfig@ request.
getResolverConfigResponse_resolverConfig :: Lens.Lens' GetResolverConfigResponse (Prelude.Maybe ResolverConfig)
getResolverConfigResponse_resolverConfig = Lens.lens (\GetResolverConfigResponse' {resolverConfig} -> resolverConfig) (\s@GetResolverConfigResponse' {} a -> s {resolverConfig = a} :: GetResolverConfigResponse)

-- | The response's http status code.
getResolverConfigResponse_httpStatus :: Lens.Lens' GetResolverConfigResponse Prelude.Int
getResolverConfigResponse_httpStatus = Lens.lens (\GetResolverConfigResponse' {httpStatus} -> httpStatus) (\s@GetResolverConfigResponse' {} a -> s {httpStatus = a} :: GetResolverConfigResponse)

instance Prelude.NFData GetResolverConfigResponse where
  rnf GetResolverConfigResponse' {..} =
    Prelude.rnf resolverConfig
      `Prelude.seq` Prelude.rnf httpStatus
