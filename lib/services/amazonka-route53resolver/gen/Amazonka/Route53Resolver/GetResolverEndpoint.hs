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
-- Module      : Amazonka.Route53Resolver.GetResolverEndpoint
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Gets information about a specified Resolver endpoint, such as whether
-- it\'s an inbound or an outbound Resolver endpoint, and the current
-- status of the endpoint.
module Amazonka.Route53Resolver.GetResolverEndpoint
  ( -- * Creating a Request
    GetResolverEndpoint (..),
    newGetResolverEndpoint,

    -- * Request Lenses
    getResolverEndpoint_resolverEndpointId,

    -- * Destructuring the Response
    GetResolverEndpointResponse (..),
    newGetResolverEndpointResponse,

    -- * Response Lenses
    getResolverEndpointResponse_resolverEndpoint,
    getResolverEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newGetResolverEndpoint' smart constructor.
data GetResolverEndpoint = GetResolverEndpoint'
  { -- | The ID of the Resolver endpoint that you want to get information about.
    resolverEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverEndpointId', 'getResolverEndpoint_resolverEndpointId' - The ID of the Resolver endpoint that you want to get information about.
newGetResolverEndpoint ::
  -- | 'resolverEndpointId'
  Prelude.Text ->
  GetResolverEndpoint
newGetResolverEndpoint pResolverEndpointId_ =
  GetResolverEndpoint'
    { resolverEndpointId =
        pResolverEndpointId_
    }

-- | The ID of the Resolver endpoint that you want to get information about.
getResolverEndpoint_resolverEndpointId :: Lens.Lens' GetResolverEndpoint Prelude.Text
getResolverEndpoint_resolverEndpointId = Lens.lens (\GetResolverEndpoint' {resolverEndpointId} -> resolverEndpointId) (\s@GetResolverEndpoint' {} a -> s {resolverEndpointId = a} :: GetResolverEndpoint)

instance Core.AWSRequest GetResolverEndpoint where
  type
    AWSResponse GetResolverEndpoint =
      GetResolverEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetResolverEndpointResponse'
            Prelude.<$> (x Core..?> "ResolverEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetResolverEndpoint where
  hashWithSalt _salt GetResolverEndpoint' {..} =
    _salt `Prelude.hashWithSalt` resolverEndpointId

instance Prelude.NFData GetResolverEndpoint where
  rnf GetResolverEndpoint' {..} =
    Prelude.rnf resolverEndpointId

instance Core.ToHeaders GetResolverEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "Route53Resolver.GetResolverEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON GetResolverEndpoint where
  toJSON GetResolverEndpoint' {..} =
    Core.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResolverEndpointId" Core..= resolverEndpointId)
          ]
      )

instance Core.ToPath GetResolverEndpoint where
  toPath = Prelude.const "/"

instance Core.ToQuery GetResolverEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetResolverEndpointResponse' smart constructor.
data GetResolverEndpointResponse = GetResolverEndpointResponse'
  { -- | Information about the Resolver endpoint that you specified in a
    -- @GetResolverEndpoint@ request.
    resolverEndpoint :: Prelude.Maybe ResolverEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetResolverEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverEndpoint', 'getResolverEndpointResponse_resolverEndpoint' - Information about the Resolver endpoint that you specified in a
-- @GetResolverEndpoint@ request.
--
-- 'httpStatus', 'getResolverEndpointResponse_httpStatus' - The response's http status code.
newGetResolverEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetResolverEndpointResponse
newGetResolverEndpointResponse pHttpStatus_ =
  GetResolverEndpointResponse'
    { resolverEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the Resolver endpoint that you specified in a
-- @GetResolverEndpoint@ request.
getResolverEndpointResponse_resolverEndpoint :: Lens.Lens' GetResolverEndpointResponse (Prelude.Maybe ResolverEndpoint)
getResolverEndpointResponse_resolverEndpoint = Lens.lens (\GetResolverEndpointResponse' {resolverEndpoint} -> resolverEndpoint) (\s@GetResolverEndpointResponse' {} a -> s {resolverEndpoint = a} :: GetResolverEndpointResponse)

-- | The response's http status code.
getResolverEndpointResponse_httpStatus :: Lens.Lens' GetResolverEndpointResponse Prelude.Int
getResolverEndpointResponse_httpStatus = Lens.lens (\GetResolverEndpointResponse' {httpStatus} -> httpStatus) (\s@GetResolverEndpointResponse' {} a -> s {httpStatus = a} :: GetResolverEndpointResponse)

instance Prelude.NFData GetResolverEndpointResponse where
  rnf GetResolverEndpointResponse' {..} =
    Prelude.rnf resolverEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
