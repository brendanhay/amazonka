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
-- Module      : Amazonka.Route53Resolver.UpdateResolverEndpoint
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the name, or enpoint type for an inbound or an outbound Resolver
-- endpoint. You can only update between IPV4 and DUALSTACK, IPV6 endpoint
-- type can\'t be updated to other type.
module Amazonka.Route53Resolver.UpdateResolverEndpoint
  ( -- * Creating a Request
    UpdateResolverEndpoint (..),
    newUpdateResolverEndpoint,

    -- * Request Lenses
    updateResolverEndpoint_name,
    updateResolverEndpoint_resolverEndpointType,
    updateResolverEndpoint_updateIpAddresses,
    updateResolverEndpoint_resolverEndpointId,

    -- * Destructuring the Response
    UpdateResolverEndpointResponse (..),
    newUpdateResolverEndpointResponse,

    -- * Response Lenses
    updateResolverEndpointResponse_resolverEndpoint,
    updateResolverEndpointResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newUpdateResolverEndpoint' smart constructor.
data UpdateResolverEndpoint = UpdateResolverEndpoint'
  { -- | The name of the Resolver endpoint that you want to update.
    name :: Prelude.Maybe Prelude.Text,
    -- | Specifies the endpoint type for what type of IP address the endpoint
    -- uses to forward DNS queries.
    resolverEndpointType :: Prelude.Maybe ResolverEndpointType,
    -- | Updates the Resolver endpoint type to IpV4, Ipv6, or dual-stack.
    updateIpAddresses :: Prelude.Maybe [UpdateIpAddress],
    -- | The ID of the Resolver endpoint that you want to update.
    resolverEndpointId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResolverEndpoint' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'name', 'updateResolverEndpoint_name' - The name of the Resolver endpoint that you want to update.
--
-- 'resolverEndpointType', 'updateResolverEndpoint_resolverEndpointType' - Specifies the endpoint type for what type of IP address the endpoint
-- uses to forward DNS queries.
--
-- 'updateIpAddresses', 'updateResolverEndpoint_updateIpAddresses' - Updates the Resolver endpoint type to IpV4, Ipv6, or dual-stack.
--
-- 'resolverEndpointId', 'updateResolverEndpoint_resolverEndpointId' - The ID of the Resolver endpoint that you want to update.
newUpdateResolverEndpoint ::
  -- | 'resolverEndpointId'
  Prelude.Text ->
  UpdateResolverEndpoint
newUpdateResolverEndpoint pResolverEndpointId_ =
  UpdateResolverEndpoint'
    { name = Prelude.Nothing,
      resolverEndpointType = Prelude.Nothing,
      updateIpAddresses = Prelude.Nothing,
      resolverEndpointId = pResolverEndpointId_
    }

-- | The name of the Resolver endpoint that you want to update.
updateResolverEndpoint_name :: Lens.Lens' UpdateResolverEndpoint (Prelude.Maybe Prelude.Text)
updateResolverEndpoint_name = Lens.lens (\UpdateResolverEndpoint' {name} -> name) (\s@UpdateResolverEndpoint' {} a -> s {name = a} :: UpdateResolverEndpoint)

-- | Specifies the endpoint type for what type of IP address the endpoint
-- uses to forward DNS queries.
updateResolverEndpoint_resolverEndpointType :: Lens.Lens' UpdateResolverEndpoint (Prelude.Maybe ResolverEndpointType)
updateResolverEndpoint_resolverEndpointType = Lens.lens (\UpdateResolverEndpoint' {resolverEndpointType} -> resolverEndpointType) (\s@UpdateResolverEndpoint' {} a -> s {resolverEndpointType = a} :: UpdateResolverEndpoint)

-- | Updates the Resolver endpoint type to IpV4, Ipv6, or dual-stack.
updateResolverEndpoint_updateIpAddresses :: Lens.Lens' UpdateResolverEndpoint (Prelude.Maybe [UpdateIpAddress])
updateResolverEndpoint_updateIpAddresses = Lens.lens (\UpdateResolverEndpoint' {updateIpAddresses} -> updateIpAddresses) (\s@UpdateResolverEndpoint' {} a -> s {updateIpAddresses = a} :: UpdateResolverEndpoint) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the Resolver endpoint that you want to update.
updateResolverEndpoint_resolverEndpointId :: Lens.Lens' UpdateResolverEndpoint Prelude.Text
updateResolverEndpoint_resolverEndpointId = Lens.lens (\UpdateResolverEndpoint' {resolverEndpointId} -> resolverEndpointId) (\s@UpdateResolverEndpoint' {} a -> s {resolverEndpointId = a} :: UpdateResolverEndpoint)

instance Core.AWSRequest UpdateResolverEndpoint where
  type
    AWSResponse UpdateResolverEndpoint =
      UpdateResolverEndpointResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateResolverEndpointResponse'
            Prelude.<$> (x Data..?> "ResolverEndpoint")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable UpdateResolverEndpoint where
  hashWithSalt _salt UpdateResolverEndpoint' {..} =
    _salt
      `Prelude.hashWithSalt` name
      `Prelude.hashWithSalt` resolverEndpointType
      `Prelude.hashWithSalt` updateIpAddresses
      `Prelude.hashWithSalt` resolverEndpointId

instance Prelude.NFData UpdateResolverEndpoint where
  rnf UpdateResolverEndpoint' {..} =
    Prelude.rnf name
      `Prelude.seq` Prelude.rnf resolverEndpointType
      `Prelude.seq` Prelude.rnf updateIpAddresses
      `Prelude.seq` Prelude.rnf resolverEndpointId

instance Data.ToHeaders UpdateResolverEndpoint where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.UpdateResolverEndpoint" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON UpdateResolverEndpoint where
  toJSON UpdateResolverEndpoint' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Name" Data..=) Prelude.<$> name,
            ("ResolverEndpointType" Data..=)
              Prelude.<$> resolverEndpointType,
            ("UpdateIpAddresses" Data..=)
              Prelude.<$> updateIpAddresses,
            Prelude.Just
              ("ResolverEndpointId" Data..= resolverEndpointId)
          ]
      )

instance Data.ToPath UpdateResolverEndpoint where
  toPath = Prelude.const "/"

instance Data.ToQuery UpdateResolverEndpoint where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newUpdateResolverEndpointResponse' smart constructor.
data UpdateResolverEndpointResponse = UpdateResolverEndpointResponse'
  { -- | The response to an @UpdateResolverEndpoint@ request.
    resolverEndpoint :: Prelude.Maybe ResolverEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'UpdateResolverEndpointResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverEndpoint', 'updateResolverEndpointResponse_resolverEndpoint' - The response to an @UpdateResolverEndpoint@ request.
--
-- 'httpStatus', 'updateResolverEndpointResponse_httpStatus' - The response's http status code.
newUpdateResolverEndpointResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  UpdateResolverEndpointResponse
newUpdateResolverEndpointResponse pHttpStatus_ =
  UpdateResolverEndpointResponse'
    { resolverEndpoint =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The response to an @UpdateResolverEndpoint@ request.
updateResolverEndpointResponse_resolverEndpoint :: Lens.Lens' UpdateResolverEndpointResponse (Prelude.Maybe ResolverEndpoint)
updateResolverEndpointResponse_resolverEndpoint = Lens.lens (\UpdateResolverEndpointResponse' {resolverEndpoint} -> resolverEndpoint) (\s@UpdateResolverEndpointResponse' {} a -> s {resolverEndpoint = a} :: UpdateResolverEndpointResponse)

-- | The response's http status code.
updateResolverEndpointResponse_httpStatus :: Lens.Lens' UpdateResolverEndpointResponse Prelude.Int
updateResolverEndpointResponse_httpStatus = Lens.lens (\UpdateResolverEndpointResponse' {httpStatus} -> httpStatus) (\s@UpdateResolverEndpointResponse' {} a -> s {httpStatus = a} :: UpdateResolverEndpointResponse)

instance
  Prelude.NFData
    UpdateResolverEndpointResponse
  where
  rnf UpdateResolverEndpointResponse' {..} =
    Prelude.rnf resolverEndpoint
      `Prelude.seq` Prelude.rnf httpStatus
