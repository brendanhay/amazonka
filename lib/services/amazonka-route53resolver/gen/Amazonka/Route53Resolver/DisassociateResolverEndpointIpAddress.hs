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
-- Module      : Amazonka.Route53Resolver.DisassociateResolverEndpointIpAddress
-- Copyright   : (c) 2013-2023 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes IP addresses from an inbound or an outbound Resolver endpoint.
-- If you want to remove more than one IP address, submit one
-- @DisassociateResolverEndpointIpAddress@ request for each IP address.
--
-- To add an IP address to an endpoint, see
-- <https://docs.aws.amazon.com/Route53/latest/APIReference/API_route53resolver_AssociateResolverEndpointIpAddress.html AssociateResolverEndpointIpAddress>.
module Amazonka.Route53Resolver.DisassociateResolverEndpointIpAddress
  ( -- * Creating a Request
    DisassociateResolverEndpointIpAddress (..),
    newDisassociateResolverEndpointIpAddress,

    -- * Request Lenses
    disassociateResolverEndpointIpAddress_resolverEndpointId,
    disassociateResolverEndpointIpAddress_ipAddress,

    -- * Destructuring the Response
    DisassociateResolverEndpointIpAddressResponse (..),
    newDisassociateResolverEndpointIpAddressResponse,

    -- * Response Lenses
    disassociateResolverEndpointIpAddressResponse_resolverEndpoint,
    disassociateResolverEndpointIpAddressResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response
import Amazonka.Route53Resolver.Types

-- | /See:/ 'newDisassociateResolverEndpointIpAddress' smart constructor.
data DisassociateResolverEndpointIpAddress = DisassociateResolverEndpointIpAddress'
  { -- | The ID of the Resolver endpoint that you want to disassociate an IP
    -- address from.
    resolverEndpointId :: Prelude.Text,
    -- | The IPv4 address that you want to remove from a Resolver endpoint.
    ipAddress :: IpAddressUpdate
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResolverEndpointIpAddress' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverEndpointId', 'disassociateResolverEndpointIpAddress_resolverEndpointId' - The ID of the Resolver endpoint that you want to disassociate an IP
-- address from.
--
-- 'ipAddress', 'disassociateResolverEndpointIpAddress_ipAddress' - The IPv4 address that you want to remove from a Resolver endpoint.
newDisassociateResolverEndpointIpAddress ::
  -- | 'resolverEndpointId'
  Prelude.Text ->
  -- | 'ipAddress'
  IpAddressUpdate ->
  DisassociateResolverEndpointIpAddress
newDisassociateResolverEndpointIpAddress
  pResolverEndpointId_
  pIpAddress_ =
    DisassociateResolverEndpointIpAddress'
      { resolverEndpointId =
          pResolverEndpointId_,
        ipAddress = pIpAddress_
      }

-- | The ID of the Resolver endpoint that you want to disassociate an IP
-- address from.
disassociateResolverEndpointIpAddress_resolverEndpointId :: Lens.Lens' DisassociateResolverEndpointIpAddress Prelude.Text
disassociateResolverEndpointIpAddress_resolverEndpointId = Lens.lens (\DisassociateResolverEndpointIpAddress' {resolverEndpointId} -> resolverEndpointId) (\s@DisassociateResolverEndpointIpAddress' {} a -> s {resolverEndpointId = a} :: DisassociateResolverEndpointIpAddress)

-- | The IPv4 address that you want to remove from a Resolver endpoint.
disassociateResolverEndpointIpAddress_ipAddress :: Lens.Lens' DisassociateResolverEndpointIpAddress IpAddressUpdate
disassociateResolverEndpointIpAddress_ipAddress = Lens.lens (\DisassociateResolverEndpointIpAddress' {ipAddress} -> ipAddress) (\s@DisassociateResolverEndpointIpAddress' {} a -> s {ipAddress = a} :: DisassociateResolverEndpointIpAddress)

instance
  Core.AWSRequest
    DisassociateResolverEndpointIpAddress
  where
  type
    AWSResponse
      DisassociateResolverEndpointIpAddress =
      DisassociateResolverEndpointIpAddressResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DisassociateResolverEndpointIpAddressResponse'
            Prelude.<$> (x Data..?> "ResolverEndpoint")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DisassociateResolverEndpointIpAddress
  where
  hashWithSalt
    _salt
    DisassociateResolverEndpointIpAddress' {..} =
      _salt `Prelude.hashWithSalt` resolverEndpointId
        `Prelude.hashWithSalt` ipAddress

instance
  Prelude.NFData
    DisassociateResolverEndpointIpAddress
  where
  rnf DisassociateResolverEndpointIpAddress' {..} =
    Prelude.rnf resolverEndpointId
      `Prelude.seq` Prelude.rnf ipAddress

instance
  Data.ToHeaders
    DisassociateResolverEndpointIpAddress
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Data.=# ( "Route53Resolver.DisassociateResolverEndpointIpAddress" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Data.ToJSON
    DisassociateResolverEndpointIpAddress
  where
  toJSON DisassociateResolverEndpointIpAddress' {..} =
    Data.object
      ( Prelude.catMaybes
          [ Prelude.Just
              ("ResolverEndpointId" Data..= resolverEndpointId),
            Prelude.Just ("IpAddress" Data..= ipAddress)
          ]
      )

instance
  Data.ToPath
    DisassociateResolverEndpointIpAddress
  where
  toPath = Prelude.const "/"

instance
  Data.ToQuery
    DisassociateResolverEndpointIpAddress
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newDisassociateResolverEndpointIpAddressResponse' smart constructor.
data DisassociateResolverEndpointIpAddressResponse = DisassociateResolverEndpointIpAddressResponse'
  { -- | The response to an @DisassociateResolverEndpointIpAddress@ request.
    resolverEndpoint :: Prelude.Maybe ResolverEndpoint,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DisassociateResolverEndpointIpAddressResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'resolverEndpoint', 'disassociateResolverEndpointIpAddressResponse_resolverEndpoint' - The response to an @DisassociateResolverEndpointIpAddress@ request.
--
-- 'httpStatus', 'disassociateResolverEndpointIpAddressResponse_httpStatus' - The response's http status code.
newDisassociateResolverEndpointIpAddressResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  DisassociateResolverEndpointIpAddressResponse
newDisassociateResolverEndpointIpAddressResponse
  pHttpStatus_ =
    DisassociateResolverEndpointIpAddressResponse'
      { resolverEndpoint =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The response to an @DisassociateResolverEndpointIpAddress@ request.
disassociateResolverEndpointIpAddressResponse_resolverEndpoint :: Lens.Lens' DisassociateResolverEndpointIpAddressResponse (Prelude.Maybe ResolverEndpoint)
disassociateResolverEndpointIpAddressResponse_resolverEndpoint = Lens.lens (\DisassociateResolverEndpointIpAddressResponse' {resolverEndpoint} -> resolverEndpoint) (\s@DisassociateResolverEndpointIpAddressResponse' {} a -> s {resolverEndpoint = a} :: DisassociateResolverEndpointIpAddressResponse)

-- | The response's http status code.
disassociateResolverEndpointIpAddressResponse_httpStatus :: Lens.Lens' DisassociateResolverEndpointIpAddressResponse Prelude.Int
disassociateResolverEndpointIpAddressResponse_httpStatus = Lens.lens (\DisassociateResolverEndpointIpAddressResponse' {httpStatus} -> httpStatus) (\s@DisassociateResolverEndpointIpAddressResponse' {} a -> s {httpStatus = a} :: DisassociateResolverEndpointIpAddressResponse)

instance
  Prelude.NFData
    DisassociateResolverEndpointIpAddressResponse
  where
  rnf
    DisassociateResolverEndpointIpAddressResponse' {..} =
      Prelude.rnf resolverEndpoint
        `Prelude.seq` Prelude.rnf httpStatus
