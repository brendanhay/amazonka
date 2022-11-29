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
-- Module      : Amazonka.NetworkManager.GetTransitGatewayPeering
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Returns information about a transit gateway peer.
module Amazonka.NetworkManager.GetTransitGatewayPeering
  ( -- * Creating a Request
    GetTransitGatewayPeering (..),
    newGetTransitGatewayPeering,

    -- * Request Lenses
    getTransitGatewayPeering_peeringId,

    -- * Destructuring the Response
    GetTransitGatewayPeeringResponse (..),
    newGetTransitGatewayPeeringResponse,

    -- * Response Lenses
    getTransitGatewayPeeringResponse_transitGatewayPeering,
    getTransitGatewayPeeringResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newGetTransitGatewayPeering' smart constructor.
data GetTransitGatewayPeering = GetTransitGatewayPeering'
  { -- | The ID of the peering request.
    peeringId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayPeering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'peeringId', 'getTransitGatewayPeering_peeringId' - The ID of the peering request.
newGetTransitGatewayPeering ::
  -- | 'peeringId'
  Prelude.Text ->
  GetTransitGatewayPeering
newGetTransitGatewayPeering pPeeringId_ =
  GetTransitGatewayPeering' {peeringId = pPeeringId_}

-- | The ID of the peering request.
getTransitGatewayPeering_peeringId :: Lens.Lens' GetTransitGatewayPeering Prelude.Text
getTransitGatewayPeering_peeringId = Lens.lens (\GetTransitGatewayPeering' {peeringId} -> peeringId) (\s@GetTransitGatewayPeering' {} a -> s {peeringId = a} :: GetTransitGatewayPeering)

instance Core.AWSRequest GetTransitGatewayPeering where
  type
    AWSResponse GetTransitGatewayPeering =
      GetTransitGatewayPeeringResponse
  request overrides =
    Request.get (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          GetTransitGatewayPeeringResponse'
            Prelude.<$> (x Core..?> "TransitGatewayPeering")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable GetTransitGatewayPeering where
  hashWithSalt _salt GetTransitGatewayPeering' {..} =
    _salt `Prelude.hashWithSalt` peeringId

instance Prelude.NFData GetTransitGatewayPeering where
  rnf GetTransitGatewayPeering' {..} =
    Prelude.rnf peeringId

instance Core.ToHeaders GetTransitGatewayPeering where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToPath GetTransitGatewayPeering where
  toPath GetTransitGatewayPeering' {..} =
    Prelude.mconcat
      ["/transit-gateway-peerings/", Core.toBS peeringId]

instance Core.ToQuery GetTransitGatewayPeering where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newGetTransitGatewayPeeringResponse' smart constructor.
data GetTransitGatewayPeeringResponse = GetTransitGatewayPeeringResponse'
  { -- | Returns information about a transit gateway peering.
    transitGatewayPeering :: Prelude.Maybe TransitGatewayPeering,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'GetTransitGatewayPeeringResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPeering', 'getTransitGatewayPeeringResponse_transitGatewayPeering' - Returns information about a transit gateway peering.
--
-- 'httpStatus', 'getTransitGatewayPeeringResponse_httpStatus' - The response's http status code.
newGetTransitGatewayPeeringResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  GetTransitGatewayPeeringResponse
newGetTransitGatewayPeeringResponse pHttpStatus_ =
  GetTransitGatewayPeeringResponse'
    { transitGatewayPeering =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns information about a transit gateway peering.
getTransitGatewayPeeringResponse_transitGatewayPeering :: Lens.Lens' GetTransitGatewayPeeringResponse (Prelude.Maybe TransitGatewayPeering)
getTransitGatewayPeeringResponse_transitGatewayPeering = Lens.lens (\GetTransitGatewayPeeringResponse' {transitGatewayPeering} -> transitGatewayPeering) (\s@GetTransitGatewayPeeringResponse' {} a -> s {transitGatewayPeering = a} :: GetTransitGatewayPeeringResponse)

-- | The response's http status code.
getTransitGatewayPeeringResponse_httpStatus :: Lens.Lens' GetTransitGatewayPeeringResponse Prelude.Int
getTransitGatewayPeeringResponse_httpStatus = Lens.lens (\GetTransitGatewayPeeringResponse' {httpStatus} -> httpStatus) (\s@GetTransitGatewayPeeringResponse' {} a -> s {httpStatus = a} :: GetTransitGatewayPeeringResponse)

instance
  Prelude.NFData
    GetTransitGatewayPeeringResponse
  where
  rnf GetTransitGatewayPeeringResponse' {..} =
    Prelude.rnf transitGatewayPeering
      `Prelude.seq` Prelude.rnf httpStatus
