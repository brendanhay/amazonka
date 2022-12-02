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
-- Module      : Amazonka.NetworkManager.CreateTransitGatewayPeering
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates a transit gateway peering connection.
module Amazonka.NetworkManager.CreateTransitGatewayPeering
  ( -- * Creating a Request
    CreateTransitGatewayPeering (..),
    newCreateTransitGatewayPeering,

    -- * Request Lenses
    createTransitGatewayPeering_tags,
    createTransitGatewayPeering_clientToken,
    createTransitGatewayPeering_coreNetworkId,
    createTransitGatewayPeering_transitGatewayArn,

    -- * Destructuring the Response
    CreateTransitGatewayPeeringResponse (..),
    newCreateTransitGatewayPeeringResponse,

    -- * Response Lenses
    createTransitGatewayPeeringResponse_transitGatewayPeering,
    createTransitGatewayPeeringResponse_httpStatus,
  )
where

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import qualified Amazonka.Data as Data
import Amazonka.NetworkManager.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | /See:/ 'newCreateTransitGatewayPeering' smart constructor.
data CreateTransitGatewayPeering = CreateTransitGatewayPeering'
  { -- | The list of key-value tags associated with the request.
    tags :: Prelude.Maybe [Tag],
    -- | The client token associated with the request.
    clientToken :: Prelude.Maybe Prelude.Text,
    -- | The ID of a core network.
    coreNetworkId :: Prelude.Text,
    -- | The ARN of the transit gateway for the peering request.
    transitGatewayArn :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayPeering' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'tags', 'createTransitGatewayPeering_tags' - The list of key-value tags associated with the request.
--
-- 'clientToken', 'createTransitGatewayPeering_clientToken' - The client token associated with the request.
--
-- 'coreNetworkId', 'createTransitGatewayPeering_coreNetworkId' - The ID of a core network.
--
-- 'transitGatewayArn', 'createTransitGatewayPeering_transitGatewayArn' - The ARN of the transit gateway for the peering request.
newCreateTransitGatewayPeering ::
  -- | 'coreNetworkId'
  Prelude.Text ->
  -- | 'transitGatewayArn'
  Prelude.Text ->
  CreateTransitGatewayPeering
newCreateTransitGatewayPeering
  pCoreNetworkId_
  pTransitGatewayArn_ =
    CreateTransitGatewayPeering'
      { tags =
          Prelude.Nothing,
        clientToken = Prelude.Nothing,
        coreNetworkId = pCoreNetworkId_,
        transitGatewayArn = pTransitGatewayArn_
      }

-- | The list of key-value tags associated with the request.
createTransitGatewayPeering_tags :: Lens.Lens' CreateTransitGatewayPeering (Prelude.Maybe [Tag])
createTransitGatewayPeering_tags = Lens.lens (\CreateTransitGatewayPeering' {tags} -> tags) (\s@CreateTransitGatewayPeering' {} a -> s {tags = a} :: CreateTransitGatewayPeering) Prelude.. Lens.mapping Lens.coerced

-- | The client token associated with the request.
createTransitGatewayPeering_clientToken :: Lens.Lens' CreateTransitGatewayPeering (Prelude.Maybe Prelude.Text)
createTransitGatewayPeering_clientToken = Lens.lens (\CreateTransitGatewayPeering' {clientToken} -> clientToken) (\s@CreateTransitGatewayPeering' {} a -> s {clientToken = a} :: CreateTransitGatewayPeering)

-- | The ID of a core network.
createTransitGatewayPeering_coreNetworkId :: Lens.Lens' CreateTransitGatewayPeering Prelude.Text
createTransitGatewayPeering_coreNetworkId = Lens.lens (\CreateTransitGatewayPeering' {coreNetworkId} -> coreNetworkId) (\s@CreateTransitGatewayPeering' {} a -> s {coreNetworkId = a} :: CreateTransitGatewayPeering)

-- | The ARN of the transit gateway for the peering request.
createTransitGatewayPeering_transitGatewayArn :: Lens.Lens' CreateTransitGatewayPeering Prelude.Text
createTransitGatewayPeering_transitGatewayArn = Lens.lens (\CreateTransitGatewayPeering' {transitGatewayArn} -> transitGatewayArn) (\s@CreateTransitGatewayPeering' {} a -> s {transitGatewayArn = a} :: CreateTransitGatewayPeering)

instance Core.AWSRequest CreateTransitGatewayPeering where
  type
    AWSResponse CreateTransitGatewayPeering =
      CreateTransitGatewayPeeringResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateTransitGatewayPeeringResponse'
            Prelude.<$> (x Data..?> "TransitGatewayPeering")
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance Prelude.Hashable CreateTransitGatewayPeering where
  hashWithSalt _salt CreateTransitGatewayPeering' {..} =
    _salt `Prelude.hashWithSalt` tags
      `Prelude.hashWithSalt` clientToken
      `Prelude.hashWithSalt` coreNetworkId
      `Prelude.hashWithSalt` transitGatewayArn

instance Prelude.NFData CreateTransitGatewayPeering where
  rnf CreateTransitGatewayPeering' {..} =
    Prelude.rnf tags
      `Prelude.seq` Prelude.rnf clientToken
      `Prelude.seq` Prelude.rnf coreNetworkId
      `Prelude.seq` Prelude.rnf transitGatewayArn

instance Data.ToHeaders CreateTransitGatewayPeering where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "Content-Type"
              Data.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Data.ToJSON CreateTransitGatewayPeering where
  toJSON CreateTransitGatewayPeering' {..} =
    Data.object
      ( Prelude.catMaybes
          [ ("Tags" Data..=) Prelude.<$> tags,
            ("ClientToken" Data..=) Prelude.<$> clientToken,
            Prelude.Just ("CoreNetworkId" Data..= coreNetworkId),
            Prelude.Just
              ("TransitGatewayArn" Data..= transitGatewayArn)
          ]
      )

instance Data.ToPath CreateTransitGatewayPeering where
  toPath = Prelude.const "/transit-gateway-peerings"

instance Data.ToQuery CreateTransitGatewayPeering where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateTransitGatewayPeeringResponse' smart constructor.
data CreateTransitGatewayPeeringResponse = CreateTransitGatewayPeeringResponse'
  { -- | Returns information about the transit gateway peering connection
    -- request.
    transitGatewayPeering :: Prelude.Maybe TransitGatewayPeering,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateTransitGatewayPeeringResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'transitGatewayPeering', 'createTransitGatewayPeeringResponse_transitGatewayPeering' - Returns information about the transit gateway peering connection
-- request.
--
-- 'httpStatus', 'createTransitGatewayPeeringResponse_httpStatus' - The response's http status code.
newCreateTransitGatewayPeeringResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateTransitGatewayPeeringResponse
newCreateTransitGatewayPeeringResponse pHttpStatus_ =
  CreateTransitGatewayPeeringResponse'
    { transitGatewayPeering =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Returns information about the transit gateway peering connection
-- request.
createTransitGatewayPeeringResponse_transitGatewayPeering :: Lens.Lens' CreateTransitGatewayPeeringResponse (Prelude.Maybe TransitGatewayPeering)
createTransitGatewayPeeringResponse_transitGatewayPeering = Lens.lens (\CreateTransitGatewayPeeringResponse' {transitGatewayPeering} -> transitGatewayPeering) (\s@CreateTransitGatewayPeeringResponse' {} a -> s {transitGatewayPeering = a} :: CreateTransitGatewayPeeringResponse)

-- | The response's http status code.
createTransitGatewayPeeringResponse_httpStatus :: Lens.Lens' CreateTransitGatewayPeeringResponse Prelude.Int
createTransitGatewayPeeringResponse_httpStatus = Lens.lens (\CreateTransitGatewayPeeringResponse' {httpStatus} -> httpStatus) (\s@CreateTransitGatewayPeeringResponse' {} a -> s {httpStatus = a} :: CreateTransitGatewayPeeringResponse)

instance
  Prelude.NFData
    CreateTransitGatewayPeeringResponse
  where
  rnf CreateTransitGatewayPeeringResponse' {..} =
    Prelude.rnf transitGatewayPeering
      `Prelude.seq` Prelude.rnf httpStatus
