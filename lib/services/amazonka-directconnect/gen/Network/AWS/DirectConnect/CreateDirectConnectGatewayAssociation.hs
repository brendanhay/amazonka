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
-- Module      : Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between a Direct Connect gateway and a virtual
-- private gateway. The virtual private gateway must be attached to a VPC
-- and must not be associated with another Direct Connect gateway.
module Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
  ( -- * Creating a Request
    CreateDirectConnectGatewayAssociation (..),
    newCreateDirectConnectGatewayAssociation,

    -- * Request Lenses
    createDirectConnectGatewayAssociation_virtualGatewayId,
    createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    createDirectConnectGatewayAssociation_gatewayId,
    createDirectConnectGatewayAssociation_directConnectGatewayId,

    -- * Destructuring the Response
    CreateDirectConnectGatewayAssociationResponse (..),
    newCreateDirectConnectGatewayAssociationResponse,

    -- * Response Lenses
    createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    createDirectConnectGatewayAssociationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDirectConnectGatewayAssociation' smart constructor.
data CreateDirectConnectGatewayAssociation = CreateDirectConnectGatewayAssociation'
  { -- | The ID of the virtual private gateway.
    virtualGatewayId :: Prelude.Maybe Prelude.Text,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway
    --
    -- This parameter is required when you create an association to a transit
    -- gateway.
    --
    -- For information about how to set the prefixes, see
    -- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
    -- in the /Direct Connect User Guide/.
    addAllowedPrefixesToDirectConnectGateway :: Prelude.Maybe [RouteFilterPrefix],
    -- | The ID of the virtual private gateway or transit gateway.
    gatewayId :: Prelude.Maybe Prelude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectConnectGatewayAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualGatewayId', 'createDirectConnectGatewayAssociation_virtualGatewayId' - The ID of the virtual private gateway.
--
-- 'addAllowedPrefixesToDirectConnectGateway', 'createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway
--
-- This parameter is required when you create an association to a transit
-- gateway.
--
-- For information about how to set the prefixes, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
-- in the /Direct Connect User Guide/.
--
-- 'gatewayId', 'createDirectConnectGatewayAssociation_gatewayId' - The ID of the virtual private gateway or transit gateway.
--
-- 'directConnectGatewayId', 'createDirectConnectGatewayAssociation_directConnectGatewayId' - The ID of the Direct Connect gateway.
newCreateDirectConnectGatewayAssociation ::
  -- | 'directConnectGatewayId'
  Prelude.Text ->
  CreateDirectConnectGatewayAssociation
newCreateDirectConnectGatewayAssociation
  pDirectConnectGatewayId_ =
    CreateDirectConnectGatewayAssociation'
      { virtualGatewayId =
          Prelude.Nothing,
        addAllowedPrefixesToDirectConnectGateway =
          Prelude.Nothing,
        gatewayId = Prelude.Nothing,
        directConnectGatewayId =
          pDirectConnectGatewayId_
      }

-- | The ID of the virtual private gateway.
createDirectConnectGatewayAssociation_virtualGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
createDirectConnectGatewayAssociation_virtualGatewayId = Lens.lens (\CreateDirectConnectGatewayAssociation' {virtualGatewayId} -> virtualGatewayId) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {virtualGatewayId = a} :: CreateDirectConnectGatewayAssociation)

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway
--
-- This parameter is required when you create an association to a transit
-- gateway.
--
-- For information about how to set the prefixes, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
-- in the /Direct Connect User Guide/.
createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociation (Prelude.Maybe [RouteFilterPrefix])
createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway = Lens.lens (\CreateDirectConnectGatewayAssociation' {addAllowedPrefixesToDirectConnectGateway} -> addAllowedPrefixesToDirectConnectGateway) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {addAllowedPrefixesToDirectConnectGateway = a} :: CreateDirectConnectGatewayAssociation) Prelude.. Lens.mapping Lens.coerced

-- | The ID of the virtual private gateway or transit gateway.
createDirectConnectGatewayAssociation_gatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation (Prelude.Maybe Prelude.Text)
createDirectConnectGatewayAssociation_gatewayId = Lens.lens (\CreateDirectConnectGatewayAssociation' {gatewayId} -> gatewayId) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {gatewayId = a} :: CreateDirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway.
createDirectConnectGatewayAssociation_directConnectGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation Prelude.Text
createDirectConnectGatewayAssociation_directConnectGatewayId = Lens.lens (\CreateDirectConnectGatewayAssociation' {directConnectGatewayId} -> directConnectGatewayId) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {directConnectGatewayId = a} :: CreateDirectConnectGatewayAssociation)

instance
  Core.AWSRequest
    CreateDirectConnectGatewayAssociation
  where
  type
    AWSResponse
      CreateDirectConnectGatewayAssociation =
      CreateDirectConnectGatewayAssociationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          CreateDirectConnectGatewayAssociationResponse'
            Prelude.<$> (x Core..?> "directConnectGatewayAssociation")
              Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    CreateDirectConnectGatewayAssociation

instance
  Prelude.NFData
    CreateDirectConnectGatewayAssociation

instance
  Core.ToHeaders
    CreateDirectConnectGatewayAssociation
  where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.CreateDirectConnectGatewayAssociation" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance
  Core.ToJSON
    CreateDirectConnectGatewayAssociation
  where
  toJSON CreateDirectConnectGatewayAssociation' {..} =
    Core.object
      ( Prelude.catMaybes
          [ ("virtualGatewayId" Core..=)
              Prelude.<$> virtualGatewayId,
            ("addAllowedPrefixesToDirectConnectGateway" Core..=)
              Prelude.<$> addAllowedPrefixesToDirectConnectGateway,
            ("gatewayId" Core..=) Prelude.<$> gatewayId,
            Prelude.Just
              ( "directConnectGatewayId"
                  Core..= directConnectGatewayId
              )
          ]
      )

instance
  Core.ToPath
    CreateDirectConnectGatewayAssociation
  where
  toPath = Prelude.const "/"

instance
  Core.ToQuery
    CreateDirectConnectGatewayAssociation
  where
  toQuery = Prelude.const Prelude.mempty

-- | /See:/ 'newCreateDirectConnectGatewayAssociationResponse' smart constructor.
data CreateDirectConnectGatewayAssociationResponse = CreateDirectConnectGatewayAssociationResponse'
  { -- | The association to be created.
    directConnectGatewayAssociation :: Prelude.Maybe DirectConnectGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'CreateDirectConnectGatewayAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayAssociation', 'createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation' - The association to be created.
--
-- 'httpStatus', 'createDirectConnectGatewayAssociationResponse_httpStatus' - The response's http status code.
newCreateDirectConnectGatewayAssociationResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  CreateDirectConnectGatewayAssociationResponse
newCreateDirectConnectGatewayAssociationResponse
  pHttpStatus_ =
    CreateDirectConnectGatewayAssociationResponse'
      { directConnectGatewayAssociation =
          Prelude.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The association to be created.
createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation :: Lens.Lens' CreateDirectConnectGatewayAssociationResponse (Prelude.Maybe DirectConnectGatewayAssociation)
createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation = Lens.lens (\CreateDirectConnectGatewayAssociationResponse' {directConnectGatewayAssociation} -> directConnectGatewayAssociation) (\s@CreateDirectConnectGatewayAssociationResponse' {} a -> s {directConnectGatewayAssociation = a} :: CreateDirectConnectGatewayAssociationResponse)

-- | The response's http status code.
createDirectConnectGatewayAssociationResponse_httpStatus :: Lens.Lens' CreateDirectConnectGatewayAssociationResponse Prelude.Int
createDirectConnectGatewayAssociationResponse_httpStatus = Lens.lens (\CreateDirectConnectGatewayAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateDirectConnectGatewayAssociationResponse' {} a -> s {httpStatus = a} :: CreateDirectConnectGatewayAssociationResponse)

instance
  Prelude.NFData
    CreateDirectConnectGatewayAssociationResponse
