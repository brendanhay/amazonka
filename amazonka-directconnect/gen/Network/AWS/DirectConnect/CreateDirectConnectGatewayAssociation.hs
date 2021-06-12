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
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newCreateDirectConnectGatewayAssociation' smart constructor.
data CreateDirectConnectGatewayAssociation = CreateDirectConnectGatewayAssociation'
  { -- | The ID of the virtual private gateway.
    virtualGatewayId :: Core.Maybe Core.Text,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway
    --
    -- This parameter is required when you create an association to a transit
    -- gateway.
    --
    -- For information about how to set the prefixes, see
    -- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
    -- in the /AWS Direct Connect User Guide/.
    addAllowedPrefixesToDirectConnectGateway :: Core.Maybe [RouteFilterPrefix],
    -- | The ID of the virtual private gateway or transit gateway.
    gatewayId :: Core.Maybe Core.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
-- in the /AWS Direct Connect User Guide/.
--
-- 'gatewayId', 'createDirectConnectGatewayAssociation_gatewayId' - The ID of the virtual private gateway or transit gateway.
--
-- 'directConnectGatewayId', 'createDirectConnectGatewayAssociation_directConnectGatewayId' - The ID of the Direct Connect gateway.
newCreateDirectConnectGatewayAssociation ::
  -- | 'directConnectGatewayId'
  Core.Text ->
  CreateDirectConnectGatewayAssociation
newCreateDirectConnectGatewayAssociation
  pDirectConnectGatewayId_ =
    CreateDirectConnectGatewayAssociation'
      { virtualGatewayId =
          Core.Nothing,
        addAllowedPrefixesToDirectConnectGateway =
          Core.Nothing,
        gatewayId = Core.Nothing,
        directConnectGatewayId =
          pDirectConnectGatewayId_
      }

-- | The ID of the virtual private gateway.
createDirectConnectGatewayAssociation_virtualGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation (Core.Maybe Core.Text)
createDirectConnectGatewayAssociation_virtualGatewayId = Lens.lens (\CreateDirectConnectGatewayAssociation' {virtualGatewayId} -> virtualGatewayId) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {virtualGatewayId = a} :: CreateDirectConnectGatewayAssociation)

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway
--
-- This parameter is required when you create an association to a transit
-- gateway.
--
-- For information about how to set the prefixes, see
-- <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes>
-- in the /AWS Direct Connect User Guide/.
createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociation (Core.Maybe [RouteFilterPrefix])
createDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway = Lens.lens (\CreateDirectConnectGatewayAssociation' {addAllowedPrefixesToDirectConnectGateway} -> addAllowedPrefixesToDirectConnectGateway) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {addAllowedPrefixesToDirectConnectGateway = a} :: CreateDirectConnectGatewayAssociation) Core.. Lens.mapping Lens._Coerce

-- | The ID of the virtual private gateway or transit gateway.
createDirectConnectGatewayAssociation_gatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation (Core.Maybe Core.Text)
createDirectConnectGatewayAssociation_gatewayId = Lens.lens (\CreateDirectConnectGatewayAssociation' {gatewayId} -> gatewayId) (\s@CreateDirectConnectGatewayAssociation' {} a -> s {gatewayId = a} :: CreateDirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway.
createDirectConnectGatewayAssociation_directConnectGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation Core.Text
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
            Core.<$> (x Core..?> "directConnectGatewayAssociation")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    CreateDirectConnectGatewayAssociation

instance
  Core.NFData
    CreateDirectConnectGatewayAssociation

instance
  Core.ToHeaders
    CreateDirectConnectGatewayAssociation
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.CreateDirectConnectGatewayAssociation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    CreateDirectConnectGatewayAssociation
  where
  toJSON CreateDirectConnectGatewayAssociation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("virtualGatewayId" Core..=)
              Core.<$> virtualGatewayId,
            ("addAllowedPrefixesToDirectConnectGateway" Core..=)
              Core.<$> addAllowedPrefixesToDirectConnectGateway,
            ("gatewayId" Core..=) Core.<$> gatewayId,
            Core.Just
              ( "directConnectGatewayId"
                  Core..= directConnectGatewayId
              )
          ]
      )

instance
  Core.ToPath
    CreateDirectConnectGatewayAssociation
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    CreateDirectConnectGatewayAssociation
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newCreateDirectConnectGatewayAssociationResponse' smart constructor.
data CreateDirectConnectGatewayAssociationResponse = CreateDirectConnectGatewayAssociationResponse'
  { -- | The association to be created.
    directConnectGatewayAssociation :: Core.Maybe DirectConnectGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

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
  Core.Int ->
  CreateDirectConnectGatewayAssociationResponse
newCreateDirectConnectGatewayAssociationResponse
  pHttpStatus_ =
    CreateDirectConnectGatewayAssociationResponse'
      { directConnectGatewayAssociation =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | The association to be created.
createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation :: Lens.Lens' CreateDirectConnectGatewayAssociationResponse (Core.Maybe DirectConnectGatewayAssociation)
createDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation = Lens.lens (\CreateDirectConnectGatewayAssociationResponse' {directConnectGatewayAssociation} -> directConnectGatewayAssociation) (\s@CreateDirectConnectGatewayAssociationResponse' {} a -> s {directConnectGatewayAssociation = a} :: CreateDirectConnectGatewayAssociationResponse)

-- | The response's http status code.
createDirectConnectGatewayAssociationResponse_httpStatus :: Lens.Lens' CreateDirectConnectGatewayAssociationResponse Core.Int
createDirectConnectGatewayAssociationResponse_httpStatus = Lens.lens (\CreateDirectConnectGatewayAssociationResponse' {httpStatus} -> httpStatus) (\s@CreateDirectConnectGatewayAssociationResponse' {} a -> s {httpStatus = a} :: CreateDirectConnectGatewayAssociationResponse)

instance
  Core.NFData
    CreateDirectConnectGatewayAssociationResponse
