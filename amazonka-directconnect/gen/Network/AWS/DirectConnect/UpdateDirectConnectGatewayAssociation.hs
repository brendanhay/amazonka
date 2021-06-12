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
-- Module      : Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the Direct Connect gateway
-- association.
--
-- Add or remove prefixes from the association.
module Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
  ( -- * Creating a Request
    UpdateDirectConnectGatewayAssociation (..),
    newUpdateDirectConnectGatewayAssociation,

    -- * Request Lenses
    updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway,
    updateDirectConnectGatewayAssociation_associationId,

    -- * Destructuring the Response
    UpdateDirectConnectGatewayAssociationResponse (..),
    newUpdateDirectConnectGatewayAssociationResponse,

    -- * Response Lenses
    updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    updateDirectConnectGatewayAssociationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newUpdateDirectConnectGatewayAssociation' smart constructor.
data UpdateDirectConnectGatewayAssociation = UpdateDirectConnectGatewayAssociation'
  { -- | The Amazon VPC prefixes to no longer advertise to the Direct Connect
    -- gateway.
    removeAllowedPrefixesToDirectConnectGateway :: Core.Maybe [RouteFilterPrefix],
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    addAllowedPrefixesToDirectConnectGateway :: Core.Maybe [RouteFilterPrefix],
    -- | The ID of the Direct Connect gateway association.
    associationId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDirectConnectGatewayAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'removeAllowedPrefixesToDirectConnectGateway', 'updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to no longer advertise to the Direct Connect
-- gateway.
--
-- 'addAllowedPrefixesToDirectConnectGateway', 'updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- 'associationId', 'updateDirectConnectGatewayAssociation_associationId' - The ID of the Direct Connect gateway association.
newUpdateDirectConnectGatewayAssociation ::
  UpdateDirectConnectGatewayAssociation
newUpdateDirectConnectGatewayAssociation =
  UpdateDirectConnectGatewayAssociation'
    { removeAllowedPrefixesToDirectConnectGateway =
        Core.Nothing,
      addAllowedPrefixesToDirectConnectGateway =
        Core.Nothing,
      associationId = Core.Nothing
    }

-- | The Amazon VPC prefixes to no longer advertise to the Direct Connect
-- gateway.
updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Core.Maybe [RouteFilterPrefix])
updateDirectConnectGatewayAssociation_removeAllowedPrefixesToDirectConnectGateway = Lens.lens (\UpdateDirectConnectGatewayAssociation' {removeAllowedPrefixesToDirectConnectGateway} -> removeAllowedPrefixesToDirectConnectGateway) (\s@UpdateDirectConnectGatewayAssociation' {} a -> s {removeAllowedPrefixesToDirectConnectGateway = a} :: UpdateDirectConnectGatewayAssociation) Core.. Lens.mapping Lens._Coerce

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Core.Maybe [RouteFilterPrefix])
updateDirectConnectGatewayAssociation_addAllowedPrefixesToDirectConnectGateway = Lens.lens (\UpdateDirectConnectGatewayAssociation' {addAllowedPrefixesToDirectConnectGateway} -> addAllowedPrefixesToDirectConnectGateway) (\s@UpdateDirectConnectGatewayAssociation' {} a -> s {addAllowedPrefixesToDirectConnectGateway = a} :: UpdateDirectConnectGatewayAssociation) Core.. Lens.mapping Lens._Coerce

-- | The ID of the Direct Connect gateway association.
updateDirectConnectGatewayAssociation_associationId :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Core.Maybe Core.Text)
updateDirectConnectGatewayAssociation_associationId = Lens.lens (\UpdateDirectConnectGatewayAssociation' {associationId} -> associationId) (\s@UpdateDirectConnectGatewayAssociation' {} a -> s {associationId = a} :: UpdateDirectConnectGatewayAssociation)

instance
  Core.AWSRequest
    UpdateDirectConnectGatewayAssociation
  where
  type
    AWSResponse
      UpdateDirectConnectGatewayAssociation =
      UpdateDirectConnectGatewayAssociationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          UpdateDirectConnectGatewayAssociationResponse'
            Core.<$> (x Core..?> "directConnectGatewayAssociation")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    UpdateDirectConnectGatewayAssociation

instance
  Core.NFData
    UpdateDirectConnectGatewayAssociation

instance
  Core.ToHeaders
    UpdateDirectConnectGatewayAssociation
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.UpdateDirectConnectGatewayAssociation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    UpdateDirectConnectGatewayAssociation
  where
  toJSON UpdateDirectConnectGatewayAssociation' {..} =
    Core.object
      ( Core.catMaybes
          [ ( "removeAllowedPrefixesToDirectConnectGateway"
                Core..=
            )
              Core.<$> removeAllowedPrefixesToDirectConnectGateway,
            ("addAllowedPrefixesToDirectConnectGateway" Core..=)
              Core.<$> addAllowedPrefixesToDirectConnectGateway,
            ("associationId" Core..=) Core.<$> associationId
          ]
      )

instance
  Core.ToPath
    UpdateDirectConnectGatewayAssociation
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    UpdateDirectConnectGatewayAssociation
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newUpdateDirectConnectGatewayAssociationResponse' smart constructor.
data UpdateDirectConnectGatewayAssociationResponse = UpdateDirectConnectGatewayAssociationResponse'
  { directConnectGatewayAssociation :: Core.Maybe DirectConnectGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'UpdateDirectConnectGatewayAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayAssociation', 'updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation' - Undocumented member.
--
-- 'httpStatus', 'updateDirectConnectGatewayAssociationResponse_httpStatus' - The response's http status code.
newUpdateDirectConnectGatewayAssociationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  UpdateDirectConnectGatewayAssociationResponse
newUpdateDirectConnectGatewayAssociationResponse
  pHttpStatus_ =
    UpdateDirectConnectGatewayAssociationResponse'
      { directConnectGatewayAssociation =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Undocumented member.
updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation :: Lens.Lens' UpdateDirectConnectGatewayAssociationResponse (Core.Maybe DirectConnectGatewayAssociation)
updateDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation = Lens.lens (\UpdateDirectConnectGatewayAssociationResponse' {directConnectGatewayAssociation} -> directConnectGatewayAssociation) (\s@UpdateDirectConnectGatewayAssociationResponse' {} a -> s {directConnectGatewayAssociation = a} :: UpdateDirectConnectGatewayAssociationResponse)

-- | The response's http status code.
updateDirectConnectGatewayAssociationResponse_httpStatus :: Lens.Lens' UpdateDirectConnectGatewayAssociationResponse Core.Int
updateDirectConnectGatewayAssociationResponse_httpStatus = Lens.lens (\UpdateDirectConnectGatewayAssociationResponse' {httpStatus} -> httpStatus) (\s@UpdateDirectConnectGatewayAssociationResponse' {} a -> s {httpStatus = a} :: UpdateDirectConnectGatewayAssociationResponse)

instance
  Core.NFData
    UpdateDirectConnectGatewayAssociationResponse
