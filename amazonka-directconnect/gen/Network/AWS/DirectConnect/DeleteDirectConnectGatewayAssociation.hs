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
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between the specified Direct Connect gateway and
-- virtual private gateway.
--
-- We recommend that you specify the @associationID@ to delete the
-- association. Alternatively, if you own virtual gateway and a Direct
-- Connect gateway association, you can specify the @virtualGatewayId@ and
-- @directConnectGatewayId@ to delete an association.
module Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
  ( -- * Creating a Request
    DeleteDirectConnectGatewayAssociation (..),
    newDeleteDirectConnectGatewayAssociation,

    -- * Request Lenses
    deleteDirectConnectGatewayAssociation_virtualGatewayId,
    deleteDirectConnectGatewayAssociation_associationId,
    deleteDirectConnectGatewayAssociation_directConnectGatewayId,

    -- * Destructuring the Response
    DeleteDirectConnectGatewayAssociationResponse (..),
    newDeleteDirectConnectGatewayAssociationResponse,

    -- * Response Lenses
    deleteDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation,
    deleteDirectConnectGatewayAssociationResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteDirectConnectGatewayAssociation' smart constructor.
data DeleteDirectConnectGatewayAssociation = DeleteDirectConnectGatewayAssociation'
  { -- | The ID of the virtual private gateway.
    virtualGatewayId :: Core.Maybe Core.Text,
    -- | The ID of the Direct Connect gateway association.
    associationId :: Core.Maybe Core.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDirectConnectGatewayAssociation' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'virtualGatewayId', 'deleteDirectConnectGatewayAssociation_virtualGatewayId' - The ID of the virtual private gateway.
--
-- 'associationId', 'deleteDirectConnectGatewayAssociation_associationId' - The ID of the Direct Connect gateway association.
--
-- 'directConnectGatewayId', 'deleteDirectConnectGatewayAssociation_directConnectGatewayId' - The ID of the Direct Connect gateway.
newDeleteDirectConnectGatewayAssociation ::
  DeleteDirectConnectGatewayAssociation
newDeleteDirectConnectGatewayAssociation =
  DeleteDirectConnectGatewayAssociation'
    { virtualGatewayId =
        Core.Nothing,
      associationId = Core.Nothing,
      directConnectGatewayId =
        Core.Nothing
    }

-- | The ID of the virtual private gateway.
deleteDirectConnectGatewayAssociation_virtualGatewayId :: Lens.Lens' DeleteDirectConnectGatewayAssociation (Core.Maybe Core.Text)
deleteDirectConnectGatewayAssociation_virtualGatewayId = Lens.lens (\DeleteDirectConnectGatewayAssociation' {virtualGatewayId} -> virtualGatewayId) (\s@DeleteDirectConnectGatewayAssociation' {} a -> s {virtualGatewayId = a} :: DeleteDirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway association.
deleteDirectConnectGatewayAssociation_associationId :: Lens.Lens' DeleteDirectConnectGatewayAssociation (Core.Maybe Core.Text)
deleteDirectConnectGatewayAssociation_associationId = Lens.lens (\DeleteDirectConnectGatewayAssociation' {associationId} -> associationId) (\s@DeleteDirectConnectGatewayAssociation' {} a -> s {associationId = a} :: DeleteDirectConnectGatewayAssociation)

-- | The ID of the Direct Connect gateway.
deleteDirectConnectGatewayAssociation_directConnectGatewayId :: Lens.Lens' DeleteDirectConnectGatewayAssociation (Core.Maybe Core.Text)
deleteDirectConnectGatewayAssociation_directConnectGatewayId = Lens.lens (\DeleteDirectConnectGatewayAssociation' {directConnectGatewayId} -> directConnectGatewayId) (\s@DeleteDirectConnectGatewayAssociation' {} a -> s {directConnectGatewayId = a} :: DeleteDirectConnectGatewayAssociation)

instance
  Core.AWSRequest
    DeleteDirectConnectGatewayAssociation
  where
  type
    AWSResponse
      DeleteDirectConnectGatewayAssociation =
      DeleteDirectConnectGatewayAssociationResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayAssociationResponse'
            Core.<$> (x Core..?> "directConnectGatewayAssociation")
              Core.<*> (Core.pure (Core.fromEnum s))
      )

instance
  Core.Hashable
    DeleteDirectConnectGatewayAssociation

instance
  Core.NFData
    DeleteDirectConnectGatewayAssociation

instance
  Core.ToHeaders
    DeleteDirectConnectGatewayAssociation
  where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "OvertureService.DeleteDirectConnectGatewayAssociation" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance
  Core.ToJSON
    DeleteDirectConnectGatewayAssociation
  where
  toJSON DeleteDirectConnectGatewayAssociation' {..} =
    Core.object
      ( Core.catMaybes
          [ ("virtualGatewayId" Core..=)
              Core.<$> virtualGatewayId,
            ("associationId" Core..=) Core.<$> associationId,
            ("directConnectGatewayId" Core..=)
              Core.<$> directConnectGatewayId
          ]
      )

instance
  Core.ToPath
    DeleteDirectConnectGatewayAssociation
  where
  toPath = Core.const "/"

instance
  Core.ToQuery
    DeleteDirectConnectGatewayAssociation
  where
  toQuery = Core.const Core.mempty

-- | /See:/ 'newDeleteDirectConnectGatewayAssociationResponse' smart constructor.
data DeleteDirectConnectGatewayAssociationResponse = DeleteDirectConnectGatewayAssociationResponse'
  { -- | Information about the deleted association.
    directConnectGatewayAssociation :: Core.Maybe DirectConnectGatewayAssociation,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteDirectConnectGatewayAssociationResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'directConnectGatewayAssociation', 'deleteDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation' - Information about the deleted association.
--
-- 'httpStatus', 'deleteDirectConnectGatewayAssociationResponse_httpStatus' - The response's http status code.
newDeleteDirectConnectGatewayAssociationResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteDirectConnectGatewayAssociationResponse
newDeleteDirectConnectGatewayAssociationResponse
  pHttpStatus_ =
    DeleteDirectConnectGatewayAssociationResponse'
      { directConnectGatewayAssociation =
          Core.Nothing,
        httpStatus = pHttpStatus_
      }

-- | Information about the deleted association.
deleteDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation :: Lens.Lens' DeleteDirectConnectGatewayAssociationResponse (Core.Maybe DirectConnectGatewayAssociation)
deleteDirectConnectGatewayAssociationResponse_directConnectGatewayAssociation = Lens.lens (\DeleteDirectConnectGatewayAssociationResponse' {directConnectGatewayAssociation} -> directConnectGatewayAssociation) (\s@DeleteDirectConnectGatewayAssociationResponse' {} a -> s {directConnectGatewayAssociation = a} :: DeleteDirectConnectGatewayAssociationResponse)

-- | The response's http status code.
deleteDirectConnectGatewayAssociationResponse_httpStatus :: Lens.Lens' DeleteDirectConnectGatewayAssociationResponse Core.Int
deleteDirectConnectGatewayAssociationResponse_httpStatus = Lens.lens (\DeleteDirectConnectGatewayAssociationResponse' {httpStatus} -> httpStatus) (\s@DeleteDirectConnectGatewayAssociationResponse' {} a -> s {httpStatus = a} :: DeleteDirectConnectGatewayAssociationResponse)

instance
  Core.NFData
    DeleteDirectConnectGatewayAssociationResponse
