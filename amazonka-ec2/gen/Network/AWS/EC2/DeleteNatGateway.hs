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
-- Module      : Network.AWS.EC2.DeleteNatGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified NAT gateway. Deleting a NAT gateway disassociates
-- its Elastic IP address, but does not release the address from your
-- account. Deleting a NAT gateway does not delete any NAT gateway routes
-- in your route tables.
module Network.AWS.EC2.DeleteNatGateway
  ( -- * Creating a Request
    DeleteNatGateway (..),
    newDeleteNatGateway,

    -- * Request Lenses
    deleteNatGateway_dryRun,
    deleteNatGateway_natGatewayId,

    -- * Destructuring the Response
    DeleteNatGatewayResponse (..),
    newDeleteNatGatewayResponse,

    -- * Response Lenses
    deleteNatGatewayResponse_natGatewayId,
    deleteNatGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteNatGateway' smart constructor.
data DeleteNatGateway = DeleteNatGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the NAT gateway.
    natGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNatGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteNatGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'natGatewayId', 'deleteNatGateway_natGatewayId' - The ID of the NAT gateway.
newDeleteNatGateway ::
  -- | 'natGatewayId'
  Core.Text ->
  DeleteNatGateway
newDeleteNatGateway pNatGatewayId_ =
  DeleteNatGateway'
    { dryRun = Core.Nothing,
      natGatewayId = pNatGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteNatGateway_dryRun :: Lens.Lens' DeleteNatGateway (Core.Maybe Core.Bool)
deleteNatGateway_dryRun = Lens.lens (\DeleteNatGateway' {dryRun} -> dryRun) (\s@DeleteNatGateway' {} a -> s {dryRun = a} :: DeleteNatGateway)

-- | The ID of the NAT gateway.
deleteNatGateway_natGatewayId :: Lens.Lens' DeleteNatGateway Core.Text
deleteNatGateway_natGatewayId = Lens.lens (\DeleteNatGateway' {natGatewayId} -> natGatewayId) (\s@DeleteNatGateway' {} a -> s {natGatewayId = a} :: DeleteNatGateway)

instance Core.AWSRequest DeleteNatGateway where
  type
    AWSResponse DeleteNatGateway =
      DeleteNatGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteNatGatewayResponse'
            Core.<$> (x Core..@? "natGatewayId")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteNatGateway

instance Core.NFData DeleteNatGateway

instance Core.ToHeaders DeleteNatGateway where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteNatGateway where
  toPath = Core.const "/"

instance Core.ToQuery DeleteNatGateway where
  toQuery DeleteNatGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteNatGateway" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "NatGatewayId" Core.=: natGatewayId
      ]

-- | /See:/ 'newDeleteNatGatewayResponse' smart constructor.
data DeleteNatGatewayResponse = DeleteNatGatewayResponse'
  { -- | The ID of the NAT gateway.
    natGatewayId :: Core.Maybe Core.Text,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteNatGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'natGatewayId', 'deleteNatGatewayResponse_natGatewayId' - The ID of the NAT gateway.
--
-- 'httpStatus', 'deleteNatGatewayResponse_httpStatus' - The response's http status code.
newDeleteNatGatewayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteNatGatewayResponse
newDeleteNatGatewayResponse pHttpStatus_ =
  DeleteNatGatewayResponse'
    { natGatewayId =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | The ID of the NAT gateway.
deleteNatGatewayResponse_natGatewayId :: Lens.Lens' DeleteNatGatewayResponse (Core.Maybe Core.Text)
deleteNatGatewayResponse_natGatewayId = Lens.lens (\DeleteNatGatewayResponse' {natGatewayId} -> natGatewayId) (\s@DeleteNatGatewayResponse' {} a -> s {natGatewayId = a} :: DeleteNatGatewayResponse)

-- | The response's http status code.
deleteNatGatewayResponse_httpStatus :: Lens.Lens' DeleteNatGatewayResponse Core.Int
deleteNatGatewayResponse_httpStatus = Lens.lens (\DeleteNatGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteNatGatewayResponse' {} a -> s {httpStatus = a} :: DeleteNatGatewayResponse)

instance Core.NFData DeleteNatGatewayResponse
