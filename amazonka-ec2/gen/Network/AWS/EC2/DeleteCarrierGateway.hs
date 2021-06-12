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
-- Module      : Network.AWS.EC2.DeleteCarrierGateway
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a carrier gateway.
--
-- If you do not delete the route that contains the carrier gateway as the
-- Target, the route is a blackhole route. For information about how to
-- delete a route, see
-- <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteRoute.html DeleteRoute>.
module Network.AWS.EC2.DeleteCarrierGateway
  ( -- * Creating a Request
    DeleteCarrierGateway (..),
    newDeleteCarrierGateway,

    -- * Request Lenses
    deleteCarrierGateway_dryRun,
    deleteCarrierGateway_carrierGatewayId,

    -- * Destructuring the Response
    DeleteCarrierGatewayResponse (..),
    newDeleteCarrierGatewayResponse,

    -- * Response Lenses
    deleteCarrierGatewayResponse_carrierGateway,
    deleteCarrierGatewayResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newDeleteCarrierGateway' smart constructor.
data DeleteCarrierGateway = DeleteCarrierGateway'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the carrier gateway.
    carrierGatewayId :: Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCarrierGateway' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteCarrierGateway_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'carrierGatewayId', 'deleteCarrierGateway_carrierGatewayId' - The ID of the carrier gateway.
newDeleteCarrierGateway ::
  -- | 'carrierGatewayId'
  Core.Text ->
  DeleteCarrierGateway
newDeleteCarrierGateway pCarrierGatewayId_ =
  DeleteCarrierGateway'
    { dryRun = Core.Nothing,
      carrierGatewayId = pCarrierGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteCarrierGateway_dryRun :: Lens.Lens' DeleteCarrierGateway (Core.Maybe Core.Bool)
deleteCarrierGateway_dryRun = Lens.lens (\DeleteCarrierGateway' {dryRun} -> dryRun) (\s@DeleteCarrierGateway' {} a -> s {dryRun = a} :: DeleteCarrierGateway)

-- | The ID of the carrier gateway.
deleteCarrierGateway_carrierGatewayId :: Lens.Lens' DeleteCarrierGateway Core.Text
deleteCarrierGateway_carrierGatewayId = Lens.lens (\DeleteCarrierGateway' {carrierGatewayId} -> carrierGatewayId) (\s@DeleteCarrierGateway' {} a -> s {carrierGatewayId = a} :: DeleteCarrierGateway)

instance Core.AWSRequest DeleteCarrierGateway where
  type
    AWSResponse DeleteCarrierGateway =
      DeleteCarrierGatewayResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteCarrierGatewayResponse'
            Core.<$> (x Core..@? "carrierGateway")
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteCarrierGateway

instance Core.NFData DeleteCarrierGateway

instance Core.ToHeaders DeleteCarrierGateway where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteCarrierGateway where
  toPath = Core.const "/"

instance Core.ToQuery DeleteCarrierGateway where
  toQuery DeleteCarrierGateway' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteCarrierGateway" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "CarrierGatewayId" Core.=: carrierGatewayId
      ]

-- | /See:/ 'newDeleteCarrierGatewayResponse' smart constructor.
data DeleteCarrierGatewayResponse = DeleteCarrierGatewayResponse'
  { -- | Information about the carrier gateway.
    carrierGateway :: Core.Maybe CarrierGateway,
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteCarrierGatewayResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'carrierGateway', 'deleteCarrierGatewayResponse_carrierGateway' - Information about the carrier gateway.
--
-- 'httpStatus', 'deleteCarrierGatewayResponse_httpStatus' - The response's http status code.
newDeleteCarrierGatewayResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteCarrierGatewayResponse
newDeleteCarrierGatewayResponse pHttpStatus_ =
  DeleteCarrierGatewayResponse'
    { carrierGateway =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the carrier gateway.
deleteCarrierGatewayResponse_carrierGateway :: Lens.Lens' DeleteCarrierGatewayResponse (Core.Maybe CarrierGateway)
deleteCarrierGatewayResponse_carrierGateway = Lens.lens (\DeleteCarrierGatewayResponse' {carrierGateway} -> carrierGateway) (\s@DeleteCarrierGatewayResponse' {} a -> s {carrierGateway = a} :: DeleteCarrierGatewayResponse)

-- | The response's http status code.
deleteCarrierGatewayResponse_httpStatus :: Lens.Lens' DeleteCarrierGatewayResponse Core.Int
deleteCarrierGatewayResponse_httpStatus = Lens.lens (\DeleteCarrierGatewayResponse' {httpStatus} -> httpStatus) (\s@DeleteCarrierGatewayResponse' {} a -> s {httpStatus = a} :: DeleteCarrierGatewayResponse)

instance Core.NFData DeleteCarrierGatewayResponse
