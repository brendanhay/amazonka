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
-- Module      : Network.AWS.EC2.DeleteVpcEndpoints
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes one or more specified VPC endpoints. You can delete any of the
-- following types of VPC endpoints.
--
-- -   Gateway endpoint,
--
-- -   Gateway Load Balancer endpoint,
--
-- -   Interface endpoint
--
-- The following rules apply when you delete a VPC endpoint:
--
-- -   When you delete a gateway endpoint, we delete the endpoint routes in
--     the route tables that are associated with the endpoint.
--
-- -   When you delete a Gateway Load Balancer endpoint, we delete the
--     endpoint network interfaces.
--
--     You can only delete Gateway Load Balancer endpoints when the routes
--     that are associated with the endpoint are deleted.
--
-- -   When you delete an interface endpoint, we delete the endpoint
--     network interfaces.
module Network.AWS.EC2.DeleteVpcEndpoints
  ( -- * Creating a Request
    DeleteVpcEndpoints (..),
    newDeleteVpcEndpoints,

    -- * Request Lenses
    deleteVpcEndpoints_dryRun,
    deleteVpcEndpoints_vpcEndpointIds,

    -- * Destructuring the Response
    DeleteVpcEndpointsResponse (..),
    newDeleteVpcEndpointsResponse,

    -- * Response Lenses
    deleteVpcEndpointsResponse_unsuccessful,
    deleteVpcEndpointsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Contains the parameters for DeleteVpcEndpoints.
--
-- /See:/ 'newDeleteVpcEndpoints' smart constructor.
data DeleteVpcEndpoints = DeleteVpcEndpoints'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | One or more VPC endpoint IDs.
    vpcEndpointIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVpcEndpoints' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'deleteVpcEndpoints_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'vpcEndpointIds', 'deleteVpcEndpoints_vpcEndpointIds' - One or more VPC endpoint IDs.
newDeleteVpcEndpoints ::
  DeleteVpcEndpoints
newDeleteVpcEndpoints =
  DeleteVpcEndpoints'
    { dryRun = Core.Nothing,
      vpcEndpointIds = Core.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
deleteVpcEndpoints_dryRun :: Lens.Lens' DeleteVpcEndpoints (Core.Maybe Core.Bool)
deleteVpcEndpoints_dryRun = Lens.lens (\DeleteVpcEndpoints' {dryRun} -> dryRun) (\s@DeleteVpcEndpoints' {} a -> s {dryRun = a} :: DeleteVpcEndpoints)

-- | One or more VPC endpoint IDs.
deleteVpcEndpoints_vpcEndpointIds :: Lens.Lens' DeleteVpcEndpoints [Core.Text]
deleteVpcEndpoints_vpcEndpointIds = Lens.lens (\DeleteVpcEndpoints' {vpcEndpointIds} -> vpcEndpointIds) (\s@DeleteVpcEndpoints' {} a -> s {vpcEndpointIds = a} :: DeleteVpcEndpoints) Core.. Lens._Coerce

instance Core.AWSRequest DeleteVpcEndpoints where
  type
    AWSResponse DeleteVpcEndpoints =
      DeleteVpcEndpointsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          DeleteVpcEndpointsResponse'
            Core.<$> ( x Core..@? "unsuccessful" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DeleteVpcEndpoints

instance Core.NFData DeleteVpcEndpoints

instance Core.ToHeaders DeleteVpcEndpoints where
  toHeaders = Core.const Core.mempty

instance Core.ToPath DeleteVpcEndpoints where
  toPath = Core.const "/"

instance Core.ToQuery DeleteVpcEndpoints where
  toQuery DeleteVpcEndpoints' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("DeleteVpcEndpoints" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        Core.toQueryList "VpcEndpointId" vpcEndpointIds
      ]

-- | Contains the output of DeleteVpcEndpoints.
--
-- /See:/ 'newDeleteVpcEndpointsResponse' smart constructor.
data DeleteVpcEndpointsResponse = DeleteVpcEndpointsResponse'
  { -- | Information about the VPC endpoints that were not successfully deleted.
    unsuccessful :: Core.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DeleteVpcEndpointsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'deleteVpcEndpointsResponse_unsuccessful' - Information about the VPC endpoints that were not successfully deleted.
--
-- 'httpStatus', 'deleteVpcEndpointsResponse_httpStatus' - The response's http status code.
newDeleteVpcEndpointsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DeleteVpcEndpointsResponse
newDeleteVpcEndpointsResponse pHttpStatus_ =
  DeleteVpcEndpointsResponse'
    { unsuccessful =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the VPC endpoints that were not successfully deleted.
deleteVpcEndpointsResponse_unsuccessful :: Lens.Lens' DeleteVpcEndpointsResponse (Core.Maybe [UnsuccessfulItem])
deleteVpcEndpointsResponse_unsuccessful = Lens.lens (\DeleteVpcEndpointsResponse' {unsuccessful} -> unsuccessful) (\s@DeleteVpcEndpointsResponse' {} a -> s {unsuccessful = a} :: DeleteVpcEndpointsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
deleteVpcEndpointsResponse_httpStatus :: Lens.Lens' DeleteVpcEndpointsResponse Core.Int
deleteVpcEndpointsResponse_httpStatus = Lens.lens (\DeleteVpcEndpointsResponse' {httpStatus} -> httpStatus) (\s@DeleteVpcEndpointsResponse' {} a -> s {httpStatus = a} :: DeleteVpcEndpointsResponse)

instance Core.NFData DeleteVpcEndpointsResponse
