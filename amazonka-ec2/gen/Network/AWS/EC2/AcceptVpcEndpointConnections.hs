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
-- Module      : Network.AWS.EC2.AcceptVpcEndpointConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Accepts one or more interface VPC endpoint connection requests to your
-- VPC endpoint service.
module Network.AWS.EC2.AcceptVpcEndpointConnections
  ( -- * Creating a Request
    AcceptVpcEndpointConnections (..),
    newAcceptVpcEndpointConnections,

    -- * Request Lenses
    acceptVpcEndpointConnections_dryRun,
    acceptVpcEndpointConnections_serviceId,
    acceptVpcEndpointConnections_vpcEndpointIds,

    -- * Destructuring the Response
    AcceptVpcEndpointConnectionsResponse (..),
    newAcceptVpcEndpointConnectionsResponse,

    -- * Response Lenses
    acceptVpcEndpointConnectionsResponse_unsuccessful,
    acceptVpcEndpointConnectionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newAcceptVpcEndpointConnections' smart constructor.
data AcceptVpcEndpointConnections = AcceptVpcEndpointConnections'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Core.Maybe Core.Bool,
    -- | The ID of the VPC endpoint service.
    serviceId :: Core.Text,
    -- | The IDs of one or more interface VPC endpoints.
    vpcEndpointIds :: [Core.Text]
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptVpcEndpointConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'acceptVpcEndpointConnections_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'serviceId', 'acceptVpcEndpointConnections_serviceId' - The ID of the VPC endpoint service.
--
-- 'vpcEndpointIds', 'acceptVpcEndpointConnections_vpcEndpointIds' - The IDs of one or more interface VPC endpoints.
newAcceptVpcEndpointConnections ::
  -- | 'serviceId'
  Core.Text ->
  AcceptVpcEndpointConnections
newAcceptVpcEndpointConnections pServiceId_ =
  AcceptVpcEndpointConnections'
    { dryRun =
        Core.Nothing,
      serviceId = pServiceId_,
      vpcEndpointIds = Core.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
acceptVpcEndpointConnections_dryRun :: Lens.Lens' AcceptVpcEndpointConnections (Core.Maybe Core.Bool)
acceptVpcEndpointConnections_dryRun = Lens.lens (\AcceptVpcEndpointConnections' {dryRun} -> dryRun) (\s@AcceptVpcEndpointConnections' {} a -> s {dryRun = a} :: AcceptVpcEndpointConnections)

-- | The ID of the VPC endpoint service.
acceptVpcEndpointConnections_serviceId :: Lens.Lens' AcceptVpcEndpointConnections Core.Text
acceptVpcEndpointConnections_serviceId = Lens.lens (\AcceptVpcEndpointConnections' {serviceId} -> serviceId) (\s@AcceptVpcEndpointConnections' {} a -> s {serviceId = a} :: AcceptVpcEndpointConnections)

-- | The IDs of one or more interface VPC endpoints.
acceptVpcEndpointConnections_vpcEndpointIds :: Lens.Lens' AcceptVpcEndpointConnections [Core.Text]
acceptVpcEndpointConnections_vpcEndpointIds = Lens.lens (\AcceptVpcEndpointConnections' {vpcEndpointIds} -> vpcEndpointIds) (\s@AcceptVpcEndpointConnections' {} a -> s {vpcEndpointIds = a} :: AcceptVpcEndpointConnections) Core.. Lens._Coerce

instance Core.AWSRequest AcceptVpcEndpointConnections where
  type
    AWSResponse AcceptVpcEndpointConnections =
      AcceptVpcEndpointConnectionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          AcceptVpcEndpointConnectionsResponse'
            Core.<$> ( x Core..@? "unsuccessful" Core..!@ Core.mempty
                         Core.>>= Core.may (Core.parseXMLList "item")
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable AcceptVpcEndpointConnections

instance Core.NFData AcceptVpcEndpointConnections

instance Core.ToHeaders AcceptVpcEndpointConnections where
  toHeaders = Core.const Core.mempty

instance Core.ToPath AcceptVpcEndpointConnections where
  toPath = Core.const "/"

instance Core.ToQuery AcceptVpcEndpointConnections where
  toQuery AcceptVpcEndpointConnections' {..} =
    Core.mconcat
      [ "Action"
          Core.=: ("AcceptVpcEndpointConnections" :: Core.ByteString),
        "Version" Core.=: ("2016-11-15" :: Core.ByteString),
        "DryRun" Core.=: dryRun,
        "ServiceId" Core.=: serviceId,
        Core.toQueryList "VpcEndpointId" vpcEndpointIds
      ]

-- | /See:/ 'newAcceptVpcEndpointConnectionsResponse' smart constructor.
data AcceptVpcEndpointConnectionsResponse = AcceptVpcEndpointConnectionsResponse'
  { -- | Information about the interface endpoints that were not accepted, if
    -- applicable.
    unsuccessful :: Core.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'AcceptVpcEndpointConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'acceptVpcEndpointConnectionsResponse_unsuccessful' - Information about the interface endpoints that were not accepted, if
-- applicable.
--
-- 'httpStatus', 'acceptVpcEndpointConnectionsResponse_httpStatus' - The response's http status code.
newAcceptVpcEndpointConnectionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  AcceptVpcEndpointConnectionsResponse
newAcceptVpcEndpointConnectionsResponse pHttpStatus_ =
  AcceptVpcEndpointConnectionsResponse'
    { unsuccessful =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the interface endpoints that were not accepted, if
-- applicable.
acceptVpcEndpointConnectionsResponse_unsuccessful :: Lens.Lens' AcceptVpcEndpointConnectionsResponse (Core.Maybe [UnsuccessfulItem])
acceptVpcEndpointConnectionsResponse_unsuccessful = Lens.lens (\AcceptVpcEndpointConnectionsResponse' {unsuccessful} -> unsuccessful) (\s@AcceptVpcEndpointConnectionsResponse' {} a -> s {unsuccessful = a} :: AcceptVpcEndpointConnectionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
acceptVpcEndpointConnectionsResponse_httpStatus :: Lens.Lens' AcceptVpcEndpointConnectionsResponse Core.Int
acceptVpcEndpointConnectionsResponse_httpStatus = Lens.lens (\AcceptVpcEndpointConnectionsResponse' {httpStatus} -> httpStatus) (\s@AcceptVpcEndpointConnectionsResponse' {} a -> s {httpStatus = a} :: AcceptVpcEndpointConnectionsResponse)

instance
  Core.NFData
    AcceptVpcEndpointConnectionsResponse
