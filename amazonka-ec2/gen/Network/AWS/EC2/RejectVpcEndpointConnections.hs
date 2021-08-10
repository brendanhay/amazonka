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
-- Module      : Network.AWS.EC2.RejectVpcEndpointConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Rejects one or more VPC endpoint connection requests to your VPC
-- endpoint service.
module Network.AWS.EC2.RejectVpcEndpointConnections
  ( -- * Creating a Request
    RejectVpcEndpointConnections (..),
    newRejectVpcEndpointConnections,

    -- * Request Lenses
    rejectVpcEndpointConnections_dryRun,
    rejectVpcEndpointConnections_serviceId,
    rejectVpcEndpointConnections_vpcEndpointIds,

    -- * Destructuring the Response
    RejectVpcEndpointConnectionsResponse (..),
    newRejectVpcEndpointConnectionsResponse,

    -- * Response Lenses
    rejectVpcEndpointConnectionsResponse_unsuccessful,
    rejectVpcEndpointConnectionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Prelude
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | /See:/ 'newRejectVpcEndpointConnections' smart constructor.
data RejectVpcEndpointConnections = RejectVpcEndpointConnections'
  { -- | Checks whether you have the required permissions for the action, without
    -- actually making the request, and provides an error response. If you have
    -- the required permissions, the error response is @DryRunOperation@.
    -- Otherwise, it is @UnauthorizedOperation@.
    dryRun :: Prelude.Maybe Prelude.Bool,
    -- | The ID of the service.
    serviceId :: Prelude.Text,
    -- | The IDs of one or more VPC endpoints.
    vpcEndpointIds :: [Prelude.Text]
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectVpcEndpointConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'dryRun', 'rejectVpcEndpointConnections_dryRun' - Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
--
-- 'serviceId', 'rejectVpcEndpointConnections_serviceId' - The ID of the service.
--
-- 'vpcEndpointIds', 'rejectVpcEndpointConnections_vpcEndpointIds' - The IDs of one or more VPC endpoints.
newRejectVpcEndpointConnections ::
  -- | 'serviceId'
  Prelude.Text ->
  RejectVpcEndpointConnections
newRejectVpcEndpointConnections pServiceId_ =
  RejectVpcEndpointConnections'
    { dryRun =
        Prelude.Nothing,
      serviceId = pServiceId_,
      vpcEndpointIds = Prelude.mempty
    }

-- | Checks whether you have the required permissions for the action, without
-- actually making the request, and provides an error response. If you have
-- the required permissions, the error response is @DryRunOperation@.
-- Otherwise, it is @UnauthorizedOperation@.
rejectVpcEndpointConnections_dryRun :: Lens.Lens' RejectVpcEndpointConnections (Prelude.Maybe Prelude.Bool)
rejectVpcEndpointConnections_dryRun = Lens.lens (\RejectVpcEndpointConnections' {dryRun} -> dryRun) (\s@RejectVpcEndpointConnections' {} a -> s {dryRun = a} :: RejectVpcEndpointConnections)

-- | The ID of the service.
rejectVpcEndpointConnections_serviceId :: Lens.Lens' RejectVpcEndpointConnections Prelude.Text
rejectVpcEndpointConnections_serviceId = Lens.lens (\RejectVpcEndpointConnections' {serviceId} -> serviceId) (\s@RejectVpcEndpointConnections' {} a -> s {serviceId = a} :: RejectVpcEndpointConnections)

-- | The IDs of one or more VPC endpoints.
rejectVpcEndpointConnections_vpcEndpointIds :: Lens.Lens' RejectVpcEndpointConnections [Prelude.Text]
rejectVpcEndpointConnections_vpcEndpointIds = Lens.lens (\RejectVpcEndpointConnections' {vpcEndpointIds} -> vpcEndpointIds) (\s@RejectVpcEndpointConnections' {} a -> s {vpcEndpointIds = a} :: RejectVpcEndpointConnections) Prelude.. Lens._Coerce

instance Core.AWSRequest RejectVpcEndpointConnections where
  type
    AWSResponse RejectVpcEndpointConnections =
      RejectVpcEndpointConnectionsResponse
  request = Request.postQuery defaultService
  response =
    Response.receiveXML
      ( \s h x ->
          RejectVpcEndpointConnectionsResponse'
            Prelude.<$> ( x Core..@? "unsuccessful" Core..!@ Prelude.mempty
                            Prelude.>>= Core.may (Core.parseXMLList "item")
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    RejectVpcEndpointConnections

instance Prelude.NFData RejectVpcEndpointConnections

instance Core.ToHeaders RejectVpcEndpointConnections where
  toHeaders = Prelude.const Prelude.mempty

instance Core.ToPath RejectVpcEndpointConnections where
  toPath = Prelude.const "/"

instance Core.ToQuery RejectVpcEndpointConnections where
  toQuery RejectVpcEndpointConnections' {..} =
    Prelude.mconcat
      [ "Action"
          Core.=: ( "RejectVpcEndpointConnections" ::
                      Prelude.ByteString
                  ),
        "Version"
          Core.=: ("2016-11-15" :: Prelude.ByteString),
        "DryRun" Core.=: dryRun,
        "ServiceId" Core.=: serviceId,
        Core.toQueryList "VpcEndpointId" vpcEndpointIds
      ]

-- | /See:/ 'newRejectVpcEndpointConnectionsResponse' smart constructor.
data RejectVpcEndpointConnectionsResponse = RejectVpcEndpointConnectionsResponse'
  { -- | Information about the endpoints that were not rejected, if applicable.
    unsuccessful :: Prelude.Maybe [UnsuccessfulItem],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'RejectVpcEndpointConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'unsuccessful', 'rejectVpcEndpointConnectionsResponse_unsuccessful' - Information about the endpoints that were not rejected, if applicable.
--
-- 'httpStatus', 'rejectVpcEndpointConnectionsResponse_httpStatus' - The response's http status code.
newRejectVpcEndpointConnectionsResponse ::
  -- | 'httpStatus'
  Prelude.Int ->
  RejectVpcEndpointConnectionsResponse
newRejectVpcEndpointConnectionsResponse pHttpStatus_ =
  RejectVpcEndpointConnectionsResponse'
    { unsuccessful =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | Information about the endpoints that were not rejected, if applicable.
rejectVpcEndpointConnectionsResponse_unsuccessful :: Lens.Lens' RejectVpcEndpointConnectionsResponse (Prelude.Maybe [UnsuccessfulItem])
rejectVpcEndpointConnectionsResponse_unsuccessful = Lens.lens (\RejectVpcEndpointConnectionsResponse' {unsuccessful} -> unsuccessful) (\s@RejectVpcEndpointConnectionsResponse' {} a -> s {unsuccessful = a} :: RejectVpcEndpointConnectionsResponse) Prelude.. Lens.mapping Lens._Coerce

-- | The response's http status code.
rejectVpcEndpointConnectionsResponse_httpStatus :: Lens.Lens' RejectVpcEndpointConnectionsResponse Prelude.Int
rejectVpcEndpointConnectionsResponse_httpStatus = Lens.lens (\RejectVpcEndpointConnectionsResponse' {httpStatus} -> httpStatus) (\s@RejectVpcEndpointConnectionsResponse' {} a -> s {httpStatus = a} :: RejectVpcEndpointConnectionsResponse)

instance
  Prelude.NFData
    RejectVpcEndpointConnectionsResponse
