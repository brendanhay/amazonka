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
-- Module      : Network.AWS.GameLift.DescribeVpcPeeringConnections
-- Copyright   : (c) 2013-2021 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information on VPC peering connections. Use this operation to
-- get peering information for all fleets or for one specific fleet ID.
--
-- To retrieve connection information, call this operation from the AWS
-- account that is used to manage the Amazon GameLift fleets. Specify a
-- fleet ID or leave the parameter empty to retrieve all connection
-- records. If successful, the retrieved information includes both active
-- and pending connections. Active connections identify the IpV4 CIDR block
-- that the VPC uses to connect.
--
-- -   CreateVpcPeeringAuthorization
--
-- -   DescribeVpcPeeringAuthorizations
--
-- -   DeleteVpcPeeringAuthorization
--
-- -   CreateVpcPeeringConnection
--
-- -   DescribeVpcPeeringConnections
--
-- -   DeleteVpcPeeringConnection
module Network.AWS.GameLift.DescribeVpcPeeringConnections
  ( -- * Creating a Request
    DescribeVpcPeeringConnections (..),
    newDescribeVpcPeeringConnections,

    -- * Request Lenses
    describeVpcPeeringConnections_fleetId,

    -- * Destructuring the Response
    DescribeVpcPeeringConnectionsResponse (..),
    newDescribeVpcPeeringConnectionsResponse,

    -- * Response Lenses
    describeVpcPeeringConnectionsResponse_vpcPeeringConnections,
    describeVpcPeeringConnectionsResponse_httpStatus,
  )
where

import qualified Network.AWS.Core as Core
import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Request as Request
import qualified Network.AWS.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeVpcPeeringConnections' smart constructor.
data DescribeVpcPeeringConnections = DescribeVpcPeeringConnections'
  { -- | A unique identifier for a fleet. You can use either the fleet ID or ARN
    -- value.
    fleetId :: Core.Maybe Core.Text
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcPeeringConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'describeVpcPeeringConnections_fleetId' - A unique identifier for a fleet. You can use either the fleet ID or ARN
-- value.
newDescribeVpcPeeringConnections ::
  DescribeVpcPeeringConnections
newDescribeVpcPeeringConnections =
  DescribeVpcPeeringConnections'
    { fleetId =
        Core.Nothing
    }

-- | A unique identifier for a fleet. You can use either the fleet ID or ARN
-- value.
describeVpcPeeringConnections_fleetId :: Lens.Lens' DescribeVpcPeeringConnections (Core.Maybe Core.Text)
describeVpcPeeringConnections_fleetId = Lens.lens (\DescribeVpcPeeringConnections' {fleetId} -> fleetId) (\s@DescribeVpcPeeringConnections' {} a -> s {fleetId = a} :: DescribeVpcPeeringConnections)

instance
  Core.AWSRequest
    DescribeVpcPeeringConnections
  where
  type
    AWSResponse DescribeVpcPeeringConnections =
      DescribeVpcPeeringConnectionsResponse
  request = Request.postJSON defaultService
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVpcPeeringConnectionsResponse'
            Core.<$> ( x Core..?> "VpcPeeringConnections"
                         Core..!@ Core.mempty
                     )
            Core.<*> (Core.pure (Core.fromEnum s))
      )

instance Core.Hashable DescribeVpcPeeringConnections

instance Core.NFData DescribeVpcPeeringConnections

instance Core.ToHeaders DescribeVpcPeeringConnections where
  toHeaders =
    Core.const
      ( Core.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeVpcPeeringConnections" ::
                          Core.ByteString
                      ),
            "Content-Type"
              Core.=# ("application/x-amz-json-1.1" :: Core.ByteString)
          ]
      )

instance Core.ToJSON DescribeVpcPeeringConnections where
  toJSON DescribeVpcPeeringConnections' {..} =
    Core.object
      ( Core.catMaybes
          [("FleetId" Core..=) Core.<$> fleetId]
      )

instance Core.ToPath DescribeVpcPeeringConnections where
  toPath = Core.const "/"

instance Core.ToQuery DescribeVpcPeeringConnections where
  toQuery = Core.const Core.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeVpcPeeringConnectionsResponse' smart constructor.
data DescribeVpcPeeringConnectionsResponse = DescribeVpcPeeringConnectionsResponse'
  { -- | A collection of VPC peering connection records that match the request.
    vpcPeeringConnections :: Core.Maybe [VpcPeeringConnection],
    -- | The response's http status code.
    httpStatus :: Core.Int
  }
  deriving (Core.Eq, Core.Read, Core.Show, Core.Generic)

-- |
-- Create a value of 'DescribeVpcPeeringConnectionsResponse' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'vpcPeeringConnections', 'describeVpcPeeringConnectionsResponse_vpcPeeringConnections' - A collection of VPC peering connection records that match the request.
--
-- 'httpStatus', 'describeVpcPeeringConnectionsResponse_httpStatus' - The response's http status code.
newDescribeVpcPeeringConnectionsResponse ::
  -- | 'httpStatus'
  Core.Int ->
  DescribeVpcPeeringConnectionsResponse
newDescribeVpcPeeringConnectionsResponse pHttpStatus_ =
  DescribeVpcPeeringConnectionsResponse'
    { vpcPeeringConnections =
        Core.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of VPC peering connection records that match the request.
describeVpcPeeringConnectionsResponse_vpcPeeringConnections :: Lens.Lens' DescribeVpcPeeringConnectionsResponse (Core.Maybe [VpcPeeringConnection])
describeVpcPeeringConnectionsResponse_vpcPeeringConnections = Lens.lens (\DescribeVpcPeeringConnectionsResponse' {vpcPeeringConnections} -> vpcPeeringConnections) (\s@DescribeVpcPeeringConnectionsResponse' {} a -> s {vpcPeeringConnections = a} :: DescribeVpcPeeringConnectionsResponse) Core.. Lens.mapping Lens._Coerce

-- | The response's http status code.
describeVpcPeeringConnectionsResponse_httpStatus :: Lens.Lens' DescribeVpcPeeringConnectionsResponse Core.Int
describeVpcPeeringConnectionsResponse_httpStatus = Lens.lens (\DescribeVpcPeeringConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcPeeringConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeVpcPeeringConnectionsResponse)

instance
  Core.NFData
    DescribeVpcPeeringConnectionsResponse
