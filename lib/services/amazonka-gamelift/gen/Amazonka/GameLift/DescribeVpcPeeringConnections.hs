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
-- Module      : Amazonka.GameLift.DescribeVpcPeeringConnections
-- Copyright   : (c) 2013-2022 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information on VPC peering connections. Use this operation to
-- get peering information for all fleets or for one specific fleet ID.
--
-- To retrieve connection information, call this operation from the Amazon
-- Web Services account that is used to manage the Amazon GameLift fleets.
-- Specify a fleet ID or leave the parameter empty to retrieve all
-- connection records. If successful, the retrieved information includes
-- both active and pending connections. Active connections identify the
-- IpV4 CIDR block that the VPC uses to connect.
--
-- __Related actions__
--
-- CreateVpcPeeringAuthorization | DescribeVpcPeeringAuthorizations |
-- DeleteVpcPeeringAuthorization | CreateVpcPeeringConnection |
-- DescribeVpcPeeringConnections | DeleteVpcPeeringConnection |
-- <https://docs.aws.amazon.com/gamelift/latest/developerguide/reference-awssdk.html#reference-awssdk-resources-fleets All APIs by task>
module Amazonka.GameLift.DescribeVpcPeeringConnections
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

import qualified Amazonka.Core as Core
import qualified Amazonka.Core.Lens.Internal as Lens
import Amazonka.GameLift.Types
import qualified Amazonka.Prelude as Prelude
import qualified Amazonka.Request as Request
import qualified Amazonka.Response as Response

-- | Represents the input for a request operation.
--
-- /See:/ 'newDescribeVpcPeeringConnections' smart constructor.
data DescribeVpcPeeringConnections = DescribeVpcPeeringConnections'
  { -- | A unique identifier for the fleet. You can use either the fleet ID or
    -- ARN value.
    fleetId :: Prelude.Maybe Prelude.Text
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

-- |
-- Create a value of 'DescribeVpcPeeringConnections' with all optional fields omitted.
--
-- Use <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/optics optics> to modify other optional fields.
--
-- The following record fields are available, with the corresponding lenses provided
-- for backwards compatibility:
--
-- 'fleetId', 'describeVpcPeeringConnections_fleetId' - A unique identifier for the fleet. You can use either the fleet ID or
-- ARN value.
newDescribeVpcPeeringConnections ::
  DescribeVpcPeeringConnections
newDescribeVpcPeeringConnections =
  DescribeVpcPeeringConnections'
    { fleetId =
        Prelude.Nothing
    }

-- | A unique identifier for the fleet. You can use either the fleet ID or
-- ARN value.
describeVpcPeeringConnections_fleetId :: Lens.Lens' DescribeVpcPeeringConnections (Prelude.Maybe Prelude.Text)
describeVpcPeeringConnections_fleetId = Lens.lens (\DescribeVpcPeeringConnections' {fleetId} -> fleetId) (\s@DescribeVpcPeeringConnections' {} a -> s {fleetId = a} :: DescribeVpcPeeringConnections)

instance
  Core.AWSRequest
    DescribeVpcPeeringConnections
  where
  type
    AWSResponse DescribeVpcPeeringConnections =
      DescribeVpcPeeringConnectionsResponse
  request overrides =
    Request.postJSON (overrides defaultService)
  response =
    Response.receiveJSON
      ( \s h x ->
          DescribeVpcPeeringConnectionsResponse'
            Prelude.<$> ( x Core..?> "VpcPeeringConnections"
                            Core..!@ Prelude.mempty
                        )
            Prelude.<*> (Prelude.pure (Prelude.fromEnum s))
      )

instance
  Prelude.Hashable
    DescribeVpcPeeringConnections
  where
  hashWithSalt _salt DescribeVpcPeeringConnections' {..} =
    _salt `Prelude.hashWithSalt` fleetId

instance Prelude.NFData DescribeVpcPeeringConnections where
  rnf DescribeVpcPeeringConnections' {..} =
    Prelude.rnf fleetId

instance Core.ToHeaders DescribeVpcPeeringConnections where
  toHeaders =
    Prelude.const
      ( Prelude.mconcat
          [ "X-Amz-Target"
              Core.=# ( "GameLift.DescribeVpcPeeringConnections" ::
                          Prelude.ByteString
                      ),
            "Content-Type"
              Core.=# ( "application/x-amz-json-1.1" ::
                          Prelude.ByteString
                      )
          ]
      )

instance Core.ToJSON DescribeVpcPeeringConnections where
  toJSON DescribeVpcPeeringConnections' {..} =
    Core.object
      ( Prelude.catMaybes
          [("FleetId" Core..=) Prelude.<$> fleetId]
      )

instance Core.ToPath DescribeVpcPeeringConnections where
  toPath = Prelude.const "/"

instance Core.ToQuery DescribeVpcPeeringConnections where
  toQuery = Prelude.const Prelude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'newDescribeVpcPeeringConnectionsResponse' smart constructor.
data DescribeVpcPeeringConnectionsResponse = DescribeVpcPeeringConnectionsResponse'
  { -- | A collection of VPC peering connection records that match the request.
    vpcPeeringConnections :: Prelude.Maybe [VpcPeeringConnection],
    -- | The response's http status code.
    httpStatus :: Prelude.Int
  }
  deriving (Prelude.Eq, Prelude.Read, Prelude.Show, Prelude.Generic)

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
  Prelude.Int ->
  DescribeVpcPeeringConnectionsResponse
newDescribeVpcPeeringConnectionsResponse pHttpStatus_ =
  DescribeVpcPeeringConnectionsResponse'
    { vpcPeeringConnections =
        Prelude.Nothing,
      httpStatus = pHttpStatus_
    }

-- | A collection of VPC peering connection records that match the request.
describeVpcPeeringConnectionsResponse_vpcPeeringConnections :: Lens.Lens' DescribeVpcPeeringConnectionsResponse (Prelude.Maybe [VpcPeeringConnection])
describeVpcPeeringConnectionsResponse_vpcPeeringConnections = Lens.lens (\DescribeVpcPeeringConnectionsResponse' {vpcPeeringConnections} -> vpcPeeringConnections) (\s@DescribeVpcPeeringConnectionsResponse' {} a -> s {vpcPeeringConnections = a} :: DescribeVpcPeeringConnectionsResponse) Prelude.. Lens.mapping Lens.coerced

-- | The response's http status code.
describeVpcPeeringConnectionsResponse_httpStatus :: Lens.Lens' DescribeVpcPeeringConnectionsResponse Prelude.Int
describeVpcPeeringConnectionsResponse_httpStatus = Lens.lens (\DescribeVpcPeeringConnectionsResponse' {httpStatus} -> httpStatus) (\s@DescribeVpcPeeringConnectionsResponse' {} a -> s {httpStatus = a} :: DescribeVpcPeeringConnectionsResponse)

instance
  Prelude.NFData
    DescribeVpcPeeringConnectionsResponse
  where
  rnf DescribeVpcPeeringConnectionsResponse' {..} =
    Prelude.rnf vpcPeeringConnections
      `Prelude.seq` Prelude.rnf httpStatus
