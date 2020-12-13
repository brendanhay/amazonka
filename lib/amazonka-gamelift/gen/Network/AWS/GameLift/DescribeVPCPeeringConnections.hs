{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DescribeVPCPeeringConnections
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Retrieves information on VPC peering connections. Use this operation to get peering information for all fleets or for one specific fleet ID.
--
-- To retrieve connection information, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Specify a fleet ID or leave the parameter empty to retrieve all connection records. If successful, the retrieved information includes both active and pending connections. Active connections identify the IpV4 CIDR block that the VPC uses to connect.
--
--     * 'CreateVpcPeeringAuthorization'
--
--
--     * 'DescribeVpcPeeringAuthorizations'
--
--
--     * 'DeleteVpcPeeringAuthorization'
--
--
--     * 'CreateVpcPeeringConnection'
--
--
--     * 'DescribeVpcPeeringConnections'
--
--
--     * 'DeleteVpcPeeringConnection'
module Network.AWS.GameLift.DescribeVPCPeeringConnections
  ( -- * Creating a request
    DescribeVPCPeeringConnections (..),
    mkDescribeVPCPeeringConnections,

    -- ** Request lenses
    dvpcpcFleetId,

    -- * Destructuring the response
    DescribeVPCPeeringConnectionsResponse (..),
    mkDescribeVPCPeeringConnectionsResponse,

    -- ** Response lenses
    dvpcrsVPCPeeringConnections,
    dvpcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDescribeVPCPeeringConnections' smart constructor.
newtype DescribeVPCPeeringConnections = DescribeVPCPeeringConnections'
  { -- | A unique identifier for a fleet. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCPeeringConnections' with the minimum fields required to make a request.
--
-- * 'fleetId' - A unique identifier for a fleet. You can use either the fleet ID or ARN value.
mkDescribeVPCPeeringConnections ::
  DescribeVPCPeeringConnections
mkDescribeVPCPeeringConnections =
  DescribeVPCPeeringConnections' {fleetId = Lude.Nothing}

-- | A unique identifier for a fleet. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcpcFleetId :: Lens.Lens' DescribeVPCPeeringConnections (Lude.Maybe Lude.Text)
dvpcpcFleetId = Lens.lens (fleetId :: DescribeVPCPeeringConnections -> Lude.Maybe Lude.Text) (\s a -> s {fleetId = a} :: DescribeVPCPeeringConnections)
{-# DEPRECATED dvpcpcFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest DescribeVPCPeeringConnections where
  type
    Rs DescribeVPCPeeringConnections =
      DescribeVPCPeeringConnectionsResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveJSON
      ( \s h x ->
          DescribeVPCPeeringConnectionsResponse'
            Lude.<$> (x Lude..?> "VpcPeeringConnections" Lude..!@ Lude.mempty)
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DescribeVPCPeeringConnections where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DescribeVpcPeeringConnections" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DescribeVPCPeeringConnections where
  toJSON DescribeVPCPeeringConnections' {..} =
    Lude.object
      (Lude.catMaybes [("FleetId" Lude..=) Lude.<$> fleetId])

instance Lude.ToPath DescribeVPCPeeringConnections where
  toPath = Lude.const "/"

instance Lude.ToQuery DescribeVPCPeeringConnections where
  toQuery = Lude.const Lude.mempty

-- | Represents the returned data in response to a request operation.
--
-- /See:/ 'mkDescribeVPCPeeringConnectionsResponse' smart constructor.
data DescribeVPCPeeringConnectionsResponse = DescribeVPCPeeringConnectionsResponse'
  { -- | A collection of VPC peering connection records that match the request.
    vpcPeeringConnections :: Lude.Maybe [VPCPeeringConnection],
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DescribeVPCPeeringConnectionsResponse' with the minimum fields required to make a request.
--
-- * 'vpcPeeringConnections' - A collection of VPC peering connection records that match the request.
-- * 'responseStatus' - The response status code.
mkDescribeVPCPeeringConnectionsResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DescribeVPCPeeringConnectionsResponse
mkDescribeVPCPeeringConnectionsResponse pResponseStatus_ =
  DescribeVPCPeeringConnectionsResponse'
    { vpcPeeringConnections =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | A collection of VPC peering connection records that match the request.
--
-- /Note:/ Consider using 'vpcPeeringConnections' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrsVPCPeeringConnections :: Lens.Lens' DescribeVPCPeeringConnectionsResponse (Lude.Maybe [VPCPeeringConnection])
dvpcrsVPCPeeringConnections = Lens.lens (vpcPeeringConnections :: DescribeVPCPeeringConnectionsResponse -> Lude.Maybe [VPCPeeringConnection]) (\s a -> s {vpcPeeringConnections = a} :: DescribeVPCPeeringConnectionsResponse)
{-# DEPRECATED dvpcrsVPCPeeringConnections "Use generic-lens or generic-optics with 'vpcPeeringConnections' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrsResponseStatus :: Lens.Lens' DescribeVPCPeeringConnectionsResponse Lude.Int
dvpcrsResponseStatus = Lens.lens (responseStatus :: DescribeVPCPeeringConnectionsResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DescribeVPCPeeringConnectionsResponse)
{-# DEPRECATED dvpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
