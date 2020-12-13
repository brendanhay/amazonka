{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.GameLift.DeleteVPCPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Removes a VPC peering connection. To delete the connection, you must have a valid authorization for the VPC peering connection that you want to delete. You can check for an authorization by calling 'DescribeVpcPeeringAuthorizations' or request a new one using 'CreateVpcPeeringAuthorization' .
--
-- Once a valid authorization exists, call this operation from the AWS account that is used to manage the Amazon GameLift fleets. Identify the connection to delete by the connection ID and fleet ID. If successful, the connection is removed.
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
module Network.AWS.GameLift.DeleteVPCPeeringConnection
  ( -- * Creating a request
    DeleteVPCPeeringConnection (..),
    mkDeleteVPCPeeringConnection,

    -- ** Request lenses
    dvpcVPCPeeringConnectionId,
    dvpcFleetId,

    -- * Destructuring the response
    DeleteVPCPeeringConnectionResponse (..),
    mkDeleteVPCPeeringConnectionResponse,

    -- ** Response lenses
    dvpcpcrsResponseStatus,
  )
where

import Network.AWS.GameLift.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | Represents the input for a request operation.
--
-- /See:/ 'mkDeleteVPCPeeringConnection' smart constructor.
data DeleteVPCPeeringConnection = DeleteVPCPeeringConnection'
  { -- | A unique identifier for a VPC peering connection. This value is included in the 'VpcPeeringConnection' object, which can be retrieved by calling 'DescribeVpcPeeringConnections' .
    vpcPeeringConnectionId :: Lude.Text,
    -- | A unique identifier for a fleet. This fleet specified must match the fleet referenced in the VPC peering connection record. You can use either the fleet ID or ARN value.
    fleetId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCPeeringConnection' with the minimum fields required to make a request.
--
-- * 'vpcPeeringConnectionId' - A unique identifier for a VPC peering connection. This value is included in the 'VpcPeeringConnection' object, which can be retrieved by calling 'DescribeVpcPeeringConnections' .
-- * 'fleetId' - A unique identifier for a fleet. This fleet specified must match the fleet referenced in the VPC peering connection record. You can use either the fleet ID or ARN value.
mkDeleteVPCPeeringConnection ::
  -- | 'vpcPeeringConnectionId'
  Lude.Text ->
  -- | 'fleetId'
  Lude.Text ->
  DeleteVPCPeeringConnection
mkDeleteVPCPeeringConnection pVPCPeeringConnectionId_ pFleetId_ =
  DeleteVPCPeeringConnection'
    { vpcPeeringConnectionId =
        pVPCPeeringConnectionId_,
      fleetId = pFleetId_
    }

-- | A unique identifier for a VPC peering connection. This value is included in the 'VpcPeeringConnection' object, which can be retrieved by calling 'DescribeVpcPeeringConnections' .
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcVPCPeeringConnectionId :: Lens.Lens' DeleteVPCPeeringConnection Lude.Text
dvpcVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: DeleteVPCPeeringConnection -> Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: DeleteVPCPeeringConnection)
{-# DEPRECATED dvpcVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

-- | A unique identifier for a fleet. This fleet specified must match the fleet referenced in the VPC peering connection record. You can use either the fleet ID or ARN value.
--
-- /Note:/ Consider using 'fleetId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcFleetId :: Lens.Lens' DeleteVPCPeeringConnection Lude.Text
dvpcFleetId = Lens.lens (fleetId :: DeleteVPCPeeringConnection -> Lude.Text) (\s a -> s {fleetId = a} :: DeleteVPCPeeringConnection)
{-# DEPRECATED dvpcFleetId "Use generic-lens or generic-optics with 'fleetId' instead." #-}

instance Lude.AWSRequest DeleteVPCPeeringConnection where
  type
    Rs DeleteVPCPeeringConnection =
      DeleteVPCPeeringConnectionResponse
  request = Req.postJSON gameLiftService
  response =
    Res.receiveEmpty
      ( \s h x ->
          DeleteVPCPeeringConnectionResponse'
            Lude.<$> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteVPCPeeringConnection where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("GameLift.DeleteVpcPeeringConnection" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteVPCPeeringConnection where
  toJSON DeleteVPCPeeringConnection' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("VpcPeeringConnectionId" Lude..= vpcPeeringConnectionId),
            Lude.Just ("FleetId" Lude..= fleetId)
          ]
      )

instance Lude.ToPath DeleteVPCPeeringConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPCPeeringConnection where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteVPCPeeringConnectionResponse' smart constructor.
newtype DeleteVPCPeeringConnectionResponse = DeleteVPCPeeringConnectionResponse'
  { -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
mkDeleteVPCPeeringConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteVPCPeeringConnectionResponse
mkDeleteVPCPeeringConnectionResponse pResponseStatus_ =
  DeleteVPCPeeringConnectionResponse'
    { responseStatus =
        pResponseStatus_
    }

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcpcrsResponseStatus :: Lens.Lens' DeleteVPCPeeringConnectionResponse Lude.Int
dvpcpcrsResponseStatus = Lens.lens (responseStatus :: DeleteVPCPeeringConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVPCPeeringConnectionResponse)
{-# DEPRECATED dvpcpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
