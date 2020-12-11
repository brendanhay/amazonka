{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteVPCPeeringConnection
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a VPC peering connection. Either the owner of the requester VPC or the owner of the accepter VPC can delete the VPC peering connection if it's in the @active@ state. The owner of the requester VPC can delete a VPC peering connection in the @pending-acceptance@ state. You cannot delete a VPC peering connection that's in the @failed@ state.
module Network.AWS.EC2.DeleteVPCPeeringConnection
  ( -- * Creating a request
    DeleteVPCPeeringConnection (..),
    mkDeleteVPCPeeringConnection,

    -- ** Request lenses
    dvpcDryRun,
    dvpcVPCPeeringConnectionId,

    -- * Destructuring the response
    DeleteVPCPeeringConnectionResponse (..),
    mkDeleteVPCPeeringConnectionResponse,

    -- ** Response lenses
    dvpcrsReturn,
    dvpcrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteVPCPeeringConnection' smart constructor.
data DeleteVPCPeeringConnection = DeleteVPCPeeringConnection'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    vpcPeeringConnectionId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCPeeringConnection' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'vpcPeeringConnectionId' - The ID of the VPC peering connection.
mkDeleteVPCPeeringConnection ::
  -- | 'vpcPeeringConnectionId'
  Lude.Text ->
  DeleteVPCPeeringConnection
mkDeleteVPCPeeringConnection pVPCPeeringConnectionId_ =
  DeleteVPCPeeringConnection'
    { dryRun = Lude.Nothing,
      vpcPeeringConnectionId = pVPCPeeringConnectionId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcDryRun :: Lens.Lens' DeleteVPCPeeringConnection (Lude.Maybe Lude.Bool)
dvpcDryRun = Lens.lens (dryRun :: DeleteVPCPeeringConnection -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteVPCPeeringConnection)
{-# DEPRECATED dvpcDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the VPC peering connection.
--
-- /Note:/ Consider using 'vpcPeeringConnectionId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcVPCPeeringConnectionId :: Lens.Lens' DeleteVPCPeeringConnection Lude.Text
dvpcVPCPeeringConnectionId = Lens.lens (vpcPeeringConnectionId :: DeleteVPCPeeringConnection -> Lude.Text) (\s a -> s {vpcPeeringConnectionId = a} :: DeleteVPCPeeringConnection)
{-# DEPRECATED dvpcVPCPeeringConnectionId "Use generic-lens or generic-optics with 'vpcPeeringConnectionId' instead." #-}

instance Lude.AWSRequest DeleteVPCPeeringConnection where
  type
    Rs DeleteVPCPeeringConnection =
      DeleteVPCPeeringConnectionResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteVPCPeeringConnectionResponse'
            Lude.<$> (x Lude..@? "return") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteVPCPeeringConnection where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteVPCPeeringConnection where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteVPCPeeringConnection where
  toQuery DeleteVPCPeeringConnection' {..} =
    Lude.mconcat
      [ "Action"
          Lude.=: ("DeleteVpcPeeringConnection" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "VpcPeeringConnectionId" Lude.=: vpcPeeringConnectionId
      ]

-- | /See:/ 'mkDeleteVPCPeeringConnectionResponse' smart constructor.
data DeleteVPCPeeringConnectionResponse = DeleteVPCPeeringConnectionResponse'
  { return ::
      Lude.Maybe Lude.Bool,
    responseStatus ::
      Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteVPCPeeringConnectionResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'return' - Returns @true@ if the request succeeds; otherwise, it returns an error.
mkDeleteVPCPeeringConnectionResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteVPCPeeringConnectionResponse
mkDeleteVPCPeeringConnectionResponse pResponseStatus_ =
  DeleteVPCPeeringConnectionResponse'
    { return = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Returns @true@ if the request succeeds; otherwise, it returns an error.
--
-- /Note:/ Consider using 'return' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrsReturn :: Lens.Lens' DeleteVPCPeeringConnectionResponse (Lude.Maybe Lude.Bool)
dvpcrsReturn = Lens.lens (return :: DeleteVPCPeeringConnectionResponse -> Lude.Maybe Lude.Bool) (\s a -> s {return = a} :: DeleteVPCPeeringConnectionResponse)
{-# DEPRECATED dvpcrsReturn "Use generic-lens or generic-optics with 'return' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dvpcrsResponseStatus :: Lens.Lens' DeleteVPCPeeringConnectionResponse Lude.Int
dvpcrsResponseStatus = Lens.lens (responseStatus :: DeleteVPCPeeringConnectionResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteVPCPeeringConnectionResponse)
{-# DEPRECATED dvpcrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
