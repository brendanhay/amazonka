{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteNatGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified NAT gateway. Deleting a NAT gateway disassociates its Elastic IP address, but does not release the address from your account. Deleting a NAT gateway does not delete any NAT gateway routes in your route tables.
module Network.AWS.EC2.DeleteNatGateway
  ( -- * Creating a request
    DeleteNatGateway (..),
    mkDeleteNatGateway,

    -- ** Request lenses
    dngnDryRun,
    dngnNatGatewayId,

    -- * Destructuring the response
    DeleteNatGatewayResponse (..),
    mkDeleteNatGatewayResponse,

    -- ** Response lenses
    dngnrsNatGatewayId,
    dngnrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteNatGateway' smart constructor.
data DeleteNatGateway = DeleteNatGateway'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    natGatewayId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNatGateway' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'natGatewayId' - The ID of the NAT gateway.
mkDeleteNatGateway ::
  -- | 'natGatewayId'
  Lude.Text ->
  DeleteNatGateway
mkDeleteNatGateway pNatGatewayId_ =
  DeleteNatGateway'
    { dryRun = Lude.Nothing,
      natGatewayId = pNatGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngnDryRun :: Lens.Lens' DeleteNatGateway (Lude.Maybe Lude.Bool)
dngnDryRun = Lens.lens (dryRun :: DeleteNatGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteNatGateway)
{-# DEPRECATED dngnDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngnNatGatewayId :: Lens.Lens' DeleteNatGateway Lude.Text
dngnNatGatewayId = Lens.lens (natGatewayId :: DeleteNatGateway -> Lude.Text) (\s a -> s {natGatewayId = a} :: DeleteNatGateway)
{-# DEPRECATED dngnNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

instance Lude.AWSRequest DeleteNatGateway where
  type Rs DeleteNatGateway = DeleteNatGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteNatGatewayResponse'
            Lude.<$> (x Lude..@? "natGatewayId") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteNatGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteNatGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteNatGateway where
  toQuery DeleteNatGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteNatGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "NatGatewayId" Lude.=: natGatewayId
      ]

-- | /See:/ 'mkDeleteNatGatewayResponse' smart constructor.
data DeleteNatGatewayResponse = DeleteNatGatewayResponse'
  { natGatewayId ::
      Lude.Maybe Lude.Text,
    responseStatus :: Lude.Int
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteNatGatewayResponse' with the minimum fields required to make a request.
--
-- * 'natGatewayId' - The ID of the NAT gateway.
-- * 'responseStatus' - The response status code.
mkDeleteNatGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteNatGatewayResponse
mkDeleteNatGatewayResponse pResponseStatus_ =
  DeleteNatGatewayResponse'
    { natGatewayId = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The ID of the NAT gateway.
--
-- /Note:/ Consider using 'natGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngnrsNatGatewayId :: Lens.Lens' DeleteNatGatewayResponse (Lude.Maybe Lude.Text)
dngnrsNatGatewayId = Lens.lens (natGatewayId :: DeleteNatGatewayResponse -> Lude.Maybe Lude.Text) (\s a -> s {natGatewayId = a} :: DeleteNatGatewayResponse)
{-# DEPRECATED dngnrsNatGatewayId "Use generic-lens or generic-optics with 'natGatewayId' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dngnrsResponseStatus :: Lens.Lens' DeleteNatGatewayResponse Lude.Int
dngnrsResponseStatus = Lens.lens (responseStatus :: DeleteNatGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteNatGatewayResponse)
{-# DEPRECATED dngnrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
