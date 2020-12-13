{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteTransitGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified transit gateway.
module Network.AWS.EC2.DeleteTransitGateway
  ( -- * Creating a request
    DeleteTransitGateway (..),
    mkDeleteTransitGateway,

    -- ** Request lenses
    dtgTransitGatewayId,
    dtgDryRun,

    -- * Destructuring the response
    DeleteTransitGatewayResponse (..),
    mkDeleteTransitGatewayResponse,

    -- ** Response lenses
    dtgfrsTransitGateway,
    dtgfrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteTransitGateway' smart constructor.
data DeleteTransitGateway = DeleteTransitGateway'
  { -- | The ID of the transit gateway.
    transitGatewayId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTransitGateway' with the minimum fields required to make a request.
--
-- * 'transitGatewayId' - The ID of the transit gateway.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteTransitGateway ::
  -- | 'transitGatewayId'
  Lude.Text ->
  DeleteTransitGateway
mkDeleteTransitGateway pTransitGatewayId_ =
  DeleteTransitGateway'
    { transitGatewayId = pTransitGatewayId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the transit gateway.
--
-- /Note:/ Consider using 'transitGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgTransitGatewayId :: Lens.Lens' DeleteTransitGateway Lude.Text
dtgTransitGatewayId = Lens.lens (transitGatewayId :: DeleteTransitGateway -> Lude.Text) (\s a -> s {transitGatewayId = a} :: DeleteTransitGateway)
{-# DEPRECATED dtgTransitGatewayId "Use generic-lens or generic-optics with 'transitGatewayId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgDryRun :: Lens.Lens' DeleteTransitGateway (Lude.Maybe Lude.Bool)
dtgDryRun = Lens.lens (dryRun :: DeleteTransitGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteTransitGateway)
{-# DEPRECATED dtgDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteTransitGateway where
  type Rs DeleteTransitGateway = DeleteTransitGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteTransitGatewayResponse'
            Lude.<$> (x Lude..@? "transitGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteTransitGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteTransitGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteTransitGateway where
  toQuery DeleteTransitGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteTransitGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "TransitGatewayId" Lude.=: transitGatewayId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteTransitGatewayResponse' smart constructor.
data DeleteTransitGatewayResponse = DeleteTransitGatewayResponse'
  { -- | Information about the deleted transit gateway.
    transitGateway :: Lude.Maybe TransitGateway,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteTransitGatewayResponse' with the minimum fields required to make a request.
--
-- * 'transitGateway' - Information about the deleted transit gateway.
-- * 'responseStatus' - The response status code.
mkDeleteTransitGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteTransitGatewayResponse
mkDeleteTransitGatewayResponse pResponseStatus_ =
  DeleteTransitGatewayResponse'
    { transitGateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deleted transit gateway.
--
-- /Note:/ Consider using 'transitGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgfrsTransitGateway :: Lens.Lens' DeleteTransitGatewayResponse (Lude.Maybe TransitGateway)
dtgfrsTransitGateway = Lens.lens (transitGateway :: DeleteTransitGatewayResponse -> Lude.Maybe TransitGateway) (\s a -> s {transitGateway = a} :: DeleteTransitGatewayResponse)
{-# DEPRECATED dtgfrsTransitGateway "Use generic-lens or generic-optics with 'transitGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dtgfrsResponseStatus :: Lens.Lens' DeleteTransitGatewayResponse Lude.Int
dtgfrsResponseStatus = Lens.lens (responseStatus :: DeleteTransitGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteTransitGatewayResponse)
{-# DEPRECATED dtgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
