{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteCarrierGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes a carrier gateway.
--
-- /Important:/ If you do not delete the route that contains the carrier gateway as the Target, the route is a blackhole route. For information about how to delete a route, see <https://docs.aws.amazon.com/AWSEC2/latest/APIReference/API_DeleteRoute.html DeleteRoute> .
module Network.AWS.EC2.DeleteCarrierGateway
  ( -- * Creating a request
    DeleteCarrierGateway (..),
    mkDeleteCarrierGateway,

    -- ** Request lenses
    dcgfDryRun,
    dcgfCarrierGatewayId,

    -- * Destructuring the response
    DeleteCarrierGatewayResponse (..),
    mkDeleteCarrierGatewayResponse,

    -- ** Response lenses
    dcgrsCarrierGateway,
    dcgrsResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteCarrierGateway' smart constructor.
data DeleteCarrierGateway = DeleteCarrierGateway'
  { -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool,
    -- | The ID of the carrier gateway.
    carrierGatewayId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCarrierGateway' with the minimum fields required to make a request.
--
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
-- * 'carrierGatewayId' - The ID of the carrier gateway.
mkDeleteCarrierGateway ::
  -- | 'carrierGatewayId'
  Lude.Text ->
  DeleteCarrierGateway
mkDeleteCarrierGateway pCarrierGatewayId_ =
  DeleteCarrierGateway'
    { dryRun = Lude.Nothing,
      carrierGatewayId = pCarrierGatewayId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgfDryRun :: Lens.Lens' DeleteCarrierGateway (Lude.Maybe Lude.Bool)
dcgfDryRun = Lens.lens (dryRun :: DeleteCarrierGateway -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteCarrierGateway)
{-# DEPRECATED dcgfDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the carrier gateway.
--
-- /Note:/ Consider using 'carrierGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgfCarrierGatewayId :: Lens.Lens' DeleteCarrierGateway Lude.Text
dcgfCarrierGatewayId = Lens.lens (carrierGatewayId :: DeleteCarrierGateway -> Lude.Text) (\s a -> s {carrierGatewayId = a} :: DeleteCarrierGateway)
{-# DEPRECATED dcgfCarrierGatewayId "Use generic-lens or generic-optics with 'carrierGatewayId' instead." #-}

instance Lude.AWSRequest DeleteCarrierGateway where
  type Rs DeleteCarrierGateway = DeleteCarrierGatewayResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteCarrierGatewayResponse'
            Lude.<$> (x Lude..@? "carrierGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteCarrierGateway where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteCarrierGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteCarrierGateway where
  toQuery DeleteCarrierGateway' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteCarrierGateway" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "DryRun" Lude.=: dryRun,
        "CarrierGatewayId" Lude.=: carrierGatewayId
      ]

-- | /See:/ 'mkDeleteCarrierGatewayResponse' smart constructor.
data DeleteCarrierGatewayResponse = DeleteCarrierGatewayResponse'
  { -- | Information about the carrier gateway.
    carrierGateway :: Lude.Maybe CarrierGateway,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteCarrierGatewayResponse' with the minimum fields required to make a request.
--
-- * 'carrierGateway' - Information about the carrier gateway.
-- * 'responseStatus' - The response status code.
mkDeleteCarrierGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteCarrierGatewayResponse
mkDeleteCarrierGatewayResponse pResponseStatus_ =
  DeleteCarrierGatewayResponse'
    { carrierGateway = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the carrier gateway.
--
-- /Note:/ Consider using 'carrierGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrsCarrierGateway :: Lens.Lens' DeleteCarrierGatewayResponse (Lude.Maybe CarrierGateway)
dcgrsCarrierGateway = Lens.lens (carrierGateway :: DeleteCarrierGatewayResponse -> Lude.Maybe CarrierGateway) (\s a -> s {carrierGateway = a} :: DeleteCarrierGatewayResponse)
{-# DEPRECATED dcgrsCarrierGateway "Use generic-lens or generic-optics with 'carrierGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcgrsResponseStatus :: Lens.Lens' DeleteCarrierGatewayResponse Lude.Int
dcgrsResponseStatus = Lens.lens (responseStatus :: DeleteCarrierGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteCarrierGatewayResponse)
{-# DEPRECATED dcgrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
