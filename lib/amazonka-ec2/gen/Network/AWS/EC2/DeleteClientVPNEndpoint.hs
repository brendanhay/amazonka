{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.EC2.DeleteClientVPNEndpoint
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Client VPN endpoint. You must disassociate all target networks before you can delete a Client VPN endpoint.
module Network.AWS.EC2.DeleteClientVPNEndpoint
  ( -- * Creating a request
    DeleteClientVPNEndpoint (..),
    mkDeleteClientVPNEndpoint,

    -- ** Request lenses
    dcveClientVPNEndpointId,
    dcveDryRun,

    -- * Destructuring the response
    DeleteClientVPNEndpointResponse (..),
    mkDeleteClientVPNEndpointResponse,

    -- ** Response lenses
    dcversStatus,
    dcversResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteClientVPNEndpoint' smart constructor.
data DeleteClientVPNEndpoint = DeleteClientVPNEndpoint'
  { -- | The ID of the Client VPN to be deleted.
    clientVPNEndpointId :: Lude.Text,
    -- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
    dryRun :: Lude.Maybe Lude.Bool
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClientVPNEndpoint' with the minimum fields required to make a request.
--
-- * 'clientVPNEndpointId' - The ID of the Client VPN to be deleted.
-- * 'dryRun' - Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
mkDeleteClientVPNEndpoint ::
  -- | 'clientVPNEndpointId'
  Lude.Text ->
  DeleteClientVPNEndpoint
mkDeleteClientVPNEndpoint pClientVPNEndpointId_ =
  DeleteClientVPNEndpoint'
    { clientVPNEndpointId =
        pClientVPNEndpointId_,
      dryRun = Lude.Nothing
    }

-- | The ID of the Client VPN to be deleted.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveClientVPNEndpointId :: Lens.Lens' DeleteClientVPNEndpoint Lude.Text
dcveClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: DeleteClientVPNEndpoint -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: DeleteClientVPNEndpoint)
{-# DEPRECATED dcveClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcveDryRun :: Lens.Lens' DeleteClientVPNEndpoint (Lude.Maybe Lude.Bool)
dcveDryRun = Lens.lens (dryRun :: DeleteClientVPNEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteClientVPNEndpoint)
{-# DEPRECATED dcveDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

instance Lude.AWSRequest DeleteClientVPNEndpoint where
  type Rs DeleteClientVPNEndpoint = DeleteClientVPNEndpointResponse
  request = Req.postQuery ec2Service
  response =
    Res.receiveXML
      ( \s h x ->
          DeleteClientVPNEndpointResponse'
            Lude.<$> (x Lude..@? "status") Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteClientVPNEndpoint where
  toHeaders = Lude.const Lude.mempty

instance Lude.ToPath DeleteClientVPNEndpoint where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteClientVPNEndpoint where
  toQuery DeleteClientVPNEndpoint' {..} =
    Lude.mconcat
      [ "Action" Lude.=: ("DeleteClientVpnEndpoint" :: Lude.ByteString),
        "Version" Lude.=: ("2016-11-15" :: Lude.ByteString),
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId,
        "DryRun" Lude.=: dryRun
      ]

-- | /See:/ 'mkDeleteClientVPNEndpointResponse' smart constructor.
data DeleteClientVPNEndpointResponse = DeleteClientVPNEndpointResponse'
  { -- | The current state of the Client VPN endpoint.
    status :: Lude.Maybe ClientVPNEndpointStatus,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteClientVPNEndpointResponse' with the minimum fields required to make a request.
--
-- * 'status' - The current state of the Client VPN endpoint.
-- * 'responseStatus' - The response status code.
mkDeleteClientVPNEndpointResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteClientVPNEndpointResponse
mkDeleteClientVPNEndpointResponse pResponseStatus_ =
  DeleteClientVPNEndpointResponse'
    { status = Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The current state of the Client VPN endpoint.
--
-- /Note:/ Consider using 'status' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcversStatus :: Lens.Lens' DeleteClientVPNEndpointResponse (Lude.Maybe ClientVPNEndpointStatus)
dcversStatus = Lens.lens (status :: DeleteClientVPNEndpointResponse -> Lude.Maybe ClientVPNEndpointStatus) (\s a -> s {status = a} :: DeleteClientVPNEndpointResponse)
{-# DEPRECATED dcversStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcversResponseStatus :: Lens.Lens' DeleteClientVPNEndpointResponse Lude.Int
dcversResponseStatus = Lens.lens (responseStatus :: DeleteClientVPNEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteClientVPNEndpointResponse)
{-# DEPRECATED dcversResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
