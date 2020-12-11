{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
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
    dcvpneDryRun,
    dcvpneClientVPNEndpointId,

    -- * Destructuring the response
    DeleteClientVPNEndpointResponse (..),
    mkDeleteClientVPNEndpointResponse,

    -- ** Response lenses
    dcvpnersStatus,
    dcvpnersResponseStatus,
  )
where

import Network.AWS.EC2.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteClientVPNEndpoint' smart constructor.
data DeleteClientVPNEndpoint = DeleteClientVPNEndpoint'
  { dryRun ::
      Lude.Maybe Lude.Bool,
    clientVPNEndpointId :: Lude.Text
  }
  deriving stock
    ( Lude.Eq,
      Lude.Ord,
      Lude.Read,
      Lude.Show,
      Lude.Generic
    )
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
    { dryRun = Lude.Nothing,
      clientVPNEndpointId = pClientVPNEndpointId_
    }

-- | Checks whether you have the required permissions for the action, without actually making the request, and provides an error response. If you have the required permissions, the error response is @DryRunOperation@ . Otherwise, it is @UnauthorizedOperation@ .
--
-- /Note:/ Consider using 'dryRun' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpneDryRun :: Lens.Lens' DeleteClientVPNEndpoint (Lude.Maybe Lude.Bool)
dcvpneDryRun = Lens.lens (dryRun :: DeleteClientVPNEndpoint -> Lude.Maybe Lude.Bool) (\s a -> s {dryRun = a} :: DeleteClientVPNEndpoint)
{-# DEPRECATED dcvpneDryRun "Use generic-lens or generic-optics with 'dryRun' instead." #-}

-- | The ID of the Client VPN to be deleted.
--
-- /Note:/ Consider using 'clientVPNEndpointId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpneClientVPNEndpointId :: Lens.Lens' DeleteClientVPNEndpoint Lude.Text
dcvpneClientVPNEndpointId = Lens.lens (clientVPNEndpointId :: DeleteClientVPNEndpoint -> Lude.Text) (\s a -> s {clientVPNEndpointId = a} :: DeleteClientVPNEndpoint)
{-# DEPRECATED dcvpneClientVPNEndpointId "Use generic-lens or generic-optics with 'clientVPNEndpointId' instead." #-}

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
        "DryRun" Lude.=: dryRun,
        "ClientVpnEndpointId" Lude.=: clientVPNEndpointId
      ]

-- | /See:/ 'mkDeleteClientVPNEndpointResponse' smart constructor.
data DeleteClientVPNEndpointResponse = DeleteClientVPNEndpointResponse'
  { status ::
      Lude.Maybe
        ClientVPNEndpointStatus,
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

-- | Creates a value of 'DeleteClientVPNEndpointResponse' with the minimum fields required to make a request.
--
-- * 'responseStatus' - The response status code.
-- * 'status' - The current state of the Client VPN endpoint.
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
dcvpnersStatus :: Lens.Lens' DeleteClientVPNEndpointResponse (Lude.Maybe ClientVPNEndpointStatus)
dcvpnersStatus = Lens.lens (status :: DeleteClientVPNEndpointResponse -> Lude.Maybe ClientVPNEndpointStatus) (\s a -> s {status = a} :: DeleteClientVPNEndpointResponse)
{-# DEPRECATED dcvpnersStatus "Use generic-lens or generic-optics with 'status' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
dcvpnersResponseStatus :: Lens.Lens' DeleteClientVPNEndpointResponse Lude.Int
dcvpnersResponseStatus = Lens.lens (responseStatus :: DeleteClientVPNEndpointResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteClientVPNEndpointResponse)
{-# DEPRECATED dcvpnersResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
