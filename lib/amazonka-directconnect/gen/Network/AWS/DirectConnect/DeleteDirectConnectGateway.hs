{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGateway
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the specified Direct Connect gateway. You must first delete all virtual interfaces that are attached to the Direct Connect gateway and disassociate all virtual private gateways associated with the Direct Connect gateway.
module Network.AWS.DirectConnect.DeleteDirectConnectGateway
  ( -- * Creating a request
    DeleteDirectConnectGateway (..),
    mkDeleteDirectConnectGateway,

    -- ** Request lenses
    ddcgfDirectConnectGatewayId,

    -- * Destructuring the response
    DeleteDirectConnectGatewayResponse (..),
    mkDeleteDirectConnectGatewayResponse,

    -- ** Response lenses
    ddcgfrsDirectConnectGateway,
    ddcgfrsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDirectConnectGateway' smart constructor.
newtype DeleteDirectConnectGateway = DeleteDirectConnectGateway'
  { -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving newtype (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDirectConnectGateway' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
mkDeleteDirectConnectGateway ::
  -- | 'directConnectGatewayId'
  Lude.Text ->
  DeleteDirectConnectGateway
mkDeleteDirectConnectGateway pDirectConnectGatewayId_ =
  DeleteDirectConnectGateway'
    { directConnectGatewayId =
        pDirectConnectGatewayId_
    }

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgfDirectConnectGatewayId :: Lens.Lens' DeleteDirectConnectGateway Lude.Text
ddcgfDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: DeleteDirectConnectGateway -> Lude.Text) (\s a -> s {directConnectGatewayId = a} :: DeleteDirectConnectGateway)
{-# DEPRECATED ddcgfDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

instance Lude.AWSRequest DeleteDirectConnectGateway where
  type
    Rs DeleteDirectConnectGateway =
      DeleteDirectConnectGatewayResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayResponse'
            Lude.<$> (x Lude..?> "directConnectGateway")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDirectConnectGateway where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ("OvertureService.DeleteDirectConnectGateway" :: Lude.ByteString),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDirectConnectGateway where
  toJSON DeleteDirectConnectGateway' {..} =
    Lude.object
      ( Lude.catMaybes
          [ Lude.Just
              ("directConnectGatewayId" Lude..= directConnectGatewayId)
          ]
      )

instance Lude.ToPath DeleteDirectConnectGateway where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDirectConnectGateway where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDirectConnectGatewayResponse' smart constructor.
data DeleteDirectConnectGatewayResponse = DeleteDirectConnectGatewayResponse'
  { -- | The Direct Connect gateway.
    directConnectGateway :: Lude.Maybe DirectConnectGateway,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDirectConnectGatewayResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGateway' - The Direct Connect gateway.
-- * 'responseStatus' - The response status code.
mkDeleteDirectConnectGatewayResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDirectConnectGatewayResponse
mkDeleteDirectConnectGatewayResponse pResponseStatus_ =
  DeleteDirectConnectGatewayResponse'
    { directConnectGateway =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgfrsDirectConnectGateway :: Lens.Lens' DeleteDirectConnectGatewayResponse (Lude.Maybe DirectConnectGateway)
ddcgfrsDirectConnectGateway = Lens.lens (directConnectGateway :: DeleteDirectConnectGatewayResponse -> Lude.Maybe DirectConnectGateway) (\s a -> s {directConnectGateway = a} :: DeleteDirectConnectGatewayResponse)
{-# DEPRECATED ddcgfrsDirectConnectGateway "Use generic-lens or generic-optics with 'directConnectGateway' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgfrsResponseStatus :: Lens.Lens' DeleteDirectConnectGatewayResponse Lude.Int
ddcgfrsResponseStatus = Lens.lens (responseStatus :: DeleteDirectConnectGatewayResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDirectConnectGatewayResponse)
{-# DEPRECATED ddcgfrsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
