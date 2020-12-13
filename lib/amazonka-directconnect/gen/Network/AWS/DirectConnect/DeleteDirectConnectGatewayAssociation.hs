{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Deletes the association between the specified Direct Connect gateway and virtual private gateway.
--
-- We recommend that you specify the @associationID@ to delete the association. Alternatively, if you own virtual gateway and a Direct Connect gateway association, you can specify the @virtualGatewayId@ and @directConnectGatewayId@ to delete an association.
module Network.AWS.DirectConnect.DeleteDirectConnectGatewayAssociation
  ( -- * Creating a request
    DeleteDirectConnectGatewayAssociation (..),
    mkDeleteDirectConnectGatewayAssociation,

    -- ** Request lenses
    ddcgaVirtualGatewayId,
    ddcgaAssociationId,
    ddcgaDirectConnectGatewayId,

    -- * Destructuring the response
    DeleteDirectConnectGatewayAssociationResponse (..),
    mkDeleteDirectConnectGatewayAssociationResponse,

    -- ** Response lenses
    ddcgarsDirectConnectGatewayAssociation,
    ddcgarsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkDeleteDirectConnectGatewayAssociation' smart constructor.
data DeleteDirectConnectGatewayAssociation = DeleteDirectConnectGatewayAssociation'
  { -- | The ID of the virtual private gateway.
    virtualGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the Direct Connect gateway association.
    associationId :: Lude.Maybe Lude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDirectConnectGatewayAssociation' with the minimum fields required to make a request.
--
-- * 'virtualGatewayId' - The ID of the virtual private gateway.
-- * 'associationId' - The ID of the Direct Connect gateway association.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
mkDeleteDirectConnectGatewayAssociation ::
  DeleteDirectConnectGatewayAssociation
mkDeleteDirectConnectGatewayAssociation =
  DeleteDirectConnectGatewayAssociation'
    { virtualGatewayId =
        Lude.Nothing,
      associationId = Lude.Nothing,
      directConnectGatewayId = Lude.Nothing
    }

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaVirtualGatewayId :: Lens.Lens' DeleteDirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
ddcgaVirtualGatewayId = Lens.lens (virtualGatewayId :: DeleteDirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayId = a} :: DeleteDirectConnectGatewayAssociation)
{-# DEPRECATED ddcgaVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The ID of the Direct Connect gateway association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaAssociationId :: Lens.Lens' DeleteDirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
ddcgaAssociationId = Lens.lens (associationId :: DeleteDirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: DeleteDirectConnectGatewayAssociation)
{-# DEPRECATED ddcgaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgaDirectConnectGatewayId :: Lens.Lens' DeleteDirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
ddcgaDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: DeleteDirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {directConnectGatewayId = a} :: DeleteDirectConnectGatewayAssociation)
{-# DEPRECATED ddcgaDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

instance Lude.AWSRequest DeleteDirectConnectGatewayAssociation where
  type
    Rs DeleteDirectConnectGatewayAssociation =
      DeleteDirectConnectGatewayAssociationResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          DeleteDirectConnectGatewayAssociationResponse'
            Lude.<$> (x Lude..?> "directConnectGatewayAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders DeleteDirectConnectGatewayAssociation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.DeleteDirectConnectGatewayAssociation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON DeleteDirectConnectGatewayAssociation where
  toJSON DeleteDirectConnectGatewayAssociation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("virtualGatewayId" Lude..=) Lude.<$> virtualGatewayId,
            ("associationId" Lude..=) Lude.<$> associationId,
            ("directConnectGatewayId" Lude..=)
              Lude.<$> directConnectGatewayId
          ]
      )

instance Lude.ToPath DeleteDirectConnectGatewayAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery DeleteDirectConnectGatewayAssociation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkDeleteDirectConnectGatewayAssociationResponse' smart constructor.
data DeleteDirectConnectGatewayAssociationResponse = DeleteDirectConnectGatewayAssociationResponse'
  { -- | Information about the deleted association.
    directConnectGatewayAssociation :: Lude.Maybe DirectConnectGatewayAssociation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'DeleteDirectConnectGatewayAssociationResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayAssociation' - Information about the deleted association.
-- * 'responseStatus' - The response status code.
mkDeleteDirectConnectGatewayAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  DeleteDirectConnectGatewayAssociationResponse
mkDeleteDirectConnectGatewayAssociationResponse pResponseStatus_ =
  DeleteDirectConnectGatewayAssociationResponse'
    { directConnectGatewayAssociation =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Information about the deleted association.
--
-- /Note:/ Consider using 'directConnectGatewayAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgarsDirectConnectGatewayAssociation :: Lens.Lens' DeleteDirectConnectGatewayAssociationResponse (Lude.Maybe DirectConnectGatewayAssociation)
ddcgarsDirectConnectGatewayAssociation = Lens.lens (directConnectGatewayAssociation :: DeleteDirectConnectGatewayAssociationResponse -> Lude.Maybe DirectConnectGatewayAssociation) (\s a -> s {directConnectGatewayAssociation = a} :: DeleteDirectConnectGatewayAssociationResponse)
{-# DEPRECATED ddcgarsDirectConnectGatewayAssociation "Use generic-lens or generic-optics with 'directConnectGatewayAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
ddcgarsResponseStatus :: Lens.Lens' DeleteDirectConnectGatewayAssociationResponse Lude.Int
ddcgarsResponseStatus = Lens.lens (responseStatus :: DeleteDirectConnectGatewayAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: DeleteDirectConnectGatewayAssociationResponse)
{-# DEPRECATED ddcgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
