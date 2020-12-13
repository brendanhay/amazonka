{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Updates the specified attributes of the Direct Connect gateway association.
--
-- Add or remove prefixes from the association.
module Network.AWS.DirectConnect.UpdateDirectConnectGatewayAssociation
  ( -- * Creating a request
    UpdateDirectConnectGatewayAssociation (..),
    mkUpdateDirectConnectGatewayAssociation,

    -- ** Request lenses
    udcgaAssociationId,
    udcgaAddAllowedPrefixesToDirectConnectGateway,
    udcgaRemoveAllowedPrefixesToDirectConnectGateway,

    -- * Destructuring the response
    UpdateDirectConnectGatewayAssociationResponse (..),
    mkUpdateDirectConnectGatewayAssociationResponse,

    -- ** Response lenses
    udcgarsDirectConnectGatewayAssociation,
    udcgarsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkUpdateDirectConnectGatewayAssociation' smart constructor.
data UpdateDirectConnectGatewayAssociation = UpdateDirectConnectGatewayAssociation'
  { -- | The ID of the Direct Connect gateway association.
    associationId :: Lude.Maybe Lude.Text,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
    addAllowedPrefixesToDirectConnectGateway :: Lude.Maybe [RouteFilterPrefix],
    -- | The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
    removeAllowedPrefixesToDirectConnectGateway :: Lude.Maybe [RouteFilterPrefix]
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDirectConnectGatewayAssociation' with the minimum fields required to make a request.
--
-- * 'associationId' - The ID of the Direct Connect gateway association.
-- * 'addAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway.
-- * 'removeAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
mkUpdateDirectConnectGatewayAssociation ::
  UpdateDirectConnectGatewayAssociation
mkUpdateDirectConnectGatewayAssociation =
  UpdateDirectConnectGatewayAssociation'
    { associationId =
        Lude.Nothing,
      addAllowedPrefixesToDirectConnectGateway = Lude.Nothing,
      removeAllowedPrefixesToDirectConnectGateway =
        Lude.Nothing
    }

-- | The ID of the Direct Connect gateway association.
--
-- /Note:/ Consider using 'associationId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgaAssociationId :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
udcgaAssociationId = Lens.lens (associationId :: UpdateDirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {associationId = a} :: UpdateDirectConnectGatewayAssociation)
{-# DEPRECATED udcgaAssociationId "Use generic-lens or generic-optics with 'associationId' instead." #-}

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'addAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgaAddAllowedPrefixesToDirectConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Lude.Maybe [RouteFilterPrefix])
udcgaAddAllowedPrefixesToDirectConnectGateway = Lens.lens (addAllowedPrefixesToDirectConnectGateway :: UpdateDirectConnectGatewayAssociation -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {addAllowedPrefixesToDirectConnectGateway = a} :: UpdateDirectConnectGatewayAssociation)
{-# DEPRECATED udcgaAddAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'addAllowedPrefixesToDirectConnectGateway' instead." #-}

-- | The Amazon VPC prefixes to no longer advertise to the Direct Connect gateway.
--
-- /Note:/ Consider using 'removeAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgaRemoveAllowedPrefixesToDirectConnectGateway :: Lens.Lens' UpdateDirectConnectGatewayAssociation (Lude.Maybe [RouteFilterPrefix])
udcgaRemoveAllowedPrefixesToDirectConnectGateway = Lens.lens (removeAllowedPrefixesToDirectConnectGateway :: UpdateDirectConnectGatewayAssociation -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {removeAllowedPrefixesToDirectConnectGateway = a} :: UpdateDirectConnectGatewayAssociation)
{-# DEPRECATED udcgaRemoveAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'removeAllowedPrefixesToDirectConnectGateway' instead." #-}

instance Lude.AWSRequest UpdateDirectConnectGatewayAssociation where
  type
    Rs UpdateDirectConnectGatewayAssociation =
      UpdateDirectConnectGatewayAssociationResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          UpdateDirectConnectGatewayAssociationResponse'
            Lude.<$> (x Lude..?> "directConnectGatewayAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders UpdateDirectConnectGatewayAssociation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.UpdateDirectConnectGatewayAssociation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON UpdateDirectConnectGatewayAssociation where
  toJSON UpdateDirectConnectGatewayAssociation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("associationId" Lude..=) Lude.<$> associationId,
            ("addAllowedPrefixesToDirectConnectGateway" Lude..=)
              Lude.<$> addAllowedPrefixesToDirectConnectGateway,
            ("removeAllowedPrefixesToDirectConnectGateway" Lude..=)
              Lude.<$> removeAllowedPrefixesToDirectConnectGateway
          ]
      )

instance Lude.ToPath UpdateDirectConnectGatewayAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery UpdateDirectConnectGatewayAssociation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkUpdateDirectConnectGatewayAssociationResponse' smart constructor.
data UpdateDirectConnectGatewayAssociationResponse = UpdateDirectConnectGatewayAssociationResponse'
  { directConnectGatewayAssociation :: Lude.Maybe DirectConnectGatewayAssociation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'UpdateDirectConnectGatewayAssociationResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayAssociation' -
-- * 'responseStatus' - The response status code.
mkUpdateDirectConnectGatewayAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  UpdateDirectConnectGatewayAssociationResponse
mkUpdateDirectConnectGatewayAssociationResponse pResponseStatus_ =
  UpdateDirectConnectGatewayAssociationResponse'
    { directConnectGatewayAssociation =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | Undocumented field.
--
-- /Note:/ Consider using 'directConnectGatewayAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgarsDirectConnectGatewayAssociation :: Lens.Lens' UpdateDirectConnectGatewayAssociationResponse (Lude.Maybe DirectConnectGatewayAssociation)
udcgarsDirectConnectGatewayAssociation = Lens.lens (directConnectGatewayAssociation :: UpdateDirectConnectGatewayAssociationResponse -> Lude.Maybe DirectConnectGatewayAssociation) (\s a -> s {directConnectGatewayAssociation = a} :: UpdateDirectConnectGatewayAssociationResponse)
{-# DEPRECATED udcgarsDirectConnectGatewayAssociation "Use generic-lens or generic-optics with 'directConnectGatewayAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
udcgarsResponseStatus :: Lens.Lens' UpdateDirectConnectGatewayAssociationResponse Lude.Int
udcgarsResponseStatus = Lens.lens (responseStatus :: UpdateDirectConnectGatewayAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: UpdateDirectConnectGatewayAssociationResponse)
{-# DEPRECATED udcgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
