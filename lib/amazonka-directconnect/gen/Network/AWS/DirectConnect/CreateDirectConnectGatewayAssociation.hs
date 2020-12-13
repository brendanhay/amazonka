{-# OPTIONS_GHC -fno-warn-deprecations #-}
{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# OPTIONS_GHC -fno-warn-unused-imports #-}
{-# OPTIONS_GHC -fno-warn-unused-matches #-}

-- Derived from AWS service descriptions, licensed under Apache 2.0.

-- |
-- Module      : Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
-- Copyright   : (c) 2013-2020 Brendan Hay
-- License     : Mozilla Public License, v. 2.0.
-- Maintainer  : Brendan Hay <brendan.g.hay+amazonka@gmail.com>
-- Stability   : auto-generated
-- Portability : non-portable (GHC extensions)
--
-- Creates an association between a Direct Connect gateway and a virtual private gateway. The virtual private gateway must be attached to a VPC and must not be associated with another Direct Connect gateway.
module Network.AWS.DirectConnect.CreateDirectConnectGatewayAssociation
  ( -- * Creating a request
    CreateDirectConnectGatewayAssociation (..),
    mkCreateDirectConnectGatewayAssociation,

    -- ** Request lenses
    cdcgaVirtualGatewayId,
    cdcgaDirectConnectGatewayId,
    cdcgaAddAllowedPrefixesToDirectConnectGateway,
    cdcgaGatewayId,

    -- * Destructuring the response
    CreateDirectConnectGatewayAssociationResponse (..),
    mkCreateDirectConnectGatewayAssociationResponse,

    -- ** Response lenses
    cdcgarsDirectConnectGatewayAssociation,
    cdcgarsResponseStatus,
  )
where

import Network.AWS.DirectConnect.Types
import qualified Network.AWS.Lens as Lens
import qualified Network.AWS.Prelude as Lude
import qualified Network.AWS.Request as Req
import qualified Network.AWS.Response as Res

-- | /See:/ 'mkCreateDirectConnectGatewayAssociation' smart constructor.
data CreateDirectConnectGatewayAssociation = CreateDirectConnectGatewayAssociation'
  { -- | The ID of the virtual private gateway.
    virtualGatewayId :: Lude.Maybe Lude.Text,
    -- | The ID of the Direct Connect gateway.
    directConnectGatewayId :: Lude.Text,
    -- | The Amazon VPC prefixes to advertise to the Direct Connect gateway
    --
    -- This parameter is required when you create an association to a transit gateway.
    -- For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
    addAllowedPrefixesToDirectConnectGateway :: Lude.Maybe [RouteFilterPrefix],
    -- | The ID of the virtual private gateway or transit gateway.
    gatewayId :: Lude.Maybe Lude.Text
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDirectConnectGatewayAssociation' with the minimum fields required to make a request.
--
-- * 'virtualGatewayId' - The ID of the virtual private gateway.
-- * 'directConnectGatewayId' - The ID of the Direct Connect gateway.
-- * 'addAllowedPrefixesToDirectConnectGateway' - The Amazon VPC prefixes to advertise to the Direct Connect gateway
--
-- This parameter is required when you create an association to a transit gateway.
-- For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
-- * 'gatewayId' - The ID of the virtual private gateway or transit gateway.
mkCreateDirectConnectGatewayAssociation ::
  -- | 'directConnectGatewayId'
  Lude.Text ->
  CreateDirectConnectGatewayAssociation
mkCreateDirectConnectGatewayAssociation pDirectConnectGatewayId_ =
  CreateDirectConnectGatewayAssociation'
    { virtualGatewayId =
        Lude.Nothing,
      directConnectGatewayId = pDirectConnectGatewayId_,
      addAllowedPrefixesToDirectConnectGateway = Lude.Nothing,
      gatewayId = Lude.Nothing
    }

-- | The ID of the virtual private gateway.
--
-- /Note:/ Consider using 'virtualGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaVirtualGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
cdcgaVirtualGatewayId = Lens.lens (virtualGatewayId :: CreateDirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {virtualGatewayId = a} :: CreateDirectConnectGatewayAssociation)
{-# DEPRECATED cdcgaVirtualGatewayId "Use generic-lens or generic-optics with 'virtualGatewayId' instead." #-}

-- | The ID of the Direct Connect gateway.
--
-- /Note:/ Consider using 'directConnectGatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaDirectConnectGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation Lude.Text
cdcgaDirectConnectGatewayId = Lens.lens (directConnectGatewayId :: CreateDirectConnectGatewayAssociation -> Lude.Text) (\s a -> s {directConnectGatewayId = a} :: CreateDirectConnectGatewayAssociation)
{-# DEPRECATED cdcgaDirectConnectGatewayId "Use generic-lens or generic-optics with 'directConnectGatewayId' instead." #-}

-- | The Amazon VPC prefixes to advertise to the Direct Connect gateway
--
-- This parameter is required when you create an association to a transit gateway.
-- For information about how to set the prefixes, see <https://docs.aws.amazon.com/directconnect/latest/UserGuide/multi-account-associate-vgw.html#allowed-prefixes Allowed Prefixes> in the /AWS Direct Connect User Guide/ .
--
-- /Note:/ Consider using 'addAllowedPrefixesToDirectConnectGateway' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaAddAllowedPrefixesToDirectConnectGateway :: Lens.Lens' CreateDirectConnectGatewayAssociation (Lude.Maybe [RouteFilterPrefix])
cdcgaAddAllowedPrefixesToDirectConnectGateway = Lens.lens (addAllowedPrefixesToDirectConnectGateway :: CreateDirectConnectGatewayAssociation -> Lude.Maybe [RouteFilterPrefix]) (\s a -> s {addAllowedPrefixesToDirectConnectGateway = a} :: CreateDirectConnectGatewayAssociation)
{-# DEPRECATED cdcgaAddAllowedPrefixesToDirectConnectGateway "Use generic-lens or generic-optics with 'addAllowedPrefixesToDirectConnectGateway' instead." #-}

-- | The ID of the virtual private gateway or transit gateway.
--
-- /Note:/ Consider using 'gatewayId' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgaGatewayId :: Lens.Lens' CreateDirectConnectGatewayAssociation (Lude.Maybe Lude.Text)
cdcgaGatewayId = Lens.lens (gatewayId :: CreateDirectConnectGatewayAssociation -> Lude.Maybe Lude.Text) (\s a -> s {gatewayId = a} :: CreateDirectConnectGatewayAssociation)
{-# DEPRECATED cdcgaGatewayId "Use generic-lens or generic-optics with 'gatewayId' instead." #-}

instance Lude.AWSRequest CreateDirectConnectGatewayAssociation where
  type
    Rs CreateDirectConnectGatewayAssociation =
      CreateDirectConnectGatewayAssociationResponse
  request = Req.postJSON directConnectService
  response =
    Res.receiveJSON
      ( \s h x ->
          CreateDirectConnectGatewayAssociationResponse'
            Lude.<$> (x Lude..?> "directConnectGatewayAssociation")
            Lude.<*> (Lude.pure (Lude.fromEnum s))
      )

instance Lude.ToHeaders CreateDirectConnectGatewayAssociation where
  toHeaders =
    Lude.const
      ( Lude.mconcat
          [ "X-Amz-Target"
              Lude.=# ( "OvertureService.CreateDirectConnectGatewayAssociation" ::
                          Lude.ByteString
                      ),
            "Content-Type"
              Lude.=# ("application/x-amz-json-1.1" :: Lude.ByteString)
          ]
      )

instance Lude.ToJSON CreateDirectConnectGatewayAssociation where
  toJSON CreateDirectConnectGatewayAssociation' {..} =
    Lude.object
      ( Lude.catMaybes
          [ ("virtualGatewayId" Lude..=) Lude.<$> virtualGatewayId,
            Lude.Just
              ("directConnectGatewayId" Lude..= directConnectGatewayId),
            ("addAllowedPrefixesToDirectConnectGateway" Lude..=)
              Lude.<$> addAllowedPrefixesToDirectConnectGateway,
            ("gatewayId" Lude..=) Lude.<$> gatewayId
          ]
      )

instance Lude.ToPath CreateDirectConnectGatewayAssociation where
  toPath = Lude.const "/"

instance Lude.ToQuery CreateDirectConnectGatewayAssociation where
  toQuery = Lude.const Lude.mempty

-- | /See:/ 'mkCreateDirectConnectGatewayAssociationResponse' smart constructor.
data CreateDirectConnectGatewayAssociationResponse = CreateDirectConnectGatewayAssociationResponse'
  { -- | The association to be created.
    directConnectGatewayAssociation :: Lude.Maybe DirectConnectGatewayAssociation,
    -- | The response status code.
    responseStatus :: Lude.Int
  }
  deriving stock (Lude.Eq, Lude.Ord, Lude.Read, Lude.Show, Lude.Generic)
  deriving anyclass (Lude.Hashable, Lude.NFData)

-- | Creates a value of 'CreateDirectConnectGatewayAssociationResponse' with the minimum fields required to make a request.
--
-- * 'directConnectGatewayAssociation' - The association to be created.
-- * 'responseStatus' - The response status code.
mkCreateDirectConnectGatewayAssociationResponse ::
  -- | 'responseStatus'
  Lude.Int ->
  CreateDirectConnectGatewayAssociationResponse
mkCreateDirectConnectGatewayAssociationResponse pResponseStatus_ =
  CreateDirectConnectGatewayAssociationResponse'
    { directConnectGatewayAssociation =
        Lude.Nothing,
      responseStatus = pResponseStatus_
    }

-- | The association to be created.
--
-- /Note:/ Consider using 'directConnectGatewayAssociation' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgarsDirectConnectGatewayAssociation :: Lens.Lens' CreateDirectConnectGatewayAssociationResponse (Lude.Maybe DirectConnectGatewayAssociation)
cdcgarsDirectConnectGatewayAssociation = Lens.lens (directConnectGatewayAssociation :: CreateDirectConnectGatewayAssociationResponse -> Lude.Maybe DirectConnectGatewayAssociation) (\s a -> s {directConnectGatewayAssociation = a} :: CreateDirectConnectGatewayAssociationResponse)
{-# DEPRECATED cdcgarsDirectConnectGatewayAssociation "Use generic-lens or generic-optics with 'directConnectGatewayAssociation' instead." #-}

-- | The response status code.
--
-- /Note:/ Consider using 'responseStatus' with <https://hackage.haskell.org/package/generic-lens generic-lens> or <https://hackage.haskell.org/package/generic-optics generic-optics> instead.
cdcgarsResponseStatus :: Lens.Lens' CreateDirectConnectGatewayAssociationResponse Lude.Int
cdcgarsResponseStatus = Lens.lens (responseStatus :: CreateDirectConnectGatewayAssociationResponse -> Lude.Int) (\s a -> s {responseStatus = a} :: CreateDirectConnectGatewayAssociationResponse)
{-# DEPRECATED cdcgarsResponseStatus "Use generic-lens or generic-optics with 'responseStatus' instead." #-}
